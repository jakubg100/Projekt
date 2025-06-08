knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
)
library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)
library(e1071)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(readr)
library(dplyr)
library(SnowballC)

# FUNCJE DO POBIERANIA NAJCZĘSTSZYCH SŁÓW DLA TEMATÓW LDA ----

top_terms_by_topic_LDA <- function(input_text, 
                                   plot = TRUE,
                                   k = number_of_topics) 
{    
  corpus <- VCorpus(VectorSource(input_text))
  DTM <- DocumentTermMatrix(corpus)
  
  unique_indexes <- unique(DTM$i) 
  DTM <- DTM[unique_indexes,]   
  
  # wykonaj LDA
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta") # pobierz słowa/tematy w uporządkowanym formacie tidy
  
  # pobierz dziesięć najczęstszych słów dla każdego tematu
  top_terms <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) # uporządkuj słowa w malejącej kolejności informatywności
  
  
  
  # rysuj wykres (domyślnie plot = TRUE)
  if(plot == T){
    # dziesięć najczęstszych słów dla każdego tematu
    top_terms %>%
      mutate(term = reorder(term, beta)) %>% # posortuj słowa według wartości beta 
      ggplot(aes(term, beta, fill = factor(topic))) + # rysuj beta według tematu
      geom_col(show.legend = FALSE) + # wykres kolumnowy
      facet_wrap(~ topic, scales = "free") + # każdy temat na osobnym wykresie
      labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
      coord_flip() +
      theme_minimal() +
      scale_fill_brewer(palette = "Set1")
  }else{ 
    # jeśli użytkownik nie chce wykresu
    # wtedy zwróć listę posortowanych słów
    return(top_terms)
  }
  
  
}

# WCZYTANIE DANYCH ----

data <- read.csv("recenzje.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
data <- select(data, -1)
colnames(data) <- c("Review_Text", "Worth_Watching")

# CZYSZCZENIE DANYCH ----

corpus <- VCorpus(VectorSource(data$Review_Text))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "@\\w+")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}")
corpus <- tm_map(corpus, toSpace, "(s?)(f|ht)tp(s?)://\\S+\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*")
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)")
corpus <- tm_map(corpus, toSpace, "www")
corpus <- tm_map(corpus, toSpace, "~")
corpus <- tm_map(corpus, toSpace, "â€“")
# usunięcie elementów w nawiasach, po upewnieniu się, że nie zawierają one istotnych informacji, jednak z uwagą na to że rozszerzając dane o inne recenzje mogą one takie zawierać
# jednak w tym kontekście ich duża ilość jako odniesienia do innych produkcji powoduje problemy przy interpretacji wyników przy wykorzystaniu dużej ilości czasu do ręcznego bądź algorytmicznego przesortowania danych z recenzji i zawartości nawiasów
# propozycją algorytmiczną byłoby pobranie filmów aktorów i reżyserów mających udzia w tym filmie i wykorzystanie ich do usunięcia odpowiadających im elementów, ale pozostaje problem filmów które mogą być używane jako porównanie w treści recenzji
removeQuotes <- content_transformer(function(x) gsub('"[^"]*"', "", x))
corpus <- tm_map(corpus, removeQuotes)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("’", "'", x)))
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[–—]", " ", x)))
removePossessive <- content_transformer(function(x) {
  gsub("'s\\b", "", x)
})
corpus <- tm_map(corpus, removePossessive)
corpus <- tm_map(corpus, removeNumbers)
replacePunctWithSpace <- content_transformer(function(x) gsub("[[:punct:]]", " ", x))
corpus <- tm_map(corpus, replacePunctWithSpace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# usunięcie słów specyficznych dla filmu, odnoszących się do jednostek oraz nie związanych z tą ekranizacją
# dodatkowo usunięcie słów ogólnych, które mogą pojawiać się w recenzjach filmowych

wlasne_stopwords <- readLines("wstop.txt", encoding = "UTF-8")

ogolne <- c("film", "movie", "scene", "scenes", "action", "character", "characters",
            "story", "plot", "cinema", "hollywood", "director", "performance",
            "sequel", "series", "installment", "blockbuster", "blockbusters", "franchise",
            "watch", "viewing", "runtime", "review", "reviewer", "actor", "actress", "read", "more", "comment", "article", "posted", "author", "source", "stunts", "franchise", "cinematic", "viewers", "audience"
            , "like", "can", "just", "also", "first", "two", "films", "movies", "II", "I", "last", "final", "sequence", "sequences", "make", "get", "feel", "will", "set", "transformers", "talked", "about", "say", "said", "think", "thought", "know", "believe", "believed", "see", "seen", "looked", "look", "watching", "watched", "watcher", "watchers", "viewer", "viewers", "conversation")
corpus <- tm_map(corpus, removeWords, c(wlasne_stopwords, ogolne))
corpus <- tm_map(corpus, stripWhitespace)
corpus[[1]]
corpus[[1]][[1]]

# STEMMING ----


corpus_copy <- corpus
corpus_stemmed <- tm_map(corpus, stemDocument)


corpus[[1]][[1]]
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") 
  paste(x, collapse = " ")                       
})

corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

# usuń NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)

# użycie corpus_completed do chmury słów i LDA dla czytelności wyników

tdm <- TermDocumentMatrix(corpus_completed)
tdm <- removeSparseTerms(tdm, 0.99)
tdm

tdm_m <- as.matrix(tdm)
v <- sort(rowSums(tdm_m), decreasing = TRUE)
tdm_df <- data.frame(word = names(v), freq = v)
head(tdm_df, 10)

# CHMURA SŁÓW ----
wordcloud(words = tdm_df$word, freq = tdm_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))

# LDA ----

# sprawdźmy dla różnej ilości tematów

number_of_topics = 4
top_terms_by_topic_LDA(tdm_df$word)
number_of_topics = 5
top_terms_by_topic_LDA(tdm_df$word)
number_of_topics = 6
top_terms_by_topic_LDA(tdm_df$word)
number_of_topics = 7
top_terms_by_topic_LDA(tdm_df$word)

# TF-IDF ----

tdm_tfidf <- TermDocumentMatrix(corpus_completed,
                                control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

tdm_tfidf
tdm_tfidf_m <- as.matrix(tdm_tfidf)
v_tfidf <- sort(rowSums(tdm_tfidf_m), decreasing = TRUE)
tdm_tfidf_df <- data.frame(word = names(v_tfidf), freq = v_tfidf)
head(tdm_tfidf_df, 10)

# CHMURA SŁÓW TF-IDF ----
wordcloud(words = tdm_tfidf_df$word, freq = tdm_tfidf_df$freq, min.freq = 7, 
          colors = brewer.pal(8, "Dark2"))

# SVM ----

dtm_df <- as.data.frame(t(tdm_tfidf_m))
dtm_df$Worth_Watching <- factor(data$Worth_Watching, levels = c("no", "yes"))
dim(dtm_df)


yes_class <- dtm_df[dtm_df$Worth_Watching == "yes", ]
no_class  <- dtm_df[dtm_df$Worth_Watching == "no",  ]

set.seed(123)
yes_train_indices <- sample(1:nrow(yes_class), size = floor(0.8 * nrow(yes_class)))
no_train_indices  <- sample(1:nrow(no_class),  size = floor(0.8 * nrow(no_class)))


trainData <- rbind(yes_class[yes_train_indices, ], no_class[no_train_indices, ])
testData  <- rbind(yes_class[-yes_train_indices, ], no_class[-no_train_indices, ])
svm_model <- svm(Worth_Watching ~ ., data = trainData, kernel = "linear", probability = TRUE)
predictions <- predict(svm_model, newdata = testData)
non_numeric <- sapply(trainData[, -ncol(trainData)], function(col) !is.numeric(col))
if (any(non_numeric)) {
  print("Some predictors are not numeric!")
  print(names(trainData)[which(non_numeric)])
}

str(trainData)
# Macierz pomyłek (confusion matrix)
confusion_matrix <- table(Predicted = predictions, Actual = testData$Worth_Watching)
print(confusion_matrix)
TP <- confusion_matrix["yes", "yes"]
TN <- confusion_matrix["no", "no"]
FP <- confusion_matrix["yes", "no"]
FN <- confusion_matrix["no", "yes"]


cat("\nTrue Positives (TP):", TP,
    "\nTrue Negatives (TN):", TN,
    "\nFalse Positives (FP):", FP,
    "\nFalse Negatives (FN):", FN, "\n")

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
specificity <- TN / (TN + FP)
accuracy <- (TP + TN) / sum(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)


cat("\nAccuracy:", round(accuracy, 2),
    "\nPrecision (dla 'yes'):", round(precision, 2),
    "\nRecall (dla 'yes'):", round(recall, 2),
    "\nSpecificity (dla 'yes'):", round(specificity, 2),
    "\nF1 Score:", round(f1_score, 2), "\n")
metrics_df <- data.frame(
  Metric = c("Accuracy", "Precision", "Recall", "Specificity", "F1 Score"),
  Value = c(accuracy, precision, recall, specificity, f1_score)
)


ggplot(metrics_df, aes(x = Metric, y = Value, fill = Metric)) +
  geom_col(width = 0.5, color = "black") +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5, size = 5) +
  ylim(0, 1) +
  labs(title = "Metryka", y = "Wartość", x = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
confusion_df <- as.data.frame(as.table(confusion_matrix))

# Tworzenie etykiet dla pól macierzy
confusion_df$Label <- c("True Negative (TN)", "False Positive (FP)", 
                        "False Negative (FN)", "True Positive (TP)")


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5) +
  scale_fill_gradient(low = "white", high = "steelblue", name= "Count") +
  labs(title = "Confusion Matrix", fill = "Count") +
  theme_minimal(base_size = 14)

confusion_df$Correctness <- ifelse(confusion_df$Label %in% c("True Positive (TP)", "True Negative (TN)"),
                                   "Correct", "Incorrect")


# Przypisanie kolorów do typów błędów
confusion_df$FillColor <- case_when(
  confusion_df$Label == "True Positive (TP)" ~ "forestgreen",
  confusion_df$Label == "True Negative (TN)" ~ "forestgreen",
  confusion_df$Label == "False Positive (FP)" ~ "orange",
  confusion_df$Label == "False Negative (FN)" ~ "red"
)


# Wiersze to Predicted (prognozowane), kolumny to Actual (rzeczywiste)
# z przypisanymi kolorami i etykietami
ggplot(confusion_df, aes(x = Actual, y = Predicted, fill = FillColor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste(Label, "\n", Freq)), size = 5, fontface = "bold") +
  scale_fill_identity(guide = "legend",
                      breaks = c("forestgreen", "orange", "red"),
                      labels = c("TP / TN (Correct)",
                                 "False Positive (Incorrect)",
                                 "False Negative (Incorrect)"),
                      name = "Wynik klasyfikacji") +
  labs(title = "Confusion Matrix") +
  theme_minimal(base_size = 14)

