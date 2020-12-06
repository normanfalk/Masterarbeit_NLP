#Packages: 
library(data.table)
library(dplyr)
library(tidytext)
library(SnowballC)
library(topicmodels)
library(tm)

#Liste der Daten:
path <- "//192.168.178.187/Shared Files/Dokumente/Masterarbeit Python/"
new_files <- list.files(path, pattern = "*.csv")
archive_files <- list.files(path, pattern = "*4.csv") #4, um die alten Dateien zu ignorieren. 
files <- c(new_files, archive_files)

#Einlesen der Daten: 
data <- data.frame()
for (i in 1:length(files)) {
  temp_data <- data.table::fread(paste0(path,files[i]), sep = ";", encoding = "UTF-8")[,c(2,8,17),]
  data <- rbind(data, temp_data)
}

data <- tibble(text = data$Text, submission = data$submission_id, comment_id = data$comment_id)

#Cleaning der Daten: tolower, unnest_token, stemming:
data2 <- data %>% 
  unnest_tokens(output = "word", input = text, token = "words") %>% 
  anti_join(stop_words) %>% 
  mutate(word = wordStem(word)) %>% 
  count(submission, word) %>% #Hinweis an der Stelle: Es sind bis dato noch Links etc. integriert, die wohl langfristig herausgenommen werden sollten. 
  cast_dtm(document = submission, term = word, value = n, weighting = weightTf)

cscq_lda <- LDA(data2, k = 4, method = 'Gibbs',
                control = list(seed = 1111))


cscq_lda_betas <-    tidy(cscq_lda, matrix = "beta")
cscq_lda_betas %>%  group_by(topic) %>%  top_n(10, beta) %>%  arrange(topic, -beta) %>%  filter(topic == 1)


cscq_lda_gammas <-    tidy(cscq_lda, matrix = "gamma")
cscq_lda_gammas %>%  group_by(topic) %>%  top_n(10, gamma) %>%  arrange(topic, -gamma) %>%  filter(topic == 3)

#Exploratives Vorgehen, wie viele Topics am ehesten diskutiert werden. 
sample_size <- floor(0.90 * nrow(data2))
set.seed(1111)
train_ind <- sample(nrow(data2), size = sample_size)
train <- data2[train_ind, ]
test <- data2[-train_ind, ]
values = c()
for(i in c(2:10)){
  svMisc::progress(i, progress.bar = T)
  lda_model <- LDA(train, k = i, method = "Gibbs",
                   control = list(seed = 1111))  
  values <- c(values, perplexity(lda_model, newdata = test))
}

perplexity(cscq_lda)
