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

#Exploratives Vorgehen, wie viele Topics am ehesten diskutiert werden. 
sample_size <- floor(0.90 * nrow(data2))
set.seed(1111)
train_ind <- sample(nrow(data2), size = sample_size)
train <- data2[train_ind, ]
test <- data2[-train_ind, ]
values = c()
for(i in c(2:15)){
  svMisc::progress(i,15)
  lda_model <- LDA(train, k = i, method = "Gibbs",
                   control = list(seed = 1111))  
  values <- c(values, perplexity(lda_model, newdata = test))
}

perp_score <- data.frame(n = 2:15, value = values)

library(ggplot2)
perp_score %>% 
  ggplot()+
  geom_point()+
  geom_line()+
  aes(n,value)

#Erst einmal testweise mit 7 Topics arbeiten: 
cscq_lda <- LDA(data2, k = 7, method = 'Gibbs',
                control = list(seed = 1111))

cscq_lda_betas <-    tidy(cscq_lda, matrix = "beta")
cscq_lda_gammas <-    tidy(cscq_lda, matrix = "gamma")
top_terms <- cscq_lda_betas %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>% 
  ggplot(aes(x=term, y = beta, fill = topic))+
  geom_col(aes(fill=as.factor(topic)))+
  scale_x_reordered() +
  facet_wrap(~topic, scales = "free")+
  coord_flip()

matching_table <- cscq_lda_gammas %>% 
  group_by(document) %>% 
  arrange(desc(gamma)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(c(1:2)) %>% 
  rename(submission = document)

data_join <- left_join(data, matching_table)

saveRDS(data_join, "Comments_by_model.rds")
saveRDS(cscq_lda, "LDA.rds")
