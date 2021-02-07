#Packages: 
library(data.table)
library(dplyr)
library(tidytext)
library(SnowballC)
library(topicmodels)
library(tm)
library(stringr)


#Liste der Daten:
# path <- "//192.168.178.187/Shared Files/Dokumente/Masterarbeit Python/"
# new_files <- list.files(path, pattern = "*.csv")
# archive_files <- list.files(path, pattern = "*4.csv") #4, um die alten Dateien zu ignorieren. 
# files <- c(new_files, archive_files)

#Einlesen der Daten: 
data <- readRDS("all_comments.rds")
data <- tibble(text = data$Text, submission = data$submission_id, comment_id = data$comment_id)
data <- data %>% 
  group_by(comment_id) %>%
  slice(1) %>% 
  ungroup()

#Cleaning der Daten, sodass keine Links mehr auftauchen. 
data$text <- gsub("?(f|ht)tp(s?)\\S+\\s*", "", data$text)

#Cleaning der Daten: tolower, unnest_token, stemming:
data2 <- data %>% 
  unnest_tokens(output = "word", input = text, token = "words") %>% 
  mutate(word= str_replace_all(string = word, pattern = "_", replacement = "")) %>% 
  anti_join(stop_words) %>% 
  mutate(word = wordStem(word)) %>% 
  count(submission, word) %>% 
  cast_dfm(document = submission, term = word, value = n) %>% 
  dfm_trim(min_docfreq = 0.01, max_docfreq = 0.99, docfreq_type = "prop") 

sel_idx <- rowSums(data2) > 0
data2<- data2[sel_idx, ]

data2 <- convert(data2, to = "tm")

#Exploratives Vorgehen, wie viele Topics am ehesten diskutiert werden. 
sample_size <- floor(0.90 * nrow(data2))
set.seed(1111)
train_ind <- sample(nrow(data2), size = sample_size)
train <- data2[train_ind, ]
test <- data2[-train_ind, ]
values = c()
value_2 = c()
for(i in c(2:20)){
  svMisc::progress(i,20)
  lda_model <- LDA(train, k = i, method = "Gibbs",
                   control = list(seed = 1111))  
  values <- c(values, perplexity(lda_model, newdata = test))
  value_2 <- c(value_2, mean(topic_coherence(lda_model, train)))
}

perp_score <- data.frame(n = 2:20, value = values)
coherence_score <- data.frame(n = 2:20, value = value_2)

plot(perp_score)

diagnostic_tc <- NULL
for(i in c(6:11)){
  svMisc::progress(i,12)
  lda_model <- LDA(data2, k = i, method = "Gibbs",
                   control = list(seed = 1111))  
  diagnostic_tm_temp <- topic_diagnostics(topic_model = lda_model, dtm_data = data2)
  diagnostic_tc <- rbind(diagnostic_tc, diagnostic_tm_temp)
}
write.csv2(diagnostic_tc, "tc_diagnostic_2.csv")

#Coherence sagt, dass fünf Topics am besten wären, Evaluation in manueller Form erfolgt: 
lda_model_6 <- LDA(data2, k = 6, method = "Gibbs",
                 control = list(seed = 1111))
lda_model_7 <- LDA(data2, k = 7, method = "Gibbs",
                 control = list(seed = 1111))  
lda_model_8 <- LDA(data2, k = 8, method = "Gibbs",
                 control = list(seed = 1111))  

#Lege mich fest auf k=6. Suche nach optimalem Alpha: 
cs_value <- NULL
for(i in c(1:10)){
  svMisc::progress(i,10)
  lda_model <- LDA(data2, k = 6, method = "Gibbs",
                   control = list(seed = 1111, alpha = as.numeric(i) )) 
  cs_value <- c(cs_value, mean(topic_coherence(lda_model, data2)))
}

best_coherence <- data.frame(n = 1:10, value = cs_value)



betas <-    tidy(lda_model_6, matrix = "beta")
gammas <-    tidy(lda_model_6, matrix = "gamma")
beta_terms <- betas %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

#Erst einmal testweise mit 7 Topics arbeiten: 
cscq_lda <- LDA(data2, k = 6, method = 'Gibbs',
                control = list(seed = 1111, alpha = 3))

cscq_lda_betas <-    tidy(cscq_lda, matrix = "beta")
cscq_lda_gammas <-    tidy(cscq_lda, matrix = "gamma")
top_terms <- cscq_lda_betas %>%
  filter(term != "job" & term != "compani") %>% 
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_docs <- cscq_lda_gammas %>% 
  group_by(topic) %>% 
  arrange(topic) %>% 
  top_n(gamma, n = 10)

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
