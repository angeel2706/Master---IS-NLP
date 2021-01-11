library(stats)
library(tidytext)
library(NLP)
library(tm)
library(ggplot2)
library(dplyr)

doc <- VCorpus(DirSource(pattern = "speech_obama.txt"))
doc <- tm_map(doc, removePunctuation)
doc <- tm_map(doc, content_transformer(tolower))

t_doc <- doc %>% tidy()
corpus <- t_doc %>% 
  select(id, text)
tidy_df <- corpus %>%
  unnest_tokens(word, text)
tidy_df
tf <- tidy_df %>%
  count(word, sort = TRUE) %>%
  head(10) %>%
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion))
tf
tidy_df %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

tidy_df <- tidy_df %>%
  count(id, word, sort = TRUE)

tidy_df
tidy_df <- tidy_df %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(idf))
tidy_df
get_stopwords()

tidy_df_sw <- tidy_df %>%
  anti_join(get_stopwords())
tidy_df_sw
tidy_df_sw %>%
  head(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

"WITH ALL DOCUMENTS"

docs <- VCorpus(DirSource("./IS NLP/"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))

t_docs <- docs %>% tidy()
corpus1 <- t_docs %>% 
  select(id, text)
tidy_df1 <- corpus1 %>%
  unnest_tokens(word, text)
tidy_df1

"Counting" 

tf1 <- tidy_df1 %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion))
tf1
tidy_df1_sw <- tidy_df1 %>%
  anti_join(get_stopwords())
tf1_sw <- tidy_df1_sw %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(proportion = n / sum(n)) %>%
  arrange(desc(proportion))
tf1_sw
"Counting cleaned"
tidy_df1 %>%
  count(word, sort = TRUE) %>%
  head(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


"TF-IDF"
tidy_df1_TFIDF <- tidy_df1 %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf))
tidy_df1_TFIDF
get_stopwords()
tidy_df1_TFIDF_sw <- tidy_df1_TFIDF %>%
  anti_join(get_stopwords())

"FINAL RESULTS"

tidy_df1_TFIDF_sw %>%
  arrange(desc(n))

"TF TABLE"
tf_table_final <- tidy_df1_TFIDF_sw %>%
  arrange(desc(tf))
tf_table_final <- tf_table_final %>%
  filter(word!="s")
tf_table_final <- tf_table_final %>%
  filter(word!="ll")
tf_table_final <- tf_table_final %>%
  filter(word!="weâ")

           
"IDF TABLE"
idf_table_final <- tidy_df1_TFIDF_sw %>%
  arrange(desc(idf))
idf_table_final <- idf_table_final%>%
  filter(idf<=2)

"TF-IDF TABLE"
tfidf_table_final <- tidy_df1_TFIDF_sw %>%
  arrange(desc(tf_idf))
tfidf_table_final <-tfidf_table_final %>%
  filter(word!="s") 
tfidf_table_final <-tfidf_table_final %>%
  filter(word!="ll") 
tfidf_table_final <-tfidf_table_final %>%
  filter(word!="weâ") 
tfidf_table_final <-tfidf_table_final %>%
  filter(idf<=2)


  
  


  



