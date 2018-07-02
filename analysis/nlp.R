
##### START NLP #####

# lets get the text first
# --> so we can analyse the sentiment score per website
sql_statement_text = "SELECT article.id, article.content_text, article.website_id, website.name FROM ba.article INNER JOIN ba.website ON article.website_id=website.id"

articles_text <- con %>% tbl(sql(sql_statement_text)) %>% collect()

# change up the encoding to prevent umlaut problems
Encoding(articles_text$content_text) <- "UTF-8"
Encoding(articles_text$name) <- "UTF-8"

tidy_articles_text <- articles_text %>%
  unnest_tokens(word, content_text) %>%
  drop_na() %>%
  as.tibble()

tidy_articles_text %>% count(word, sort = TRUE)

tidy_articles_text %>% group_by(name) %>% summarise(n()) %>% View()

# get stop word (en and ger)
# watch out: utf-encoding (!)
stop_words <- read.delim("c:/hdm/bachelorarbeit/analysis/text_mining/resources/german_stopwords_plain.txt", header=FALSE, stringsAsFactors=FALSE)
stop_words_en <- read.delim("c:/hdm/bachelorarbeit/analysis/text_mining/resources/english_stopwords.txt", header=FALSE, stringsAsFactors=FALSE)
Encoding(additional_stop_words$value) <- "UTF-8"
additional_stop_words <- c("für", "the", "sei", "o", "über", "to", "of", "r", "on", "for", "is", "s", "from", "teilen", "var", "2", "können") %>% as.tibble()

stop_words %<>%
  as.tibble() %>%
  add_row(V1=stop_words_en$V1) %>%
  add_row(V1=additional_stop_words$value) %$%
  str_remove_all(V1, ",") %>%
  enc2utf8() %>%
  str_squish() %>%
  as.tibble()

tidy_articles_text <- tidy_articles_text %>% anti_join(stop_words, by = c("word" = "value"))

tx <- tidy_articles_text %>%
  group_by(word) %>%
  summarize(word_count = n()) %>%
  filter(word_count > 10000) %>%
  arrange(desc(word_count))

# word counts per website
tidy_articles_text %>% count(word, name, sort = TRUE) %>% filter(n > 5000) %>% View()

### lookup of mentions of german / austrian parties

parties <- c("spd", "fdp", "csu", "cdu", "grünen", "grün", "afd", "npd", "spö", "linke") %>% as.tibble()

txx <- tx

txx %>% filter(word %in% parties$value) %>% View()

### calculate a score for each website for each emotion...
# load emotion lexicon
emotion_index <- read_excel("c://hdm/bachelorarbeit/analysis/text_mining/resources/NRC-Emotion-Lexicon-v0.92.xlsx")

emotion_index %<>%
  select(English = 'English (en)', German = 'German (de)', "Positive", "Negative", "Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust") %>%
  filter(German != "NO TRANSLATION")

emotion_results <- tidy_articles_text %>%
  inner_join(emotion_index, by = c("word" = "German")) %>%
  group_by(name) %>%
  arrange(website_id) %>%
  summarise_if(is.numeric, sd) %>%
  select(-id, -website_id)

View(emotion_results)

# add emotion sentiments to vertices of graph

emotion_results_2 <- emotion_results %>% 
  inner_join((all_websites %>%
               mutate(url = str_remove(url, "http.{0,1}://")) %>%
               mutate(url = str_remove(url, "www.")) %>%
               mutate(url = str_remove(url, "/"))),
    by=c("name" = "name"))

for (v in V(graph1)) {
  for (w in emotion_results_2$url) {
    if(V(graph1)[v]$name == w) {
      V(graph1)[v]$positive <- filter(emotion_results_2, url == w)$Positive
      V(graph1)[v]$negative <- filter(emotion_results_2, url == w)$Negative
      V(graph1)[v]$anticipation <- filter(emotion_results_2, url == w)$Anticipation
      V(graph1)[v]$anger <- filter(emotion_results_2, url == w)$Anger
      V(graph1)[v]$disgust <- filter(emotion_results_2, url == w)$Disgust
      V(graph1)[v]$fear <- filter(emotion_results_2, url == w)$Fear
      V(graph1)[v]$joy <- filter(emotion_results_2, url == w)$Joy
      V(graph1)[v]$sadness <- filter(emotion_results_2, url == w)$Sadness
      V(graph1)[v]$surprise <- filter(emotion_results_2, url == w)$Surprise
      V(graph1)[v]$trust <- filter(emotion_results_2, url == w)$Trust
    }
  }
}


##### STOP NLP #####
