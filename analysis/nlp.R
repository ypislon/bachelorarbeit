
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
V(graph1)$positive <- emotion_results$Positive
V(graph1)$negative <- emotion_results$Negative
V(graph1)$anger <- emotion_results$Anger
V(graph1)$anticipation <- emotion_results$Anticipation
V(graph1)$disgust <- emotion_results$Disgust
V(graph1)$fear <- emotion_results$Fear
V(graph1)$joy <- emotion_results$Joy
V(graph1)$sadness <- emotion_results$Sadness
V(graph1)$surprise <- emotion_results$Surprise
V(graph1)$trust <- emotion_results$Trust

##### STOP NLP #####
