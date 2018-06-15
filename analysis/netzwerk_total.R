library("igraph")
library("dplyr")
library("ggplot2")
library("tidyr")
library("tibble")
library("stringr")
library("lubridate")
library("DBI")
library("tidygraph")
library("tidytext")
library("magrittr")
library("readr")
library("readxl")
library("purrr")
library("visNetwork")
library("data.table")

con <- dbConnect(RMySQL::MySQL(),
                 dbname = "ba",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "1234"
)

##### collect data from the database #####

# collect the data

sql_statement <- "SELECT ba.website.url AS website_url, ba.link.domain AS link_url, ba.link.article_id AS article_id FROM ba.link INNER JOIN ba.article ON ba.link.article_id=ba.article.id INNER JOIN ba.website ON article.website_id=website.id"

all_links <- con %>% tbl(sql(sql_statement)) %>% collect()

sql_statement_website <- "SELECT id, name, url FROM ba.website"

all_websites <- con %>% tbl(sql(sql_statement_website)) %>% collect()

sql_all_articles <- "SELECT id, url, content_text, date_published, website_id FROM ba.article"

all_articles <- con %>% tbl(sql(sql_all_articles)) %>% collect()

### cleanup and summarise data ###

sample_articles <- all_articles %>%
  filter(as.Date(date_published) >= "2016-03-01" & as.Date(date_published) <= "2018-03-01")

sample_links <- all_links %>%
  filter(article_id %in% sample_articles$id) %>%
  filter(!(link_url==website_url))

sample_links_clean <- sample_links %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/"))

platforms <- "linkedin|wikipedia|wikimedia|commons|google|youtube|telegram|whatsapp|facebook|vk.com|mailto|javascript|creativecommons|xing|pinterest|addtoany|amzn.to|twitter|instagram|vkontakte|youtu.be|vimeo|amazon|ebay|imgur|medium|yahoo"
# flickr, tumblr, t.co

sample_links_without_platforms <- sample_links_clean %>%
  filter(!str_detect(link_url, platforms)) %>%
  filter(!(link_url==website_url))

flat_sample_links_without_platforms <- sample_links_without_platforms %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n()) %>%
  filter(!(link_url == "t.co")) %>%
  filter(linked_count >= 1)

##### generate graph #####

graph2 <- graph_from_data_frame(flat_sample_links_without_platforms)

graph2 %>% visIgraph() %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 0), nodesIdSelection = TRUE)

graph2 %>% visIgraph() %>% visExport(type = "png", name = "network_full")

plot(graph2)

V(graph2)$deg <- degree(graph2, mode="all")
V(graph2)$deg_in <- degree(graph2, mode="in")
V(graph2)$deg_out <- degree(graph2, mode="out")

V(graph2)$betw <- betweenness(graph2)
V(graph2)$closen <- closeness(graph2)

E(graph2)$weight <- E(graph2)$linked_count
E(graph2)$width <- E(graph2)$weight / 250
V(graph2)$size <- V(graph2)$deg_in * 5

##### calculate network dimensions #####

edge_density(graph2)

# how to call a single node
# V(graph2)["compact-online.de"]

graph2_as_df <- as_long_data_frame(graph2)

graph2_as_df %<>% mutate(connection = paste(as.character(from), "/", as.character(to)) )

ggplot(graph2_as_df, aes(x = linked_count)) +
  geom_density()

