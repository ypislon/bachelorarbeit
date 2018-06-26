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

platforms <- "linkedin|wikipedia|wikimedia|commons|google|youtube|telegram|whatsapp|facebook|vk.com|mailto|javascript|creativecommons|xing|pinterest|addtoany|amzn.to|twitter|instagram|vkontakte|youtu.be|vimeo|amazon|ebay|imgur|medium|yahoo|flickr|tumblr|http"
# t.co will be filtered out later

sample_links_without_platforms <- sample_links_clean %>%
  filter(!str_detect(link_url, platforms)) %>%
  filter(!(link_url==website_url))

flat_sample_links_without_platforms <- sample_links_without_platforms %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n()) %>%
  filter(!(link_url == "t.co")) %>%
  filter(linked_count >= 0)

##### generate graph #####

graph2 <- graph_from_data_frame(flat_sample_links_without_platforms)

graph2 %>% visIgraph() %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = FALSE, degree = 0), nodesIdSelection = TRUE)

graph2_as_data_frame <- as_long_data_frame(graph2) %>% as.tibble()

graph2_vertex_metrics_incoming <- graph2_as_data_frame %>%
  group_by(to_name) %>%
  summarise(deg_in = n(), linked_count_total_incoming = sum(linked_count), average_weight_of_incoming_edge = (sum(linked_count) / n()), poi_deg_in_times_linked_count = (sum(linked_count) * n()), page_ranking = round(to_page_ranking[1], 6), stren = to_stren[1]) %>%
  select(-poi_deg_in_times_linked_count)

graph2_vertex_metrics_outgoing <- graph2_as_data_frame %>%
  group_by(from_name) %>%
  summarise(deg_out = n(), linked_count_total_outgoing = sum(linked_count), average_weight_of_outgoing_edge = (sum(linked_count) / n()))

setwd("./analysis/render_networks")
write.csv(graph2_vertex_metrics_incoming, file = "full_network_vertex_metrics_incoming.csv")
write.csv(graph2_vertex_metrics_outgoing, file = "full_network_vertex_metrics_outgoing.csv")

#graph2 %>% visIgraph() %>% visExport(type = "png", name = "network_full")

plot(graph2)

V(graph2)$deg <- degree(graph2, mode="all")
V(graph2)$deg_in <- degree(graph2, mode="in")
V(graph2)$deg_out <- degree(graph2, mode="out")

V(graph2)$betw <- betweenness(graph2)
V(graph2)$closen <- closeness(graph2)

E(graph2)$weight <- E(graph2)$linked_count
E(graph2)$width <- E(graph2)$weight / 250
V(graph2)$size <- V(graph2)$deg_in * 5

V(graph2)$page_ranking <- page_rank(graph2)$vector

# coreness(graph2, mode = "in") %>% as.tibble() %>% View()

##### calculate network dimensions #####

edge_density(graph2)

# how to call a single node
# V(graph2)["compact-online.de"]

graph2_as_df <- as_long_data_frame(graph2)

graph2_as_df %<>% mutate(connection = paste(as.character(from), "/", as.character(to)) )

ggplot(graph2_as_df, aes(x = linked_count)) +
  geom_density()


##### experiments #####

mainstream_medien <- read_xlsx('c:/hdm/bachelorarbeit/analysis/text_mining/resources/Mainstream-Medien_komplett.xlsx', col_names = c("name", "url"))

mainstream_medien <- mainstream_medien %>%
  mutate(url = str_remove(url, "http.{0,1}://")) %>%
  mutate(url = str_remove(url, "www."))

sample_links_only_mainstream_media <- sample_links_clean %>%
  filter(str_detect(link_url, mainstream_medien$url))

flat_sample_links_only_mainstream_media <- sample_links_only_mainstream_media %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n())

graph3 <- graph_from_data_frame(flat_sample_links_only_mainstream_media)

V(graph3)$size <- degree(graph3, mode = "in") * 2
E(graph3)$width <- E(graph3)$linked_count

graph3 %>% visIgraph() %>% visOptions(nodesIdSelection = TRUE)

##

dorian_sample <- "alpenschau|austropress|bayern-depesche|berlinjournal|deutsch.rt.com|contra-magazin|denken-macht-frei|deutsche-wirtschafts-nachrichten|fmpolitics|freiewelt|freiezeiten|kenfm|netzfrauen|newpi|pi-news|propagandaschau|zeitenschrift|bereicherungswahrheit|blaue-flora|freisleben-news|gegenfrage|labour|mmnews|perspektive|sputnik|wsws"

sample_links_dorian_sample <- sample_links_clean %>%
  filter(str_detect(link_url, dorian_sample))

## TODO: filter sputnik links and assign them to one node
k = 0
for (s in sample_links_dorian_sample$link_url) {
  if(str_detect(s, "sputnik")) {
    #print(s)
    print(sample_links_dorian_sample$link_url[k])
  }
  k = k + 1
}

flat_sample_links_dorian_sample <- sample_links_dorian_sample %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n())

graph4 <- graph_from_data_frame(flat_sample_links_dorian_sample)

V(graph4)$size <- degree(graph4, mode = "in") * 2
E(graph4)$width <- E(graph4)$linked_count / 1000

graph4 %>% visIgraph() %>% visOptions(nodesIdSelection = TRUE)

ffff <- rbind(flat_sample_links_dorian_sample, flat_sample_links_only_mainstream_media)

gffff %>% visIgraph() %>% visOptions(nodesIdSelection = TRUE)

gffff <- graph_from_data_frame(ffff)
V(gffff)$size <- degree(gffff, mode = "in") * 2
E(gffff)$width <- E(gffff)$linked_count / 1000
