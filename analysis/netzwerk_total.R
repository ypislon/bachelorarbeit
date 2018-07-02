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

sql_statement <- "SELECT ba.website.url AS website_url, ba.link.domain AS link_url, ba.link.url AS full_link_url, ba.link.article_id AS article_id FROM ba.link INNER JOIN ba.article ON ba.link.article_id=ba.article.id INNER JOIN ba.website ON article.website_id=website.id"

all_links <- con %>% tbl(sql(sql_statement)) %>% collect()

sql_statement_website <- "SELECT id, name, url FROM ba.website"

all_websites <- con %>% tbl(sql(sql_statement_website)) %>% collect()

sql_all_articles <- "SELECT id, url, content_text, date_published, website_id FROM ba.article"

all_articles <- con %>% tbl(sql(sql_all_articles)) %>% collect()


sql_statement_2 <- "SELECT ba.website.url AS website_url, ba.link.url AS link_url, ba.link.article_id AS article_id FROM ba.link INNER JOIN ba.article ON ba.link.article_id=ba.article.id INNER JOIN ba.website ON article.website_id=website.id"

all_links_full <- con %>% tbl(sql(sql_statement_2)) %>% collect()

### cleanup and summarise data ###

sample_articles <- all_articles %>%
  filter(as.Date(date_published) >= "2016-03-01" & as.Date(date_published) <= "2018-03-01")

sample_links <- all_links %>%
  filter(article_id %in% sample_articles$id) %>%
  filter(!str_detect(full_link_url, "Shariff")) %>%
  filter(!(link_url==website_url))

sample_links_clean <- sample_links %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/"))

platforms <- "linkedin|wikipedia|wikimedia|commons|google|youtube|telegram|whatsapp|facebook|vk.com|mailto|javascript|creativecommons|xing|pinterest|addtoany|amzn.to|twitter|instagram|vkontakte|youtu.be|vimeo|amazon|ebay|imgur|medium|yahoo|flickr|tumblr|http"
# t.co and ct's "shariff" will be filtered out later

sample_links_without_platforms <- sample_links_clean %>%
  filter(!str_detect(link_url, platforms)) %>%
  filter(!(link_url==website_url))

flat_sample_links_without_platforms <- sample_links_without_platforms %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n()) %>%
  filter(!(link_url == "t.co")) %>%
  filter(linked_count >= 10)

##### generate graph #####

graph2 <- graph_from_data_frame(flat_sample_links_without_platforms)

graph2 %>% visIgraph() %>%
  visOptions(nodesIdSelection = TRUE, highlightNearest = list(enabled = TRUE, hover = FALSE, degree = 1)) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEvents(selectNode = "function(properties) {
            showCustomWidget(this.body.data.nodes.get(properties.nodes[0]));
            }",
            selectEdge = "function(properties) {
            showCustomEdgeWidget(this.body.data.edges.get(properties.edges[0]));
            }") %>%
  visSave(file = "full-interactive-network-alternative-media.html")

plot(graph2)

### create tables with the network data for further analysis

graph2_as_data_frame <- as_long_data_frame(graph2) %>% as.tibble()

graph2_vertex_metrics_incoming <- graph2_as_data_frame %>%
  group_by(to_name) %>%
  summarise(deg_in = n(), linked_count_total_incoming = sum(linked_count), average_weight_of_incoming_edge = (sum(linked_count) / n()), poi_deg_in_times_linked_count = (sum(linked_count) * n()), page_rank = round(to_page_rank[1], 6)) %>%
  select(-poi_deg_in_times_linked_count)

graph2_vertex_metrics_outgoing <- graph2_as_data_frame %>%
  group_by(from_name) %>%
  summarise(deg_out = n(), linked_count_total_outgoing = sum(linked_count), average_weight_of_outgoing_edge = (sum(linked_count) / n()))

#setwd("./analysis/render_networks")
write.csv(graph2_vertex_metrics_incoming, file = "full_network_vertex_metrics_incoming.csv")
write.csv(graph2_vertex_metrics_outgoing, file = "full_network_vertex_metrics_outgoing.csv")

### calculate vertex dimensions

V(graph2)$deg <- degree(graph2, mode="all")
V(graph2)$deg_in <- degree(graph2, mode="in")
V(graph2)$deg_out <- degree(graph2, mode="out")

V(graph2)$betw <- betweenness(graph2)
V(graph2)$closen <- closeness(graph2)

V(graph2)$page_rank <- page_rank(graph2)$vector

V(graph2)$eigenv <- eigen_centrality(graph2)$vector

# change vertex attributes for ploing
E(graph2)$weight <- E(graph2)$linked_count
E(graph2)$width <- E(graph2)$weight / 250
V(graph2)$size <- V(graph2)$deg_in * 1.7

# change vertex color for plotting
# blue = mainstream media
# red = website which was also in dorian sample

mainstream_medien <- read_xlsx('c:/hdm/bachelorarbeit/analysis/text_mining/resources/Mainstream-Medien_komplett.xlsx', col_names = c("name", "url"))

mainstream_medien <- mainstream_medien %>%
  mutate(url = str_remove(url, "http.{0,1}://")) %>%
  mutate(url = str_remove(url, "www."))

mainstream_medien_vertices <- which(V(graph2)$name %in% mainstream_medien$url)

for (v in mainstream_medien_vertices) {
  V(graph2)[v]$color <- "#84a07c"
}

dorian_sample <- "alpenschau|austropress|bayern-depesche|berlinjournal|deutsch.rt.com|contra-magazin|denken-macht-frei|deutsche-wirtschafts-nachrichten|fmpolitics|freiewelt|freiezeiten|kenfm|netzfrauen|newpi|pi-news|propagandaschau|zeitenschrift|bereicherungswahrheit|blaue-flora|freisleben-news|gegenfrage|labour|mmnews|perspektive|sputnik|wsws"

dorian_sample_vertices <- which(str_detect(V(graph2)$name, dorian_sample))

for (v in dorian_sample_vertices) {
  V(graph2)[v]$color <- "#ff8552"
}

sample_vertices <- which(V(graph2)$name %in% (all_websites %>%
                           mutate(url = str_remove(url, "http.{0,1}://")) %>%
                           mutate(url = str_remove(url, "www.")) %>%
                           mutate(url = str_remove(url, "/")))$url)

for (v in sample_vertices) {
  V(graph2)[v]$color <- "#eab996"
}

colorless_nodes <- which(is.na(V(graph2)$color))

for (v in colorless_nodes) {
  V(graph2)[v]$color = "#f7edf0"
  V(graph2)[v]$`label.color` <- "transparent"
}

V(graph2)$font.size = 24

##### calculate network dimensions #####

# good to know: how to call a single node
# V(graph2)["compact-online.de"]

### network density ###

g2_edge_dens <- edge_density(graph2) # density: 0.00266

### network centralization ###

g2_centr_deg <- centr_degree(graph2, mode = "all")$centralization # degree centralisation: 0.1199
g2_centr_deg_in <- centr_degree(graph2, mode = "in")$centralization # 0.0212
g2_centr_deg_out <- centr_degree(graph2, mode = "out")$centralization # 0.2423

g2_centr_betw <- centr_betw(graph2)$centralization # betweenness centralisation: 0.00268
g2_centr_clo <- centr_clo(graph2)$centralization # closeness centralisation: 0.0083
g2_centr_eigen <- centr_eigen(graph2)$centralization # eigen-vector centralisation: 0.9440

### network reciprocity ###
g2_recip <- reciprocity(graph2) # reciprocity: 0.0059

### network transitivity ###
g2_transi <- transitivity(g1_undirected_mutual, type = "global") # transitivity: 0.4161

g2_network_metrics <- g2_edge_dens %>% tibble() %>%
  add_column(g2_centr_betw) %>%
  add_column(g2_centr_deg) %>%
  add_column(g2_centr_deg_in) %>%
  add_column(g2_centr_deg_out) %>%
  add_column(g2_centr_clo) %>%
  add_column(g2_centr_eigen) %>%
  add_column(g2_recip) %>%
  add_column(g2_transi) %>%
  select(density = ".", "reciprocity" = "g2_recip", "transitivity" = "g2_transi", "betweenness centralisation" = "g2_centr_betw", "degree centralisation" = "g2_centr_deg", "degree centralisation (incoming)" = "g2_centr_deg_in", "degree centralisation (outgoing)" = "g2_centr_deg_out",  "closeness centralisation" = "g2_centr_clo", "eigenvector centralisation" = "g2_centr_eigen")

# write to csv file
write.csv(g2_network_metrics, file = "network_metrics_full.csv")

graph2_as_df <- as_long_data_frame(graph2)

graph2_as_df %<>% mutate(connection = paste(as.character(from), "/", as.character(to)) )

# take a look at the distribution of the number of links
ggplot(graph2_as_df, aes(x = linked_count)) +
  geom_density()

##### experiments #####

# create graph containing only sample websites and mainstream media websites
sample_links_only_mainstream_media <- sample_links_clean %>%
  filter(str_detect(link_url, mainstream_medien$url))

flat_sample_links_only_mainstream_media <- sample_links_only_mainstream_media %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n())

graph3 <- graph_from_data_frame(flat_sample_links_only_mainstream_media)

V(graph3)$size <- degree(graph3, mode = "in") * 2
E(graph3)$width <- E(graph3)$linked_count

graph3 %>% visIgraph() %>% visOptions(nodesIdSelection = TRUE)

# filter the sample by all other websites which are in the dorian sample and create a graph for it
sample_links_dorian_sample <- sample_links_clean %>%
  filter(str_detect(link_url, dorian_sample))

## normalize links to sputnik, DWN and propagandaschau, assign them to one node
k = 0
for (s in sample_links_dorian_sample$link_url) {
  if(str_detect(s, "sputnik")) {
    #print(s)
    sample_links_dorian_sample$link_url[sample_links_dorian_sample$link_url==s] = "sputniknews.com"
    #print(sample_links_dorian_sample$link_url[k])
  }
  k = k + 1
}

k = 0
for (s in sample_links_dorian_sample$link_url) {
  if(str_detect(s, "deutsche-wirtschafts-nachrichten")) {
    #print(s)
    sample_links_dorian_sample$link_url[sample_links_dorian_sample$link_url==s] = "deutsche-wirtschafts-nachrichten.de"
    #print(sample_links_dorian_sample$link_url[k])
  }
  k = k + 1
}

k = 0
for (s in sample_links_dorian_sample$link_url) {
  if(str_detect(s, "propagandaschau")) {
    #print(s)
    sample_links_dorian_sample$link_url[sample_links_dorian_sample$link_url==s] = "propagandaschau.de"
    #print(sample_links_dorian_sample$link_url[k])
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

degree(graph4, mode = "in")

# render graph for both mainstream and "alternative" media websites

links_alt_and_mainstream_media <- rbind(flat_sample_links_dorian_sample, flat_sample_links_only_mainstream_media)
graph_alt_mainstream_media <- graph_from_data_frame(links_alt_and_mainstream_media)
graph_alt_mainstream_media %>% visIgraph() %>% visOptions(nodesIdSelection = TRUE)
V(graph_alt_mainstream_media)$size <- degree(graph_alt_mainstream_media, mode = "in") * 2
E(graph_alt_mainstream_media)$width <- E(graph_alt_mainstream_media)$linked_count / 1000
