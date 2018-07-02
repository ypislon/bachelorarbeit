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
library("scales")
library("gridExtra")
library("colorRamps")

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

sample_links_internal <- sample_links %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/")) %>%
  filter(!(link_url==website_url)) %>%
  inner_join((all_websites %>%
               mutate(url = str_remove(url, "http.{0,1}://")) %>%
               mutate(url = str_remove(url, "www.")) %>%
               mutate(url = str_remove(url, "/"))),
             by=c("link_url" = "url"))

sample_links_clean <- sample_links %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/"))

flat_sample_links_internal <- sample_links_internal %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n(), id[1])

##### generate graph #####

graph1 <- graph_from_data_frame(flat_sample_links_internal)

# create the undirected graphs
g1_undirected_collapse <- as.undirected(graph1, mode = "collapse")
g1_undirected_each <- as.undirected(graph1, mode = "each")
g1_undirected_mutual <- as.undirected(graph1, mode = "mutual")

# render networks with visNetwork, select field for id and hover

# render interactive, directed network with vertex metrics
graph1 %>% visIgraph() %>%
  visOptions(nodesIdSelection = TRUE, highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 0)) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEvents(selectNode = "function(properties) {
              showCustomWidget(this.body.data.nodes.get(properties.nodes[0]));
            }",
            selectEdge = "function(properties) {
              showCustomEdgeWidget(this.body.data.edges.get(properties.edges[0]));
            }") %>%
  visSave(file = "internal-interactive-network.html")

# render interactive, undirected network with vertex metrics
g1_undirected_mutual %>% visIgraph() %>%
  visOptions(nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEvents(selectNode = "function(properties) {
            alert(
              'Website: ' + this.body.data.nodes.get(properties.nodes[0]).id + '\\n' + 'Degree: ' + this.body.data.nodes.get(properties.nodes[0]).deg + '\\n' + 'Incoming Degrees: ' + this.body.data.nodes.get(properties.nodes[0]).deg_in + '\\n' + 'Outgoing Degree: ' + this.body.data.nodes.get(properties.nodes[0]).deg_out + '\\n' + 'Veröffentlichte Artikel: ' + this.body.data.nodes.get(properties.nodes[0]).article_count + '\\n' + 'Closeness (In&Out): ' + this.body.data.nodes.get(properties.nodes[0]).closen + '\\n' + 'Betweenness: ' + this.body.data.nodes.get(properties.nodes[0]).betw + '\\n' + 'Degree: ' + this.body.data.nodes.get(properties.nodes[0]).deg + '\\n'
            )
            }") #%>%
  visSave(file = "internal-interactive-network-undirected.html")

##### change graph attributes #####

# calculate centrality values for directed graph
V(graph1)$deg <- degree(graph1, mode="all")
V(graph1)$deg_in <- degree(graph1, mode="in")
V(graph1)$deg_out <- degree(graph1, mode="out")

V(graph1)$closen <- closeness(graph1, mode = "all")
V(graph1)$closen_in <- closeness(graph1, mode = "in")
V(graph1)$closen_out <- closeness(graph1, mode = "out")

V(graph1)$betw <- betweenness(graph1)

V(graph1)$eigenv <- as.numeric(eigen_centrality(graph1)$vector)
V(graph1)$page_rank <- as.numeric(page_rank(graph1)$vector)

#coreness(graph1, mode = "in") %>% as.tibble()

#coreness(graph1, mode = c("all"))

# change plot settings
V(graph1)$size <- V(graph1)$deg_in * 0.7
#V(graph1)$size <- as.numeric(V(graph1)$page_rank) * 200

E(graph1)$weight <- E(graph1)$linked_count
E(graph1)$width <- E(graph1)$weight / 20

# add article count to graph
for (v in V(graph1)) {
  if(V(graph1)[v]$name == "anonymousnews.ru") {
    V(graph1)[v]$article_count = 2136
  } else if(V(graph1)[v]$name == "blauerbote.com") {
    V(graph1)[v]$article_count = 1129
  } else if(V(graph1)[v]$name == "blog.halle-leaks.de") {
    V(graph1)[v]$article_count = 3221
  } else if(V(graph1)[v]$name == "noack-finsterwalde.de") {
    V(graph1)[v]$article_count = 67
  } else if(V(graph1)[v]$name == "news-for-friends.de") {
    V(graph1)[v]$article_count = 3700
  } else if(V(graph1)[v]$name == "noch.info") {
    V(graph1)[v]$article_count = 546
  } else if(V(graph1)[v]$name == "smopo.ch") {
    V(graph1)[v]$article_count = 718
  } else if(V(graph1)[v]$name == "allesroger.at") {
    V(graph1)[v]$article_count = 560
  } else if(V(graph1)[v]$name == "compact-online.de") {
    V(graph1)[v]$article_count = 2694
  } else if(V(graph1)[v]$name == "guidograndt.de") {
    V(graph1)[v]$article_count = 1113
  } else if(V(graph1)[v]$name == "de.sott.net") {
    V(graph1)[v]$article_count = 9490
  } else if(V(graph1)[v]$name == "dieunbestechlichen.com") {
    V(graph1)[v]$article_count = 2043
  } else if(V(graph1)[v]$name == "opposition24.com") {
    V(graph1)[v]$article_count = 1704
  } else if(V(graph1)[v]$name == "philosophia-perennis.com") {
    V(graph1)[v]$article_count = 2921
  } else if(V(graph1)[v]$name == "journalistenwatch.com") {
    V(graph1)[v]$article_count = 6209
  } else if(V(graph1)[v]$name == "epochtimes.de") {
    V(graph1)[v]$article_count = 44042
  } else if(V(graph1)[v]$name == "new.euro-med.dk") {
    V(graph1)[v]$article_count = 1146
  } else if(V(graph1)[v]$name == "info-direkt.eu") {
    V(graph1)[v]$article_count = 1909
  } else if(V(graph1)[v]$name == "zuerst.de") {
    V(graph1)[v]$article_count = 4857
  } else if(V(graph1)[v]$name == "unzensuriert.de") {
    V(graph1)[v]$article_count = 1381
  } else if(V(graph1)[v]$name == "truth24.net") {
    V(graph1)[v]$article_count = 2360
  } else if(V(graph1)[v]$name == "schluesselkindblog.com") {
    V(graph1)[v]$article_count = 857
  } else if(V(graph1)[v]$name == "rapefugees.net") {
    V(graph1)[v]$article_count = 70
  }
}

# create index for weight of edges
for (e in E(graph1)) {
  E(graph1)[e]$weight <- 0.2
  E(graph1)[e]$width <- 0.2
  if (E(graph1)[e]$linked_count > 60) {
    E(graph1)[e]$weight <- 30
    E(graph1)[e]$width <- 30
  } else if (E(graph1)[e]$linked_count > 20) {
    E(graph1)[e]$weight <- 15
    E(graph1)[e]$width <- 15
  } else if (E(graph1)[e]$linked_count > 5) {
    E(graph1)[e]$weight <- 7
    E(graph1)[e]$width <- 7
  } else if (E(graph1)[e]$linked_count > 2) {
    E(graph1)[e]$weight <- 2
    E(graph1)[e]$width <- 2
  }
}

# create index for weight of vertex based on article counts
for (v in V(graph1)) {
  if (V(graph1)[v]$article_count > 10000) {
    V(graph1)[v]$size <- 50
  } else if (V(graph1)[v]$article_count > 5000) {
    V(graph1)[v]$size <- 30
  } else if (V(graph1)[v]$article_count > 1000) {
    V(graph1)[v]$size <- 10
  } else if (V(graph1)[v]$article_count > 20) {
    V(graph1)[v]$size <- 5
  }
}

### undirected graph
# calculate centrality values for undirected graph
V(g1_undirected_mutual)$deg <- degree(g1_undirected_mutual, mode="all")

V(g1_undirected_mutual)$closen <- closeness(g1_undirected_mutual, mode = "all")

V(g1_undirected_mutual)$betw <- betweenness(g1_undirected_mutual)

V(g1_undirected_mutual)$transit <- transitivity(g1_undirected_mutual, type = "local")

E(g1_undirected_mutual)$weight <- E(g1_undirected_each)$linked_count

# set attributes for plotting
V(g1_undirected_mutual)$size <- V(g1_undirected_mutual)$deg * 2
E(g1_undirected_mutual)$width <- E(g1_undirected_mutual)$weight / 20

##### calculate network metrics #####

### network density ###

g_edge_dens <- edge_density(graph1) # density: 0.2766798

### network centralization ###

g_centr_deg <- centr_degree(graph1, mode = "all")$centralization # degree centralisation: 0.3099
g_centr_deg_in <- centr_degree(graph1, mode = "in")$centralization # 0.5237
g_centr_deg_out <- centr_degree(graph1, mode = "out")$centralization # 0.5692

g_centr_betw <- centr_betw(graph1)$centralization # betweenness centralisation: 0.1421
g_centr_clo <- centr_clo(graph1)$centralization # closeness centralisation: 0.1421
g_centr_eigen <- centr_eigen(graph1)$centralization # eigen-vector centralisation: 0.4956

### network reciprocity ###
g_recip <- reciprocity(graph1) # reciprocity: 0.3488

### network transitivity ###
g_transi <- transitivity(g1_undirected_mutual, type = "global") # transitivity: 0.4161

g_network_metrics <- g_edge_dens %>% tibble() %>%
  add_column(g_centr_betw) %>%
  add_column(g_centr_deg) %>%
  add_column(g_centr_deg_in) %>%
  add_column(g_centr_deg_out) %>%
  add_column(g_centr_clo) %>%
  add_column(g_centr_eigen) %>%
  add_column(g_recip) %>%
  add_column(g_transi) %>%
  select(density = ".", "reciprocity" = "g_recip", "transitivity" = "g_transi", "betweenness centralisation" = "g_centr_betw", "degree centralisation" = "g_centr_deg", "degree centralisation (incoming)" = "g_centr_deg_in", "degree centralisation (outgoing)" = "g_centr_deg_out",  "closeness centralisation" = "g_centr_clo", "eigenvector centralisation" = "g_centr_eigen")

# write to csv file
write.csv(g_network_metrics, file = "network_metrics.csv")

##### calculate vertex metrics #####

### deg_in, deg_out, deg_total, betweenness, closeness and eigen centrality per vertex in table format ###

g_centrality_eigen <- eigen_centrality(graph1, weights = V(graph1)$linked_count)$vector %>% as.data.frame() %>% rownames_to_column(var = "name") %>% select(name, eigen_centr = ".")
g_page_rank <- page_rank(graph1)$vector %>% as.data.frame() %>% rownames_to_column(var = "name") %>% select(name, page_rank = ".")

g_v_values_list <- as.tibble(V(graph1)$name) %>%
  bind_cols(as.tibble(V(graph1)$deg_in)) %>%
  bind_cols(as.tibble(V(graph1)$deg_out)) %>%
  bind_cols(as.tibble(V(graph1)$betw)) %>%
  bind_cols(as.tibble(V(graph1)$closen)) %>%
  bind_cols(as.tibble(centr_degree(graph1, normalized = TRUE, mode = "total")$res)) %>%
  bind_cols(as.tibble(V(graph1)$deg_in) / as.tibble(V(graph1)$deg_out)) %>%
  select(name = value, deg_in = value1, deg_out = value2, betw = value3, closen = value4, centr_degree = value5, deg_in_out = value6) %>%
  inner_join(g_centrality_eigen) %>%
  inner_join(g_page_rank)

write.csv(g_v_values_list, file = "vertex_metrics.csv")

### linked count for incoming and outgoing edges per vertex

g_linked_count_incoming <- as_long_data_frame(graph1) %>%
  group_by(to_name) %>%
  mutate(to_linked_count = sum(linked_count)) %>%
  summarise(to_deg_in = to_deg_in[1], to_linked_count = to_linked_count[1])

g_linked_count_outgoing <- as_long_data_frame(graph1) %>%
  group_by(from_name) %>%
  mutate(from_linked_count = sum(linked_count)) %>%
  summarise(from_deg_out = from_deg_out[1], from_linked_count = from_linked_count[1])

g_linked_count_total <- g_linked_count_incoming %>%
  full_join(g_linked_count_outgoing, by = c("to_name" = "from_name"), copy = TRUE) %>%
  select("Name der Website" = to_name, "Anzahl der incoming Degrees" = to_deg_in, "Anzahl der eingehenden Links" = to_linked_count, "Anzahl der outgoing Degrees" = from_deg_out, "Anzahl der ausgehenden Links" = from_linked_count)

#png(filename = "vertices_with_linked_counts.png", width = 1200, height = 800, units = "px")
grid.table(g_linked_count_total)
#dev.off()

#write.csv(g_linked_count_total, file = "vertices_with_linked_counts.csv")

### calculate weight of degrees (in/out) for the vertices ###

g_v_values_incoming <- as_long_data_frame(graph1) %>%
  select(from_name, to_name, linked_count) %>%
  group_by(to_name) %>%
  summarise(deg_in = n(), linked_count_total_incoming = sum(linked_count), average_weight_of_incoming_edge = (sum(linked_count) / n()))

g_v_values_outgoing <- as_long_data_frame(graph1) %>%
  select(from_name, to_name, linked_count) %>%
  group_by(from_name) %>%
  summarise(deg_out = n(), linked_count_total_outgoing = sum(linked_count), average_weight_of_outgoing_edge = (sum(linked_count) / n()))

##### plot vertex metrics #####

# plot
pp_1 <- ggplot(g_v_values_list, aes(x = name, fill = centr_degree)) +
  geom_col(aes(y = deg_in), position = position_dodge(width = 0.9)) +
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  guides(fill = guide_legend("Degree Zentralität")) +
  labs(x = "Namen der Websites", y = "Anzahl der eingehenden Edges (Degree)", title = "Eingehende Verbindungen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

pp_1

# plot
pp_2 <- ggplot(g_v_values_list, aes(x = name, fill = centr_degree)) +
  geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  guides(fill = guide_legend("Degree Zentralität")) +
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Edges (Degree)", title = "Ausgehende Verbindungen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot
pp_6 <- ggplot(g_v_values_list, aes(x = name)) +
  geom_col(aes(y = betw)) +
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  guides(fill = guide_legend("Degree Zentralität")) +
  labs(x = "Namen der Websites", y = "Betweenness", title = "Betweenness Centrality") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot
pp_7 <- ggplot(g_v_values_list, aes(x = name)) +
  geom_col(aes(y = closen)) +
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  guides(fill = guide_legend("Degree Zentralität")) +
  labs(x = "Namen der Websites", y = "Closenness", title = "Closenness Centrality") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot
pp_8 <- ggplot(g_v_values_list, aes(x = name)) +
  geom_col(aes(y = deg_in_out)) +
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  guides(fill = guide_legend("Degree Zentralität")) +
  labs(x = "Summe eingehender Links / Summe ausgehender Links", y = "Betweenness", title = "Verhältnis \nEingehende Verlinkungen vs. Ausgehende Verlinkungen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot
pp_9 <- ggplot(g_v_values_list, aes(x = name)) +
  geom_col(aes(y = eigen_centr)) +
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) +
  guides(fill = guide_legend("Degree Zentralität")) +
  labs(x = "Namen der Websites", y = "Eigen-Vektor Zentralität", title = "Eigen-Vektor Zentralität") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot
pp_10 <- ggplot(g_v_values_outgoing, aes(x = from_name)) +
  geom_col(aes(y = deg_out, fill = average_weight_of_outgoing_edge)) +
  guides(fill = guide_legend(title = "Durchschnittliche Anzahl von Links \npro eingehender Verbindung")) +
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Verbindungen", title = "Anzahl der ausgehenden Verbindungen mit Gewichtung") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

g_v_values <- g_v_values_incoming %>%
  inner_join(g_v_values_outgoing, by = c("to_name" = "from_name")) %>%
  mutate(name = to_name, weird = linked_count_total_incoming / linked_count_total_outgoing) %>%
  select(name, linked_count_total_incoming, linked_count_total_outgoing, weird)

# plot
pp_3 <- ggplot(g_v_values, aes(x = name)) +
  geom_col(aes(y = linked_count_total_incoming, fill = "red"), position = "dodge") +
  geom_col(aes(y = linked_count_total_outgoing, fill = "blue"), position = "dodge")

# plot
pp_4 <- ggplot(g_v_values, aes(x = name)) +
  geom_col(aes(y = linked_count_total_incoming)) +
  labs(x = "Namen der Websites", y = "Anzahl der eingehenden Verlinkungen ", title = "Anzahl der eingehenden Verlinkungen je Website") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot
pp_5 <- ggplot(g_v_values, aes(x = name)) +
  geom_col(aes(y = linked_count_total_outgoing))  +
  #geom_col(aes(y = linked_count_total_incoming)) +
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Verlinkungen ", title = "Anzahl der ausgehenden Verlinkungen je Website") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# plot - take a peak at distribution of counted links
ggplot(as.tibble(get.edge.attribute(graph1, "linked_count")), aes(x = value)) +
  geom_density() +
  lims(x = c(0, 100))

##### calculate edge metrics #####

g_e_values_list <- as_long_data_frame(graph1) %>%
  select(from_name, linked_count, from_deg, from_deg_in, from_deg_out, from_betw, to_name, to_deg, to_deg_in, to_deg_out, to_closen, to_closen_in, to_closen_out, to_betw)

# save list of edge metrics to file
write.table(g_e_values_list, file = "vertex_edge_metrics.txt", sep = "\t")
write.csv2(g_e_values_list, file = "vertex_edge_metrics.csv")

# plot
ggplot(g_e_values_list, aes(x = from_name, y = linked_count)) +
  geom_col() +
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Verlinkungen ", title = "Anzahl der absoluten ausgehenden Verlinkungen je Website") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

ggplot(as_long_data_frame(graph2), aes(x = from, y = linked_count)) +
  geom_col()

network_internal_link_count_per_page <- as_long_data_frame(graph1) %>%
  select(from_name, linked_count) %>%
  group_by(name = from_name) %>%
  summarise(count = sum(linked_count))

network_full_link_count_per_page <- as_long_data_frame(graph2) %>%
  select(name = from_name, linked_count, to) %>%
  group_by(name) %>%
  summarise(count = sum(linked_count))

ratio_links_internal_vs_full <- network_full_link_count_per_page %>%
  inner_join(network_internal_link_count_per_page, by = c("name" = "name")) %>%
  mutate(ratio = round(count.y / count.x, 6))

## print graphs

#setwd("./../render_pictures")
ggsave("Eingehende_Verbindungen.png", plot = pp_1, width = 40, height = 30, units = "cm")
ggsave("Ausgehende Verbindungen.png", plot = pp_2, width = 40, height = 30, units = "cm")
ggsave("Anzahl der eingehenden Verlinkungen je Website.png", plot = pp_4, width = 40, height = 30, units = "cm")
ggsave("Anzahl der ausgehenden Verlinkungen je Website.png", plot = pp_5, width = 40, height = 30, units = "cm")
ggsave("Betweenness Centrality.png", plot = pp_6, width = 40, height = 30, units = "cm")
ggsave("Closeness Centrality.png", plot = pp_7, width = 40, height = 30, units = "cm")
ggsave("Verhältnis Eingehende Verlinkungen vs. Ausgehende Verlinkungen.png", plot = pp_8, width = 40, height = 30, units = "cm")
ggsave("Eigen-Vektor Zentralität.png", plot = pp_9, width = 40, height = 30, units = "cm")
ggsave("Anzahl der ausgehenden Verbindungen mit Gewichtung.png", plot = pp_10, width = 40, height = 30, units = "cm")

##### community detection #####

# which algorithms can be used for directed networks and which fits best for our network?
# -> fitting best: optimal, spinglass, walktrap

g_c_optimal <- cluster_optimal(graph1)
#g_c_spinglass <- cluster_spinglass(graph1, weights = E(graph1)$weight)
#g_c_walktrap <- cluster_walktrap(graph1, weights = E(graph1)$weight)

# modularity(g_c_optimal)
# membership(g_c_optimal)
g_c_spinglass <- cluster_spinglass(graph1, weights = E(graph1)$weight)
g_c_walktrap <- cluster_walktrap(graph1, weights = E(graph1)$weight)

# plot and save to disk
#png(filename = "communities_optimal_algo.png", width = 1600, height = 1600, units = "px", res = 72)
plot(g_c_optimal, graph1, vertex.frame.color = "transparent", vertex.label.cex=2, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.15)
#dev.off()

# take a look at the undirected graph
g_c_fast_greedy <- cluster_fast_greedy(g1_undirected_mutual)
plot(g_c_fast_greedy, as.undirected(g1_undirected_mutual))

#plot
plot(g_c_fast_greedy, graph1, vertex.frame.color = "transparent", vertex.label.cex=.8, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.12)

# see http://igraph.org/r/doc/triad_census.html
triad_census(graph1)
# see http://igraph.org/r/doc/dyad_census.html
dyad_census(graph1)

##### cohesion & blocks #####

g1_undirected_simple <- igraph::simplify(g1_undirected_mutual)
g1_undirected_each_simple <- igraph::simplify(g1_undirected_each)
V(g1_undirected_mutual)$deg <- degree(g1_undirected_mutual)


c_b_mutual <- cohesive_blocks(g1_undirected_simple)
c_b_each <- cohesive_blocks(g1_undirected_each_simple)

#png(filename = "cohesive_blocks.png", width = 1600, height = 1600, units = "px", res = 72)
plot(c_b_mutual, g1_undirected_mutual, vertex.frame.color = "transparent", vertex.label.cex=2, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.3)
#dev.off()

plot(c_b_each, g1_undirected_each)

blocks(c_b_mutual)
cohesion(c_b_mutual)
length(c_b_mutual)

blocks(c_b_mutual)

# get a single block and render it
blocks(c_b_each)[[9]]
g_c_bb <- induced_subgraph(g1_undirected_each_simple, blocks(c_b_each)[[6]])

# plot the most cohesive block
g_c_b <- induced_subgraph(g1_undirected_simple, blocks(c_b_mutual)[[4]])

V(g_c_b)$deg_in <- degree(g_c_b, mode = "in")
V(g_c_b)$deg_out <- degree(g_c_b, mode = "out")

V(g_c_b)$size <- V(g_c_b)$deg_in * 5

g_c_b %>% visIgraph() %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 0), nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEvents(selectNode = "function(properties) {
            showCustomWidget(this.body.data.nodes.get(properties.nodes[0]));
            }",
            selectEdge = "function(properties) {
            showCustomEdgeWidget(this.body.data.edges.get(properties.edges[0]));
            }") %>%
  visSave(file = "example.html")

##### import ranking of similarweb and add to the graph's vertices ######

popularity_ranking <- read_excel("c://hdm/bachelorarbeit/analysis/text_mining/resources/Sample_Charakterisierung.xlsx") %>% filter(!is.na(URL))

for (v in V(graph1)) {
  for (w in popularity_ranking$URL) {
    if(V(graph1)[v]$name == w) {
      V(graph1)[v]$popularity_ranking <- filter(popularity_ranking, URL == w)$`Country Rank`
      V(graph1)[v]$claim <- filter(popularity_ranking, URL == w)$`Claim`
      V(graph1)[v]$total_visits <- (as.numeric(gsub(",", ".", filter(popularity_ranking, URL == w)$`Total Visits (K)`)) * 1000)
      if(is.na(V(graph1)[v]$total_visits)) {
        V(graph1)[v]$total_visits = 1
      }
      V(graph1)[v]$referrals <- filter(popularity_ranking, URL == w)$`-> referrals (%)`
      V(graph1)[v]$redaktionsstaerke <- filter(popularity_ranking, URL == w)$`Redaktionsstärke`
      V(graph1)[v]$standort <- filter(popularity_ranking, URL == w)$`Standort laut Impressum`
    }
  }
}

# plot graph with visits as the vertex size and with additional color scale

V(graph1)$color <- round(sqrt(sqrt(V(graph1)$total_visits / 20)))
V(graph1)$size <- V(graph1)$color

graph1$palette <- ygobb((round(max(V(graph1)$color)) + 1))

getwd()
png(filename = "internal_network_size_page_visits.png", width = 1600, height = 1600, units = "px", res = 72)
plot(graph1, vertex.frame.color = "transparent", vertex.label.cex=2, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.3)
dev.off()

##### construct subgraph with most dense block, adding authors #####

# render subgraph of most dense cluster / block
# graph1 needs to be "simple" without any attributes, optionally the size of V and width of E
g33 <- graph_from_data_frame(flat_sample_links_internal) %>%
  induced_subgraph(c("dieunbestechlichen.com", "journalistenwatch.com", "philosophia-perennis.com", "opposition24.com")) %>%
  set_edge_attr("color", value = "darkgrey") %>%
  set_vertex_attr("shape", value = "circle") +
  edge("opposition24.com", "dieunbestechlichen.com", color = "yellow") +
  edge("philosophia-perennis.com", "opposition24.com", color = muted("green")) +
  vertex("Guido Grandt", shape = "square") +
  edge("Guido Grandt", "dieunbestechlichen.com", color = "red") +
  vertex("David Brenner", shape = "square") +
  edge("David Brenner", "philosophia-perennis.com", color = muted("red")) +
  edge("David Brenner", "dieunbestechlichen.com", color = "red") +
  vertex("Wolfgang van de Gydt", shape = "square") +
  edge("Wolfgang van de Gydt", "opposition24.com", color = muted("red")) +
  edge("Wolfgang van de Gydt", "dieunbestechlichen.com", color = "red") +
  vertex("Jürgen Fritz", shape = "square") +
  edge("Jürgen Fritz", "dieunbestechlichen.com", color = "red")


png(filename = "authors_cooperation.png", width = 1600, height = 1600, units = "px", res = 72)
plot(g33, vertex.frame.color = "transparent", vertex.size = 10, vertex.label.cex=2, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=2, edge.size = 2)
legend("topleft",
       legend = c("Author", "Gründer", "Empfehlung", "Werbung", "Verlinkung"),
       col = c("red", muted("red"), muted("green"), "yellow", "grey"),
       fill = c("red", muted("red"), muted("green"), "yellow", "grey"),
       cex = 2,
       title = "Kantenfarben")
dev.off()

##### experiments #####

# eigen_centrality(graph1, weights = V(graph1)$linked_count)

# centralization.degree(graph1)

graph1 %>% degree_distribution()

assortativity(graph1, types1 = V(graph1))
assortativity_degree(graph1)
# assortativity_nominal calculates a correlation score for categorical variable (label) and position
assortativity_nominal(graph1, types = V(graph1))

compare(g_c_optimal, g_c_spinglass, method = "adjusted.rand")

communities(g_c_optimal)
membership(g_c_optimal)
