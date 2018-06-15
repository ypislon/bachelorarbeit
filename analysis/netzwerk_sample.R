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
  inner_join(all_websites, by=c("link_url" = "url")) %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/")) %>%
  filter(!(link_url==website_url))

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


as.data.frame(V(graph1)) %>%
  inner_join(article_count_per_website, by = c("name" = "url"))

##### generate graph #####

graph1 <- graph_from_data_frame(flat_sample_links_internal)

# look at the undirected graph
g1_undirected <- as.undirected(graph1, mode = "each")
g1_undirected <- as.undirected(graph1, mode = "mutual")
plot(g1_undirected)

# render networks with visNetwork, select field for id and hover
setwd("./analysis/render_networks")
graph1 %>% visIgraph() %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 0), nodesIdSelection = TRUE) #%>%
  #visSave(file = "network_internal_size_of_deg_in_indexed.html", selfcontained = TRUE)

# render interactive network with vertex metrics
graph1 %>% visIgraph() %>%
  visOptions(nodesIdSelection = TRUE) %>%
  visEvents(selectNode = "function(properties) {
            alert(
              'Website: ' + this.body.data.nodes.get(properties.nodes[0]).id + '\\n' + 'Degree: ' + this.body.data.nodes.get(properties.nodes[0]).deg + '\\n' + 'Incoming Degrees: ' + this.body.data.nodes.get(properties.nodes[0]).deg_in + '\\n' + 'Outgoing Degree: ' + this.body.data.nodes.get(properties.nodes[0]).deg_out + '\\n' + 'Veröffentlichte Artikel: ' + this.body.data.nodes.get(properties.nodes[0]).article_count + '\\n' + 'Closeness (In&Out): ' + this.body.data.nodes.get(properties.nodes[0]).closen + '\\n' + 'Betweenness: ' + this.body.data.nodes.get(properties.nodes[0]).betw + '\\n' + 'Degree: ' + this.body.data.nodes.get(properties.nodes[0]).deg + '\\n')
            }") %>%
  visSave(file = "internal-interactive-network.html")

##### change graph attributes #####

# get count of articles in sample
a1_sample <- all_articles %>%
  inner_join(all_websites, by = c("website_id" = "id")) %>%
  filter(as.Date(date_published) >= "2016-03-01" & as.Date(date_published) <= "2018-03-01") %>%
  group_by(name) %>%
  summarise(articles_count = n(), website_id[1])
  
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
  
# calculate centrality values
V(graph1)$deg <- degree(graph1, mode="all")
V(graph1)$deg_in <- degree(graph1, mode="in")
V(graph1)$deg_out <- degree(graph1, mode="out")

V(graph1)$closen <- closeness(graph1, mode = "all")
V(graph1)$closen_in <- closeness(graph1, mode = "in")
V(graph1)$closen_out <- closeness(graph1, mode = "out")

V(graph1)$betw <- betweenness(graph1)

V(graph1)$transit <- transitivity(graph1_undirected, type = "local")

coreness(graph1, mode = "in") %>% as.tibble()

# change plot settings
# V(graph1)$size <- V(graph1)$betw
V(graph1)$size <- V(graph1)$deg_in * 2
V(graph1)$size <- V(graph1)$article_count

E(graph1)$width <- E(graph1)$weight / 20

# take a peak at distribution of counted links
ggplot(as.tibble(get.edge.attribute(graph1, "linked_count")), aes(x = value)) +
  geom_density() +
  lims(x = c(0, 100))

# create index for weight of edges
for (e in E(graph1)) {
  if (E(graph1)[e]$linked_count > 60) {
    E(graph1)[e]$weight <- 7
    E(graph1)[e]$width <- 7
  } else if (E(graph1)[e]$linked_count > 20) {
    E(graph1)[e]$weight <- 5
    E(graph1)[e]$width <- 5
  } else if (E(graph1)[e]$linked_count > 5) {
    E(graph1)[e]$weight <- 3
    E(graph1)[e]$width <- 3
  } else if (E(graph1)[e]$linked_count > 2) {
    E(graph1)[e]$weight <- 1
    E(graph1)[e]$width <- 1
  }
}

##### calculate network metrics #####

### network density ###

g_edge_dens <- edge_density(graph1) # density: 0.2766798

### network centralization ###

g_centr_deg <- centr_degree(graph1, mode = "all")$centralization # degree centralisation: 0.3047521
g_centr_betw <- centr_betw(graph1)$centralization # betweenness centralisation: 0.1261669
# TODO: find formula for proximity prestige -> would tell us, how well embedded the website is into the network
g_centr_clo <- centr_clo(graph1)$centralization # closeness centralisation: 0.1348803
g_centr_eigen <- centr_eigen(graph1)$centralization # eigen-vector centralisation: 0.5440952

centr_degree(graph1, mode = "in")$centralization # 0.3142292
centr_degree(graph1, mode = "out")$centralization # 0.541502

### network reciprocity ###

g_recip <- reciprocity(graph1)

g_network_metrics <- g_edge_dens %>% tibble() %>% 
  add_column(g_centr_betw) %>%
  add_column(g_centr_deg) %>%
  add_column(g_centr_clo) %>%
  add_column(g_centr_eigen) %>% 
  add_column(g_recip) %>%
  select(density = ".", "reciprocity" = "g_recip", "betweenness centralisation" = "g_centr_betw", "degree centralisation" = "g_centr_deg", "closeness centralisation" = "g_centr_clo", "eigenvector centralisation" = "g_centr_eigen")
  
# write to csv file
write.csv(g_network_metrics, file = "network_metrics.csv")

##### calculate vertex metrics #####

### deg_in, deg_out, deg_total, betweenness, closeness and eigen centrality per vertex in table format ###

g_centrality_eigen <- eigen_centrality(graph1, weights = V(graph1)$linked_count)$vector %>% as.data.frame() %>% rownames_to_column(var = "name") %>% select(name, eigen_centr = ".")

g_v_values_list <- as.tibble(V(graph1)$name) %>%
  bind_cols(as.tibble(V(graph1)$deg_in)) %>%
  bind_cols(as.tibble(V(graph1)$deg_out)) %>%
  bind_cols(as.tibble(V(graph1)$betw)) %>%
  bind_cols(as.tibble(V(graph1)$closen)) %>%
  bind_cols(as.tibble(centr_degree(graph1, normalized = TRUE, mode = "total")$res)) %>%
  bind_cols(as.tibble(V(graph1)$deg_in) / as.tibble(V(graph1)$deg_out)) %>%
  select(name = value, deg_in = value1, deg_out = value2, betw = value3, closen = value4, centr_degree = value5, deg_in_out = value6) %>%
  inner_join(g_centrality_eigen)
View(g_v_values_list)

# plot
pp_1 <- ggplot(g_v_values_list, aes(x = name, fill = centr_degree)) +
  geom_col(aes(y = deg_in), position = position_dodge(width = 0.9)) + 
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) + 
  guides(fill = guide_legend("Degree Zentralität")) + 
  labs(x = "Namen der Websites", y = "Anzahl der eingehenden Edges (Degree)", title = "Eingehende Verbindungen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))
  
# plot
pp_2 <- ggplot(g_v_values_list, aes(x = name, fill = centr_degree)) +
  geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) + 
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) + 
  guides(fill = guide_legend("Degree Zentralität")) + 
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Edges (Degree)", title = "Ausgehende Verbindungen") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))
pp_2

# plot
pp_6 <- ggplot(g_v_values_list, aes(x = name)) +
  geom_col(aes(y = betw)) + 
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) + 
  guides(fill = guide_legend("Degree Zentralität")) + 
  labs(x = "Namen der Websites", y = "Betweenness", title = "Betweenness Centrality") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

pp_6


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

pp_8

# plot
pp_9 <- ggplot(g_v_values_list, aes(x = name)) +
  geom_col(aes(y = eigen_centr)) + 
  #geom_col(aes(y = deg_out), position = position_dodge(width = 0.9)) + 
  guides(fill = guide_legend("Degree Zentralität")) + 
  labs(x = "Namen der Websites", y = "Eigen-Vektor Zentralität", title = "Eigen-Vektor Zentralität") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

pp_9

### calculate weight of degrees (in/out) for the vertices ###

g_v_values_incoming <- as_long_data_frame(graph1) %>%
  select(from_name, to_name, linked_count) %>%
  group_by(to_name) %>%
  summarise(deg_in = n(), linked_count_total_incoming = sum(linked_count), average_weight_of_incoming_edge = (sum(linked_count) / n()))
View(g_v_values_incoming)

g_v_values_outgoing <- as_long_data_frame(graph1) %>%
  select(from_name, to_name, linked_count) %>%
  group_by(from_name) %>%
  summarise(deg_out = n(), linked_count_total_outgoing = sum(linked_count), average_weight_of_outgoing_edge = (sum(linked_count) / n()))
View(g_v_values_outgoing)

# plot
pp_10 <- ggplot(g_v_values_outgoing, aes(x = from_name)) + 
  geom_col(aes(y = deg_out, fill = average_weight_of_outgoing_edge)) + 
  guides(fill = guide_legend(title = "Durchschnittliche Anzahl von Links \npro eingehender Verbindung")) + 
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Verbindungen", title = "Anzahl der ausgehenden Verbindungen mit Gewichtung") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

pp_10

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
pp_5

View(as_long_data_frame(graph1))

l_g_df <- as_long_data_frame(graph1)
View(l_g_df)

##### calculate edge metrics #####

g_e_values_list <- as_long_data_frame(graph1) %>%
  select(from_name, linked_count, from_deg, from_deg_in, from_deg_out, from_betw, to_name, to_deg, to_deg_in, to_deg_out, to_closen, to_closen_in, to_closen_out, to_betw)
View(g_e_values_list)

write.table(g_e_values_list, file = "vertex_edge_metrics.txt", sep = "\t")
write.csv2(g_e_values_list, file = "vertex_edge_metrics.csv")

ggplot(g_e_values_list, aes(x = from_name, y = linked_count)) + 
  geom_col() +
  labs(x = "Namen der Websites", y = "Anzahl der ausgehenden Verlinkungen ", title = "Anzahl der absoluten ausgehenden Verlinkungen je Website") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

## print graphs

ggsave("Eingehende_Verbindungen.png", plot = pp_1, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Ausgehende Verbindungen.png", plot = pp_2, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Anzahl der eingehenden Verlinkungen je Website.png", plot = pp_4, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Anzahl der ausgehenden Verlinkungen je Website.png", plot = pp_5, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Betweenness Centrality.png", plot = pp_6, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Closeness Centrality.png", plot = pp_7, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Verhältnis Eingehende Verlinkungen vs. Ausgehende Verlinkungen.png", plot = pp_8, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Eigen-Vektor Zentralität.png", plot = pp_9, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Anzahl der ausgehenden Verbindungen mit Gewichtung.png", plot = pp_10, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")

##### community detection #####

# which algorithms can be used for directed networks and which fits best for our network?
# -> fitting best: optimal, spinglass (evtl. walktrap)

g_c_optimal <- cluster_optimal(graph1)
g_c_spinglass <- cluster_spinglass(graph1, weights = E(graph1)$weight)
# modularity(g_c)
# membership(g_c)

# plot
png(filename = "./analysis/render_networks/communities_optimal_algo.png", width = 1024, height = 1024, units = "px")
plot(g_c_spinglass, graph1, vertex.frame.color = "transparent", vertex.label.cex=.8, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.15)
dev.off()

# take a look at the undirected graph
g_c_fast_greedy <- cluster_fast_greedy(g1_undirected)
plot(g_c_fast_greedy, as.undirected(g1_undirected))

plot(as.dendrogram(g_c_fast_greedy))

#plot
plot(g_c_fast_greedy, graph1, vertex.frame.color = "transparent", vertex.label.cex=.8, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.12)
png(filename = "./analysis/render_networks/communities_eigen_algo.png", width = 1024, height = 1024, units = "px")
plot(g_c, graph1, vertex.frame.color = "transparent", vertex.label.cex=.8, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.15)
dev.off()

# see http://igraph.org/r/doc/triad_census.html
triad_census(graph1)
# see http://igraph.org/r/doc/dyad_census.html
dyad_census(graph1)

motifs(graph1, 3)

################### experiments #####

# eigen_centrality(graph1, weights = V(graph1)$linked_count)

# centralization.degree(graph1)

graph1 %>% degree_distribution()

plot(graph1)

assortativity(graph1, types1 = V(graph1))
assortativity_degree(graph1)
# assortativity_nominal calculates a correlation score for categorical variable (label) and position
assortativity_nominal(graph1, types = V(graph1))

compare(g_c_optimal, g_c_spinglass, method = "adjusted.rand")

communities(g_c_optimal)
membership(g_c_optimal)

induced_subgraph(graph1, c(1, 2, 3))
plot(induced_subgraph(graph1, c(1, 2, 3)))
