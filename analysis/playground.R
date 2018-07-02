##### setup #####
install.packages("igraph")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tibble")
install.packages("stringr")
install.packages("lubridate")
install.packages("DBI")
install.packages("RMySQL")
install.packages("dbplyr")
install.packages("rgl")
install.packages("visNetwork")
install.packages("tidygraph")
install.packages("tidytext")
install.packages("readr")
install.packages("readxl")
install.packages("purrr")

# network tools
library("igraph")
# new grammar for R
library("dplyr")
# create plots
library("ggplot2")
# clean up data
library("tidyr")
# reimagening data frames
library("tibble")
# working with strings
library("stringr")
# handling date and time
library("lubridate")
# database adapter
library("DBI")
# tidyverse-like graphs
library("tidygraph")
# text mining tidy-style
library("tidytext")
# piping - tidy-style
library("magrittr")
# reading data from other files
library("readr")
# reading excel sheets
library("readxl")
# functions - tidy-style
library("purrr")
# create network visualizations which are interactive and exportable as html
library("visNetwork")

# watch a nice demo of igraph...
# demo("community", package="igraph")

con <- dbConnect(RMySQL::MySQL(),
  dbname = "ba",
  host = "localhost",
  port = 3306,
  user = "root",
  password = "1234"
)

##### testing #####

dbListTables(con)
dbListFields(con, "article")

# without collect(), the SQL is not 'really' executed - dplyr is lazy!
# this way we are free to modify our query before executing
# with fancy things like group_by, summarise, arrange and filter
websites <- con %>% tbl("website") %>% collect()

articles <- con %>% tbl("article") %>% filter(!is.na(content_raw)) %>% select(id)

links <- con %>% tbl("link") %>% filter(!is.na(domain))

#### was wollen wir für den ersten primitiven Graphen?

# data frame
# Vertex: Website_ID --> Domain_Id
# Edge: Link

# -> Zwei Joins, einmal die Artikel, dann die Website
# "SELECT ba.website.url AS website_url, ba.link.domain AS link_url FROM ba.link INNER JOIN ba.article ON ba.link.article_id=ba.article.id INNER JOIN ba.website ON article.website_id=website.id"

sql_statement <- "SELECT ba.website.url AS website_url, ba.link.domain AS link_url FROM ba.link INNER JOIN ba.article ON ba.link.article_id=ba.article.id INNER JOIN ba.website ON article.website_id=website.id LIMIT 1000"

link_website_df <- con %>% tbl(sql(sql_statement)) %>% collect()

g <- graph_from_data_frame(link_website_df)

# makes an interactive plot
# nice for exploring the graph
g %>% tkplot()

g %>% plot(vertex.label = V(g)$website_url ,vertex.size=2, vertex.color="orange", vertex.frame.color = "transparent", vertex.label.size=1, vertex.label.color=transparent, vertex.label.dist=0, edge.arrow.size=.3, edge.color="red")

deg <- degree(g, mode="all")
V(g)$size <- deg
# V(g)$label <- g$website_url
E(g)$width <- E(g)$weight/10
plot(g)

visNetwork::visIgraph(g)

visNetwork::visIgraph(g) %>% visSave(file="example-network.html")

visNetwork::visIgraph(g) %>% visNetwork::visSave(file="C:\\hdm\\bachelorarbeit\\example-network.html")

is.directed(g)
vcount(g)
ecount(g)
summary(g)

# abandoned sql statement to filter the nodes in the db
#sql_filtered_articles <- "SELECT ba.website.url AS website, ba.link.domain AS target_domain FROM ba.link INNER JOIN ba.article ON ba.link.article_id = ba.article.id INNER JOIN ba.website ON article.website_id = website.id WHERE ba.link.domain IN (SELECT ba.link.domain FROM ba.link GROUP BY ba.link.domain HAVING COUNT(*) > 10000) AND (NOT ba.link.domain LIKE '%vk.com%') AND (NOT ba.link.domain LIKE '%facebook%') AND (NOT ba.link.domain LIKE '%google%') AND (NOT ba.link.domain LIKE '%twitter%') AND (NOT ba.link.domain LIKE '%linkedin%') AND (NOT ba.link.domain LIKE '%mailto%') AND (NOT ba.link.domain LIKE '%javascript%') AND (NOT ba.link.domain LIKE '%t.co%') AND (NOT ba.link.domain LIKE '%telegram%') AND (NOT ba.link.domain LIKE '%amzn.to%')  AND (NOT ba.link.domain LIKE '%addtoany%')  AND (NOT ba.link.domain LIKE '%whatsapp%') AND (NOT ba.link.domain LIKE '%pinterest%') AND (NOT ba.link.domain LIKE '%xing%') AND (NOT ba.link.domain LIKE '%creativecommons%')"

##### collect data from the database #####

# collect the data

sql_statement <- "SELECT ba.website.url AS website_url, ba.link.domain AS link_url, ba.link.article_id AS article_id FROM ba.link INNER JOIN ba.article ON ba.link.article_id=ba.article.id INNER JOIN ba.website ON article.website_id=website.id"

all_links <- con %>% tbl(sql(sql_statement)) %>% collect()

sql_statement_website <- "SELECT id, name, url FROM ba.website"

all_websites <- con %>% tbl(sql(sql_statement_website)) %>% collect()

sql_all_articles <- "SELECT * FROM ba.article"

all_articles <- con %>% tbl(sql(sql_all_articles)) %>% collect()

##### graph of sample websites #####

# draw the graph of links between all websites from the sample

# draw graph containing only websites from the sample
# and without counting the nodes
links_from_sample <- all_links %>%
  semi_join(all_websites, by=c("link_url" = "url")) %>%
  filter(!(link_url==website_url)) %>%
  distinct()

# cleanup link names
links_from_sample %>% 
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/"))

# now we want to add the weight of edges directly in the graph
links_from_sample_2 <- all_links %>%
  semi_join(all_websites, by=c("link_url" = "url")) %>%
  filter(!(link_url==website_url)) %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/")) %>%
  group_by(link_url, website_url) %>%
  summarise(weight = n())

g3 <- graph_from_data_frame(links_from_sample_2)

deg <- degree(g3, mode="in")
V(g3)$size <- deg
E(g3)$width <- E(g3)$weight/30

g3 %>% 
  set_vertex_attr("color", value = "orange") %>%
  set_vertex_attr("label", value = V(g3)$website_url) %>%
  plot(vertex.frame.color = "transparent", vertex.label.cex=.8, vertex.label.color="blue", vertex.label.dist=1, vertex.label.degree=pi/2, edge.arrow.size=.15, edge.color="red")

# interesting: layout_with_kk, drl, fr
g3 %>% plot(layout = layout_with_drl(.))

g3 %>% add_layout_(with_kk()) %>% visNetwork::visIgraph() %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 0), 
             nodesIdSelection = TRUE) %>%
  visSave("network-sample-nodes.html", selfcontained = FALSE)

### try out first network measures

# Betweenness
V(g3)$size <- betweenness(g3) / 5

# Closeness
V(g3)$size <- closeness(g3, mode = "out") * 1000

cohesion(g3, checks = TRUE, source = 8, target = 13)

### try out community detection

largest_cliques(g3)

clb <- cluster_optimal(g3)
clb

plot(clb, g3)

##### links - filtered and displayed as graph #####

# filter links - remove social media platforms, plugin data, meta data and others 

### list of nodes to ignore
ignore_nodes <- c("linkedin", "wikipedia", "wikimedia", "commons", "google", "youtube", "telegram", "whatsapp", "facebook", "vk.com", "t.co", "mailto", "javascript", "creativecommons", "xing", "pinterest", "addtoany", "amzn.to", "twitter", "instagram", "vkontakte", "youtu.be", "vimeo", "amazon", "ebay")

filtered_links <- all_links

### remove links which contain the "no nodes" list - exceptions like facebook pages etc.
for (i in ignore_nodes) {
  filtered_links <- filtered_links %>% 
    filter(!str_detect(filtered_links$link_url , i))
}

### remove links pointing to itself
filtered_links %<>%
  filter(!(link_url==website_url))

### count all outgoing links
ggplot(filtered_links, mapping = aes(x = website_url)) + geom_bar()

### summarize data per website: number of articles,  outgoing links, link/article ratio
filtered_links %>%
  inner_join(all_websites, by = c("website_url" = "url")) %>% 
  inner_join(aa, by = c("name" = "name")) %>%
  group_by(name) %>%
  summarise(number_of_articles[[1]], number_of_outgoing_links = n(), link_per_article = round(n() / number_of_articles[[1]], digits = 4)) %>% View()

# TODO: replacing links by domain names
regex_links <- "http.*\\/\\/(.*)\\..*\\/"

# str_match(filtered_links$website_url, regex(regex_links, dotall = TRUE))

### filter links with edge weight and with more than XX links
flat_filtered_links <- filtered_links %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/")) %>%
  group_by(website_url, link_url) %>%
  summarise(linked = n()) %>%
  filter(linked > 20)

# graph creation

g4 <- graph_from_data_frame(flat_filtered_links)

E(g4)$width = E(g4)$linked/300
f_deg <- degree(g4, mode="in")
V(g4)$size <- f_deg*3

f_bet <- betweenness(g4)
V(g4)$size <- f_bet/10

### identify the nodes which represent websites from the sample
website_domain_names <- all_websites %>% 
  mutate(url = str_remove(url, "http.{0,1}://")) %>%
  mutate(url = str_remove(url, "www.")) %>%
  mutate(url = str_remove(url, "/"))

xy <- which(V(g4)$name %in% website_domain_names$url)

for (x in xy) {
  #print(V(g4)[x])
  V(g4)[x]$color <- "blue"
}

plot(g4)

g4 %>%
  add_layout_(with_kk()) %>%
  visNetwork::visIgraph() %>%
  visInteraction(navigationButtons = TRUE) %>%
  visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, degree = 0), nodesIdSelection = TRUE)

##### network of sample websites and internet platforms #####

platforms <- c("linkedin", "wikipedia", "wikimedia", "commons", "google", "youtube", "telegram", "whatsapp", "facebook", "vk.com", "t.co", "mailto", "javascript", "creativecommons", "xing", "pinterest", "addtoany", "amzn.to", "twitter")

platforms <- "linkedin|wikipedia|wikimedia|commons|google|youtube|telegram|whatsapp|facebook|vk.com|mailto|javascript|creativecommons|xing|pinterest|addtoany|amzn.to|twitter"

filtered_links_2 <- all_links

filtered_links_2 <- filter(filtered_links_2, str_detect(filtered_links_2$link_url , regex(platforms)))

### remove links pointing to itself
filtered_links_2 %<>%
  filter(!(link_url==website_url))

filtered_links_2 %>%
  inner_join(all_websites, by = c("website_url" = "url")) %>% 
  inner_join(aa, by = c("name" = "name")) %>%
  group_by(name) %>%
  summarise(number_of_articles[[1]], number_of_outgoing_links = n(), link_per_article = round(n() / number_of_articles[[1]], digits = 4))

# TODO: replacing links by domain names
regex_links <- "http.*\\/\\/(.*)\\..*\\/"

# str_match(filtered_links$website_url, regex(regex_links, dotall = TRUE))

### filter links with edge weight and with more than XX links
flat_filtered_links_2 <- filtered_links_2 %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/")) %>%
  group_by(website_url, link_url) %>%
  summarise(linked = n()) %>%
  filter(linked > 20)

g6 <- graph_from_data_frame(flat_filtered_links_2)

g6 %>% visIgraph()

##### articles by date ##### 

# sorting articles by date and displaying them in graphs

##### get number of articles for all sites

aa <- all_articles %>% 
  inner_join(all_websites, by=c("website_id" = "id")) %>% 
  group_by(name) %>% 
  summarise(number_of_articles = n())

ggplot(aa, mapping = aes(x = name, y = number_of_articles)) +
  geom_col()

##### get published articles by date in line graph

ab <- all_articles %>% 
  inner_join(all_websites, by=c("website_id" = "id")) %>% 
  mutate(date_only = as.Date(str_extract(all_articles$date_published, "([0-9]{4}-[0-9]{2}-[0-9]{2})"))) %>%
  filter(!is.na(date_only)) %>%
  filter(date_only > "2016-02-29") %>%
  group_by(date_only) %>%
  summarise(n = n())

ggplot(ab, mapping = aes(x = date_only, y = n)) %+% geom_line()

# articles by website 
ac <- all_articles %>% 
  inner_join(all_websites, by=c("website_id" = "id")) %>% 
  mutate(date_only = as.Date(str_extract(all_articles$date_published, "([0-9]{4}-[0-9]{2}-[0-9]{2})"))) %>%
  filter(!is.na(date_only)) %>%
  filter(date_only > "2016-02-29") %>% 
  select(id, name, date_only)

# binwidth changes the width of bins, in this case, the interval to paint a point
ggplot(ac, mapping = aes(x = date_only, color = name)) + geom_line(stat = "bin", binwidth = 1)

ggplot(ac, mapping = aes(x = date_only, fill = name)) + stat_count()

##### graph of articles (and links) by one platform #####

selected_articles <- all_articles %>%
  filter(website_id == 9)

selected_links <- all_links %>%
  filter(article_id %in% selected_articles$id)

s_links <- selected_links %>%
  select(article_id, link_url)

g5 <- graph_from_data_frame(s_links)

f_deg <- degree(g5, mode="in")
V(g5)$size <- f_deg*0.01

V(g5)$color <- ifelse(!is.na(as.numeric(V(g5)$name)), "blue", "green")

g5 <- set_vertex_attr(g5, "label.color", value = "None")

g5 %>% visIgraph() %>%
  visInteraction(navigationButtons = TRUE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visExport(name = "article_graph")

### idee: vielleicht alle "zentralsten" bezugsquellen der jeweiligen artikel mappen und im netzwerk darstellen!

all_articles %>%
  filter(date_published == "" | is.na(date_published)) %>%
  group_by(website_id) %>%
  summarise(n()) %>% View()

g6 <- graph(edges=c(1,2, 2,3, 3, 1), n=3, directed=F)

plot(g6)

# close the db connection
dbDisconnect(con)