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

sample_links_clean <- sample_links %>%
  mutate(website_url = str_remove(website_url, "http.{0,1}://")) %>%
  mutate(website_url = str_remove(website_url, "www.")) %>%
  mutate(website_url = str_remove(website_url, "/")) %>%
  mutate(link_url = str_remove(link_url, "http.{0,1}://")) %>%
  mutate(link_url = str_remove(link_url, "www.")) %>%
  mutate(link_url = str_remove(link_url, "/"))

platforms <- "linkedin|wikipedia|wikimedia|commons|google|youtube|telegram|whatsapp|facebook|vk.com|mailto|javascript|creativecommons|xing|pinterest|addtoany|amzn.to|twitter|instagram|vkontakte|youtu.be|vimeo|amazon|ebay"

sample_links_without_platforms <- sample_links %>%
  filter(!str_detect(link_url, platforms)) %>%
  filter(!(link_url==website_url))

flat_sample_links_without_platforms <- sample_links_without_platforms %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n())

flat_sample_links_internal <- sample_links %>%
  semi_join(all_websites, by=c("link_url" = "url")) %>%
  filter(!(link_url==website_url)) %>%
  group_by(website_url, link_url) %>%
  summarise(linked_count = n())

##### articles per website #####

counted_articles <- all_articles %>% 
  filter(as.Date(date_published)>="2016-03-01" | is.na(date_published)) %>%
  inner_join(all_websites, by = c("website_id" = "id")) %>%
  group_by(name) %>%
  summarise(article_count = n(), id = website_id[1])

pa_1 <- ggplot(counted_articles, aes(x = name, y = article_count), guide = guide_legend(title = "Name der Websites")) + 
  geom_col() + 
  labs(x = "Name der Website", y = "Anzahl der Artikel", title = "Anzahl der veröffentlichten Artikel im Zeitraum ") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

##### average number of words per article per website #####

article_count_per_website <- all_articles %>% 
  inner_join(all_websites, by = c("website_id" = "id")) %>%
  filter(content_text != "" | content_text != " " | !is.na(content_text)) %>%
  filter(as.Date(date_published)>="2016-03-01" | is.na(date_published)) %>%
  select(id, content_text, website_id) %>%
  group_by(website_id) %>%
  summarise(article_count = n())

aaa <- all_articles %>% 
  inner_join(all_websites, by = c("website_id" = "id")) %>%
  filter(as.Date(date_published)>="2016-03-01" | is.na(date_published)) %>%
  select(id, content_text, website_id, name) %>%
  group_by(name) %>%
  mutate(article_count = n()) %>%
  unnest_tokens(word, content_text) %>%
  drop_na() %>%
  as.tibble() %>%
  summarise(word_count = n(), article_count = article_count[1], avg_length = n() / article_count[1])

pa_2 <- ggplot(aaa, aes(x = name, y = avg_length)) +
  geom_col() +
  labs(x = "Name der Website", y = "Durchschnittiche Artikellänge (in Wörtern) ", title = "Artikellänge je Website") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

# see the distribution of length of articles

aaaa <- all_articles %>%
  filter(as.Date(date_published)>="2016-03-01" | is.na(date_published)) %>%
  select(id, content_text, website_id) %>%
  group_by(id) %>%
  unnest_tokens(word, content_text) %>%
  drop_na() %>%
  as.tibble() %>%
  summarise(word_count = n())

pa_3 <- ggplot(aaaa, aes(x = word_count)) + 
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(0, 1700)) + 
  labs(x = "Anzahl der Wörter", y = "Anzahl der Artikel ", title = "Verteilung der Artikellänge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

pa_4 <- ggplot(aaaa, aes(x = word_count)) + 
  geom_density() +
  scale_x_continuous(limits = c(0, 2500)) + 
  labs(x = "Anzahl der Wörter", y = "Anzahl der Artikel ", title = "Verteilung der Artikellänge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5))

##### average number of links per article per website

aab <- all_articles %>%
  inner_join(all_links, by = c("id" = "article_id")) %>%
  filter(as.Date(date_published)>="2016-03-01" | is.na(date_published)) %>%
  select(id, content_text, website_id) %>%
  group_by(id) %>% 
  summarise(n = n(), website_id = website_id[1]) %>%
  group_by(website_id) %>%
  summarise(link_count = sum(n)) %>%
  inner_join(article_count_per_website) %>%
  mutate(avg_link_per_article = (link_count / article_count))

aac <- all_articles %>%
  inner_join(all_links, by = c("id" = "article_id")) %>%
  inner_join(all_websites, by = c("website_id" = "id")) %>%
  filter(as.Date(date_published)>="2016-03-01" | is.na(date_published)) %>%
  filter(str_detect(link_url , regex(platforms))) %>% 
  select(id, content_text, website_id, name) %>%
  group_by(id) %>% 
  summarise(n = n(), website_id = website_id[1], name = first(name)) %>%
  group_by(website_id) %>%
  summarise(link_count = sum(n), name = first(name)) %>%
  inner_join(article_count_per_website) %>%
  mutate(avg_link_per_article = (link_count / article_count))

aad <- aab %>%
  inner_join(aac, by = c("website_id" = "website_id")) %>%
  select(website_id, avg_links = avg_link_per_article.x, avg_links_without_platforms = avg_link_per_article.y, name)

# plot
pa_5 <- ggplot(aad, aes(x = name)) + 
  geom_col(aes(y = avg_links, fill = "blue")) +
  geom_col(aes(y = avg_links_without_platforms, fill = "red")) + 
  labs(x = "Name der Website", y = "Durchschnittliche Anzahl der Links", title = "Durchschnittliche Anzahl der Links pro Artikel") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  theme(axis.title.x = element_text(vjust=0,hjust=.5)) + 
  guides(fill = guide_legend(title = "Anzahl der Verlinkungen \nmit/ohne Plattformen"))

##### articles by date #####

##### number of articles for all sites #####

aa <- all_articles %>% 
  inner_join(all_websites, by=c("website_id" = "id")) %>% 
  group_by(name) %>%
  summarise(number_of_articles = n())

pa_6 <- ggplot(aa, mapping = aes(x = name, y = number_of_articles)) +
  geom_col() +
  labs(x = "Name der Website", y = "Anzahl der Artikel", title = "Veröffentlichte Artikel im Zeitraum von März 2016 bis März 2018") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  guides(fill = guide_legend(title = "Anzahl der Verlinkungen \nmit/ohne Plattformen"))

# get published articles by date in line graph

ab <- all_articles %>% 
  inner_join(all_websites, by=c("website_id" = "id")) %>% 
  mutate(date_only = as.Date(str_extract(all_articles$date_published, "([0-9]{4}-[0-9]{2}-[0-9]{2})"))) %>%
  filter(!is.na(date_only)) %>%
  filter(date_only >= "2016-03-01" & date_only < "2018-03-01") %>% 
  group_by(date_only) %>%
  summarise(n = n())

pa_7 <- ggplot(ab, mapping = aes(x = date_only, y = n)) + 
  geom_line() + 
  geom_smooth() +
  labs(x = "Monat", y = "Anzahl der veröffentlichen Artikel", title = "Veröffentlichte Artikel im Zeitverlauf") +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm"))

# articles by website 
ac <- all_articles %>% 
  inner_join(all_websites, by=c("website_id" = "id")) %>% 
  mutate(date_only = as.Date(str_extract(all_articles$date_published, "([0-9]{4}-[0-9]{2}-[0-9]{2})"))) %>%
  filter(!is.na(date_only)) %>%
  filter(date_only >= "2016-03-01" & date_only < "2018-03-01") %>% 
  #filter(name != "Epoch Times") %>%
  select(id, name, date_only)

# binwidth changes the width of bins, in this case, the interval to paint a point
pa_8 <- ggplot(ac, mapping = aes(x = date_only, color = name)) + 
  geom_line(stat = "bin", binwidth = 30, size = 1.2) + 
  labs(x = "Monat", y = "Anzahl der veröffentlichen Artikel", title = "Artikel im Zeitverlauf je Website") +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  guides(color = guide_legend(title = "Namen der Websites"))
pa_8

pa_9 <- ggplot(ac, mapping = aes(x = date_only, fill = name)) + geom_histogram(binwidth = 30)  + 
  labs(x = "Monat", y = "Anzahl der veröffentlichen Artikel", title = "Artikel im Zeitverlauf je Website") +
  theme(plot.margin=unit(c(.5,.5,.5,.5),"cm")) +
  guides(fill = guide_legend(title = "Namen der Websites"))
pa_9

##### count links from internal network vs. all links #####

## save graphics

ggsave("Anzahl der veröffentlichten Artikel.png", plot = pa_1, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Artikellänge je Website.png", plot = pa_2, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Verteilung der Artikellänge.png", plot = pa_3, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Verteilung der Artikellänge.png", plot = pa_4, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Durchschnittliche Anzahl der Links pro Artikel.png", plot = pa_5, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Veröffentlichte Artikel im Sample.png", plot = pa_6, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Veröffentlichte Artikel im Zeitverlauf.png", plot = pa_7, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Artikel im Zeitverlauf je Website.png", plot = pa_8, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
ggsave("Artikel im Zeitverlauf je Website.png", plot = pa_9, width = 40, height = 30, units = "cm", path = "./analysis/render_pictures/")
