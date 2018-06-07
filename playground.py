from db_schema import Website, Article
import db_seeder
import helper
import scrapy
from datetime import datetime, time, timedelta
from urllib.parse import urljoin, urlparse
from lxml import html
from io import StringIO
from peewee import SelectQuery, JOIN, SQL, RawQuery
import db_connection

ws = Website.select()

for w in ws:
    w
    w, urls = helper.parse_articles_url(w)

### --> unzensuriert links sind kaputt
### sehen zwar ganz aus, aber funktionieren nicht - da kommt als url nur ein fragment zurück
### ---> epoch times macht auch was komisches
### links, die extrahiert werden, sind keine artikel-links! (TODO: content_identifier verbessern)
### --> Sott-Links sollten jetzt gefixt seinx

## sanitize html

##raw
# remove <script> tags
#### DONE

for w in ws:
    if w.name == "SchluesselkingBlog":
        print("Hey")

a = Article.get(Article.id==614)
a.url

mytree = html.parse(StringIO(a.content_raw))
mytree_parsed = mytree.xpath("//*[not(script)]")

##text only
# remove multiple line breaks
#### DONE
a.replace("\r", "\n").replace("\n\n", "").replace("  ", " ").replace("  ", "")


## parse articles

# get number of images per article
# get number of iframes
# get number of twitter / youtube embeds

sql = "SELECT * FROM article a1 INNER JOIN article a2 WHERE a1.url=a2.url"
w = Article.select(SQL(sql))
w.count()

sql_2 = "SELECT url, count(*) FROM article GROUP BY 1 HAVING count(*) > 0"

# w2 = Article.select(Article.url).count().group_
# w2

sql_3 = "SELECT url FROM article WHERE url IN (SELECT url FROM article GROUP BY url HAVING count(*) > 1)"
sql_3 = "SELECT url FROM article WHERE website_id = 2"

rq = RawQuery(sql_3)
aq = Article.select(SQL(sql_3))
i = 0
c = db_connection.db_connection.execute_sql(sql_3)

i = 0
for row in c.fetchall():
    i += 1
    print(row)
print(i)




for a in aq:
    i += 1
print(i)

ww = Article.select(SQL(sql_3)).execute()
ww.count()

weird_results = Article.select()

weird = Article.select(Article.url).join(Article, join_type=JOIN.INNER, on=Article.url)
weird.count()

articles = Article.select(Article.url).distinct()
articles.count()

no_a = Article.select(articles)
no_a.count()

i = 0
for a in articles:
    i += 1
print(i)



Article_alias = Article.alias()
query = Article.select().join(Article_alias, on=(Article.url == Article_alias.url))
query.count()


ws = Website.select().where((Website.id==1) | (Website.id==4))
for w in ws:
    print(w.name)
