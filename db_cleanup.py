from db_connection import db_connection
from db_schema import Website, Article

db_connection.connect()
articles = Article.select()

for a in articles:
    a.url = a.url.strip()
    a.save()

## to get rid of dupliate entries:
# https://www.navicat.com/en/company/aboutus/blog/681-eliminating-duplicate-rows-using-select-distinct-in-mysql-part-4.html
# dirty hack tho...
## to add unique to url:
## ALTER ba.article ADD UNIQUE (url)

## cleanup broken epoch times links:
# DELETE FROM ba.article WHERE url LIKE "%/./%"

# unzensuriert Links
# DELETE FROM ba.article WHERE url LIKE "%commons.wikimedia%"

# DELETE FROM ba.article WHERE website_id = 1 AND NOT url LIKE "%unzensuriert%"

# DELETE FROM ba.article WHERE url LIKE "%unzensuriert.at%"

# DELETE FROM ba.article WHERE website_id = 1 AND NOT url LIKE "%/content/%"

# DELETE FROM ba.article WHERE website_id = 14 AND url LIKE "%/page/%"

# unbestechlichen Link

# DELETE FROM ba.article WHERE url LIKE "%pi-news%" and website_id = 18

# not used, but may be useful in the future...
#### DELETE FROM ba.article WHERE website_id = 3 AND url LIKE "%/2015/%"

###### might be nice to make article.title a Text
###### might be nice to make article.content_raw, article.content_text a Mediumtext
# -> https://stackoverflow.com/questions/13932750/tinytext-text-mediumtext-and-longtext-maximum-storage-sizes
# -> ALTER TABLE table_name MODIFY col_name DATATYPE

# group articles by domain and get only most cited domains, while filtering
# "SELECT article_id, domain FROM ba.link WHERE domain IN (SELECT domain FROM ba.link WHERE NOT ba.link.name LIKE "%vk.com%" group by domain having count(*) > 100);"

## get articles which have no title or no raw text
ab = Article.select(Article.title, Article.content_raw).where(Article.title.is_null() | Article.content_raw.is_null())
ab
ab.count()
