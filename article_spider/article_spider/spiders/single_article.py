import scrapy
from datetime import date, datetime, timedelta
import re

# TODO: black magic - dont do this!
import sys
sys.path.insert(0, 'C:\\hdm\\bachelorarbeit')
from db_schema import Website, Article, Link, Author
from helper import *

class ArticlesSpider(scrapy.Spider):
    name = "single_articles"

    def start_requests(self):
        # articles = Article.select()
        # for article in articles:
        #     yield scrapy.Request(url=article.url, callback=self.parse, meta={"article_id" : article.id})

        # test one article for each website
        ws = Website.select()
        for w in ws:
            wa = w.articles
            for i, a in enumerate(wa):
                if i == 0:
                    # print(a.url)
                    try:
                        yield scrapy.Request(url=a.url, callback=self.parse, meta={"article_id" : a.id})
                    except:
                        print("Error at url %s", a.url)

    def parse(self, response):
        # Get the article from the db
        article = Article.get(Article.id==response.meta["article_id"])
        website = article.website

        # Title assignment
        article.title = response.xpath("//title/text()").extract_first()

        # Content (text) assignment
        if "//" in website.content_identifier:
            c_i = response.xpath(website.content_identifier)
            c_i = c_i.xpath("descendant::*/text()|text()").extract()
        else:
            c_i = response.css(website.content_identifier)
            c_i = c_i.xpath("descendant::*/text()|text()").extract()
        total_content = ""
        for c in c_i:
            total_content = total_content + c
        article.content_text = total_content

        # Content (raw HTML) assignment
        if "//" in website.content_identifier:
            cr_i = response.xpath(website.content_identifier)
        else:
            cr_i = response.css(website.content_identifier)
        cr_i = cr_i.extract()
        total_content = ""
        for cr in cr_i:
            total_content = total_content + cr
        article.content_raw = total_content

        # Published date assignment
        if "//" in website.date_identifier:
            d_i = response.xpath(website.date_identifier)
        else:
            d_i = response.css(website.date_identifier)
        try:
            article_date = re.sub(r"([\+|\-]\d\d)(:)(\d\d)", r"\1\3", d_i.extract_first())
            article_date = datetime.strptime(article_date, website.date_format)
            # Note: required format for db:
            # 2018-05-14 00:14:53
            article.date_published = datetime.strftime(article_date, "%Y-%m-%d %H:%M:%S")
        except:
            article.date_published = None

        # Author assignment
        if len(website.author_identifier) is 0:
            author_name = None
        elif "//" in website.author_identifier:
            a_i = response.xpath(website.author_identifier)
            author_name = a_i.extract_first()
        else:
            a_i = response.css(website.author_identifier).xpath("/text()")
            author_name = a_i.extract_first()
        try:
            article.author = Author.get(Author.name==author_name)
        except:
            author = Author()
            author.name = author_name
            article.author = author
            author.save()

        article.save()

        parse_articles_links(article)

        self.log('Saved article %s' % article.title)
