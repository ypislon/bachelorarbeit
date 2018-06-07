import scrapy
from urllib.parse import urljoin

# TODO: black magic - dont do this!
import sys
sys.path.insert(0, 'C:\\hdm\\bachelorarbeit')
from db_schema import Website, Article
from helper import *

class ArticlesSpider(scrapy.Spider):
    name = "articles"

    def start_requests(self):
        for website in Website.select():
            website, urls_per_website = parse_articles_url(website)

            for url in urls_per_website:
                # construct full url from base url and parsed fragment
                url = urljoin(website.url, url)
                yield scrapy.Request(url=url, callback=self.parse, meta={"website_name" : website.name})

    def parse(self, response):
        # get the website from the db
        website_name = response.meta["website_name"]
        website = Website.get(Website.name==website_name)
        # get all links to articles
        if "//" in website.article_identifier:
            r = response.xpath(website.article_identifier)
        else:
            r = response.css(website.article_identifier)
        for r_article in r:
            # create a new article and populate it
            article = Article()
            article.website = website
            article_url = r_article.css("a::attr(href)").extract_first()
            if "http" not in article_url:
                article.url = urljoin(website.url, article_url)
            else:
                article.url = article_url
            article.save()

            self.log('Saved article %s' % article.url)
