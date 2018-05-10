import scrapy
from helper import *
from datetime import time, date, timedelta
from urllib.parse import urljoin

# TODO: black magic - dont do this!
import sys
sys.path.insert(0, 'C:\\hdm\\bachelorarbeit')
from db_schema import Website, Article, Link, Author

class ArticlesSpider(scrapy.Spider):
    name = "articles"

    def start_requests(self):
        urls = list()
        for website in Website.select():
            website, urls_per_website = parse_articles_url(website)
            for url in urls_per_website:
                url = urljoin(website.url, url)
                yield scrapy.Request(url=url, callback=self.parse, meta={"website_name" : website.name})

    def parse(self, response):
        # get the website from the db
        website_name = response.meta["website_name"]
        website = Website.get(Website.name==website_name)
        # get all links to articles
        for article_links in response.css(website.article_identifier):
            # create a new article and populate it
            article = Article()
            article.website = website
            article.url = article_links.css("a::attr(href)").extract_first()
            article.title = article_links.css("a::text").extract_first()

            # TODO: seed!
            # article.date_published = response.css(Website.date_identifier + "::text").extract_first()

            # TODO: seed!
            # author_name = Website.author_identifier + "::text"
            author_name = "Example Author"
            author = Author()
            author.name = author_name
            article.author = author
            # Author assignment
            # if(author_name.contains("," || ";" "|"):
            #     # do something! you maybe have more than one author
            # if(Author.get(Author.name==author_name)):
            #     article.author = Author.get(Author.name==author_name)
            # else:
            #     author = Author()
            #     author.name = author_name
            #     article.author = author

            article.save()

            # get all links from the current article
            # TODO: refactor this to the new spider!
            # TODO: do we need that additional a?
            # article_links = response.css(Website.article_identifier).extract()
            # print(article_links[0])
            # for article_link in article_links:
                # link = Link()
                # a_element = article_link
                # dafuq? is there a method? maybe XPATH
                # print(a_element)
                # link.url = a_element
                ### parse domain
                # link.domain = a_element.getDomain()
                # dafuq? is there a method? maybe XPATH
                # link.link_text = article_link.getText()
                # dafuq? is there a method? maybe XPATH
                # link.article = article
                # print(link)
                # link.save()

        self.log('Saved article %s' % article.title)
