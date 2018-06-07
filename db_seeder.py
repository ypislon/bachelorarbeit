from peewee import *
from db_schema import Website, Category

from db_connection import db_connection

# added alexa ranking - world wide, taken from alexa on 7.5.18

websites = {
    # Author: is usually in a span with text: "Text: AUTHOR"
    ("Alles Roger", "http://www.allesroger.at/", "/archiv?seite=$x$", ".row div h2 a", 1154989, ".post_content", "", "li.detail.date", "%d.%m.%Y"),
    ("Anonymus News", "http://www.anonymousnews.ru/", "/archiv/page/$x$", "h3.entry-title a", 30108, ".td-post-content", "//span[@itemprop='author']/meta[@content]/@content", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    # watch out: author is linked here, need to use ::text to get just the text...
    ("Compact Magazin", "https://www.compact-online.de/", "/compact-archiv/page/$x$", ".post-title a", 30658, "article .post-container .post-content", ".author-info .description a", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Epoch Times", "https://www.epochtimes.de/", "/$c$/page/$x$", "main a", 6201, "//div[@id='news-content']/p", ".post-meta .author", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Halle Leaks", "https://blog.halle-leaks.de/", "/page/$x$", ".entry-title a", 123679, ".entry-content", "", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Info Direkt", "https://www.info-direkt.eu/", "/$y$/$m$/page/$x$", ".entry-title a", 208584, ".entry-content", "", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Journalisten Watch", "https://www.journalistenwatch.com/", "/category/$c$/page/$x$", ".entry-title a", 10187, "article .entry-content", "", ".entry-meta-date a", "%d. %B %Y"),
    ("Noch Info", "http://noch.info/", "/page/$x$", ".post-title a", 258831, ".entry-inner", ".vcard.author a", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Philosophia Perennis", "https://philosophia-perennis.com/", "/page/$x$", ".post-title a", 27339, ".post-content", ".post-meta-author a", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    # Shitload of JS stuff happening here...watch out!
    ("Rapefugees", "http://www.rapefugees.net/", "/page/$x$", ".entry-title a", 911901, ".entry-content", "//meta[@property='article:author']/@content", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Truth24", "http://www.truth24.net/", "/page/$x$", ".entry-title a", 173120, ".entry-content", "//meta[@property='article:author']/@content", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("Zuerst", "http://zuerst.de/", "/page/$x$", ".bl2page-title a", 185203, ".single-archive", "", ".single-info", "%d. %B %Y"),
    ("Blauer Bote", "http://blauerbote.com/", "/page/$x$", ".entry-title a", 355997, ".entry-content", "", "//time/@datetime", "%Y-%m-%dT%H:%M:%S%z"),
    # No real date...
    ("Die Unbestechlichen", "https://dieunbestechlichen.com/", "/page/$x$", "h2.entry-title a", 44740, ".entry-content", ".entry-meta-author .a", ".meta-item span.updated", "text"),
    ("Euro-Med", "http://new.euro-med.dk/", "/page/$x$", ".entry-title a", 139870, ".entry-content", ".author.vcard a", "span.entry-date", "%B %d, %Y"),
    ("Guido Grandt", "http://www.guidograndt.de/", "/category/$c$", ".pt-cv-content a", 97481, ".entry-content", ".author.vcard a", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    ("News For Friends", "http://news-for-friends.de/", "/page/$x$", ".entry-title a", 67515, ".td-post-content", ".td-author-by a", "//time/@datetime", "%Y-%m-%dT%H:%M:%S%z"),
    ("No Islam-Noack Finsterwalde", "http://noack-finsterwalde.de/", "/page/$x$" ,".wpex-loop-entry-title a", 2393062, ".wpex-post-content", "", ".wpex-date", "%B %d, %Y"),
    ("Opposition24", "https://opposition24.com/", "/category/$c$/page/$x$", ".entry-title a", 60406, ".entry-content", ".author.vcard a", "//time[@class='entry-date']/@datetime", "%Y-%m-%dT%H:%M:%S%z"),
    ("Schlüsselkind-Blog", "https://schluesselkindblog.com/", "/page/$x$", ".posttitle a", 71745, "//section[@class='entry']/*[not(@class='taxonomies')]", ".author.vcard a", "//time[@class='entry-date']/@datetime", "%Y-%m-%dT%H:%M:%S%z"),
    ("Schweizer Morgenpost", "http://smopo.ch/", "/page/$x$", ".post-box-title a", 243059, ".post-container", ".post-meta .author a", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z"),
    # Example: https://de.sott.net/signs/archive/de/2018/signs20180502.htm
    ("Sott", "https://de.sott.net/", "/signs/archive/de/$y$/signs$y$$m$$d$.htm", ".attl a", 19777, ".article-body", ".mbar a", "//div[@class='m-bar']/text()", "%a, %d $B %Y %H:%M %Z"),
    # Example: https://www.unzensuriert.de/taxonomy/term/51327/all?page=1
    ("Unzensuriert", "https://www.unzensuriert.de/", "/taxonomy/term/$c$/all?page=$x$", "li.views-row .field-content a", 249629, "#content .region-content .content .field", "//meta[@property='article:author']/@content", "//meta[@property='article:published_time']/@content", "%Y-%m-%dT%H:%M:%S%z") }


categories = {
    ("Epoch Times", "deutschland", "europa", "welt", "china", "wirtschaft", "umwelt", "gesundheit", "feuilleton", "sport", "wissen", "lifestyle", "themen/blaulicht", "genial", "wissen/mystery"),
    # Category "freie-medien" lists other "trusted" websites - might be nice to add some to te sample
    ("Journalisten Watch", "inland", "ausland", "klartext", "wirtschaft", "medienkritik", "satire"),
    ("Guido Grandt", "politik", "wirtschaftfinanzen", "zeitgeschichte", "medienkritik", "terror", "kriminalitaetpaedokriminalitaet", "geheimgesellschaften", "literatur", "videofilm", "kollegenbeitrag", "goodnews", "wissenschaft"),
    ("Opposition24", "polikritik", "geldreform", "satire", "psychiatrie", "meldungen", "gesellschaft"),
    ("Unzensuriert", "51335", "51327", "51328", "51329", "51330", "51331", "51332", "51333", "51334") }

def seed_db():
    for website in websites:
        name = website[0]
        url = website[1]
        article_page = website[2]
        article_identifier = website[3]
        alexa_ranking = website[4]
        content_identifier = website[5]
        author_identifier = website[6]
        date_identifier = website[7]
        date_format = website[8]

        try:
            website_db = Website()
            website_db.name = name
            website_db.url = url
            website_db.article_page = article_page
            website_db.article_identifier = article_identifier
            website_db.alexa_ranking = alexa_ranking
            website_db.content_identifier = content_identifier
            website_db.author_identifier = author_identifier
            website_db.date_identifier = date_identifier
            website_db.date_format = date_format
            website_db.save()
        except Exception:
            print("An error occured during seeding the websites.\n" + Exception)
            pass

    for category in categories:
        name = category[0]

        for i in enumerate(category):
            if(i[0] == 0):
                pass
            else:
                try:
                    category_db = Category(website=Website().get(Website.name == name))
                    category_db.name = category[i[0]]
                    category_db.save()
                except Exception:
                    print("An error occured during seeding the categories.\n" + Exception)
                    pass
