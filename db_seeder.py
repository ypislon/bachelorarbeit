from peewee import *
from db_schema import Website

from db_connection import db_connection

# TODO: add alexa ranking

websites = {
    ("Alles Roger", "http://www.allesroger.at/", "/archiv?seite=$x$", ".row div h2 a"),
    ("Anonymus News", "http://www.anonymousnews.ru/", "/archiv/page/$x$", "h3.entry-title a"),
    ("Compact Magazin", "https://www.compact-online.de/", "/compact-archiv/page/$x$", ".post-title a"),
    ("Epoch Times", "https://www.epochtimes.de/", "/$c$/page/$x$", "main a"),
    ("Halle Leaks", "https://blog.halle-leaks.de/", "/page/$x$", ".entry-title a"),
    ("Info Direkt", "https://www.info-direkt.eu/", "/$y$/$m$/page/$x$", ".entry-title a"),
    ("Journalisten Watch", "https://www.journalistenwatch.com/", "/category/$c$/page/$x$", ".entry-title a"),
    ("Noch Info", "http://noch.info/", "/page/$x$", ".post-title a"),
    ("Philosophia Perennis", "https://philosophia-perennis.com/", "/page/$x$", ".post-title a"),
    # Shitload of JS stuff happening here...watch out!
    ("Rapefugees", "http://www.rapefugees.net/", "/page/$x$", ".entry-title a"),
    ("Truth24", "http://www.truth24.net/", "/page/$x$", ".entry-title a"),
    ("Zuerst", "http://zuerst.de/", "/page/$x$", ".bl2page-title a"),
    ("Blauer Bote", "http://blauerbote.com/", "/page/$x$", ".entry-title a"),
    ("Die Unbestechlichen", "https://dieunbestechlichen.com/", "/page/$x$", "h2.entry-title a"),
    ("Euro-Med", "http://new.euro-med.dk/", "/page/$x$", ".entry-title a"),
    # No pagination, but per category all articles listed
    ("Guido Grandt", "http://www.guidograndt.de/", "/category/$c$", ".pt-cv-content a"),
    ("News For Friends", "http://news-for-friends.de/", "/page/$x$", ".entry-title a"),
    ("No Islam-Noack Finsterwalde", "http://noack-finsterwalde.de/", "/page/$x$" ,".wpex-loop-entry-title a"),
    ("Opposition24", "https://opposition24.com/", "/category/$c$/page/$x$", ".entry-title a"),
    ("Schlüsselkind-Blog", "https://schluesselkindblog.com/", "/page/$x$", ".posttitle a"),
    ("Schweizer Morgenpost", "http://smopo.ch/", "/page/$x$", ".post-box-title a"),
    # https://de.sott.net/signs/archive/de/2018/signs20180502.htm
    ("Sott", "https://de.sott.net/", "/signs/archive/de/$y$/signs$y$$m$$d$.htm", ".attl a"),
    # https://www.unzensuriert.de/taxonomy/term/51327/all?page=1
    ("Unzensuriert", "https://www.unzensuriert.de/", "/taxonomy/term/$c$/all?page=$x$", ".field-content a") }

def seed_db():
    for website in websites:
        name = website[0]
        url = website[1]
        article_page = website[2]
        article_identifier = website[3]

        try:
            website = Website()
            website.name = name
            website.url = url
            website.article_page = article_page
            website.article_identifier = article_identifier
            website.save()
        except:
            # TODO: proper error handling
            print("error!")
            pass

db_connection.connect()
seed_db()
db_connection.close()
