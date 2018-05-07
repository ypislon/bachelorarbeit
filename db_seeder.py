from peewee import *
from db_schema import Website, Category

from db_connection import db_connection

# added alexa ranking - world wide, taken from alexa on 7.5.18

websites = {
    ("Alles Roger", "http://www.allesroger.at/", "/archiv?seite=$x$", ".row div h2 a", 1154989),
    ("Anonymus News", "http://www.anonymousnews.ru/", "/archiv/page/$x$", "h3.entry-title a", 30108),
    ("Compact Magazin", "https://www.compact-online.de/", "/compact-archiv/page/$x$", ".post-title a", 30658),
    ("Epoch Times", "https://www.epochtimes.de/", "/$c$/page/$x$", "main a", 6201),
    ("Halle Leaks", "https://blog.halle-leaks.de/", "/page/$x$", ".entry-title a", 123679),
    ("Info Direkt", "https://www.info-direkt.eu/", "/$y$/$m$/page/$x$", ".entry-title a", 208584),
    ("Journalisten Watch", "https://www.journalistenwatch.com/", "/category/$c$/page/$x$", ".entry-title a", 10187),
    ("Noch Info", "http://noch.info/", "/page/$x$", ".post-title a", 258831),
    ("Philosophia Perennis", "https://philosophia-perennis.com/", "/page/$x$", ".post-title a", 27339),
    # Shitload of JS stuff happening here...watch out!
    ("Rapefugees", "http://www.rapefugees.net/", "/page/$x$", ".entry-title a", 911901),
    ("Truth24", "http://www.truth24.net/", "/page/$x$", ".entry-title a", 173120),
    ("Zuerst", "http://zuerst.de/", "/page/$x$", ".bl2page-title a", 185203),
    ("Blauer Bote", "http://blauerbote.com/", "/page/$x$", ".entry-title a", 355997),
    ("Die Unbestechlichen", "https://dieunbestechlichen.com/", "/page/$x$", "h2.entry-title a", 44740),
    ("Euro-Med", "http://new.euro-med.dk/", "/page/$x$", ".entry-title a", 139870),
    # No pagination, but per category all articles listed
    ("Guido Grandt", "http://www.guidograndt.de/", "/category/$c$", ".pt-cv-content a", 97481),
    ("News For Friends", "http://news-for-friends.de/", "/page/$x$", ".entry-title a", 67515),
    ("No Islam-Noack Finsterwalde", "http://noack-finsterwalde.de/", "/page/$x$" ,".wpex-loop-entry-title a", 2393062),
    ("Opposition24", "https://opposition24.com/", "/category/$c$/page/$x$", ".entry-title a", 60406),
    ("Schlüsselkind-Blog", "https://schluesselkindblog.com/", "/page/$x$", ".posttitle a", 71745),
    ("Schweizer Morgenpost", "http://smopo.ch/", "/page/$x$", ".post-box-title a", 243059),
    # https://de.sott.net/signs/archive/de/2018/signs20180502.htm
    ("Sott", "https://de.sott.net/", "/signs/archive/de/$y$/signs$y$$m$$d$.htm", ".attl a", 19777),
    # https://www.unzensuriert.de/taxonomy/term/51327/all?page=1
    ("Unzensuriert", "https://www.unzensuriert.de/", "/taxonomy/term/$c$/all?page=$x$", ".field-content a", 249629) }


categories = {
    ("Epoch Times", "deutschland", "europa", "welt", "china", "wirtschaft", "umwelt", "gesundheit", "feuilleton", "sport", "wissen", "lifestyle", "themen/blaulicht", "genial", "wissen/mystery"),
    ("Journalisten Watch", "inland", "ausland", "klartext", "wirtschaft", "medienkritik", "satire", "freie medien"),
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

        try:
            website_db = Website()
            website_db.name = name
            website_db.url = url
            website_db.article_page = article_page
            website_db.article_identifier = article_identifier
            website_db.alexa_ranking = alexa_ranking
            website_db.save()
        except:
            # TODO: proper error handling
            print("error!")
            pass

    for category in categories:
        name = category[0]
        print(category[0])

        for i in enumerate(category):
            if(i[0] == 0):
                pass
            else:
                try:
                    category_db = Category(website=Website().get(Website.name == name))
                    category_db.name = category[i[0]]
                    category_db.name
                    category_db.save()
                except:
                    # TODO: proper error handling
                    print("error!")
                    pass
