from peewee import *
from playhouse.migrate import *
import datetime

from db_connection import db_connection

# db_connection is available from db_schema
# and looks like this
## db_connection = MySQLDatabase('ba', user='root', password='1234',
#                          host='127.0.0.1', port=3306)


# define the models
## id as PK is always included as a default (thanks to peewee)

class BaseModel(Model):

    class Meta:
        database = db_connection

class Website(BaseModel):
    name = CharField()
    url = CharField()
    article_page = CharField(null=True)
    article_identifier = CharField(null=True)
    alexa_ranking = IntegerField(null=True)
    timestamp = DateTimeField(default=datetime.datetime.now)
    content_identifier = CharField(null=True)
    author_identifier = CharField(null=True)
    date_identifier = CharField(null=True)
    date_format = CharField(null=True)

class Author(BaseModel):
    name = CharField(null=True)

class Article(BaseModel):
    title = CharField(null=True)
    url = CharField()
    content_raw = TextField(null=True)
    content_text = TextField(null=True)
    date_published = DateTimeField(null=True)
    website = ForeignKeyField(Website, backref="articles")
    author = ForeignKeyField(Author, backref="articles", null=True)
    timestamp = DateTimeField(default=datetime.datetime.now)

class Link(BaseModel):
    url = TextField()
    domain = CharField(null=True)
    link_text = CharField(null=True)
    article = ForeignKeyField(Article, backref="links")
    timestamp = DateTimeField(default=datetime.datetime.now)

class Topic(BaseModel):
    name = CharField()

class Keyword(BaseModel):
    word = CharField()
    topic = ForeignKeyField(Topic, backref="keywords", null=True)

class Category(BaseModel):
    name = CharField()
    website = ForeignKeyField(Website, backref="categories")

list_of_models = [Website, Article, Link, Author, Keyword, Topic, Category]

def create_tables(hard_reset = False):
    if(hard_reset):
        with db_connection:
            db_connection.drop_tables(list_of_models)
            db_connection.create_tables(list_of_models)
    else:
        with db_connection:
            db_connection.create_tables(list_of_models)

def drop_tables():
    with db_connection:
        db_connection.drop_tables(list_of_models)
