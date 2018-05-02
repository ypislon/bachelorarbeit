from peewee import *

# pg_db = PostgresqlDatabase('ba', user='postgres', password='1234',
#                            host='127.0.0.1', port=5432)

mysql_db = MySQLDatabase('ba', user='root', password='1234',
                         host='127.0.0.1', port=3306)

class Person(Model):
    name = CharField()
    birthday = DateField()
    is_relative = BooleanField()

    class Meta:
        database = mysql_db

mysql_db.connect()
mysql_db.create_tables([Person])

# --> works!
