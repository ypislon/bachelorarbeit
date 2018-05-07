from db_schema import create_tables
from db_seeder import seed_db

create_tables(hard_reset=True)
seed_db()
