# Connecting to the database

library(RPostgres)
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='aumari96a8')

# The standard format
res <- dbSendQuery(wrds, "SELECT * FROM dataset")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

# Seeing which libraries are available at WRTS
res <- dbSendQuery(wrds, "select distinct table_schema
                   from information_schema.tables
                   where table_type ='VIEW'
                   or table_type ='FOREIGN TABLE'
                   order by table_schema")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

# Seeing which tables axist in a certain library
res <- dbSendQuery(wrds, "SELECT DISTINCT table_name
                   FROM information_schema.columns
                   WHERE table_schema='crsp' -- Specifying the particular library
                   ORDER BY table_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

# Specifying specific table within a library
res <- dbSendQuery(wrds, "select column_name
                   from information_schema.columns
                   where table_schema='crspa'
                   and table_name='dsf'
                   order by column_name")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data

# Get the first 10 rows of data from dataset
res <- dbSendQuery(wrds, "SELECT * FROM crsp_a_stock.dsf")
data <- dbFetch(res, n=10)
dbClearResult(res)
data





