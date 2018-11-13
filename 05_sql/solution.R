
library(DBI)
library(RSQLite)
library(dplyr)
library(dbplyr)

diamanter <- ggplot2::diamonds

con <- dbConnect(SQLite(), dbname="../data/nydb.sqlite")

dbWriteTable()

dbWriteTable(con, "diamant_table", diamanter)
dbReadTable(con, "diamant_table")

query <- dbSendQuery(con, "select * from diamant_table where clarity = 'VVS2' and color = 'E'")
specielle_diamanter <- dbFetch(query)

tbl(con, "diamant_table") %>%
  mutate(pris_pr_carat = price / carat) %>% 
  group_by(clarity) %>% 
  summarise(gnspris = mean(price), gns_pris_carat = mean(pris_pr_carat)) %>% 
  arrange(desc(gns_pris_carat))
