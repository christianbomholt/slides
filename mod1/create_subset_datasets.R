library(stringr)
library(dplyr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


avocadoCSV <- "../data/avocado.csv"

avocadoDF <- read.csv(avocadoCSV)[,-1]
head(avocadoDF, 10)

subset <- avocadoDF %>%
  filter(as.Date(Date)<=as.Date("2015-02-01")) %>%
  filter(str_detect(region, "^A|^B"))

write.csv(subset,file="../data/subset_avocado.csv")

### SQL Example

library(DBI)
library(RSQLite)
db <- RSQLite::datasetsDb()
dbReadTable(db, "mtcars")
dbReadTable(db, "mtcars", row.names = FALSE)
dbDisconnect(db)

airports <- read.csv("../data/flight_data/airports.csv")

con <- dbConnect(SQLite(), dbname="../data/flight_data/airports.sqlite")
dbWriteTable(con, "airport", airports,overwrite=T)
dbWriteTable(con, "cars", mtcars,overwrite=T)
dbWriteTable(con, "avocados", avocadoDF,overwrite=T)
dbReadTable(con, "airport")
dbReadTable(con, "cars")
dbReadTable(con, "avocados")
