library(readr)

path <- "R/couse_oct/data/exercise.csv"


data <- read_csv(path, na = "#NNAA")
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
