library(plotly)

avocados <- read_csv("R/couse_oct/data/avocado.csv") %>%
  filter(region == "Albany" | region =="Boston")

plot_ly(avocados, x = ~Date, y = ~AveragePrice, symbol = ~region, color = ~type)

