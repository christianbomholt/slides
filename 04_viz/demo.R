df <- read_excel('../data/xl/R.xlsx',col_names = T)
df$Date <- as.Date(df$Date,origin = '1970-01-01')

plot(df$Date,df$Yield, ylab = 'Yield', xlab='Date')

vals <- c('Yield','CPI')
matplot(df$Date,df[,vals],ylab='Value',xlab='Date',xaxt="n", pch=c('o+'))
legend(15000, 2, vals, pch = "o+", col = rep(c(1,2)))

ggplot(df,aes(x=Date, y=Yield)) +
  geom_point()


df %>% select(Date,Yield,CPI) %>% gather(key,value,-Date) %>% 
ggplot(aes(x=Date, y=value, color=key)) +
  geom_line() +
  geom_point()

df %>% select(Date,Yield,CPI) %>% gather(key,value,-Date) %>% 
  ggplot(aes(x=Date, y=value, color=key)) +
  geom_line() +
  geom_point() +
  theme_bw()


library(plotly)
library(tidyverse)
plot_ly(df, x= ~ Date, y = ~Yield)

df %>% select(Date,Yield,CPI) %>% gather(key,value,-Date) %>% 
  plot_ly(x= ~ Date, y = ~value, symbol=~key)

# %>% 
#   plot_ly(x= ~ Date, y = ~value, symbol=~variable)
library(highcharter)
df %>% select(Date,Yield,CPI) %>% gather(key,value,-Date) %>% 
  hchart("scatter", hcaes(x=Date,y=value, group= key))


df %>% select(Date,Yield,CPI) %>% gather(key,value,-Date) %>% 
  hchart("line", hcaes(x=Date,y=value, group= key))
# ```

## Interaktive plots `dygraphs`

pacman::p_load(dygraphs,xts)
df1 <- df %>% column_to_rownames(var = "Date") %>% 
  select(Yield,CPI) %>%
  as.xts() 
dygraph(df1)


## Line charts `plotly`


df %>% select(Date,Yield,CPI) %>% gather(key,value,-Date) %>% 
  plot_ly(x= ~ Date, y = ~value, linetype=~key,
          mode='lines', type='scatter')


## Stock styled

highchart(type = "stock") %>% 
  hc_add_series_xts(df1[,1], id = colnames(df1)[1],name = colnames(df1)[1]) %>% 
  hc_add_series_xts(df1[,2], id = colnames(df1)[2],name = colnames(df1)[2])




## Multiple plots

library(plotly)
p <- df %>%
  tidyr::gather(variable, value, -Date) %>%
  transform(id = as.integer(factor(variable))) %>%
  plot_ly(x = ~Date, y = ~value, color = ~variable, colors = "Dark2",
          yaxis = ~paste0("y", id)) %>%
  add_lines() %>% 
  subplot(nrows = 5, shareX = TRUE)
p


data(unemployment)

map <- hcmap("countries/us/us-all-all", data = unemployment,
             name = "Unemployment", value = "value", joinBy = c("hc-key", "code"),
             borderColor = "transparent") 

map %>%
  hc_colorAxis(dataClasses = color_classes(c(seq(0, 10, by = 2), 50))) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") 


library(leaflet)
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=12.4111338, lat=55.8354388, popup="Beredskabsstyrelsen")

data <- read_csv("../data/avocado.csv") %>% filter(region == "Boston" | region == "Albany")
plot_ly(data,x = ~Date, y = ~AveragePrice, symbol = ~region, color = ~type)

## Videre læsning
http://jkunst.com/highcharter/
https://r4ds.had.co.nz/data-visualisation.html
https://plot.ly/r/
https://rstudio.github.io/leaflet/
