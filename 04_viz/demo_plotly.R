library(tidyverse)
library(plotly)
data <- read_excel("../data/xl/udrykningsaktivitet2.xlsx")



data %>%
  group_by(Hændelsestype) %>%
  summarise(avg_time = mean(`Mandtimer i alt`)) %>% 
  mutate(Hændelsestype = fct_reorder(Hændelsestype, avg_time)) %>%  
  plot_ly(x = ~Hændelsestype, y = ~avg_time) %>% 
  layout(margin = list(l = 50, r = 50, b = 150, t = 50, pad = 4))

data %>% count(Hændelsestype, `Hændelsesplacering hovedgruppe`) %>%
  mutate(Hændelsestype = fct_reorder(Hændelsestype, n)) %>% 
  plot_ly(x = ~Hændelsestype, y = ~n, color = ~`Hændelsesplacering hovedgruppe`) %>% 
  layout(margin = list(l = 50, r = 50, b = 150, t = 50, pad = 4))


library(lubridate)
library(hms)
library(scales)

data$Alarmmodtaget
data$time <- hms::hms(second(data$Alarmmodtaget), minute(data$Alarmmodtaget), hour(data$Alarmmodtaget))  
data$time <- as.POSIXct(data$time)
ggplot(data, aes(time)) + 
  geom_density(fill = "red", alpha = 0.5) + 
  scale_x_datetime(breaks = date_breaks("2 hours"), labels=date_format("%H:%M")) 
  
data %>% group_by(Døgntime) %>% count() %>% arrange(desc(n))


d1<- data %>% filter(`Hændelsesplacering hovedgruppe`=="Bygning")%>% select(Døgntime) %>% unlist() %>% as.numeric()
d2<- data %>% filter(`Hændelsesplacering hovedgruppe`!="Bygning")%>% select(Døgntime) %>% unlist() %>% as.numeric()

plot_ly(alpha = 0.6) %>%
  add_histogram(x = ~d1, name = "bygning") %>%
  add_histogram(x = ~d2, name = "Ikke bygning") %>%
  layout(barmode = "overlay")


plot_ly(data, x = ~`Antal indsatte personer i alt`, y = ~`Mandtimer i alt`, type = "scatter")
plot_ly(data %>% filter(`Antal indsatte personer i alt`<50 & `Mandtimer i alt`<500), x = ~`Antal indsatte personer i alt`, y = ~`Mandtimer i alt`, type = "histogram2dcontour")


(ggplot(diamonds, aes(x = price)) + 
  geom_density(aes(fill = color), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")) %>% 
  ggplotly


plot_ly(z = volcano, type = "heatmap")


library(gapminder)

gapminder %>%
  plot_ly(
    x = ~gdpPercap, 
    y = ~lifeExp, 
    size = ~pop, 
    color = ~continent, 
    frame = ~year, 
    text = ~country, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )
  )


economics %>%
  tidyr::gather(variable, value, -date) %>%
  transform(id = as.integer(factor(variable))) %>%
  plot_ly(x = ~date, y = ~value, color = ~variable, colors = "Dark2",
          yaxis = ~paste0("y", id)) %>%
  add_lines() %>%
  subplot(nrows = 5, shareX = TRUE)


(ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  stat_smooth()+
  facet_wrap(~year)) %>% 

ggplotly()


accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

d <- txhousing %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area")) %>%
  accumulate_by(~date)

d %>%
  plot_ly(
    x = ~date, 
    y = ~median,
    split = ~city,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  ) %>% animation_opts(
    frame = 100, 
    transition = 1, 
    redraw = FALSE
  )

library(quantmod)

getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 300)
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>%
  accumulate_by(~ID)
df %>%
  plot_ly(
    x = ~ID, 
    y = ~AAPL.Close, 
    frame = ~frame,
    type = 'scatter', 
    mode = 'lines', 
    fill = 'tozeroy', 
    fillcolor='rgba(114, 186, 59, 0.5)',
    line = list(color = 'rgb(114, 186, 59)'),
    text = ~paste("Day: ", ID, "<br>Close: $", AAPL.Close), 
    hoverinfo = 'text'
  ) 

# economics %>%
#   mutate(id = seq.int(nrow(economics))) %>% 
#   plot_ly(
#     x = ~id, 
#     y = ~pce, 
#     frame = ~date,
#     type = 'scatter', 
#     mode = 'lines', 
#     fill = 'tozeroy', 
#     fillcolor='rgba(114, 186, 59, 0.5)',
#     line = list(color = 'rgb(114, 186, 59)'),
#     text = ~paste("Day: ", id, "<br>Close: $", pce), 
#     hoverinfo = 'text'
#   )

