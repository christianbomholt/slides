
library(plotly)
library(readr)


avocados <- read_csv("../data/avocado.csv") %>%
  filter(region=="Albany") %>% 
  filter(type=="conventional")

avocados <- avocados %>% rename(totalvol = `Total Volume`)

fit <- lm(AveragePrice~totalvol, data = avocados)

fit$coefficients
broom::tidy(fit)

avocados <- avocados %>% mutate(estimates = predict(fit, .))

plot(fit)

avocados %>% 
plot_ly(x = ~totalvol, y= ~AveragePrice) %>% 
  add_trace(y=~estimates, mode = 'lines') %>% 
  add_trace(y=~AveragePrice)
