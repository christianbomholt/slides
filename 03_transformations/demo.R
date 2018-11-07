library(dplyr)
library(readr)


#  INTRO
# Pick observations by their values (filter()).
# Reorder the rows (arrange()).
# Pick variables by their names (select()).
# Create new variables with functions of existing variables (mutate()).
# Collapse many values down to a single summary (summarise()).
# These can all be used in conjunction with group_by() which changes the scope of each function from operating on the entire dataset to 
# operating on it group-by-group. These six functions provide the verbs for a language of data manipulation.


avocados <- read_csv("../data/avocado.csv")[,-1]

substr("Hello world!",1,5)
substr("Hello world!",7,12)

avocados <- mutate(avocados, month = substr(Date,6,7)) 
avocados <- mutate(avocados, day = substr(Date,9,10)) 


(subset <- select(avocados,Date,month,day))

filter(subset, month == 1, day == 1)

# As numeric
avocados <- mutate(avocados, month = as.numeric(substr(Date,6,7))) 
avocados <- mutate(avocados, day = as.numeric(substr(Date,9,10))) 

filter(avocados, month == 1, day == 1)

# Caution

(sqrt(2) ^ 2) == 2
(1 / 49 * 49) == 1
(1 / 8 * 8) == 1


near(sqrt(2) ^ 2,  2)
#> [1] TRUE
near(1 / 49 * 49, 1)

filter(avocados, month == 1, day == 1)

filter(avocados, type == "conventional", region == "Albany")

filter(avocados, AveragePrice < 0.5)
filter(avocados, AveragePrice < 0.5 | `Total Volume`<450)

filter(avocados, month %in% c(1, 12))

# Not conditions
filter(avocados, !(AveragePrice > 0.5 | year > 2015))


# NA values
# NA + 10
# 10 == NA
# NA > 5
# NA == NA
# y <- NA
# is.na(y)

(df <- tibble(x = c(1, NA, 3)))
filter(df, x > 1)
filter(df, x > 1 | is.na(x))

# Logical operators

#
(subset <- select(avocados,day,month,year,Date))
arrange(subset, year, month, day) 
arrange(subset, desc(year), desc(month), desc(day))

# Rename
(avocados <- rename(avocados, total_volume = `Total Volume`))


#Brugbart når der er variable man vil flytte frem som første søjler
select(avocados, year, month, everything())

select(avocados, year, month, day)

select(avocados, year:day)
select(avocados, -(year:day))
select(avocados, -day)

select(avocados, -(year:day))

fin_report <- select(avocados,total_volume,averageprice,date,region)

mutate(fin_report,revenue = total_volume*averageprice)
transmute(fin_report,revenue = total_volume*averageprice)

# Summarise

summarise(avocados, avgprice = mean(averageprice))

avocados <- mutate(avocados,log_price_minus_one = log(averageprice - 1-runif(1)))
summarise(avocados, avgprice = mean(log_price_minus_one))

summarise(avocados, avgprice = mean(log_price_minus_one,na.rm = T))

by_region <- group_by(avocados, region,year)
summarise(by_region, avgprice = mean(averageprice, na.rm = TRUE))

# Se pipe

by_region <- group_by(avocados, region,year)
region_stats <- summarise(by_region,
                   count = n(),
                   vol = mean(total_volume, na.rm = TRUE),
                   prc = mean(averageprice, na.rm = TRUE)
)
region_stats <- filter(region_stats, count > 40, region != "Albany", vol < 1e6)

library(ggplot2)
ggplot(data = region_stats, mapping = aes(x = vol, y = prc)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)

# Pipe
# ctrl + shift + m
region_stats <- avocados %>%
  group_by(region) %>%
  summarise(
    count = n(),
    vol = mean(total_volume, na.rm = TRUE),
    prc = mean(averageprice, na.rm = TRUE)
    ) %>% filter(count > 40, region != "Albany", vol < 1e6)

# Ungroup

avocados %>% group_by(region,year) %>% 
  summarise(avg_price = mean(averageprice)) %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(avocados = n()) 

# Extra
(colnames(avocados) <-  tolower(colnames(avocados)))
(colnames(avocados) <- gsub(x = colnames(avocados),pattern = " ",replacement =  "_"))
(colnames(avocados) <- gsub(x = colnames(avocados),pattern = "^\\d",replacement =  "type_4"))

###

flight<- read_csv("../data/2000.csv") %>%
  filter(DayOfWeek>5) %>% 
  group_by(UniqueCarrier) %>%
  summarise(avgDelay = mean(DepDelay,na.rm = T)) %>%
  arrange(desc(avgDelay)) %>%
  head()

###

library(readxl)
library(plotly)
options <- read_excel("../data/xl/SPXoptions_changed.xlsx") 
S_init = 2080.15
df <- options %>% mutate(exdate = as.Date(exdate)-as.Date(date)) 
df$exdate <- as.numeric(df$exdate)/365
b_bool <- as.numeric(df$best_bid!=0)
b = data.frame(x=1:length(b_bool))
b$x[b_bool==0] <-"Bid=0"
b$x[b_bool!=0] <-"Bid>0"

b_bool =as.factor(b$x)#*2 + as.numeric(df$best_bid==0)
# , color= b_bool, colors = jbcolor[c(2,5)]
plot_ly(x = abs(df$nonbundle/S_init-1), y = df$best_offer-df$best_bid) %>%
  add_markers(symbol = b_bool, name = "hollow", alpha=0.9) %>%
  layout(xaxis=list(title="|K/S-1|"), yaxis= list(title="Bid-ask spread"),
         shapes = list(
           list(type = "rect",
                fillcolor = 'rgb(0,128,128)', line = list(color = 'rgb(0,128,128)'), opacity = 0.3,
                x0 = 0.4, x1 = 1, xref = "x",
                y0 = 0, y1 = 20, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(255,165,0)', line = list(color = 'rgb(255,165,0)'), opacity = 0.3,
                x0 = 0, x1 = 1, xref = "x",
                y0 = 5, y1 = 20, yref = "y"))
  )
