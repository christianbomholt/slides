


library(readxl)
path <- "../data/xl/haver.xlsx"
data <- read_excel(path, skip = 1, na = "#N/A")#, col_types = c("text",rep("numeric",5))) 
str(data)
colnames(data)[1] <- "Date"

# system(paste("xdg-open",path))

library(plotly)
data %>%
  tidyr::gather(variable, value, -Date) %>%
  transform(id = as.integer(factor(variable))) %>%
  plot_ly(x = ~Date, y = ~value, color = ~variable, colors = "Dark2",
          yaxis = ~paste0("y", id)) %>%
  add_lines() %>% 
  subplot(nrows = 4, shareX = TRUE)

library(readr)
nr_of_rows <- 2000

date <- seq(from = as.Date("2013-01-01"),length.out = nr_of_rows, by=1)


df <- data.frame(
  Date = format(date, "%m/%d/%Y"),
  Logical = sample(c(TRUE, FALSE, NA), prob = c(0.85, 0.1, 0.05), nr_of_rows, replace = TRUE),
  Integer = sample(1L:100L, nr_of_rows, replace = TRUE),
  Real = sample(sample(1:10000, 20) / 100, nr_of_rows, replace = TRUE),
  Factor = as.factor(sample(labels(UScitiesD), nr_of_rows, replace = TRUE))
) %>% apply(., 2, as.character)

tdf <- data.frame(
  Date = c("#NNAA","#NNAA"),
  Logical = c("#NNAA","#NNAA"),
  Integer = c("#NNAA","#NNAA"),
  Real = c("#NNAA","#NNAA"),
  Factor = c("#NNAA","#NNAA")
)

df <- rbind.data.frame(tdf,df)

path <- "../data/exercise.csv"
write_csv(df, path)

data <- read_csv(path, na = "#NNAA")


df <- data.frame(
  Date = format(date, "%Y-%m-%d"),
  Logical = sample(c(TRUE, FALSE, NA), prob = c(0.85, 0.1, 0.05), nr_of_rows, replace = TRUE),
  Integer = sample(1L:100L, nr_of_rows, replace = TRUE),
  Real = sample(sample(1:10000, 20) / 100, nr_of_rows, replace = TRUE),
  Factor = as.factor(sample(labels(UScitiesD), nr_of_rows, replace = TRUE))
) %>% apply(., 2, as.character)

tdf <- data.frame(
  Date = LETTERS[1:4],
  Logical = LETTERS[1:4],
  Integer = LETTERS[1:4],
  Real = LETTERS[1:4],
  Factor = LETTERS[1:4]
)

df <- rbind.data.frame(tdf,df)

path <- "../data/exercise_2.csv"
write_csv(df, path)
data <- read_csv(path)
data <- read_csv(path, col_types = "Dlinc")#c("D","l",'i','n',"c")
data <- read_csv(path, col_names = colnames(data), skip = 5)

data %>% filter(complete.cases(.))

data <- read_csv(readr_example("challenge.csv"))
