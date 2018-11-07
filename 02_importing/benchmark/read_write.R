library(feather)
library(data.table)
library(fst)
library(readr)

# The first two entries are the total user and system CPU 
# times of the current R process and any child processes on
# which it has waited, and the third entry is the ‘real’ elapsed time since the process was started.
# 
# The definition of ‘user’ and ‘system’ times is from your OS.
# Typically it is something like The ‘user time’ is the CPU time
# charged for the execution of user instructions of the calling process.
# The ‘system time’ is the CPU time charged for execution by the system on behalf of the calling process.

nr_of_rows <- 1e6

df <- data.frame(
  Logical = sample(c(TRUE, FALSE, NA), prob = c(0.85, 0.1, 0.05), nr_of_rows, replace = TRUE),
  Integer = sample(1L:100L, nr_of_rows, replace = TRUE),
  Real = sample(sample(1:10000, 20) / 100, nr_of_rows, replace = TRUE),
  Factor = as.factor(sample(labels(UScitiesD), nr_of_rows, replace = TRUE))
)

timer <- function(expr){
  res <- system.time(expr)
  as.numeric(res[3]) #%>% round(9)
}

# fst
fstwrite <- timer(write.fst(df, "dataset.fst"))
fstread  <- timer(read.fst("dataset.fst"))

#fread
dtwrite <- timer(fwrite(df, "dataset.csv"))
dtread  <- timer(fread("dataset.csv"))

#base

basewrite <- timer(write_rds(df, "dataset.rds"))
baseread  <- timer(read_rds("dataset.rds"))


#feather

featherwrite <- timer(write_feather(df, "dataset.feather"))
featherread  <- timer(read_feather("dataset.feather"))

(write_df <- data.frame(base = basewrite,
                       datatable = dtwrite,
                       fst = fstwrite,
                       feather = featherwrite))

(read_df <- data.frame(base = baseread,
                      datatable = dtread,
                      fst = fstread,
                      feather = featherread))
library(highcharter)

hchart(write_df %>% gather %>% arrange(value), "column", hcaes(x = key, y = value)) %>%
  hc_xAxis(title = list(text = "Package name")) %>% 
  hc_yAxis(title = list(text = "Write time in S for 1.000.000 rows"))

hchart(read_df %>% gather %>% arrange(value), "column", hcaes(x = key, y = value)) %>%
  hc_xAxis(title = list(text = "Package name")) %>% 
  hc_yAxis(title = list(text = "Read time in S for 1.000.000 rows"))

