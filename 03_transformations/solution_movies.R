
library(readr)
library(dplyr)
movies <- read_csv("R/couse_oct/data/tmdb_5000_movies.csv")

movies <- filter(movies, vote_average > 5)
movies <- filter(movies, original_language != "en")

top_budget_moovies <- arrange(movies,desc(budget))

top_budget_moovies <- select(top_budget_moovies, budget, original_title, overview)

best_rated_movies <- arrange(movies,desc(vote_average))
best_rated_movies <- select(best_rated_movies, budget, original_title, overview, vote_average)

