setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(readxl)
library(dplyr)
library(glmnet)

df <- read_excel('../data/xl/R.xlsx',col_names = T)
df$Date <- as.Date(df$Date,origin = '1970-01-01')

train <- df %>% filter(Date<'2013-01-01')
test <- df %>% filter(Date>='2013-01-01')

X <- train %>% select(-c(Date,Yield)) %>% as.matrix()
y <- train$Yield

fit <- glmnet(X,y)

X_test <- test %>% select(-c(Date,Yield)) %>% as.matrix()

predictions <- predict(fit,X_test)

sum(abs(predictions[,40] - test$Yield))

# Find bedste værdi for flexibilitet / lambda
fit <- cv.glmnet(X,y)
predictions <- predict(fit,X_test)

sum(abs(predictions - test$Yield))
