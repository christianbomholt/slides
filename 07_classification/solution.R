library(rpart)
library(visNetwork)
# Basic classification tree
res <- rpart(Species~., data=iris, control = rpart.control(cp = 0.15))
res
visTree(res, main = "Iris classification Tree", width = "100%")

?rpart

res <- rpart(Petal.Length~., data=iris)
visTree(res, edgesFontSize = 14, nodesFontSize = 16, width = "100%")

data("solder")
res <- rpart(Opening~., data = solder, control = rpart.control(cp = 0.00005))
visTree(res, height = "800px", nodesPopSize = TRUE, minNodeSize = 10, 
        maxNodeSize = 30, width = "100%")

df <- ggplot2::diamonds %>% mutate(value = if_else(price >= 4000,  "high", "low"))
df <- df %>% mutate(id = row_number())
#Check IDs
head(df$id)
#Create training set
train <- df %>% sample_frac(.70)
#Create test set
test  <- anti_join(df, train, by = 'id')

rpart(value ~ cut + carat + color + clarity,
      data = train,
      control = rpart.control(cp = 0.001))


dt1 <- rpart(value ~ cut + carat + color + clarity,
             data = train, control = rpart.control(cp = 0.001))
visTree(dt1)
# Prune the tree to remove unnecessary complexity from the model
dt2 <- prune(dt1, cp=0.1)
visTree(dt2)


pred <- predict(dt2,test, type = 'class')
mean(pred==test$value)