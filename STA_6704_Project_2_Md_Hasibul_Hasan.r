# Loading packages
library(recommenderlab)
library(maditr)
library(ggplot2)
# Load the train data
df_train <-read.csv("movielens_100k.base", header = FALSE, sep ='\t')
names(df_train) <- c('userId', 'movieId', 'rating', 'timestamp')
# Load the test data
df_test<-read.csv("movielens_100k.test", header = FALSE, sep ='\t')
names(df_test) <- c('userId', 'movieId', 'rating', 'timestamp')
#Exploratory Data Analysis
df_train$userId <- as.factor(df_train$userId)
df_train$movieId <- as.factor(df_train$movieId)
rm <- dcast(df_train, userId~movieId, value.var = "rating", na.rm=FALSE)
rm <- as.matrix(rm[,-1])
rm <- as(rm, "realRatingMatrix")
rating_values <- as.vector(rm@data)
#Removing 0 (Missing Value)
rating_values <- rating_values[rating_values != 0]
rating_values <- factor(rating_values)
qplot(rating_values,xlab = "Rating", ylab = "Frequency",colour =  I("blue"), fill = I("blue")) +
  ggtitle("")
#Top 10 Movies
views_per_movie <- colCounts(rm)
views_count <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie)
views_count <- views_count[order(views_count$views, 
                                 decreasing = TRUE), ]
views_count[1:10,]
ggplot(views_count[1:10, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity",colour =  I("blue"), fill = I("blue")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Top 10 Movies based on views")
#Average Rating Distribution
average_ratings <- colMeans(rm)
qplot(average_ratings,xlab = "Rating", ylab = "Frequency") + 
  stat_bin(bins = 10) +
  ggtitle("Distribution of the average movie rating")
#Average Rating Per User
average_ratings_per_user <- rowMeans(rm)
qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of Average Ratings, per User")
#Filter
df_test$movieId <- ifelse(df_test$movieId %in% df_train$movieId,df_test$movieId,10000)
df_train$movieId <- ifelse(df_train$movieId %in% df_test$movieId,df_train$movieId,10000)
df_test <- df_test[df_test$movieId != 10000, ]
df_train <- df_train[df_train$movieId != 10000, ]
df_train$userId <- as.factor(df_train$userId)
df_train$movieId <- as.factor(df_train$movieId)
rm_train <- dcast(df_train, userId~movieId, value.var = "rating", na.rm_train=FALSE)
rm_train <- as.matrix(rm_train[,-1])
rm_train <- as(rm_train, "realRatingMatrix")
df_test$userId <- as.factor(df_test$userId)
df_test$movieId <- as.factor(df_test$movieId)
rm_test <- dcast(df_test, userId~movieId, value.var = "rating", na.rm_test=FALSE)
rm_test <- as.matrix(rm_test[,-1])
rm_test <- as(rm_test, "realRatingMatrix")
#IBCF
movie_model_IBCF <- Recommender(data = rm_train, 
                                method = "IBCF",
                                parameter = list(k = 90))
model_predictions_IBCF <- predict(object = movie_model_IBCF, 
                                  newdata = rm_test, 
                                  n = 10, 
                                  type = "ratingMatrix")
evaluate_accuracy_IBCF <- calcPredictionAccuracy(x = model_predictions_IBCF, 
                                            data = rm_test, 
                                            byUser = FALSE,
                                            goodRating = 3,
                                            given = 3)
evaluate_accuracy_IBCF
evaluate_accuracy_by_user_IBCF <- calcPredictionAccuracy(x = model_predictions_IBCF, 
                                            data = rm_test, 
                                            byUser = TRUE,
                                            goodRating = 3,
                                            given = 3)
model_predictions_IBCF <- predict(object = movie_model_IBCF, 
                                  newdata = rm_test, 
                                  n = 10)
user_recommendation_1 <- model_predictions_IBCF@items[[1]] 
movies_user_1 <- model_predictions_IBCF@itemLabels[user_recommendation_1]
movies_user_2 <- movies_user_1
for (i in 1:10){
  movies_user_2[i] <- as.character(subset(df_train, 
                                          df_train$movieId == movies_user_1[i])$movieId)
}
movies_user_2
matrix_recommendation <- sapply(model_predictions_IBCF@items, 
                                function(x){ as.integer(colnames(rm)[x]) }) 
#UBCF
movie_model_UBCF <- Recommender(data = rm_train, 
                           method = "UBCF")
model_predictions_UBCF <- predict(object = movie_model_UBCF, 
                                  newdata = rm_test, 
                                  n = 10, 
                                  type = "ratingMatrix")
evaluate_accuracy_UBCF <- calcPredictionAccuracy(x = model_predictions_UBCF, 
                                            data = rm_test, 
                                            byUser = FALSE,
                                            goodRating = 3,
                                            given = 3)
evaluate_accuracy_by_user_UBCF <- calcPredictionAccuracy(x = model_predictions_UBCF, 
                                                    data = rm_test, 
                                                    byUser = TRUE,
                                                    goodRating = 3,
                                                    given = 3)
evaluate_accuracy_UBCF
evaluate_accuracy_by_user_UBCF
#Recommended Movies UBCF
model_predictions_UBCF <- predict(object = movie_model_UBCF, 
                                  newdata = rm_test, 
                                  n = 10)
user_recommendation_1 <- model_predictions_UBCF@items[[1]] 
movies_user_1 <- model_predictions_UBCF@itemLabels[user_recommendation_1]
movies_user_2 <- movies_user_1
for (i in 1:10){
  movies_user_2[i] <- as.character(subset(df_train, 
                                          df_train$movieId == movies_user_1[i])$movieId)
}
matrix_recommendation_UBCF <- sapply(model_predictions_UBCF@items, 
                                function(x){ as.integer(colnames(rm)[x]) }) 