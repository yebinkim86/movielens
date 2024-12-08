# Install & load packages that can be needed in this project
install.packages("tidyverse")
install.packages("caret")
install.packages("dslabs")

library(tidyverse)
library(caret)
library(dslabs)

# Load the MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
data(movielens)

# Validation data set will be 10% of MovieLens data
set.seed(1)
test_index<-createDataPartition(y=movielens$rating, times=1, p=0.1, list=FALSE)
edx<-movielens[-test_index,]
valid<-movielens[test_index,]

# Make sure userId and movieId in validation data set are also in edx set
validation<-valid %>% 
  semi_join(edx, by="movieId") %>%
  semi_join(edx, by="userId")

# Add rows removed from validation set back into edx set
removed<-anti_join(valid, validation)
edx<-rbind(edx,removed)

# Split edx dataset into training and test set
## Create train set and test set
set.seed(1)
test_index<-createDataPartition(y=edx$rating, times=1, p=0.2, list=FALSE)
edx_train_set<-edx[-test_index,]
edx_test_set<-edx[test_index,]

## Make sure userId and movieId in validation set are also in edx set
edx_test_set <- valid %>% 
  semi_join(edx_train_set, by = 'movieId') %>%
  semi_join(edx_train_set, by = 'userId')

## Add rows removed from validation set back into edx set
removed <- anti_join(valid, edx_test_set)
edx_train_set <- rbind(edx_train_set, removed)

#The RMSE function that can be used in this project
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}


## Extract the genre in edx&validation datasets
#edx
edx <- edx %>%
  mutate(genres = fct_explicit_na(genres,
                                 na_level = "(no genres listed)")
  ) %>%
  separate_rows(genres, sep = "\\|")

#validation
validation <- validation %>%
  mutate(genres = fct_explicit_na(genres,
                                 na_level = "(no genres listed)")
  ) %>%
  separate_rows(genres,
                sep = "\\|")

# Select necessary columns in edx&validation dataset
edx <- edx %>% select(userId, movieId, rating, title, genres)
validation <- validation %>% select(userId, movieId, rating, title, genres)

# Average of all movies
mu_hat <- mean(edx_train_set$rating)
naive_rmse <- RMSE(edx_test_set$rating, mu_hat)
naive_rmse

# Average by movie
movie_avg <- edx_train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))

# Prediction by movie
predicted_ratings_m<-mu_hat + edx_test_set %>% left_join(movie_avg,by='movieId') %>% pull(b_i)
movie_effect<-RMSE(predicted_ratings_m,edx_test_set$rating)
movie_effect

# Average rating for users that have rated over 120 movies
edx_train_set %>%
group_by(userId) %>%
summarize(b_u = mean(rating)) %>%
filter(n()>=120) %>%
ggplot(aes(b_u)) +
geom_histogram(bins = 30, color = "black")

# Average by movie&user
user_avg <- edx_train_set %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat-b_i))

# Prediction by movie&user
predicted_ratings_m_u<-edx_test_set %>%
  left_join(movie_avg,by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(p=mu_hat+b_i+b_u) %>% pull(p)
movie_user_effect<-RMSE(predicted_ratings_m_u,edx_test_set$rating)
movie_user_effect

# Average by movie&user&genre
genre_avg <- edx_train_set %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg,by='userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu_hat-b_i-b_u))

# Prediction by movie&user&genre
predicted_ratings_m_u_g<-edx_test_set %>%
  left_join(movie_avg,by='movieId') %>%
  left_join(user_avg, by='userId') %>% 
  left_join(genre_avg, by='genres') %>%
  mutate(p=mu_hat+b_i+b_u+b_g) %>%
  pull(p)

movie_user_genre_effect<-RMSE(predicted_ratings_m_u_g,edx_test_set$rating)
movie_user_genre_effect

#Find the optimal lambda for Regularization
lambdas <- seq(0, 10, 0.1)
r_rmse <- sapply(lambdas, function(l){
  b_i_s <- edx_train_set %>%
    group_by(movieId) %>%
    summarize(b_i_s = sum(rating - mu_hat)/(n()+l))
  b_u_s <- edx_train_set %>%
    left_join(b_i_s, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u_s = sum(rating - b_i_s - mu_hat)/(n()+l))
  b_g_s <- edx_train_set %>%
    left_join(b_i_s, by='movieId') %>%
    left_join(b_u_s, by='userId') %>%
    group_by(genres) %>%
    summarize(b_g_s = sum(rating - b_i_s - b_u_s - mu_hat)/(n()+l))
    
  predicted_ratings <- edx_test_set %>%
    left_join(b_i_s, by = 'movieId') %>%
    left_join(b_u_s, by = 'userId') %>%
    left_join(b_g_s, by= 'genres') %>%
    mutate(p_r = mu_hat + b_i_s + b_u_s + b_g_s) %>%
    pull(p_r)
  return(RMSE(predicted_ratings, edx_test_set$rating))
})

qplot(lambdas, r_rmse)

lambda <- lambdas[which.min(r_rmse)]
lambda


# Regularization
movie_reg_avg <- edx_train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu_hat) / (n() + lambda), n_i = n())

user_reg_avg <- edx_train_set %>%
  left_join(movie_reg_avg, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu_hat - b_i) / (n() + lambda), n_u = n())

genre_reg_avg <- edx_train_set %>%
  left_join(movie_reg_avg, by = "movieId") %>%
  left_join(user_reg_avg, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu_hat - b_i - b_u) / (n() + lambda), n_g = n())

predicted_ratings <- edx_test_set %>%
  left_join(movie_reg_avg, by = "movieId") %>%
  left_join(user_reg_avg, by = "userId") %>%
  left_join(genre_reg_avg, by = "genres") %>%
  summarize(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

movie_user_genre_reg_model <- RMSE(predicted_ratings, edx_test_set$rating)
movie_user_genre_reg_model

############################################

# Final check with Validation data set
predicted_ratings <- validation %>%
  left_join(movie_reg_avg, by = 'movieId') %>%
  left_join(user_reg_avg, by = 'userId') %>%
  left_join(genre_reg_avg, by = 'genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_g) %>%
  pull(pred)

valid_predictions <- predicted_ratings[!is.na(predicted_ratings)]
valid_ratings <- validation$rating[!is.na(predicted_ratings)]

final_model <- RMSE(valid_predictions, valid_ratings)
final_model