##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##########################################################
# MovieLens Project 2021
##########################################################
#Install useful packages that could help with the analysis

packages <- c("ggplot2", "dplyr", "tidyr", "corrplot", "grid", "gridExtra", "Hmisc", "lattice", "readxl", "tidyverse", 
              "scales","dslabs", "stringr","forcats","lubridate","validate","caret")

# Install packages not yet installed

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}

# Packages loading

lapply(packages, library, character.only = TRUE)

# After reading the information https://files.grouplens.org/datasets/movielens/ml-10m-README.html: 
# 1. Step: Data overview, I creaeted a copy of edx (edx1) 
edx1 <-edx
edx1
# 1.1: Look at the data in general
summary(edx1)

# 1.2: Are there any NA's?

anyNA(edx1)

#1.3: Convert timestamp in readible date

edx1$timestamp <- as.numeric(edx1$timestamp)
edx1$timestamp <- as.POSIXct(edx1$timestamp, origin="1970-01-01")

validation$timestamp <- as.numeric(validation$timestamp)
validation$timestamp <- as.POSIXct(validation$timestamp, origin="1970-01-01")

class(edx1$timestamp)

#Take year of rating in new column
edx1$ratingyear <-format(edx1$timestamp, format="%Y")

validation$ratingyear <-format(validation$timestamp, format="%Y")

#1.4: Take year of release in new column and set as numeric
edx1$releaseyear<-substr(edx1$title,nchar(as.character(edx1$title))-4,nchar(as.character(edx1$title))-1)
edx1$title<-paste0(substr(edx1$title,1,nchar(as.character(edx1$title))-6))

edx1$releaseyear<-as.numeric(edx1$releaseyear)

validation$releaseyear<-substr(validation$title,nchar(as.character(validation$title))-4,nchar(as.character(validation$title))-1)
validation$title<-paste0(substr(validation$title,1,nchar(as.character(validation$title))-6))

validation$releaseyear<-as.numeric(validation$releaseyear)

#1.5: Group by Year of release and define mean rating and number of ratings for release year

edx1[order(edx1$releaseyear),]

edx1 %>% group_by(releaseyear) %>%
  summarise(meanrating = mean(rating), count=n()) %>%
  ggplot(aes(x=releaseyear, y=meanrating))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title="Average ratings per release year",x="Release year",y="Average of Ratings")

#Movies released up to approx.1980 have an average rating of 3.8-3.9 while movies after 1980 have a lower rating

edx1 %>% group_by(releaseyear) %>%
  summarise(meanrating = mean(rating), count=n()) %>%
  ggplot(aes(x=releaseyear, y=count))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title="Nr. of ratings per released year",x="Release year",y="Counts")

#There are not a lot of ratings for movies before 1980, after 1980 the number of ratings per release year increased up to approx. 1995 and decreased once more.

#1.6: Group by Year of rating and define mean rating and number of ratings for rating year
edx1[order(edx1$ratingyear),]

edx1 %>% group_by(ratingyear) %>%
  summarise(meanrating = mean(rating), count=n()) %>%
  ggplot(aes(x=ratingyear, y=meanrating))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title="Average ratings of movies per rating year",x="Year of rating",y="Average of Ratings")

edx1 %>% group_by(ratingyear) %>%
  summarise(meanrating = mean(rating), count=n()) %>%
  ggplot(aes(x=ratingyear, y=count))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  labs(title="Number of movies rated per year",x="Year",y="Counts") 

#With these graph we can confirm partially the observations made before. Ratings started from 1995 and the average of rating in the first year is 4.
#From 1996 up to 2009 people rated in general the movies in average with 3.5. This lower average rating can be explained by the quantity of movies that were at disposal and users that could rate. 
#Also movies released before 1980 were not much rated as maybe the inreset in these movies were low.

# 1.7 Review rating distribution

edx1 %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.5,colour="darkblue", fill="blue") +
  scale_y_continuous(breaks=c(seq(0,3000000,500000))) +
  labs(title="Distribution of ratings",x="Rating",y="Nr. of Ratings") 

#It is interesting to see that people in general rated either with a "full" number (ex.3,4,5) and did not rate movies with ex. ".5" numbers. 

#2. Step: Model

#Variables to include in model are : Movie ID, User ID and Release year 
#Please be aware, summarise is written with "s" --> summarize with "z" did not work with my R and gave me an error

#2.1 Residual Mean Squared Error function to be used for analysis

RMSE <- function(true_prediction, predicted_prediction){
  sqrt(mean((true_prediction - predicted_prediction)^2))}


#2.2 First Model predicting rating regardless of other factors (user,year, etc..)
mu_rating <- mean(edx1$rating)
mu_rating #3.512465

first_rmse <- RMSE(validation$rating, mu_rating)
first_rmse #1.061202

#Create results dataframe with RMSE results

RMSE_Results <- data.frame(model="Baseline Model", RMSE=first_rmse)
RMSE_Results %>% knitr::kable()

#This model will be used as base and RMSE will be tried to improve.  

#2.3.1 Second model predicting the movie effect

movie_avg <- edx1 %>%
  group_by(movieId) %>%
  summarise(b_m = mean(rating - mu_rating))

predicted_ratings <- mu_rating + validation %>% 
  left_join(movie_avg, by='movieId') %>%
  pull(b_m)

Movie_effect <- RMSE(predicted_ratings, validation$rating) #0.9439087

#Create results dataframe with RMSE results

RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(model="Movie effect Model",  
                                     RMSE = Movie_effect))
RMSE_Results %>% knitr::kable()

#2.3.2 Second model predicting the user effect

user_avg <- edx1 %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_rating))

predicted_ratings <- mu_rating + validation %>% 
  left_join(user_avg, by='userId') %>%
  pull(b_u)
User_effect <- RMSE(predicted_ratings, validation$rating) #0.978336

#Create results dataframe with RMSE results

RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(model="User effect Model",  
                                     RMSE = User_effect))
RMSE_Results %>% knitr::kable()

#2.3.3 Second model predicting the release year effect

releaseyear_avg <- edx1 %>%
  group_by(releaseyear) %>%
  summarise(b_r = mean(rating - mu_rating))

predicted_ratings <- mu_rating + validation %>% 
  left_join(releaseyear_avg, by='releaseyear') %>%
  pull(b_r)
Releaseyear_effect <- RMSE(predicted_ratings, validation$rating) #1.0500259

#Create results dataframe with RMSE results

RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(model="Release Year effect Model",  
                                     RMSE = Releaseyear_effect))
RMSE_Results %>% knitr::kable()

#2.4.1 Third model predicting the movie effect and user to improve the RMSE

user_avg <- edx1 %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_rating - b_m))

predicted_ratings <- validation %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu_rating + b_m + b_u) %>%
  pull(pred)

Mo_U_effect <-RMSE(predicted_ratings, validation$rating) #0.8653488

#Create results dataframe with RMSE results

RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(model="Movie and User effect Model",  
                                     RMSE = Mo_U_effect))
RMSE_Results %>% knitr::kable()

#2.4.2 Third model predicting the movie effect, user and release year to improve the RMSE

user_avg <- edx1 %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_rating - b_m))
  
releaseyear_avg <- edx1 %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  group_by(releaseyear) %>%
  summarise(b_r = mean(rating - mu_rating - b_m - b_u))

predicted_ratings <- validation %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  left_join(releaseyear_avg, by='releaseyear') %>%
  mutate(pred = mu_rating + b_m + b_u + b_r) %>%
  pull(pred)

Combined_effect <-RMSE(predicted_ratings, validation$rating) #0.8650043

#Create results dataframe with RMSE results

RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(model="Combined effect Model",  
                                     RMSE = Combined_effect))
RMSE_Results %>% knitr::kable()

#2.5.1 Regularized model, chose lambda (tuning parameter) for movie and user effect 
# Note: the below code takes some time
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu_rating <- mean(edx1$rating)
  
  b_m <- edx1 %>% 
    group_by(movieId) %>%
    summarise(b_m = sum(rating - mu_rating)/(n()+l))
  
  b_u <- edx1 %>% 
    left_join(b_m, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_m - mu_rating)/(n()+l))
  
  predicted_ratings <- validation %>% 
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu_rating + b_m + b_u) %>%
    pull(pred)

  return(RMSE(predicted_ratings, validation$rating))
  })

# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  

#Find ideal lambda
lambda1 <- lambdas[which.min(rmses)]
lambda1 #5.25

#Regularized model using lambda to improve predictions for movie and user effect

movie_avg <- edx1 %>% 
  group_by(movieId) %>% 
  summarise(b_m = sum(rating - mu_rating)/(n()+lambda1), n_m = n())

user_avg <- edx1 %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu_rating - b_m)/(n()+lambda1), n_u = n())

predicted_ratings <- validation %>% 
    left_join(movie_avg, by='movieId') %>% 
    left_join(user_avg, by='userId') %>%
    mutate(pred = mu_rating + b_m + b_u) %>%
    pull(pred)

reg_m_u_effect <- RMSE(predicted_ratings, validation$rating)

#Create results dataframe with RMSE results

RMSE_Results <- bind_rows(RMSE_Results,
                          data_frame(model="Reg. Model Movie and User effect",  
                                     RMSE = reg_m_u_effect))
RMSE_Results %>% knitr::kable()

#In this analysis we used only Movie and User effect combination and we saw that 
#RMSE improved from 0.8653488 to 0.8648170 using the regularized model. 
#As we expect to see a similar impact if we would repeat this analysis with 3 parameters (movie, user and release year), the analysis was not performed. 

