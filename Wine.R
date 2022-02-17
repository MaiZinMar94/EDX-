#Install useful packages that could help with the analysis

packages <- c("ggplot2", "dplyr", "tidyr", "corrplot", "grid", "gridExtra", "Hmisc", "lattice", "readxl", "patchwork", "tidyverse", 
              "scales","dslabs", "stringr","forcats","lubridate","validate","caret","data.table","ggridges", "randomForest")

# Install packages not yet installed

installed_packages <- packages %in% rownames(installed.packages())

if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}

# Packages loading

lapply(packages, library, character.only = TRUE)

##########################################################
# Look up the data
##########################################################

wine <- read.csv("~/Desktop/wineQualityReds.csv")
view(wine)

#Make a new set to start the data analysis 

wine1 <- wine

# 1.1: Look at the data set
summary(wine1)
str(wine1)

# 1.2: Are there any NA's?

anyNA(wine1)

#--> no NA's in data set 

wine1 <- rename(wine1,  Score = quality)

#Data visualization
#Convert wide table to long:
wine1_long <- gather(wine1, analysis, results,fixed.acidity:alcohol, factor_key=TRUE)
wine1_long

#Group analysis:
wine1_long %>% group_by(analysis) 

#Convert analysis and score into factor
wine1_long$analysis <- factor(wine1_long$analysis, levels=unique(wine1_long$analysis))
wine1_long$Score <- factor(wine1_long$Score, levels = c("3","4","5","6","7","8"))

#Overview all values: 
p0 <- wine1_long %>% group_by(Score) %>% 
  ggplot(aes(Score, results, color=Score))+
  geom_point()

p0 +  facet_wrap(~analysis, scales="free_y")


#Boxplot: 
p1 <- wine1_long %>% group_by(Score) %>% 
  ggplot(aes(Score, results, fill=Score))+
  geom_boxplot()

p1 +  facet_wrap(~analysis, scales="free_y")

#Boxplot has it's limitation too when it handles large datasets, 
#therefore, the ridge plot was created in order to see if we might have
#a bimodal distribution

p2 <- wine1_long %>% group_by(Score) %>%
  ggplot(aes(results, Score, fill=Score)) + 
  scale_x_continuous()+
  geom_density_ridges()

p2 +  facet_wrap(~analysis, scales="free_x")

#Correlation matrix to see which parameters correlate with results with a 
#significance level of 0.05.
#first step remove the column X, for this a new copy of wine1 data was created (not necessary needed if you don't want to) and a new name was given

wine1corr <- wine1

wine1corr <- wine1corr[,-1]

Mycorr = rcorr(as.matrix(wine1corr), type = "spearman")

corrplot(corr =Mycorr$r,
         p.mat = Mycorr$P,
         order ="original",
         sig.level =0.05,
         insig ="blank",
         diag = F,
         type = "lower")

#We can see that fixed acidity correlates positively with citric acid and density, while it correlates negatively with pH. 

#There are only weak correlations with score. it seems like the 2 parameters having the most impact are alcohol and volatile acidity
#volatile acidity, alcohol and sulphates were investigated a bit further:

p3 <- wine1 %>% group_by(Score) %>% 
  summarise(mean_alc = mean(alcohol), count=n()) %>%
  ggplot(aes(Score, mean_alc))+
  geom_point()+
  geom_smooth()+
  labs(title="Average alcohol content per score",x="Score",y="Average of alcohol content")

p3

p4 <- wine1 %>% group_by(Score) %>% 
  summarise(mean_vol = mean(volatile.acidity), count=n()) %>%
  ggplot(aes(Score, mean_vol))+
  geom_point()+
  geom_smooth()+
  labs(title="Average volatile acidity per score",x="Score",y="Average of volatile acidity")

p4

p5 <- wine1 %>% group_by(Score) %>% 
  summarise(mean_sul = mean(sulphates), count=n()) %>%
  ggplot(aes(Score, mean_sul))+
  geom_point()+
  geom_smooth()+
  labs(title="Average sulphates content per score",x="Score",y="Average of sulphates content")

p5

p3 + p4 + p5

#the graph confirms the observations seen in the correlation matrix

# Validation set will be 50% of Wine data
set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(wine1$Score, times = 1, p = 0.5, list = FALSE)

#Split into training and test sets
train <- wine1[test_index,]
test <- wine1[-test_index,]

#2. Step: Model 

#Variables to include in model are : alcohol, volatile acidity and sulphates
#Please be aware, summarise is written with "s" --> summarize with "z" did not work with my R and gave me an error

#To not include variables in the test set that do not appear in the training set
#the entire were removed with semi joint
test <- test %>% 
  semi_join(train, by = "alcohol") %>%
  semi_join(train, by = "volatile.acidity") %>%
  semi_join(train, by = "sulphates")

#2.1 Residual Mean Squared Error function to be used for analysis

RMSE <- function(true_prediction, predicted_prediction){
  sqrt(mean((true_prediction - predicted_prediction)^2))}

#2.2 First Model predicting score regardless of other factors
mu_score <- mean(train$Score)
mu_score 

mu_score2 <- mean(test$Score)
mu_score2 

first_rmse <- RMSE(test$Score, mu_score)
first_rmse 

#Create results dataframe with RMSE results

RMSE_Result <- data.frame(model="Baseline Model", RMSE=first_rmse)
RMSE_Result %>% knitr::kable()

#This model will be used as base and RMSE will be tried to improve.  

#2.2 Model predicting the alcohol, volatile acidity and sulphates effect 

alcohol_avg <- train %>%
  group_by(alcohol) %>%
  summarise(b_a = mean(Score - mu_score))

acidity_avg <- train %>% 
  left_join(alcohol_avg, by='alcohol') %>%
  group_by(volatile.acidity) %>%
  summarise(b_v = mean(Score - mu_score - b_a))

sulphates_avg <- train %>% 
  left_join(alcohol_avg, by='alcohol') %>%
  left_join(acidity_avg, by='volatile.acidity') %>%
  group_by(sulphates) %>%
  summarise(b_s = mean(Score - mu_score - b_a - b_v))

predicted_score <- test %>% 
  left_join(alcohol_avg, by='alcohol') %>%
  left_join(acidity_avg, by='volatile.acidity') %>%
  left_join(sulphates_avg, by='sulphates') %>%
  mutate(pred = mu_score + b_a + b_v + b_s) %>%
  pull(pred)

Combined_effect <-RMSE(predicted_score, test$Score)
Combined_effect

#Create results dataframe with RMSE results

RMSE_Result <- bind_rows(RMSE_Result,
                          data_frame(model="Combined effect Model",  
                                     RMSE = Combined_effect))
RMSE_Result %>% knitr::kable()

#2.3 Regularized model, chose lambda (tuning parameter) for alcohol, volatile acidity and sulphates effect 
# Note: the below code takes some time
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  mu_score <- mean(train$Score)
  
  b_a <- train %>% 
    group_by(alcohol) %>%
    summarise(b_a = sum(Score - mu_score)/(n()+l))
  
  b_v <- train %>% 
    left_join(b_a, by="alcohol") %>%
    group_by(volatile.acidity) %>%
    summarise(b_v = sum(Score - b_a - mu_score)/(n()+l))
  
  b_s <- train %>% 
    left_join(b_a, by="alcohol") %>%
    left_join(b_v, by="volatile.acidity") %>%
    group_by(sulphates) %>%
    summarise(b_s = sum(Score - b_a - b_v - mu_score)/(n()+l))
  
  predicted_score <- test %>% 
    left_join(b_a, by = "alcohol") %>%
    left_join(b_v, by = "volatile.acidity") %>%
    left_join(b_s, by = "sulphates") %>%
    mutate(pred = mu_score + b_a + b_v + b_s) %>%
    pull(pred)
  
  return(RMSE(predicted_score, test$Score))
})

# Plot rmses vs lambdas to select the optimal lambda
qplot(lambdas, rmses)  

#Find ideal lambda
lambda1 <- lambdas[which.min(rmses)]
lambda1 

#Regularized model using lambda to improve predictions for the 3 effects

alcohol_avg <- train %>%
  group_by(alcohol) %>%
  summarise(b_a = sum(Score - mu_score)/(n()+lambda1), n_a = n())

acidity_avg <- train %>% 
  left_join(alcohol_avg, by='alcohol') %>%
  group_by(volatile.acidity) %>%
  summarise(b_v = sum(Score - mu_score - b_a)/(n()+lambda1), n_v = n())

sulphates_avg <- train %>% 
  left_join(alcohol_avg, by='alcohol') %>%
  left_join(acidity_avg, by='volatile.acidity') %>%
  group_by(sulphates) %>%
  summarise(b_s = sum(Score - mu_score - b_a - b_v)/(n()+lambda1), n_s = n())

predicted_score <- test %>% 
  left_join(alcohol_avg, by = "alcohol") %>%
  left_join(acidity_avg, by = "volatile.acidity") %>%
  left_join(sulphates_avg, by = "sulphates") %>%
  mutate(pred = mu_score + b_a + b_v + b_s) %>%
  pull(pred)

reg_m_u_effect <- RMSE(predicted_score, test$Score)

#Create results dataframe with RMSE results

RMSE_Result <- bind_rows(RMSE_Result,
                          data_frame(model="Reg. Model 3 parameters effect",  
                                     RMSE = reg_m_u_effect))
RMSE_Result %>% knitr::kable()

#Another approach would be to use the random forest model, 
# which helps to improve prediction performance
#this model is based on creating multiple decision trees to improve prediction
#As we are working with a regression model and not a classification model, no accuracy can be predicted. 
#The prediction will be made for RMSE (Root Mean Squared Error) and MAE (Mean Absolute Error)

#First define the control measurements and evaluate the model with a grid search of 10 folder
trainControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


#try rainforest model simple formula
rf1 <- train(Score~., train, method = "rf", trControl = trainControl)

rf1$results
print(rf1)

#the best mtry for the the model is 2 with an RMSE of 0.6029878. Let's try to optimize this and search for a better mtry between 1 and 12.
#be aware this code takes some time

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 12))

rf2 <- train(Score~., train, method = "rf", trControl = trainControl, tuneGrid =tuneGrid, importance = TRUE)

rf2$results
print(rf2)

#the best mtry for the the model is 5 with an RMSE of 0.6015840. 
#(results might vary as the calculation is made randomly)

#Now let's evaluate the model

prediction <-predict(rf2, test)
prediction

table(prediction,test$Score)

RF_Model<- RMSE(prediction, test$Score)

#Create results dataframe with RMSE results

RMSE_Result <- bind_rows(RMSE_Result,
                         data_frame(model="Random Forest Model",  
                                    RMSE = RF_Model))
RMSE_Result %>% knitr::kable()


