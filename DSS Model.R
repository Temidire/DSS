library(caret)
library(tidyverse)
library(corrplot)
library(glmnet)
library(gbm)
library(randomForest)
library(xgboost)
library(nnet)

draft <- read.csv("First Draft.csv")

#DATA CLEANING
draft[!complete.cases(draft),]

sum(is.na(draft))

draft <- draft %>%  
  mutate(Metascore = ifelse(is.na(Metascore), 
                            round(mean(Metascore, na.rm = TRUE)),
                            Metascore))

str(draft)

#DATA PREPROCESSING
draft <- draft %>%
  mutate(Release.Date = as.Date(Release.Date, format = "%d/%m/%Y")) %>% 
  mutate(Year = as.numeric(format(Release.Date, "%Y"))) %>%
  mutate(Month = as.numeric(format(Release.Date, "%m"))) %>%
  mutate(Day = as.numeric(format(Release.Date, "%d")))

draft$Budget.... <- as.numeric(draft$Budget....)
draft$Runtime..mins. <- as.numeric(draft$Runtime..mins.)
draft$Total.Hours <- as.numeric(draft$Total.Hours)
draft$Total.Views <- as.numeric(draft$Total.Views)
draft$Num.Votes <- as.numeric(draft$Num.Votes)

draft <- subset(draft, select=-c(Day, Release.Date))
draft <- subset(draft, select=-c(Total.Hours))

draft$Target <- draft$Total.Views 

draft$Budget_Normalised <- draft$Budget....

#FEATURE ENGINEERING
#Feature Encoding Categorical Variables
draft$AgeRating_enc <- factor(draft$Age_Rating,
                              levels = c("G", "PG", "PG-13", 15, "R"),
                              labels = c(0,1,2,3,4)) #Label Encode (ordinal)

draft$IsEnglish <- as.numeric(draft$Language == "English") #Binary encode
draft$Genre_enc <- factor(draft$Genre) #Label encode
#draft$Directors_enc <- factor(draft$Directors)

# Calculate average IMDb rating for each director to mean encode it to IMDb Rating
director_avg_rating <- draft %>%
  group_by(Directors) %>%
  summarise(Director_Avg_IMDb_Rating = round(mean(IMDb.Rating, na.rm = TRUE), 1))

# Merge average Director IMDb rating back to the main dataset
draft <- left_join(draft, director_avg_rating, by = "Directors")

#Mean Encode the Stars column to Total Views
star1_means <- draft %>%
  group_by(Star1) %>%
  summarise(Star1_encode = round(mean(Total.Views)))
draft <- left_join(draft, star1_means)

star2_means <- draft %>%
  group_by(Star2) %>%
  summarise(Star2_encode = round(mean(Total.Views)))
draft <- left_join(draft, star2_means)

star3_means <- draft %>%
  group_by(Star3) %>%
  summarise(Star3_encode = round(mean(Total.Views)))
draft <- left_join(draft, star3_means) #Mean Encoding

#Correlation Analysis
corIMDb <- cor(draft$IMDb.Rating, draft$Metascore)
print(corIMDb)

selected_columns <- c("Runtime..mins.", "Year", "IMDb.Rating", "Metascore", "Num.Votes", 
                      "Month", "Budget_Normalised", "IsEnglish", "Star1_encode", 
                      "Star2_encode", "Star3_encode", "Director_Avg_IMDb_Rating")

corMatrix <- cor(draft[selected_columns])
print(corMatrix)
corrplot(corMatrix)

#Remove columns not required
draft <- subset(draft, select=-c(Title, Genre, Star1, Star2, Star3, Age_Rating, 
                                 Language, Total.Views, Directors, Star1, Star2, 
                                 Star3, Age_Rating, Budget....))

draft <- draft %>%
  select(Runtime..mins., Year, Month, IMDb.Rating, Metascore,Num.Votes, 
         Budget_Normalised, IsEnglish, Genre_enc, AgeRating_enc, Star1_encode, Star2_encode,
         Star3_encode, Director_Avg_IMDb_Rating, Target)

#Normalise columns with varying figures
draft$Target <- log(draft$Target)
#draft$Target <-exp(draft$Target)
draft$Budget_Normalised <- log(draft$Budget_Normalised) #Normalise budget and Target
draft$Star1_encode <- log(draft$Star1_encode)
draft$Star2_encode <- log(draft$Star2_encode)
draft$Star3_encode <- log(draft$Star3_encode) #Normalise Stars column


#PREDICTION MODELS
#Train-Test Split
set.seed(123)
trainIndex <- createDataPartition(draft$Target, p = 0.8, list = FALSE)
train <- draft[trainIndex,]
test <- draft[-trainIndex,]

independent_vars <- c("Runtime..mins.", "Year", "Month", "IMDb.Rating", "Metascore", 
                      "Num.Votes", "Budget_Normalised", "IsEnglish", "Genre_enc", 
                      "AgeRating_enc", "Star1_encode", "Star2_encode","Star3_encode", 
                      "Director_Avg_IMDb_Rating")

#Train independent variables and combine with dependent target variable
train_data <- train[independent_vars]
train_target <- train$Target
train_combined <- cbind(train_data, Target = train_target)

#Test
test_data <- test[independent_vars]
actual_values <- test$Target

# Define the control object for cross-validation
control <- trainControl(method = "cv", number = 5)

# 1.Linear Regression
lm_model <- train(Target~., data = train_combined, method = "lm", trControl = control)
print(lm_model)
summary(lm_model)

linear_predictions <- predict(lm_model, newdata = test_data)
print(linear_predictions)

#Evaluation Metrics
rmse <- RMSE(linear_predictions, actual_values)
mae <- MAE(linear_predictions, actual_values)
r_squared <- R2(linear_predictions, actual_values)


print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# 2.Lasso Regression
lasso_model <- train(Target~., data = train_combined, method = "glmnet", 
                     trControl = control, tuneGrid = expand.grid(alpha = 1,
                                                                 lambda = seq(0.01, 1, by = 0.01)))
print(lasso_model)
summary(lasso_model)

lasso_predictions <- predict(lasso_model, newdata = test_data)
print(lasso_predictions)


rmse_lasso <- RMSE(lasso_predictions, actual_values)
mae_lasso <- MAE(lasso_predictions, actual_values)
rsquared_lasso <- R2(lasso_predictions, actual_values)

print(paste("RMSE:", rmse_lasso))
print(paste("MAE:", mae_lasso))
print(paste("R-squared:", rsquared_lasso))

saveRDS(lasso_model, "lasso_model.rds") #Save Model Object for best performing model

# 3.Random Forest
rf_model <- train(Target~., data = train_combined, method = "rf", trControl = control)
print(rf_model)
summary(rf_model)

rf_predictions <- predict(rf_model, newdata = test_data)
print(rf_predictions)

rmse_rf <- RMSE(rf_predictions, actual_values)
mae_rf <- MAE(rf_predictions, actual_values)
rsquared_rf <- R2(rf_predictions, actual_values)

print(paste("RMSE:", rmse_rf))
print(paste("MAE:", mae_rf))
print(paste("R-squared:", rsquared_rf))

#4. Gradient Boosting
gbm_model <- train(Target ~ ., data = train_combined, method = "gbm", 
                   trControl = control)
print(gbm_model)
summary(gbm_model)

gb_predictions <- predict(gbm_model, newdata = test_data)
print(gb_predictions)

#Evaluation Metrics
rmse_gb <- RMSE(gb_predictions, actual_values)
mae_gb <- MAE(gb_predictions, actual_values)
rsquared_gb <- R2(gb_predictions, actual_values)

print(paste("RMSE:", rmse_gb))
print(paste("MAE:", mae_gb))
print(paste("R-squared:", rsquared_gb))

#5. XGBoost
xgb_model <- train(Target ~ ., data = train_combined, method = "xgbTree", 
                   trControl = control)
print(xgb_model)
summary(xgb_model)

xgb_predictions <- predict(xgb_model, newdata = test_data)
print(xgb_predictions)

#Evaluation Metrics
rmse_xgb <- RMSE(xgb_predictions, actual_values)
mae_xgb <- MAE(xgb_predictions, actual_values)
rsquared_xgb <- R2(xgb_predictions, actual_values)

print(paste("RMSE:", rmse_xgb))
print(paste("MAE:", mae_xgb))
print(paste("R-squared:", rsquared_xgb))

#6. Neural Network
nn_model <- train(Target ~ ., data = train_combined, method = "nnet", 
                  trControl = control)
tuneGrid <- expand.grid(size = c(5, 10),
                        decay = c(0.001, 0.01))  
nn_model <- train(Target ~ . , data = train_combined,  
                  method = "nnet",
                  trControl = control,
                  tuneGrid = tuneGrid)

print(nn_model)
summary(nn_model)

nn_predictions <- predict(nn_model, newdata = test_data)
print(nn_predictions)

#Evaluation Metrics
rmse_nn <- RMSE(nn_predictions, actual_values)
mae_nn <- MAE(nn_predictions, actual_values)
rsquared_nn <- R2(nn_predictions, actual_values)

print(paste("RMSE:", rmse_nn))
print(paste("MAE:", mae_nn))
print(paste("R-squared:", rsquared_nn))



#The best performing model is Lasso Regression
#Use it to train the entire dataset without the train-tes split
draft_LR <- draft
draft_LR$Genre_enc <- as.numeric(draft_LR$Genre_enc)
draft_LR$AgeRating_enc <- as.numeric(draft_LR$AgeRating_enc)

# Split the data into predictors and target
X <- draft_LR[, -which(names(draft) == "Target")]
y <- draft_LR$Target

# Train Lasso Regression model using the entire dataset
lasso_model_final <- glmnet(X, y, alpha=1)

# Print the coefficients of the trained model
print(lasso_model_final)
saveRDS(lasso_model_final, "lasso_model_final.rds")

# Make predictions using the trained model
lasso_predictions_final <- predict(lasso_model_final, s=0.01, newx = as.matrix(X)) # Adjust lambda (s) value as needed
print(lasso_predictions_final)
print(y)

# Evaluate the model using metrics of your choice
rmse_lasso_final <- RMSE(lasso_predictions_final, y)
mae_lasso_final <- MAE(lasso_predictions_final, y)
rsquared_lasso_final <- R2(lasso_predictions_final, y)

# Print evaluation metrics
print(paste("RMSE:", rmse_lasso_final))
print(paste("MAE:", mae_lasso_final))
print(paste("R-squared:", rsquared_lasso_final))

#Residual Plot
lasso_residuals <- draft$Target - lasso_predictions_final
print(lasso_residuals)

plot(lasso_predictions_final, lasso_residuals, xlab = "Predicted Values", ylab =  "Residuals",
     main = "Residual Plot for Lasso Regression")
abline(h = 0, col = "red") #Residual Plot

# Get a list of independent variable names
independent_vars2 <- colnames(draft)[colnames(draft) != "Total_Views"]  # Exclude the target variable

# Create a loop to plot residuals against each independent variable
for (var in independent_vars2) {
  plot(draft[, var], lasso_residuals, xlab = var, ylab = "Residuals", 
       main = paste("Residual Plot for", var))
  abline(h = 0, col = "red")
}








