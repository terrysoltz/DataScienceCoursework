# R Machine Learning

library(tidyverse)
library(reshape2)

setwd("C:/Users/soltz/OneDrive/Documents/TechAcademy2020/R_Machine_Learning")

# Load Data

housing <- read.csv("housing.csv")

head(housing)

summary(housing)

# Not sure what this does

par(mfrow=c(2, 5))

# Examine Column Names

colnames(housing)

# Examine Variables

ggplot(
  data = melt(housing),
  mapping = aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~variable, scales = 'free_x')

# Clean Data
# Impute Missing Values

housing$total_bedrooms[is.na(housing$total_bedrooms)] = 
  median(housing$total_bedrooms, na.rm = T)

# Fix the Total Columns - Make Them Means

housing$mean_bedrooms <- housing$total_bedrooms/housing$households
housing$mean_rooms <- housing$total_rooms/housing$households

drops <- c('total_bedrooms', 'total_rooms')

housing <- housing[ , !(names(housing) %in% drops)]

head(housing)

# Turn Categoricals into Booleans

categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)

for(cat in categories) {
  cat_housing[, cat] = rep(0, times = nrow(cat_housing))
}

head(cat_housing)

for(i in 1:length(cat_housing$ocean_proximity)) {
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[, cat][i] = 1
}

head(cat_housing)

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing, one_of(keep_columns))

tail(cat_housing)

# Scale Numeric Values

colnames(housing)

drops <- c('ocean_proximity', 'median_house_value')
housing_num <- housing[ , !(names(housing) %in% drops)]

head(housing_num)

scaled_housing_num <- scale(housing_num)

head(scaled_housing_num)

# Merge Altered Numerical and Categorical Dataframes

cleaned_housing <- cbind(cat_housing, scaled_housing_num, 
                         median_house_value = housing$median_house_value)

head(cleaned_housing)

# Create Test Set of Data

set.seed(1738)

sample <- sample.int(
  n = nrow(cleaned_housing), 
  size = floor(.8 * nrow(cleaned_housing)),
  replace = F)
train <- cleaned_housing[sample, ]
test <- cleaned_housing[-sample, ]

head(train)

nrow(train) + nrow(test) == nrow(cleaned_housing)

# Test Some Predictive Models

# Linear Regression

library(boot)

?cv.glm

glm_house <- glm(
  median_house_value ~ median_income +
    mean_rooms +
    population,
  data = cleaned_housing
)
k_fold_cv_error <- cv.glm(cleaned_housing,
                          glm_house, K = 5)
k_fold_cv_error$delta

# Check RMSE

glm_cv_rmse <- sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

# Off by almost $84k

names(glm_house)    # What parts of model are callable?

glm_house$coefficients   # What are the components of the linear fit?

# Random Forest

library(randomForest)

?randomForest

names(train)

set.seed(1738)

train_y = train[ , 'median_house_value']
train_x = train[ , names(train) != 'median_house_value']

head(train_y)
head(train_x)

# R Format
# rf_model <- randomForest(
#   median_house_value ~ .,
#   data = train,
#   ntree = 500,
#   importance = T
# )

rf_model <- randomForest(
  train_x,
  y = train_y,
  ntree = 500,
  importance = T
)

names(rf_model)

rf_model$importance

# Out-of-bag (oob) Error Estimate

oob_prediction <- predict(rf_model)  # Leaving out a data source forces OOB predictions

# This info is available via $mse in model as well
train_mse <- mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse <- sqrt(train_mse)
oob_rmse

# Better, ~ $49K

# Check vs Test Data

test_y <- test[ , 'median_house_value']
test_x <- test[ , names(test) != 'median_house_value']

y_pred <- predict(rf_model, test_x)
test_mse <- mean(((y_pred - test_y)^2))
test_rmse <- sqrt(test_mse)

test_rmse

# Similar to training results, ~ $48K, so not overfit

# Let's see if we can better that by adding more trees

rf_model <- randomForest(
  train_x,
  y = train_y,
  ntree = 1000,
  importance = T
)

y_pred <- predict(rf_model, test_x)
test_mse <- mean(((y_pred - test_y)^2))
test_rmse <- sqrt(test_mse)

test_rmse

# ~ $48K, so not much better, and that took a long time to run

# Let's See if Gradient Boosting Helps

library(gbm)

model_boost = gbm(
  median_house_value ~ .,
  distribution = 'gaussian',
  data = train,
  n.trees = 500,
  shrinkage = 0.2,
  interaction.depth = 4
)

summary(model_boost)

y_pred <- predict(model_boost, test_x)
test_mse <- mean(((y_pred - test_y)^2))
test_rmse <- sqrt(test_mse)

test_rmse

# Still ~ $48K, so not any better, though much quicker

# Lets try more trees (and less shrinkage)

model_boost = gbm(
  median_house_value ~ .,
  distribution = 'gaussian',
  data = train,
  n.trees = 2000,
  shrinkage = 0.1,
  interaction.depth = 4
)

summary(model_boost)

y_pred <- predict(model_boost, test_x)
test_mse <- mean(((y_pred - test_y)^2))
test_rmse <- sqrt(test_mse)

test_rmse

# ~ $46K, so still not much better
# More Trees?

model_boost = gbm(
  median_house_value ~ .,
  distribution = 'gaussian',
  data = train,
  n.trees = 5000,
  shrinkage = 0.05,
  interaction.depth = 4
)

summary(model_boost)

y_pred <- predict(model_boost, test_x)
test_mse <- mean(((y_pred - test_y)^2))
test_rmse <- sqrt(test_mse)

test_rmse

# ~ $45K, so we're not really getting anywhere

# Let's look at the data we're targeting again:

hist(train_y)

# I think the hard ceiling is messing things up. 
# Let's see if things improve without them.

clipped_cleaned_housing <- cleaned_housing[cleaned_housing$median_house_value < 500000, ]

hist(clipped_cleaned_housing$median_house_value, breaks = 30)

nrow(cleaned_housing) - nrow(clipped_cleaned_housing)

# Create Test Set of Data

set.seed(1738)

sample <- sample.int(
  n = nrow(clipped_cleaned_housing), 
  size = floor(.8 * nrow(clipped_cleaned_housing)),
  replace = F)
train <- clipped_cleaned_housing[sample, ]
test <- clipped_cleaned_housing[-sample, ]

head(train)

nrow(train) + nrow(test) == nrow(clipped_cleaned_housing)

train_y = train[ , 'median_house_value']
train_x = train[ , names(train) != 'median_house_value']

test_y <- test[ , 'median_house_value']
test_x <- test[ , names(test) != 'median_house_value']

model_boost = gbm(
  median_house_value ~ .,
  distribution = 'gaussian',
  data = train,
  n.trees = 2000,
  shrinkage = 0.1,
  interaction.depth = 4
)

summary(model_boost)

y_pred <- predict(model_boost, test_x)
test_mse <- mean(((y_pred - test_y)^2))
test_rmse <- sqrt(test_mse)

test_rmse

# ~ $41K, so that was a bit of an improvement

