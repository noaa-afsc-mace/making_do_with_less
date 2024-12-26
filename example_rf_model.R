# This is a small example of training a random forest model for use in multifrequency acoustic identification.
# it uses a small subset of the data included in the Fisheries Research manuscript
# "Making do with less: extending an acoustic-based time series of euphausiid abundance
# using an uncrewed surface vehicle with fewer frequencies." By Mike Levine and Alex De Robertis

# the goals of this script are to:
# 1. Validate that a series of predictor variables are appropriate to include in a random forest model
# 2. Construct two random forest models
# 3. Do a comparison of models and select one to move forward with
# 4. Examine the performance of this model more closely and use it to make predictions.
# 5: test the model against withheld data to see how well it does 

# NOTE: this will not produce results that are identical with those in the manuscript, as it includes only
# a small subset of the data for speed and storage reasons.  This data set contains 10% of the data
# in each year, randomly selected within each year.  It is instead meant to act as a demonstration of
# the method to benefit other researchers.


######################################################################
# preliminary setup: load some libraries and functions we'll need

# clear out old data
rm(list = ls())

# load some libraries:

# this is the list of packages we need
packages_list <- c("lubridate", "dplyr", "ggplot2", "caret", "vip", "car", "magrittr", "ranger")

# this function will either load the package (if you have it) or install them (if you don't)
load_or_install_packages <- function(package_name){
  
  ifelse(package_name %in% installed.packages(),
         library(package_name, character.only = TRUE),
         install.packages(package_name))
  
}

# apply the function 
purrr::walk(packages_list, load_or_install_packages)

# load the training data
cells_38_200_training <- readRDS("data/example_training_cells_for_rf.rds")

# load the the withheld test data (i.e. none of these 20,000 observations were used in model training)
test_data <- readRDS("data/example_testing_cells_for_rf.rds")

# this function is a working version of caret::groupKFold

# this is needed to explicitly define our cross-validation folds by year.

# the current (caret v 6.0-93) version does not return the expected number of groups:
# Mike-Levine posted a github issue about this issue.
# https://github.com/topepo/caret/issues/1150

# this is an earlier implementation:
# https://github.com/topepo/caret/issues/540

# function created by Max Kuhn
group_cv <- function(x, k = length(unique(x))) {
  dat <- data.frame(index = seq(along = x), group = x)
  groups <- data.frame(group = unique(dat$group))
  group_folds <- createFolds(groups$group, returnTrain = TRUE, k = k)
  group_folds <- lapply(group_folds, function(x, y) y[x, , drop = FALSE], y = groups)
  dat_folds <- lapply(group_folds, function(x, y) merge(x, y), y = dat)
  lapply(dat_folds, function(x) sort(x$index))
}

#######################################################################

# step 1. Validate that a series of predictor variables are appropriate to include in a random forest model

# use: variance inflation factor. In general, interpretation varies in the literature
# we rely on Chatterje and Hadi 2006, Naimi et al. 2014 (see manuscript works cited)
# VIF > 10 'severe' and should not be used

# fit a regression model with all terms
# (except: Sv_38_999, which is entirely predicted from Sv_200_999 and dSv_200_38)
mod <- glm(
  is_krill ~ year + Sv_200_999 + dSv_200_38 + Lat + Lon + datetime_hhmm + Exclude_below_line_depth_mean +
    layer_depth + off_bot_height + prop_depth,
  data = cells_38_200_training, family = "binomial"
)

# check variance inflation factor
car::vif(mod)

# all the depth variables are strongly correlated; pick Exclude_below_line_depth_mean (i.e. depth of habitat) and off_bot_height
# i.e. depth of observations
mod <- glm(
  is_krill ~ year + Sv_200_999 + dSv_200_38 + Lat + Lon + datetime_hhmm + Exclude_below_line_depth_mean +
    +off_bot_height,
  data = cells_38_200_training, family = "binomial"
)

# check variance inflation factor
car::vif(mod)

# now off_bot_height is not > 10, include this to represent the depth variables.

# So the final list of variables for testing includes:
# 200 kHz Sv (Sv_200_999),
# Delta Sv 200 kHz - 38 kHz (dSv_200_38)
# Latitude (Lat)
# Longitude (Lon)
# Time of day (datetime_hhmm)
# bottom depth (Exclude_below_line_depth_mean)
# cell height off bottom (off_bot_height)

#####################################################################

# 2. Step 2: Construct two random forest models

# note the for example purposes we are only testing 2 models here; more were tested in development but
# these are left our of the demonstration

# model 1: everything that doesn't greatly violate colinearity (values <10) + year (for use in cross-validation)

# identify the predictors to use
preds_model_1 <- c("year", "Sv_200_999", "dSv_200_38", "Lat", "Lon", "datetime_hhmm", "Exclude_below_line_depth_mean", "off_bot_height", "is_krill")

# limit training cells to these predictors
training_cells_model_1 <- cells_38_200_training %>%
  select(all_of(preds_model_1))

# model 2: no location data
preds_model_2 <- c("year", "Sv_200_999", "dSv_200_38", "Exclude_below_line_depth_mean", "off_bot_height", "is_krill")

training_cells_model_2 <- cells_38_200_training %>%
  select(all_of(preds_model_2))


# now set up the random forest testing parameters-

# we want to use 'year' as factor, so cross-validation can be via a 'leave-one-year-out' approach

# identify how many total years to test- we'll use this to determine number of folds
# here- every year will be a fold
n_test_years <- length(unique(cells_38_200_training$year))

# use this function to build a rf model for each dataset
build_rf_model <- function(data_set_name) {
  # this takes a while: let the user know what model is getting built
  print(paste("Building random forest for:", deparse(substitute(data_set_name))))

  # define the folds for validation as each year in the dataset
  # see: https://github.com/topepo/caret/issues/540
  folds <- group_cv(data_set_name$year, k = n_test_years)

  # fit using the 10-fold cross-validation approach
  fitControl <- trainControl(
    # K-fold cross validation
    method = "cv",
    # n folds = number of years in dataset
    number = n_test_years,
    # use the index argument to specify the folds as years
    index = folds
  )

  #########
  # for reproducibility
  set.seed(1325)

  # build the model using ranger package implementation of random forests;
  # variable importance from 'permutation'
  mod <- train(is_krill ~ . - year,
    data = data_set_name,
    method = "ranger",
    trControl = fitControl,
    importance = "permutation",
    num.trees = 500
  )

  return(mod)
}

# apply function to each dataset (be patient! Even with a reduced data set it is slow.)
# note the Ranger package defaults to trying a series of 'mtry' and 'splitrule' parameters based on the
# dataset provided.
# we use the defaults and select amongst them.

rf_model_1 <- build_rf_model(data_set_name = training_cells_model_1)
rf_model_2 <- build_rf_model(data_set_name = training_cells_model_2)

###################################################################
# 3. Do a comparison of models and select one to move forward with

# this function will grab some output details simply for demonstration-
# the model objects have much more information in them,
# but this gets some basics to compare
get_mod_results <- function(mod_name) {
  mod_results <- mod_name$results
  mod_results$pred_error <- mod_name$finalModel$prediction.error
  mod_results$mod_name <- deparse(substitute(mod_name))

  return(mod_results)
}

results_mod_1 <- get_mod_results(mod_name = rf_model_1)
results_mod_2 <- get_mod_results(mod_name = rf_model_2)

# compile for plotting/comparison
results_comp <- bind_rows(results_mod_1, results_mod_2)

# plot the output by Accuracy for a preliminary view
ggplot(results_comp, aes(x = mtry, y = Accuracy, group = splitrule, color = splitrule)) +
  geom_point() +
  geom_path() +
  # look at the big picture- from 0 to 1
  scale_y_continuous(limits=c(0,1))+
  facet_wrap(~mod_name, nrow = 1)

# note that all tested models have high (and similar) accuracy in this preliminary look (> 0.93)
# big-picture these are very similar and any would suffice for our application

##################################
# 4. Examine the performance of the chosen model more closely and use it to make predictions. 

# Though the second model performs marginally lower than model 1 in terms
# of accuracy, it is more general as it does not require location information (see manuscript for 
# discussion on this point).

# model summaries - what model was the highest performing among those constructed?
rf_model_2$finalModel$tuneValue
# model 2 was selected

# how much variation was there? I.e. do the choices (mtry, splitrule) have large variations?
rf_model_2$results
# No. For our purpose, this suggests that it is fine to move on with the selected values of mtry, splitrule.

# get the confusion matrix, and compute precision, recall, and F1
# visually. the confusion matrix summarizes true positives , false positives, 
# true negatives, and false negatives:
rf_model_2$finalModel$confusion.matrix

# calculate precision, recall, F1
# Precision = TruePositives / (TruePositives + FalsePositives)
precision <- caret::confusionMatrix(rf_model_2)[[1]][[4]] /
  (caret::confusionMatrix(rf_model_2)[[1]][[4]] + caret::confusionMatrix(rf_model_2)[[1]][[2]])

# Recall = TruePositives / (TruePositives + FalseNegatives)
recall <- caret::confusionMatrix(rf_model_2)[[1]][[4]] /
  (caret::confusionMatrix(rf_model_2)[[1]][[4]] + caret::confusionMatrix(rf_model_2)[[1]][[3]])

# F1 score (balancing precision and recall)
f1 <- 2 * ((precision * recall) / (precision + recall))

# express in percentage terms:
precision * 100
recall * 100
f1 * 100

# happy with this performance? consider this (model 2) your final RF model!

#################################################

# Step 5: test the model against withheld data to see how well it does 

# (this is a secondary confirmation of model performance)

# make predictions using the selected model, using the 'predict' function
test_data$rf_krill <- predict(rf_model_2, newdata = test_data)

# get the confusion matrix- this will compare the krill identifications that were made using the 
# traditional 4-frequency ID method (the 'reference method' in our manuscript) which are in the 'is_krill' column
# with those made using the random forest model, which are in the 'rf_krill' column
cmat <- caret::confusionMatrix(as.factor(test_data$is_krill), test_data$rf_krill, positive = "1")

# again, the confusion matrix visually:
cmat$table

# Precision = TruePositives / (TruePositives + FalsePositives)
test_precision <- cmat$table[4] / (cmat$table[4] + cmat$table[3])

# Recall = TruePositives / (TruePositives + FalseNegatives)
test_recall <- cmat$table[4] / (cmat$table[4] + cmat$table[2])

# F1 score (balancing precision and recall)
test_f1 <- 2 * ((test_precision * test_recall)/(test_precision + test_recall))

# express as percentage
test_precision * 100
test_recall * 100
test_f1 * 100

# note that these are very close to the cross-validation results (within 1% for each metric). 

# in a real use case, you would then use 'predict' function on your data to get random forest identifications 
# (see L262 above).

