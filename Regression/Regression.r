source("./Regression/Libraries.r")
source("./Regression/Prepare.r")

View(imp_car_data)

set.seed(123)
split_data <- initial_split(imp_car_data, prop = 0.7)
train_data <- training(split_data)
test_data <- testing(split_data)


the_recipe <- recipe(acceleration ~ ., data = train_data) %>%
    step_rm(car_name) %>%
    # step_string2factor(...) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), -all_outcomes())

prep_recipe <- prep(the_recipe, training = train_data)
prep_recipe_test <- prep(the_recipe, training = test_data)

linear_model <- lm(prep_recipe, data = train_data)

linear_predictions <- predict(linear_model, newdata = test_data)

linear_rmse <- sqrt(mean((linear_predictions - test_data$acceleration)^2))

cat("Root Mean Squared Error (Linear Regression):", linear_rmse, "\n")



## Cross Validation

control <- trainControl(
    method = "cv",
    number = 10
)

tuneGrid <- expand.grid(C = c(0.1, 1, 10, 100), sigma = c(0.01, 0.1, 1, 10))

fit <- train(
    acceleration ~ .,
    data = juice(prep_recipe),
    method = "svmRadial",
    trControl = control,
    tuneGrid = tuneGrid
)

cv_predictions <- predict(fit, newdata = juice(prep_recipe_test))

cv_rmse <- sqrt(mean((cv_predictions - test_data$acceleration)^2))

cat("Root Mean Squared Error (Cross Validation):", cv_rmse, "\n")