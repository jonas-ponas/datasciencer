source("./Classification/Libraries.r")

source("./Classification/Prepare.r")

set.seed(123)
split <- initial_split(imp_titanic, prop = 0.8, strata = "Survived")
train_data <- training(split)
test_data <- testing(split)

logistic_recipe <- recipe(Survived ~ ., data = train_data) %>%
    update_role(PassengerId, new_role = "ID") %>%
    step_dummy(all_nominal_predictors(), -all_outcomes()) %>%
    step_normalize(all_numeric_predictors())

logistic_model <- logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification")


logistic_wf <- workflow() %>%
    add_recipe(logistic_recipe) %>%
    add_model(logistic_model)

logistic_fit <- fit(logistic_wf, data = train_data)



# Prognose erstellen
predictions <- logistic_fit %>%
    predict(new_data = test_data) %>%
    as.tibble() %>%
    bind_cols(test_data) %>%
    select(c(PassengerId, .pred_class, Survived, everything()))

accuracy_value <- predictions %>%
    yardstick::accuracy(truth = Survived, estimate = .pred_class) %>%
    pull()

# Präzision berechnen
precision_value <- predictions %>%
    yardstick::precision(truth = Survived, estimate = .pred_class) %>%
    pull()

# Rückruf (Recall) berechnen
recall_value <- predictions %>%
    yardstick::recall(truth = Survived, estimate = .pred_class) %>%
    pull()

# Ausdrucken der Metriken
cat("Accuracy:", accuracy_value, "\n")
cat("Precision:", precision_value, "\n")
cat("Recall:", recall_value, "\n")

# Confusion Matrix
conf_matrix <- conf_mat(predictions, truth = Survived, estimate = .pred_class)

autoplot(conf_matrix, type = "heatmap", scale = "count") +
    ggtitle("Confusion Matrix") +
    labs(x = "Predicted", y = "Actual")



coefs <- tidy(logistic_fit)
# Auswahl der Top N Variablen
top_n_vars <- coefs %>%
    filter(!term == "(Intercept)") %>%
    arrange(desc(abs(estimate))) %>%
    slice_head(n = 6) # Hier 4 steht für die Anzahl der wichtigsten Variablen

# Visualisierung der Variable Importance mit ggplot2
ggplot(top_n_vars, aes(x = reorder(term, estimate), y = estimate, fill = factor(sign(estimate)))) +
    geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
    labs(
        title = "Top Variable Importance",
        x = "Variable",
        y = "Estimate",
        fill = "Direction"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


