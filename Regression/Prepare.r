car_data <- read_csv("./Regression/data/Sample_car_dataset.csv")

car_data <- car_data %>%
    # mutate(across(c(...), as.factor)) %>%
    # mutate(SportLuxury = case_when(
    #     data$Ps > 300 ~ "yes",
    #     (data$Price > 300.000 & data$Benz == "yes") ~ "yes",
    #     TRUE ~ "no"
    # )) %>%
    unique() # Entferne Dopplungen
    # na.omit() %>% # Entferne NA Zeilen
    # filter(!is.na(horsepower)) # Filtere für Zeilen, die Horsepower enthalten



# Imputation
imp_car_data <- mice(
        car_data,
        m = 5,
        maxit = 10,
        method = "pmm" # rf, logreg
    ) %>% 
    complete() %>%
    as.data.frame()
