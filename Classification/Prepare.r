titanic_data <- read.csv(
    "./Classification/data/titanic.csv",
    header = TRUE,
    sep = ";",
    na.strings = c("")
)

# Clean and transform data
titanic_data <- titanic_data %>%
    select(-c(Cabin, Ticket, Name)) %>% # Unnötig für Analyse
    mutate(Age = as.numeric(Age)) %>%
    mutate(Age = case_when( # Cleanup for stupid data
        Age >= 100 ~ Age / 1000,
        TRUE ~ Age
    )) %>%
    mutate_at(vars(everything()), ~ ifelse(. == "NA", NA, .)) %>%
    mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor))

# Imputate Data

imp_titanic <- mice(
        titanic_data,
        m = 5,
        maxit = 10,
        mathod = "rf"
    ) %>%
    complete() %>%
    as.data.frame()




# skim(titanic_data)
