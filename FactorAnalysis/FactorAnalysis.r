library(readr)
library(DataExplorer)
library(magrittr)
library(skimr)
library(psych)
library(tibble)
library(dplyr)


data <- read_delim("./FactorAnalysis/data/EFA.csv", delim = ";", trim_ws = TRUE)

skim(data)

parallel <- fa.parallel(data, fm = "varimax", fa = "fa") # Oder fm="varimax"
# Den Wert auswählen wo blaue line noch über rot ist aber relativ nah dranne ist.

# Für diese Daten 3, 4, 5 ?

factors <- fa(data,
    nfactors = 3,
    rotate = "oblimin", fm = "minres"
)

# Bessere Darstellung:
print(factors$loadings, cutoff = 0.3)

# Schönes Diagramm :)
fa.diagram(factors)

data <- data %>%
    mutate(ID = 1:90) %>% # Create ID
    select(c(ID, everything()))

finalData <- cbind(data[, 1], factor_load$scores) %>%
    rename("economic" = "MR1") %>%
    rename("functional" = "MR2") %>%
    rename("aesthetics" = "MR4") %>%
    rename("credibility" = "MR3")
skim(finalData)
