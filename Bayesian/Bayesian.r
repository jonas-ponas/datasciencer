source("./Bayesian/Libraries.r")

source("./Bayesian/Prepare.r")

source("./Bayesian/Explore.r")

linear_model <- lm(lwage ~ . - wage - feduc - meduc - sibs - age - kww - black - south - brthord, data = na.omit(data))
BIC(linear_model)

# Step AIC probiert andere Formeln aus und gibt ein optimiertes model zurück
opt_linear_model <- stepAIC(
    lm(lwage ~ . - wage, data = na.omit(wage)), # Linear Model to use
    # trace = FALSE, 
    k = log(length(na.omit(wage)))
)
BIC(opt_linear_model)


bma_lwage <- bas.lm(lwage ~ . - wage,
    data = na.omit(wage),
    prior = "BIC",
    modelprior = uniform()
)
summary(bma_lwage)

image(bma_lwage, top.models = 5, rotate = F, cex.axis = 1)

