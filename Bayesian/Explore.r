# Histogram shows
hist(wage$wage, main = "Wöchentliche Löhne", xlab = "Wöchentlicher Lohn in Dollar", breaks = 30)

# Anzeigen von Zusammenhang IQ ~ wage
ggplot(
    wage,
    aes(iq, wage) + # aes (X-Variable, Y-Variable)
    geom_point() +
    geom_smooth() +
    geom_point(col = "steelblue", alpha = 1 / 2)
)


# Wage ~ IQ; Wage = Abhängige Variable, IQ = Predictor
linear_model <- lm(wage ~ iq, wage)

# Residuen = Abweichung von Vorhersage zu tatsächlicem Wert

# Scatter-Plot von Residuen
ggplot(linear_model, aes(x = .fitted, y = .resid)) +
    geom_jitter() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    xlab("Angepasste Werte") +
    ylab("Residuen")

# Verteilung von Residuen
ggplot(linear_model, aes(x = .resid)) +
    geom_histogram(binwidth = 100) +
    xlab("Residuen")
