# Interpreting the Results of `summary`

```{r}
summary(linear_model)

# lm(formula = lwage ~ . - wage, data = wage)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -1.96887 -0.19460  0.00923  0.22401  1.34185
# 
# Coefficients:
#              Estimate Std. Error t value             Pr(>|t|)
# (Intercept)  5.156439   0.225286  22.888 < 0.0000000000000002 ***
# hours       -0.006548   0.001934  -3.385             0.000754 ***
# iq           0.003186   0.001223   2.604             0.009425 **
# kww          0.003735   0.002390   1.562             0.118662
# educ         0.041267   0.008942   4.615       0.000004741579 ***
# exper        0.010749   0.004435   2.424             0.015629 *
# tenure       0.007102   0.002894   2.454             0.014401 *
# age          0.009107   0.005977   1.524             0.128058
# married1     0.200760   0.045998   4.365       0.000014826606 ***
# black1      -0.105141   0.055667  -1.889             0.059373 .
# south1      -0.049076   0.030753  -1.596             0.111019
# urban1       0.195658   0.031240   6.263       0.000000000688 ***
# sibs         0.009619   0.007876   1.221             0.222423
# brthord     -0.018465   0.011569  -1.596             0.110975
# meduc        0.009633   0.006167   1.562             0.118753
# feduc        0.005590   0.005398   1.036             0.300805
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3507 on 647 degrees of freedom
#   (272 Beobachtungen als fehlend gelöscht)
# Multiple R-squared:  0.2925,    Adjusted R-squared:  0.2761
# F-statistic: 17.84 on 15 and 647 DF,  p-value: < 0.00000000000000022
```

| Begriff | Bedeutung |
| -- | -- |
| Residuals | Zeigt Statistik über die Differenz zwischen Vorhergesagten und tatsächlichen Werten. Sollte idealerweise nahe null sein. |
| Intercept | Gibt die "Baseline" für Lineare-Modelle an. (Da wo die Gerade die y-Achse schneidet). Nicht interpretierbar, wenn es keinen Sinn macht alle unabhängigen Variablen auf 0 zu setzen
| Estimate | Zeigt an wie sehr der Wert die Zielvariable beeinflusst. Umso höher umso mehr signifikant |
| Std. Error | Standard Abweichung von Estimate. Gibt die "Unsicherheit" an |
| t value | Gibt die Signifikanz des Werts an. t = Estimate / Std. Error. Hohe (absolute) Zahl = Signifkant. Nahe 0 = unsignifikant | 
| Pr(>\|t\|) | siehe t |
| Multiple R-squared | Misst Anteil der Varianz die durch unabhängige Variablen erklärt werden kann. Der Wert sollte Nahe 1 sein. |
| Adjusted R-squared | Angepasster R-squared. ZU unabhängige Werte werden nicht in Betracht gezogen. Macht erkennbar, ob nicht signifikante Prädikatoren enthält |
| F-statistic | | 

# Computing model performance
RMSE (Root Mean Squared Error)

```{r}

```

# Accuracy / Precision / Recall

```{r}
# Genauigkeit berechnen
accuracy_value <- updated_predictions %>%
  yardstick::accuracy(truth = Survived, estimate = .pred_class) %>%
  pull()

# Präzision berechnen
precision_value <- updated_predictions %>%
  yardstick::precision(truth = Survived, estimate = .pred_class) %>%
  pull()

# Rückruf (Recall) berechnen
recall_value <- updated_predictions %>%
  yardstick::recall(truth = Survived, estimate = .pred_class) %>%
  pull()
```

Accuracy (Genauigkeit): Der Prozentsatz der korrekten Vorhersagen. Hier liegt die Genauigkeit bei etwa 80,4%, was bedeutet, dass etwa 80,4% der Vorhersagen korrekt sind.

Precision (Präzision): Der Prozentsatz der wahren positiven Vorhersagen im Verhältnis zu allen positiven Vorhersagen. Hier liegt die Präzision bei etwa 83%, was darauf hinweist, dass von den als positiv vorhergesagten Fällen etwa 83% tatsächlich positiv sind.

Recall (Rückruf): Der Prozentsatz der wahren positiven Vorhersagen im Verhältnis zu allen tatsächlich positiven Fällen. Hier liegt der Recall bei etwa 87,4%, was bedeutet, dass das Modell etwa 87,4% der tatsächlich positiven Fälle erfasst.


