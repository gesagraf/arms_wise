#Aufgabe 1

#Hier erstelle ich die Variable X
Mittelwert <- 0
Standardabweichung <- 4
Stichprobengrosse <- 10

df <- data.frame(ID=c(1:Stichprobengrosse))
df$X <- c(rnorm(n = Stichprobengrosse, mean = Mittelwert, sd = Standardabweichung))

#Hier erstelle ich den Fehler
MittelwertFehler <- 0
StandardabweichungFehler <- 1

df$Fehler <- c(rnorm(n = Stichprobengrosse, mean = MittelwertFehler, sd = StandardabweichungFehler))

#Y
BetaNull <- 50
BetaEins <- 0

df$Y <- BetaNull + BetaEins * df$X + df$Fehler

#Aufgabe 2
#korrelation von X und Y
model1 <- cor.test(df$X, df$Y)
corpvalue <- c(model1$p.value)
cor <- c(model1$estimate)


model <- lm(Y ~ X, data=df)
summary(model)
coef <- c(model$coefficients)

#Aufgabe 4
corpvalue <- c(corpvalue, model1$p.value)
cor <- c(cor, model1$estimate)
coef <- c(coef, model$coefficients)

#Aufgabe 5
XundYerzeugen <- function(Stichprobengrosse, Mittelwert, Standardabweichung, MittelwertFehler, StandardabweichungFehler, BetaNull, BetaEins) {
  X <- rnorm(n = Stichprobengrosse, mean = Mittelwert, sd = Standardabweichung)
  Y <- BetaNull + BetaEins * X + rnorm(n = Stichprobengrosse, mean = MittelwertFehler, sd = StandardabweichungFehler)
  data.frame(x=X, y=Y)
}

df <- XundYerzeugen(10, 5, 2, 3, 1, 50, 0)

regressions_daten <- function(daten) {
  # Lineare Regression durchf?hren
  lm_model <- lm(y ~ x, data = daten)
  # Extrahieren der Regressionskoeffizienten
  coefficients <- coef(lm_model)
  # Extrahieren der Standardfehler der Koeffizienten
  se <- summary(lm_model)$coef[, "Std. Error"]
  #p-werte
  p_values <- summary(lm_model)$coef[, "Pr(>|t|)"]
  # Berechnen der Korrelation
  correlation <- cor(daten$x, daten$y)
  # Die extrahierten Informationen in einem Vektor zur?ckgeben
  result <- c(coef = coefficients, se = se, correlation = correlation, p_values=p_values)
  return(result)
}

regressions_daten(df)
nsample <- 1000
#Aufagbe 6
column_names <- c("coef.(Intercept)","coef.x","se.(Intercept)","se.x","correlation ","p_values.(Intercept)","p_values.x" )
#results <- list()
results <- as.data.frame(matrix(ncol = length(column_names), nrow = nsample))
colnames(results) <- column_names

for (i in 1:nsample) {
  # Stichprobe generieren
  df <- XundYerzeugen(10, 5, 2, 3, 1, 50, 0)

  results[i,] <- regressions_daten(df)
}

results <- sapply(1:1000, FUN = function(i){
  df <- XundYerzeugen(10, 5, 2, 3, 1, 50, 0)

  regressions_daten(df)
}, simplify = "array")

#mittelwerte und standardabweichungen berechnen
mean(results$`coef.(Intercept)`, na.rm=TRUE)
sd(results$`coef.(Intercept)`, na.rm=TRUE)

mean(results$coef.x, na.rm=TRUE)
sd(results$coef.x, na.rm=TRUE)

boxplot(results$coef.x)
boxplot(results$`coef.(Intercept)`)


