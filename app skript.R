#### setup ####
library(ggplot2)

#### setting variables ####

mu <- 150         # Population mean
sd <- 15          # Population sd
n <- 100          # sample size
number <- 300      # number of samples
mu_prior <- 140
tau_prior <- 10

# Anzahl der Klassen nach Sturges' Regel (für Histogram)
num_classes <- ceiling(log2(n) + 1)

#### generating data ####

# draws samples of "number" iterations
samp_df <- sapply(1:number, FUN = function(i) {
  samp <- rnorm(n = n, mean = mu, sd = sd)
}
)


#### mean ####
#colum wise
estimators <- apply(samp_df, MARGIN = 2, mean)


# over all mean
mean_est <- mean(estimators)
#### min/max ####
# mean colum wise
minmax <- sapply(1:ncol(samp_df), function(i) {
  (max(samp_df[,i]) + min(samp_df[,i])) / 2
})

# over all mean
mean_minmax <- mean(minmax)

#### Bayes #####
# calculate likelihood
mu_hat <- seq(mu - 2 * sd, mu + 2 * sd, length.out = 200)

# Prior
prior_dens <- dnorm(mu_hat, mean = mu_prior, sd = tau_prior)

results <- as.data.frame(matrix(ncol = number, nrow = 200))#200 weil wir für 200 punkte die likelihood berechnen

for (i in 1:number) {
likelihood_function <- sapply(mu_hat, FUN = function(i_mu){
  prod(dnorm(samp_df[,i], mean = i_mu, sd = sd(samp_df[,i])))#bei i 1 einsetzen für testi
  })

  #Normierung der Likelihood #ich glaube wir müssen das nicht normieren aber macht es trotzdem Sinn? nicht so kleine zahlen
  den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
  likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood

  #calculate posteriori
  posterior0 <- prior_dens * likelihood_function_norm

  #Posteriori normieren#hier nochmal
  den_post <- Bolstad::sintegral(mu_hat, posterior0)
  posterior <- posterior0 / den_post$value

results[[i]] <- posterior
}

# Index des Maximums in jeder Spalte finden
index_maximum <- as.numeric(sapply(1:length(results), function(i) {
  which.max(results[[i]])
}))

# Wert von 'x' für das Maximum von 'y' finden
bayesWerte <- sapply(1:length(results), function(i) {
  mu_hat[index_maximum[i]]
})

# over all mean Bayes
mean_estBayes <- mean(bayesWerte)


#### plotting Mean####
# single Sample
# setting specific sample
specific <- 5

### sturges rule anzahl der bins
ggplot(NULL, aes(x = samp_df[ , specific])) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classes) +
  geom_point(aes(x = mean(samp_df[ , specific]), y = 0), shape = 17, size = 4, colour = "darkgrey") +
  geom_vline(xintercept = mean(samp_df[ , specific]), colour = "black", linetype = "dotted") +

  # Skalen, Theme, Labs etc.
  #   xlim(mu - 2 * sd, mu + 2 * sd) +        # x Achse festlegen
  labs(
    title = "Einzelne Stichprobe mit Mittelwert",
    y = "Häufigkeit",
    x = "x",
    labs = "blibla"
  ) +
  theme_bw()


# SKV
ggplot(NULL, aes(x = estimators)) +
  geom_histogram(fill = "lightgrey", color = "black") +

  # every sample as triangle
  geom_point(aes(x = estimators, y = 0, color = "estimators"), shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2, colour = "mu"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu, colour = "mu")) +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 4,  colour = "mean_est"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est, colour = "mean_est"), linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  #  xlim(mu - sd, mu + sd) +  # xAchse festlegen
  # coord_cartesian()
  labs(
    title = "Stichprobenkennwerteverteilung \n des arithmetischen Mittels",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_est = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_est = "Mean aller Stichprobenmeans",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()
#### plotting MinMax####
# single Sample
# setting specific sample
specific <- 5

ggplot(NULL, aes(x = samp_df[ , specific])) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classes) +
  geom_point(aes(x = mean(samp_df[ , specific]), y = 0), shape = 17, size = 4, colour = "darkgrey") +
  geom_vline(xintercept = mean(samp_df[ , specific]), colour = "black", linetype = "dotted") +

  # Skalen, Theme, Labs etc.
  #   xlim(mu - 2 * sd, mu + 2 * sd) +        # x Achse festlegen
  labs(
    title = "Einzelne Stichprobe mit Mittelwert",
    y = "Häufigkeit",
    x = "x",
    labs = "blibla"
  ) +
  theme_bw()


# SKV
ggplot(NULL, aes(x = estimators)) +
  geom_histogram(fill = "lightgrey", color = "black") +

  # every sample as triangle
  geom_point(aes(x = estimators, y = 0, color = "estimators"), shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2, colour = "mu"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu, colour = "mu")) +

  # mean over all samples
  geom_point(aes(x = mean_minmax, y = 4,  colour = "mean_minmax"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_minmax, colour = "mean_minmax"), linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  #  xlim(mu - sd, mu + sd) +  # xAchse festlegen
  labs(
    title = "Stichprobenkennwerteverteilung \n des alternativen schätzers",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_minmax = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_minmax = "Mean über alle Minima/Maxima",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()

#### plotting Bayes####

# single Sample
# setting specific sample
specific <- 5

ggplot(NULL, aes(x = samp_df[ , specific])) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classes) +
  geom_point(aes(x = mean(samp_df[ , specific]), y = 0), shape = 17, size = 4, colour = "darkgrey") +
  geom_vline(xintercept = mean(samp_df[ , specific]), colour = "black", linetype = "dotted") +

  # Skalen, Theme, Labs etc.
  #   xlim(mu - 2 * sd, mu + 2 * sd) +        # x Achse festlegen
  labs(
    title = "Einzelne Stichprobe mit Mittelwert",
    y = "Häufigkeit",
    x = "x",
    labs = "blibla"
  ) +
  theme_bw()


# SKV
ggplot(NULL, aes(x = estimators)) +
  geom_histogram(fill = "lightgrey", color = "black") +

  # every sample as triangle
  geom_point(aes(x = estimators, y = 0, color = "estimators"), shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2, colour = "mu"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu, colour = "mu")) +

  # mean over all samples
  geom_point(aes(x = mean_estBayes, y = 4,  colour = "mean_estBayes"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_estBayes, colour = "mean_estBayes"), linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  #  xlim(mu - sd, mu + sd) +  # xAchse festlegen
  # coord_cartesian()
  labs(
    title = "Stichprobenkennwerteverteilung \n des arithmetischen Mittels",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_estBayes = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_estBayes = "Mean aller Bayesschätzer",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()
