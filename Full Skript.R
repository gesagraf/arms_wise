#### setup ####
library(ggplot2)

#### setting variables ####

mu <- 150         # Population mean
sd <- 15          # Population sd
n <- 100          # sample size
number <- 300     # number of samples
mu_prior <- 130   # mittelwert der priori
tau_prior <- 10   # sd der priori
flache_priori <- TRUE # flache oder normalveteile priori
min_uni_priori <- 135 # minimum für gleichverteilte priori
max_uni_priori <- 145 # max für gleichverteilte priori

# Anzahl der Klassen nach Sturges' Regel (für Histogram der einzelnen Stichprobe)
num_classes <- ceiling(log2(n) + 1)
# Anzahl der Klassen nach Sturges' Regel (für Histogram der SKV)
num_classesSKV <- ceiling(log2(number) + 1)

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
max_min_mean <- function(vec) {
  (max(vec) + min(vec)) / 2
}

minmax <- apply(samp_df, MARGIN = 2, max_min_mean)

# over all mean
mean_minmax <- mean(minmax)

#### Bayes mit gleichverteilter Priori mit min max #####
# calculate likelihood
mu_hat <- seq(mu - 2 * sd, mu + 2 * sd, length.out = 200)

# Prior
calculate_prior <- function(flache_priori, mu_hat, mu_prior, tau_prior) {
  if (flache_priori == TRUE) {
    return(dunif(mu_hat, min = min_uni_priori, max = max_uni_priori)) # alternative, noch unklar

  } else {
    return(dnorm(mu_hat, mean = mu_prior, sd = tau_prior))
  }
}

prior_dens <- calculate_prior(flache_priori, mu_hat, mu_prior, tau_prior)

results <- matrix(ncol = number, nrow = 200) # 200 weil wir für 200 punkte die likelihood berechnen

for (i in 1:number) {

  likelihood_function <- sapply(mu_hat, FUN = function(i_mu){   # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
    prod(dnorm(samp_df[,i], mean = i_mu, sd = sd(samp_df[,i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
  })                                                            # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()

  # Normierung der Likelihood #ich glaube wir müssen das nicht normieren aber macht es trotzdem Sinn? nicht so kleine zahlen
  den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
  likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood

  # calculate posteriori
  posterior0 <- prior_dens * likelihood_function_norm

  # Posteriori normieren # hier nochmal
  den_post <- Bolstad::sintegral(mu_hat, posterior0)
  posterior <- posterior0 / den_post$value

  results[ , i] <- posterior

}



# Index des Maximums in jeder Spalte finden
index_maximum <- apply(results, MARGIN = 2, which.max)

# Wert von 'x' für das Maximum von 'y' finden
bayesWerte <- mu_hat[index_maximum]

# over all mean Bayes
mean_estBayes <- mean(bayesWerte)


#### Bayes mit nv Priori #####
# calculate likelihood
flache_priori <- FALSE
# Prior

prior_densNV <- calculate_prior(flache_priori, mu_hat, mu_prior, tau_prior)

resultsNV <- matrix(ncol = number, nrow = 200) # 200 weil wir für 200 punkte die likelihood berechnen

for (i in 1:number) {

  likelihood_function <- sapply(mu_hat, FUN = function(i_mu){   # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
    prod(dnorm(samp_df[,i], mean = i_mu, sd = sd(samp_df[,i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
  })                                                            # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()

  # Normierung der Likelihood #ich glaube wir müssen das nicht normieren aber macht es trotzdem Sinn? nicht so kleine zahlen
  den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
  likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood

  # calculate posteriori
  posterior0 <- prior_dens * likelihood_function_norm

  # Posteriori normieren # hier nochmal
  den_post <- Bolstad::sintegral(mu_hat, posterior0)
  posterior <- posterior0 / den_post$value

  resultsNV[ , i] <- posterior

}



# Index des Maximums in jeder Spalte finden
index_maximumNV <- apply(resultsNV, MARGIN = 2, which.max)

# Wert von 'x' für das Maximum von 'y' finden
bayesWerteNV <- mu_hat[index_maximumNV]

# over all mean Bayes
mean_estBayesNV <- mean(bayesWerteNV)
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
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classesSKV) +

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
  geom_point(aes(x = minmax[specific], y = 0), shape = 17, size = 4, colour = "darkgrey") +
  geom_vline(xintercept = minmax[specific], colour = "black", linetype = "dotted") +

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
ggplot(NULL, aes(x = minmax)) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classesSKV) +

  # every sample as triangle
  geom_point(aes(x = minmax, y = 0, color = "estimators"), shape = 17, size = 4) +

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
  geom_point(aes(x = bayesWerte[specific], y = 0), shape = 17, size = 4, colour = "darkgrey") +
  geom_vline(xintercept = bayesWerte[specific], colour = "black", linetype = "dotted") +

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
ggplot(NULL, aes(x = bayesWerte)) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classesSKV) +

  # every sample as triangle
  geom_point(aes(x = bayesWerte, y = 0, color = "estimators"), shape = 17, size = 4) +

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
    title = "Stichprobenkennwerteverteilung \n des arithmetischen Mittels mit mitlerem Bayesschätzer",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_estBayes = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_estBayes = "Mean aller Bayesschätzer",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()


#### plotting Bayes####

# single Sample
# setting specific sample
specific <- 5

ggplot(NULL, aes(x = samp_df[ , specific])) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classes) +
  geom_point(aes(x = bayesWerteNV[specific], y = 0), shape = 17, size = 4, colour = "darkgrey") +
  geom_vline(xintercept = bayesWerteNV[specific], colour = "black", linetype = "dotted") +

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
ggplot(NULL, aes(x = bayesWerteNV)) +
  geom_histogram(fill = "lightgrey", color = "black", bins = num_classesSKV) +

  # every sample as triangle
  geom_point(aes(x = bayesWerteNV, y = 0, color = "estimators"), shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2, colour = "mu"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu, colour = "mu")) +

  # mean over all samples
  geom_point(aes(x = mean_estBayesNV, y = 4,  colour = "mean_estBayes"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_estBayesNV, colour = "mean_estBayes"), linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  #  xlim(mu - sd, mu + sd) +  # xAchse festlegen
  # coord_cartesian()
  labs(
    title = "Stichprobenkennwerteverteilung \n des arithmetischen Mittels mit mitlerem Bayesschätzer",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_estBayesNV = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_estBayesNV = "Mean aller Bayesschätzer",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()

