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
max_uni_priori <- 170 # max für gleichverteilte priori

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

norm_likelihood_uni <- matrix(ncol = number, nrow = 200)
results <- matrix(ncol = number, nrow = 200) # 200 weil wir für 200 punkte die likelihood berechnen

for (i in 1:number) {

  likelihood_function <- sapply(mu_hat, FUN = function(i_mu){   # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
    prod(dnorm(samp_df[,i], mean = i_mu, sd = sd(samp_df[,i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
  })                                                            # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()

  # Normierung der Likelihood #ich glaube wir müssen das nicht normieren aber macht es trotzdem Sinn? nicht so kleine zahlen
  den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
  likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood
  # normierte likelihood speicher, für die plots
  norm_likelihood_uni[ , i] <- likelihood_function_norm

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

prior_densNV <- dnorm(mu_hat, mean = mu_prior, sd = tau_prior)

norm_likelihood_nv <- matrix(ncol = number, nrow = 200)
resultsNV <- matrix(ncol = number, nrow = 200) # 200 weil wir für 200 punkte die likelihood berechnen


for (i in 1:number) {

  likelihood_function <- sapply(mu_hat, FUN = function(i_mu){   # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
    prod(dnorm(samp_df[ , i], mean = i_mu, sd = sd(samp_df[ , i]))) # prod = produkt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
  })                                                            # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()

  # Normierung der Likelihood #ich glaube wir müssen das nicht normieren aber macht es trotzdem Sinn? nicht so kleine zahlen
  den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
  likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood
  # normierte likelihood speicher, für die plots
  norm_likelihood_nv[ , i] <- likelihood_function_norm

  # calculate posteriori
  posterior0 <- prior_densNV * likelihood_function_norm

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
