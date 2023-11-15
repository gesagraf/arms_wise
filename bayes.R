#### setup ####
library(ggplot2)

### setting variables ####

est <- mean()     # Estimator
mu <- 100         # Population mean
sd <- 15          # Population sd
n <- 100          # sample size
number <- 100      # number of samples
mu_prior <- 80
tau_prior <- 10

#### generating data ####

# draws samples of "number" iterations
samp_df <- sapply(1:number, FUN = function(i) {
  samp <- rnorm(n = n, mean = mu, sd = sd)
}
)


##### likelihood #####
# calculate likelihood
mu_hat <- seq(mu - 2 * sd, mu + 2 * sd, length.out = 200)

#### Prior ####
prior_dens <- dnorm(mu_hat, mean = mu_prior, sd = tau_prior)

results <- list()

for (i in 1:number) {
likelihood_function <- sapply(mu_hat, FUN = function(i_mu){
  prod(dnorm(samp_df[,i], mean = i_mu, sd = sd(samp_df[,1])))
})

# Normierung der Likelihood
den_like <- Bolstad::sintegral(mu_hat, likelihood_function)      # Normierungskonstante
likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood


##### posteoriori #####
# calculate posteriori
posterior0 <- prior_dens * likelihood_function_norm

# Posteriori normieren
den_post <- Bolstad::sintegral(mu_hat, posterior0)
posterior <- posterior0 / den_post$value

results[[i]] <- posterior
}




# mean colum wise
estimators <- sapply(1:length(results), function(i) {
  mean(results[[i]])
})

# over all mean
mean_est <- mean(estimators)



#### plotting ####

##### single Sample #####
# setting specific sample
specific <- 5

ggplot(NULL, aes(x = samp_df[ , specific])) +
  geom_histogram(fill = "lightgrey", color = "black") +
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


##### SKV #####
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
  #  xlim(mu - sd, mu + sd) +  # x Achse festlegen
  labs(
    title = "Stichprobenkennwerteverteilung \n mit Bayesschätzer",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_est = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_est = "Mean aller Stichprobenmeans",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()


