#### setup ####
library(ggplot2)

### setting variables ####

est <- mean()     # Estimator
mu <- 100         # Population mean
sd <- 15          # Population sd
n <- 100          # sample size
number <- 100      # number of samples



#### generating data ####

# draws samples of "number" iterations
samp_df <- sapply(1:number, FUN = function(i) {
  samp <- rnorm(n = n, mean = mu, sd = sd)
  }
)

# mean colum wise
estimators <- sapply(1:ncol(samp_df), function(i) {
    mean(samp_df[, i])
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
 #  xlim(mu - sd, mu + sd) +  # xAchse festlegen
  labs(
    title = "Stichprobenkennwerteverteilung \n des arithmetischen Mittels",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  scale_color_manual(values = c(mu = "red", mean_est = "black", estimators = "darkgrey"),
                     labels = c(mu = expression(mu), mean_est = "Mean aller Stichprobenmeans",
                                estimators = "Stichprobenmittelwerte")) +
  theme_bw()
