#### plotting Mean####
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

