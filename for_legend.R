colours <- c(viridis(7, direction = -1), "red", "black")
names(colours) <- c("est_mean", "est_minmax", "est_bayes_uni", "est_bayes_nv", "likelihood", "prior_uni", "prior_nv", "mu", "mean_est")
custom_colors <- scale_color_manual(values = colours, name = "Legende",
                                    labels = c(mu = expression(mu),
                                               est_mean = "Arithmetisches \n Mittel",
                                               est_minmax = "Alternativer \n Schätzer",
                                               est_bayes_uni = "Bayesschätzer: \n gleichverteile \n Priori",
                                               est_bayes_nv = "Bayesschätzer: \n normalverteile \n Priori",
                                               likelihood = "Likelihood",
                                               prior_uni = "Gleichverteilter \n Prior",
                                               prior_nv = "Normalverteilter \n Prior",
                                               mean_est = "Mean aller \n Mittelwertschätzer"),
                                    breaks = factor(c("mu","est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood",
                                                      "prior_uni", "prior_nv", "mean_est"),
                                                    levels = c("mu", "est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood",
                                                               "prior_uni", "prior_nv", "mean_est")))

data <- data.frame(
  class = c("est_mean", "est_minmax", "est_bayes_uni", "est_bayes_nv", "likelihood", "prior_uni", "prior_nv", "mu", "mean_est"),
  x = 1:9
)


ggplot(data = data, aes(x = x, y = x, colour = class, fill = class)) +
  geom_point(shape = 24, size = 6) +
  custom_colors +
  scale_fill_manual(values = colours, name = "Legende",
                    labels = c(mu = expression(mu),
                               est_mean = "Arithmetisches \n Mittel",
                               est_minmax = "Alternativer \n Schätzer",
                               est_bayes_uni = "Bayesschätzer: \n gleichverteile \n Priori",
                               est_bayes_nv = "Bayesschätzer: \n normalverteile \n Priori",
                               likelihood = "Likelihood",
                               prior_uni = "Gleichverteilter \n Prior",
                               prior_nv = "Normalverteilter \n Prior",
                               mean_est = "Mean aller \n Mittelwertschätzer"),
                    breaks = factor(c("mu","est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood",
                                      "prior_uni", "prior_nv", "mean_est"),
                                    levels = c("mu", "est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood",
                                               "prior_uni", "prior_nv", "mean_est"))) +
  theme_bw() +
  theme(legend.position = "top",
        legend.text.align = 0
  )

ggsave("for_legend.png", last_plot(), scale = 9)
