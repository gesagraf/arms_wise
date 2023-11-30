#### setup ####
# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("grid")
library(gridExtra)
library(ggplot2)
library(grid)
library(cowplot)



#### definitions ####
min_coord <- min(c(mu - 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, min_uni_priori))
max_coord <- max(c(mu + 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, max_uni_priori))
coord <- c(min_coord, max_coord)


sum(prior_dens)
sum(norm_likelihood_nv[ , specific])
sum(norm_likelihood_uni[ , specific])
sum(prior_densNV)

#### plots ####
p_samp <-
ggplot(NULL, aes(x = samp_df[ , specific])) +
  geom_histogram(fill = "lightgrey", binwidth = ((max(samp_df[ , specific]) - min(samp_df[ , specific])) / num_classes)) +

  # mu
  geom_point(aes(x = mu, y = 2, colour = "mu"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu, colour = "mu")) +


  # Likelihood of data
  # hier muss nochmal drüber geschaut werden
  geom_line(aes(x = mu_hat, y = (norm_likelihood_nv[ , specific] * number), color = "likelihood")) + # Muss normalisiert werden
  geom_area(aes(x = mu_hat, y = (norm_likelihood_nv[ , specific] * number)), fill = "yellow", alpha = .4) + # Muss normalisiert werden

  # prior uni
  geom_line(aes(x = mu_hat, y = (prior_dens * number), color = "prior_uni")) + # Muss normalisiert werden
  geom_area(aes(x = mu_hat, y = (prior_dens * number)), fill = "orange", alpha = .4) + # Muss normalisiert werden


  # prior nv
  geom_line(aes(x = mu_hat, y = (prior_densNV * number), color = "prior_nv")) + # Muss normalisiert werden
  geom_area(aes(x = mu_hat, y = (prior_densNV * number)), fill = "brown", alpha = .4) + # Muss normalisiert werden

  # Schätzer
  # Mean
  geom_point(aes(x = estimators[specific], y = 0, colour = "est_mean"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = estimators[specific], colour = "est_mean"), linetype = "dotted") +

  # Minmax
  geom_point(aes(x = minmax[specific], y = 0, colour = "est_minmax"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = minmax[specific], colour = "est_minmax"), linetype = "dotted") +

  # bayes uni
  geom_point(aes(x = bayesWerte[specific], y = 0, colour = "est_bayes_uni"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = bayesWerte[specific], colour = "est_bayes_uni"), linetype = "dotted") +

  # bayes nv
  geom_point(aes(x = bayesWerteNV[specific], y = 0, colour = "est_bayes_nv"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = bayesWerteNV[specific], colour = "est_bayes_nv"), linetype = "dotted") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Einzelne Stichprobe",
    y = "Häufigkeit",
    x = "x",
    colour = NULL) +

  # legende
   scale_color_manual(values = c(mu = "red",est_mean = "green", est_minmax = "blue", est_bayes_uni = "purple",
                                 est_bayes_nv = "hotpink", likelihood = "yellow", prior_uni = "orange", prior_nv = "brown"),
                   labels = c(mu = expression(mu),
                              est_mean = "Arithmetisches Mittel",
                              est_minmax = "Alternativer Schätzer",
                              est_bayes_uni = "Bayesschätzer mit \n gleichverteilert Priori",
                              est_bayes_nv = "Bayesschätzer mit \n normalverteilert Priori",
                              likelihood = "Likelihood",
                              prior_uni = "Gleichverteilter Prior",
                              prior_nv = "Normalverteilter Prior"),
                   breaks = factor(c("mu","est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood", "prior_uni", "prior_nv"),
                                   levels = c("mu", "est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood", "prior_uni", "prior_nv"))) +
  theme_bw()










p_mean <-
  ggplot(NULL, aes(x = estimators)) +
  geom_histogram(fill = "green", bins = num_classesSKV, alpha = .5) +

  # every sample as triangle
  geom_point(aes(x = estimators, y = 0), colour = "green", shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 4),  colour = "black", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est), colour = "black", linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

labs(
    title = "Arithmetisches Mittel",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw()



p_minmax <-
  ggplot(NULL, aes(x = minmax)) +
  geom_histogram(fill = "blue", bins = num_classesSKV, alpha = .5) +

  # every sample as triangle
  geom_point(aes(x = minmax, y = 0), color = "blue", shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_minmax, y = 4),  colour = "black", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_minmax), colour = "black", linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Alternativer Schätzer",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw()


p_bayes_uni <-
  ggplot(NULL, aes(x = bayesWerte)) +
  geom_histogram(fill = "purple", binwidth = ((max(bayesWerteNV) - min(bayesWerteNV)) / num_classesSKV), alpha = .5) +

  # Prior
  geom_line(aes(x = mu_hat, y = (prior_dens * number)), color = "orange") + # Muss normalisiert werden
  geom_area(aes(x = mu_hat, y = (prior_dens * number)), fill = "orange", alpha = .4) + # Muss normalisiert werden

  # every sample as triangle
  geom_point(aes(x = bayesWerte, y = 09), color = "purple", shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_estBayes, y = 4,  colour = "mean_est"), shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_estBayes, colour = "mean_est"), linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Bayesschätzer mit gleichverteilter Priori",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +

 # legende
  scale_color_manual(values = c(mean_est = "black"),
                     labels = c(mean_est = "Mean aller \n Stichprobenschätzer")) +
  theme_bw()



p_bayes_nv <-
ggplot(NULL, aes(x = bayesWerteNV)) +
  geom_histogram(fill = "hotpink", binwidth = ((max(bayesWerteNV) - min(bayesWerteNV)) / num_classesSKV), alpha = .5) +

  # Prior
  geom_line(aes(x = mu_hat, y = (prior_densNV * number)), color = "brown") + # Muss normalisiert werden
  geom_area(aes(x = mu_hat, y = (prior_densNV * number)), fill = "brown", alpha = .4) + # Muss normalisiert werden


  # every sample as triangle
  geom_point(aes(x = bayesWerteNV, y = 0), colour = "hotpink", shape = 17, size = 4) +

  # mu
  geom_point(aes(x = mu, y = 2), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 4),  colour = "black", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est), colour = "black", linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Arithmetisches Mittel",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw()



#### combine them ####
# title, legend
title_skv <- textGrob("Stichprobenkennwerteverteilung", gp = gpar(fontsize = 15))
title_top <- textGrob("Beispielstichprobe", gp = gpar(fontsize = 20))
legend_skv <- cowplot::get_legend(p_bayes_uni)
legend_top <- cowplot::get_legend(p_samp)

# combine plots
combined_legends <- arrangeGrob(legend_top, legend_skv, ncol = 2)
combined_plots_top <- arrangeGrob(p_samp + theme(legend.position = "none"),
                                  combined_legends, top = title_top, ncol = 2)

combined_plots_skv <- arrangeGrob(p_mean + theme(legend.position = "none"),
                                          p_minmax + theme(legend.position = "none"),
                                          p_bayes_uni + theme(legend.position = "none"),
                                          p_bayes_nv + theme(legend.position = "none"),
                                          ncol = 2, top = title_skv)


# add legend
combined_plots_with_legend <- arrangeGrob(
  combined_plots_top, combined_plots_skv, heights = c(1, 2)
)


# show result
grid.arrange(combined_plots_with_legend)
