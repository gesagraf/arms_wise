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



#### plots ####
# single sample
(p_samp <-
   ggplot(NULL, aes(x = samp_df[ , specific])) +
   geom_histogram(fill = "lightgrey", bins = num_classes, aes(y = ..density..)) +

   geom_line(aes(x = mu_hat, y = (norm_likelihood_nv[ , specific]), color = "likelihood")) +
   geom_area(aes(x = mu_hat, y = (norm_likelihood_nv[ , specific])), fill = "yellow", alpha = .4) +

   # prior uni
   geom_line(aes(x = mu_hat, y = prior_dens, color = "prior_uni")) +
   geom_area(aes(x = mu_hat, y = prior_dens), fill = "orange", alpha = .4) +


   # prior nv
   geom_line(aes(x = mu_hat, y = prior_densNV, color = "prior_nv")) +
   geom_area(aes(x = mu_hat, y = prior_densNV), fill = "brown", alpha = .4) +

   # mu
   geom_point(aes(x = mu, y = 0, colour = "mu"), shape = 17, size = 4) +
   geom_vline(aes(xintercept = mu, colour = "mu")) +

   # Schätzer
   # Mean
   geom_point(aes(x = estimators[specific], y = 0), fill = "green", colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = estimators[specific], colour = "est_mean"), linetype = "dotted") +

   # Minmax
   geom_point(aes(x = minmax[specific], y = 0), fill = "blue",  colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = minmax[specific], colour = "est_minmax"), linetype = "dotted") +

   # bayes uni
   geom_point(aes(x = bayesWerte[specific], y = 0), fill = "purple", colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = bayesWerte[specific], colour = "est_bayes_uni"), linetype = "dotted") +

   # bayes nv
   geom_point(aes(x = bayesWerteNV[specific], y = 0), fill = "hotpink", colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = bayesWerteNV[specific], colour = "est_bayes_nv"), linetype = "dotted") +


   # Skalen, Theme, Labs etc.
   coord_cartesian(xlim = coord) +

   labs(
     title = paste0("Einzelne Stichprobe (#", specific,")"),
     y = "Relative Häufigkeit",
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
   theme_bw() +

   # pinke Umrandung
   annotation_custom(
     grob = rectGrob(gp = gpar(col = "magenta", lty = "dashed", lwd = 5, fill = NA)),
     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
   )

)










(p_mean <-
  ggplot(NULL, aes(x = estimators)) +
  geom_histogram(aes(y = ..density..), fill = "green", bins = num_classesSKV, alpha = .5) +
    # every sample as triangle
  geom_point(aes(x = estimators, y = 0), colour = "green", shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = estimators[specific], y = 0), colour = "magenta", fill = "green", shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 0),  colour = "black", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est), colour = "black", linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

labs(
    title = "Arithmetisches Mittel",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw())



(p_minmax <-
  ggplot(NULL, aes(x = minmax)) +
  geom_histogram(aes(y = ..density..), fill = "blue", bins = num_classesSKV, alpha = .5) +

  # every sample as triangle
  geom_point(aes(x = minmax, y = 0), color = "blue", shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = minmax[specific], y = 0), colour = "magenta", fill = "blue", shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_minmax, y = 0),  colour = "black", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_minmax), colour = "black", linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Alternativer Schätzer",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw())


(p_bayes_uni <-
  ggplot(NULL, aes(x = bayesWerte)) +
  geom_histogram(aes(y = ..density..), fill = "purple", bins = num_classesSKV, alpha = .5) +

  # Prior
  geom_line(aes(x = mu_hat, y = prior_dens), color = "orange") + # Muss normalisiert werden
  geom_area(aes(x = mu_hat, y = prior_dens), fill = "orange", alpha = .4) + # Muss normalisiert werden

  # every sample as triangle
  geom_point(aes(x = bayesWerte, y = 0), color = "purple", shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = bayesWerte[specific], y = 0), colour = "magenta", fill = "purple", shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_estBayes, y = 0,  colour = "mean_est"), shape = 17, size = 4) +
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
  theme_bw())



(p_bayes_nv <-
ggplot(NULL, aes(x = bayesWerteNV)) +
  geom_histogram(aes(y = ..density..), fill = "hotpink", bins = num_classesSKV, alpha = .5) +

  # Prior
  geom_line(aes(x = mu_hat, y = prior_densNV), color = "brown") +
  geom_area(aes(x = mu_hat, y = prior_densNV), fill = "brown", alpha = .4) +


  # every sample as triangle
  geom_point(aes(x = bayesWerteNV, y = 0), colour = "hotpink", shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = bayesWerteNV[specific], y = 0), colour = "magenta", fill = "hotpink", shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = "red", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = "red") +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 0),  colour = "black", shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est), colour = "black", linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Arithmetisches Mittel",
    y = "Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw())



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
(plot <- grid.arrange(combined_plots_with_legend))
