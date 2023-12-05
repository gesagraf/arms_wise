#### setup ####
# install.packages("gridExtra")
# install.packages("cowplot")
# install.packages("grid")
library(gridExtra)
library(ggplot2)
library(grid)
library(cowplot)
library(viridis)



#### definitions ####
min_coord <- min(c(mu - 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, min_uni_priori))
max_coord <- max(c(mu + 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, max_uni_priori))
coord <- c(min_coord, max_coord)

colours <- c(viridis(7, direction = -1), "red", "black")
names(colours) <- c("est_mean", "est_minmax", "est_bayes_uni", "est_bayes_nv", "likelihood", "prior_uni", "prior_nv", "mu", "mean_est")
custom_colors <- scale_color_manual(values = colours,
                                    labels = c(mu = expression(mu),
                                               est_mean = "Arithmetisches Mittel",
                                               est_minmax = "Alternativer Schätzer",
                                               est_bayes_uni = "Bayesschätzer mit \n gleichverteilert Priori",
                                               est_bayes_nv = "Bayesschätzer mit \n normalverteilert Priori",
                                               likelihood = "Likelihood",
                                               prior_uni = "Gleichverteilter Prior",
                                               prior_nv = "Normalverteilter Prior",
                                               mean_est = "Mean aller Mittelwertschätzer"),
                                    breaks = factor(c("mu","est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood",
                                                      "prior_uni", "prior_nv", "mean_est"),
                                                    levels = c("mu", "est_mean", "est_minmax","est_bayes_uni", "est_bayes_nv", "likelihood",
                                                               "prior_uni", "prior_nv", "mean_est")))



#### plots ####
# single sample
(p_samp <-
   ggplot(NULL, aes(x = samp_df[ , specific])) +
   geom_histogram(fill = "lightgrey", bins = num_classes, aes(y = ..density..)) +

   # Platzhalter
   geom_point(aes(x = bayesWerte[specific], y = 0, colour = "mean_est"), shape = 24, size = 0.001) +

   geom_line(aes(x = mu_hat, y = (norm_likelihood_nv[ , specific]), color = "likelihood")) +
   geom_area(aes(x = mu_hat, y = (norm_likelihood_nv[ , specific])), fill = colours["likelihood"], alpha = .4) +

   # prior uni
   geom_line(aes(x = mu_hat, y = prior_dens, color = "prior_uni")) +
   geom_area(aes(x = mu_hat, y = prior_dens), fill = colours["prior_uni"], alpha = .4) +


   # prior nv
   geom_line(aes(x = mu_hat, y = prior_densNV, color = "prior_nv")) +
   geom_area(aes(x = mu_hat, y = prior_densNV), fill = colours["prior_nv"], alpha = .4) +

   # mu
   geom_point(aes(x = mu, y = 0, colour = "mu"), shape = 17, size = 4) +
   geom_vline(aes(xintercept = mu, colour = "mu")) +

   # Schätzer
   # Mean
   geom_point(aes(x = estimators[specific], y = 0), fill = colours["est_mean"], colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = estimators[specific], colour = "est_mean"), linetype = "dotted") +

   # Minmax
   geom_point(aes(x = minmax[specific], y = 0), fill = colours["est_minmax"],  colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = minmax[specific], colour = "est_minmax"), linetype = "dotted") +

   # bayes uni
   geom_point(aes(x = bayesWerte[specific], y = 0), fill = colours["est_bayes_uni"], colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = bayesWerte[specific], colour = "est_bayes_uni"), linetype = "dotted") +

   # bayes nv
   geom_point(aes(x = bayesWerteNV[specific], y = 0), fill = colours["est_bayes_nv"], colour = "magenta", shape = 24, size = 8) +
   geom_vline(aes(xintercept = bayesWerteNV[specific], colour = "est_bayes_nv"), linetype = "dotted") +


   # Skalen, Theme, Labs etc.
   coord_cartesian(xlim = coord) +

   labs(
     title = paste0("Einzelne Stichprobe (#", specific,")"),
     y = "Relative Häufigkeit",
     x = "x",
     colour = NULL,
     fill = NULL) +

   # legende
    custom_colors +

   theme_bw() +

   # pinke Umrandung
   annotation_custom(
     grob = rectGrob(gp = gpar(col = "magenta", lty = "dashed", lwd = 5, fill = NA)),
     xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
   )

)










(p_mean <-
  ggplot(NULL, aes(x = estimators)) +
  geom_histogram(aes(y = ..density..), fill = colours["est_mean"], bins = num_classesSKV, alpha = .5) +
    # every sample as triangle
  geom_point(aes(x = estimators, y = 0), colour = colours["est_mean"], shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = estimators[specific], y = 0), colour = "magenta", fill = colours["est_mean"], shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = colours["mu"]) +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est), colour = colours["mean_est"], linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

labs(
    title = "Arithmetisches Mittel",
    y = "Relative Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw())



(p_minmax <-
  ggplot(NULL, aes(x = minmax)) +
  geom_histogram(aes(y = ..density..), fill = colours["est_minmax"], bins = num_classesSKV, alpha = .5) +

  # every sample as triangle
  geom_point(aes(x = minmax, y = 0), color = colours["est_minmax"], shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = minmax[specific], y = 0), colour = "magenta", fill = colours["est_minmax"], shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = colours["mu"]) +

  # mean over all samples
  geom_point(aes(x = mean_minmax, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_minmax), colour = colours["mean_est"], linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Alternativer Schätzer",
    y = "Relative Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw())


(p_bayes_uni <-
  ggplot(NULL, aes(x = bayesWerte)) +
  geom_histogram(aes(y = ..density..), fill = colours["est_bayes_uni"], binwidth = 1, alpha = .5) +

  # Prior
  geom_line(aes(x = mu_hat, y = prior_dens), color = colours["prior_uni"]) +
  geom_area(aes(x = mu_hat, y = prior_dens), fill = colours["prior_uni"], alpha = .4) +


  # every sample as triangle
  geom_point(aes(x = bayesWerte, y = 0), color = colours["est_bayes_uni"], shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = bayesWerte[specific], y = 0), colour = "magenta", fill = colours["est_bayes_uni"], shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = colours["mu"]) +

  # mean over all samples
  geom_point(aes(x = mean_estBayes, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_estBayes), colour = colours["mean_est"], linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Bayesschätzer mit gleichverteilter Priori",
    y = "Relative Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +

  theme_bw())



(p_bayes_nv <-
ggplot(NULL, aes(x = bayesWerteNV)) +
  geom_histogram(aes(y = ..density..), fill = colours["est_bayes_nv"], bins = num_classesSKV, alpha = .5) +

  # Prior
  geom_line(aes(x = mu_hat, y = prior_densNV), color = colours["prior_nv"]) +
  geom_area(aes(x = mu_hat, y = prior_densNV), fill = colours["prior_nv"], alpha = .4) +


  # every sample as triangle
  geom_point(aes(x = bayesWerteNV, y = 0), colour = colours["est_bayes_nv"], shape = 17, size = 4) +

  # frame selected sample
  geom_point(aes(x = bayesWerteNV[specific], y = 0), colour = "magenta", fill = colours["est_bayes_nv"], shape = 24, size = 8) +

  # mu
  geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mu), colour = colours["mu"]) +

  # mean over all samples
  geom_point(aes(x = mean_est, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
  geom_vline(aes(xintercept = mean_est), colour = colours["mean_est"], linetype = "dashed") +


  # Skalen, Theme, Labs etc.
  coord_cartesian(xlim = coord) +

  labs(
    title = "Bayesschätzer mit normalverteilter Priori",
    y = "Relative Häufigkeit",
    x = expression(bar(x)),
    colour = NULL) +
  theme_bw())



#### combine them ####
# title, legend
title_skv <- textGrob("Stichprobenkennwerteverteilung", gp = gpar(fontsize = 15))
title_top <- textGrob("Beispielstichprobe", gp = gpar(fontsize = 20))
legend <- cowplot::get_legend(p_samp)

# combine plots
combined_plots_top <- arrangeGrob(p_samp + theme(legend.position = "none"),
                                  legend, top = title_top, ncol = 2)

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
