server <- function(input, output) {
  set.seed(12345)

  output$skv_plot <- renderPlot({

    #### data ####
    # get input from ui
    mu <- input$mu                         # Population mean
    sd <- input$sd                         # Population sd
    n  <- input$n                          # sample size
    number <- input$number                 # number of samples
    specific <- input$specific             # specific sample
    min_uni_priori <- input$min_uni_priori # minimum für gleichverteilte priori
    max_uni_priori <- input$max_uni_priori # max für gleichverteilte priori
    mu_prior <-  input$mu_prior            # mittelwert der priori
    tau_prior <- input$tau_prior           # sd der priori

    # Anzahl der Klassen nach Sturges' Regel (für Histogram der einzelnen Stichprobe)
    num_classes <- ceiling(log2(n) + 1)
    # Anzahl der Klassen nach Sturges' Regel (für Histogram der SKV)
    num_classesSKV <- ceiling(log2(number) + 1)

    #### create data ####
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


    #### Plot ####
    #### definitions ####
    min_coord <- mu - 2 * sd
      # min(c(mu - 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, min_uni_priori))
    max_coord <- mu + 2 * sd
      # max(c(mu + 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, max_uni_priori))
    coord <- c(min_coord, max_coord)

    colours <- c(viridis(7, direction = -1), "red", "black")
    names(colours) <- c("est_mean", "est_minmax", "est_bayes_uni", "est_bayes_nv", "likelihood", "prior_uni", "prior_nv", "mu", "mean_est")
    custom_colors <- scale_color_manual(values = colours,
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



    #### plots ####
    # single sample

    p_samp <-
       ggplot(NULL, aes(x = samp_df[ , specific])) +

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

       # Verteilung
       geom_histogram(fill = "lightgrey", colour = "lightgrey", bins = num_classes, aes(y = ..density..), alpha = .9) +


       # Schätzer
       # Mean
       geom_point(aes(x = estimators[specific], y = 0), fill = colours["est_mean"], colour = "magenta", shape = 24, size = 8) +
       geom_vline(aes(xintercept = estimators[specific], colour = "est_mean"), linetype = "dotted", size = .75) +

       # Minmax
       geom_point(aes(x = minmax[specific], y = 0), fill = colours["est_minmax"],  colour = "magenta", shape = 24, size = 8) +
       geom_vline(aes(xintercept = minmax[specific], colour = "est_minmax"), linetype = "dotted", size = .75) +

       # bayes uni
       geom_point(aes(x = bayesWerte[specific], y = 0), fill = colours["est_bayes_uni"], colour = "magenta", shape = 24, size = 8) +
       geom_vline(aes(xintercept = bayesWerte[specific], colour = "est_bayes_uni"), linetype = "dotted", size = .75) +

       # bayes nv
       geom_point(aes(x = bayesWerteNV[specific], y = 0), fill = colours["est_bayes_nv"], colour = "magenta", shape = 24, size = 8) +
       geom_vline(aes(xintercept = bayesWerteNV[specific], colour = "est_bayes_nv"), linetype = "dotted", size = .75) +

       # mu
       geom_point(aes(x = mu, y = 0, colour = "mu"), shape = 17, size = 4) +
       geom_vline(aes(xintercept = mu, colour = "mu"), size = 1) +

       # Skalen, Theme, Labs etc.
       coord_cartesian(xlim = coord) +

       # 2. y-Achse
       scale_y_continuous(
         name = "Relative Häufigkeit",
         sec.axis = sec_axis( trans=~.*number, name = "Anzahl TN")
       ) +

       labs(
         title = paste0("Einzelne Stichprobe (#", specific,")"),
         x = "x",
         colour = NULL,
         fill = NULL) +

       # legende
       custom_colors +

      theme_bw() +
      theme(legend.text = element_text(size = 15))

       # pinke Umrandung
       annotation_custom(
         grob = rectGrob(gp = gpar(col = "magenta", lwd = 5, fill = NA)),
         xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
         )


    # Platzhalter für forestplot
    p_forest <-
      ggplot(NULL, aes(x = samp_df[ , specific])) +
      labs(title = "some title") +
      theme_bw()



    # Mean
    p_mean <-
        ggplot(NULL, aes(x = estimators)) +
        geom_histogram(aes(y = ..density..), fill = colours["est_mean"], bins = num_classesSKV, alpha = .5) +
        # every sample as triangle
        geom_point(aes(x = estimators, y = 0), colour = colours["est_mean"], shape = 17, size = 4) +

        # frame selected sample
        geom_point(aes(x = estimators[specific], y = 0), colour = "magenta", fill = colours["est_mean"], shape = 24, size = 8) +

        # mu
        geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mu), colour = colours["mu"], size = 1) +

        # mean over all samples
        geom_point(aes(x = mean_est, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_est), colour = colours["mean_est"], linetype = "dashed", size = .75) +




        # Skalen, Theme, Labs etc.
        coord_cartesian(xlim = coord) +

      # 2. y-Achse
      scale_y_continuous(
        sec.axis = sec_axis( trans=~.*number)
      ) +

        labs(
          title = "Arithmetisches Mittel",
          x = expression(bar(x)),
          y = NULL,
          colour = NULL) +
        theme_bw()

    # Alternativer Schätzer
    p_minmax <-
        ggplot(NULL, aes(x = minmax)) +
        geom_histogram(aes(y = ..density..), fill = colours["est_minmax"], bins = num_classesSKV, alpha = .5) +

        # every sample as triangle
        geom_point(aes(x = minmax, y = 0), color = colours["est_minmax"], shape = 17, size = 4) +

        # frame selected sample
        geom_point(aes(x = minmax[specific], y = 0), colour = "magenta", fill = colours["est_minmax"], shape = 24, size = 8) +

        # mu
        geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mu), colour = colours["mu"], size = 1) +

        # mean over all samples
        geom_point(aes(x = mean_minmax, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_minmax), colour = colours["mean_est"], linetype = "dashed", size = .75) +



        # Skalen, Theme, Labs etc.
        coord_cartesian(xlim = coord) +

      # 2. y-Achse
      scale_y_continuous(
        sec.axis = sec_axis( trans=~.*number)
      ) +

        labs(
          title = "Alternativer Schätzer",
          x = expression(bar(x)),
          y = NULL,
          colour = NULL) +
        theme_bw()

    # Bayes Gleichverteilt
    p_bayes_uni <-
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
        geom_vline(aes(xintercept = mu), colour = colours["mu"], size = 1) +

        # mean over all samples
        geom_point(aes(x = mean_estBayes, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_estBayes), colour = colours["mean_est"], linetype = "dashed", size = .75) +


        # Skalen, Theme, Labs etc.
        coord_cartesian(xlim = coord) +

      # 2. y-Achse
      scale_y_continuous(
        sec.axis = sec_axis( trans=~.*number)
      ) +

        labs(
          title = "Bayesschätzer mit gleichverteilter Priori",
          x = expression(bar(x)),
          y = NULL,
          colour = NULL) +

        theme_bw()


    # Bayes Normalverteilt
    p_bayes_nv <-
        ggplot(NULL, aes(x = bayesWerteNV)) +
        geom_histogram(aes(y = ..density..), fill = colours["est_bayes_nv"], binwidth = round((max(bayesWerteNV) - min(bayesWerteNV)) / num_classesSKV), alpha = .5) +

        # Prior
        geom_line(aes(x = mu_hat, y = prior_densNV), color = colours["prior_nv"]) +
        geom_area(aes(x = mu_hat, y = prior_densNV), fill = colours["prior_nv"], alpha = .4) +


        # every sample as triangle
        geom_point(aes(x = bayesWerteNV, y = 0), colour = colours["est_bayes_nv"], shape = 17, size = 4) +

        # frame selected sample
        geom_point(aes(x = bayesWerteNV[specific], y = 0), colour = "magenta", fill = colours["est_bayes_nv"], shape = 24, size = 8) +

        # mu
        geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mu), colour = colours["mu"], size = 1) +

        # mean over all samples
        geom_point(aes(x = mean_estBayesNV, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_estBayesNV), colour = colours["mean_est"], linetype = "dashed", size = .75) +



        # Skalen, Theme, Labs etc.
        coord_cartesian(xlim = coord) +

        # 2. y-Achse
        scale_y_continuous(
          sec.axis = sec_axis( trans=~.*number)
        ) +

        labs(
          title = "Bayesschätzer mit normalverteilter Priori",
          x = expression(bar(x)),
          y = NULL,
          colour = NULL) +
        theme_bw()


    #### combine them ####
    # title, legend
    title_skv <- textGrob("Stichprobenkennwerteverteilung", gp = gpar(fontsize = 15))
    title_top <- textGrob("Beispielstichprobe", gp = gpar(fontsize = 15))
    legend <- cowplot::get_legend(p_samp)
    label_left <- ggdraw() + draw_label("Relative Häufigkeit", angle = 90, size = 12)
    label_right <- ggdraw() + draw_label("Anzahl SP", angle = 270, size = 12)


        # combine plots
    combined_plots_top <- arrangeGrob(p_samp + theme(legend.position = "none"),
                                      p_forest + theme(legend.position = "none"),
                                      top = title_top, ncol = 2)

    combined_plots_skv <- arrangeGrob(p_mean + theme(legend.position = "none"),
                                      p_minmax + theme(legend.position = "none"),
                                      p_bayes_uni + theme(legend.position = "none"),
                                      p_bayes_nv + theme(legend.position = "none"),
                                      ncol = 2, top = title_skv)

    combined_plots_labels <- arrangeGrob(
      label_left, combined_plots_skv, label_right,
      widths = c(.1, 2, .1))


    # add top
    combined_plots_with_top <- arrangeGrob(
      combined_plots_top, combined_plots_labels, heights = c(1, 2)
    )

    # add legend
    combined_plots_with_legend <- arrangeGrob(
      combined_plots_with_top, legend, widths = c(4, 1.5)
    )


    # show result
    plot <- grid.arrange(combined_plots_with_legend)

    return(plot)










  })
}
