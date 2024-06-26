server <- function(input, output, session) {
  # setup
  library(viridis)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(cowplot)
  library(latex2exp)

  set.seed(12345)

  #slider updaten
  observeEvent(input$number, {
    updateSliderInput(inputId = "specific", max = input$number)
  })
  #range der priori auf mu hat begrenzen
  # observeEvent(eventExpr = {
  #  input$mu
  # input$std
  #}, {
  # updateSliderInput(inputId = "rangePriori", min = (input$mu - 2*input$std), max = (input$mu + 2*input$std))
  #})

  #### data ####
  # get input from ui
  mu <- reactive(input$mu)                         # Population mean
  std <- reactive(input$std)                       # Population sd
  n  <- reactive(input$n)                          # sample size
  number <- reactive(input$number)                 # number of samples
  specific <- reactive(input$specific)             # specific sample
  min_uni_priori <- reactive(input$rangePriori[1]) # minimum für gleichverteilte priori
  max_uni_priori <- reactive(input$rangePriori[2]) # max für gleichverteilte priori
  mu_prior <-  reactive(input$mu_prior)            # mittelwert der priori
  tau_prior <- reactive(input$tau_prior)           # sd der priori
                               # die Länge von mu_hat, NICHT REAKTIV!


  #### create data ####
  # draws samples of "number" iterations
  samp_df <- reactive(
    sapply(1:number(), FUN = function(i) {
      samp <- rnorm(n = n(), mean = mu(), sd = std())
    }
    )
  )


  #### mean ####
  #colum wise
  estimators <- reactive(apply(samp_df(), MARGIN = 2, mean))


  # over all mean
  mean_est <- reactive(mean(estimators()))

  #### min/max ####
  # mean colum wise
  max_min_mean <-function(vec) {(max(vec) + min(vec)) / 2}

  minmax <- reactive(apply(samp_df(), MARGIN = 2, max_min_mean))

  # over all mean
  mean_minmax <- reactive(mean(minmax()))



  #### Bayes mit gleichverteilter Priori mit min max #####
  #take xlim coordinates of plots to get boundaries of mu_hat
  min_coord <- reactive(mu() - 3 * std())
  # # min(c(mu - 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, min_uni_priori))
  max_coord <- reactive(mu() + 3 * std())

  #prior reatio was 1000 points between -200 and 200
  #1000/400 = 2.5
  # use same ratio
  # max 50*2.5*6
  #set to 6*std() since every

  lengthout <- reactive(2.5*abs(min_coord()-max_coord()))

  # calculate likelihood
  mu_hat <- reactive({seq(min_coord(), max_coord(), length.out = lengthout())})

  # density of prior
  prior_dens <- reactive(dunif(mu_hat(), min_uni_priori(), max_uni_priori()))



  ##start computations


  ## get Likelihood of Date for all Bayes Estimates

  norm_likelihood<-reactive({
    sapply(1:number(), function(i){
      # isolate({
      samp_df<-samp_df()
      # like_mat<-matrix(ncol = 3, nrow)
      sd_i<-apply(samp_df, 2, sd)


      likelihood_function <-
          # apply(matrix(c(samp_df[,i],rep(i_mu, nrow(samp_df))))

          sapply(mu_hat(), FUN = function(i_mu){                                # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
            prod(dnorm(samp_df[ , i], mean = i_mu, sd = sd_i[i])) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
          })                                                                  # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()






        # Normierung der Likelihood
        den_like <- Bolstad::sintegral(mu_hat(), likelihood_function)      # Normierungskonstante
        likelihood_function / den_like$value # normierte Likelihood
      # })
    })
  })

  # return_list_uni1 <- reactive({
  #   # mit festgesetztem objekt
  #   # norm_likelihood_uni
  #   # mit berechnetem objekt
  #   norm_likelihood_uni <- matrix(ncol = number(), nrow = lengthout())
  #
  #
  #   for (i in 1:number()) {
  #
  #     likelihood_function <-
  #       sapply(mu_hat, FUN = function(i_mu){                                # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
  #         prod(dnorm(samp_df()[ , i], mean = i_mu, sd = sd(samp_df()[ , i]))) # prod = prdukt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
  #       })                                                                  # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()
  #
  #
  #
  #
  #
  #
  #     # Normierung der Likelihood
  #     den_like <- Bolstad::sintegral(mu_hat(), likelihood_function)      # Normierungskonstante
  #     likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood
  #
  #     # normierte likelihood speicher, für die plots
  #     norm_likelihood_uni[ , i] <- likelihood_function_norm
  #   }
  #   return(norm_likelihood_uni)
  # })



  return_list_uni2 <- reactive({

    results <- matrix(ncol = number(), nrow = lengthout())

    # isolate({
      norm_likelihood_uni <- norm_likelihood() # likelihood_function_norm aus vorheriger for schleife dazu holen

      for (i in 1:number()) {







        # calculate posteriori
        posterior0 <- prior_dens() * norm_likelihood_uni[ , i]
        #
        #   # Posteriori normieren
        den_post <- Bolstad::sintegral(mu_hat(), posterior0)
        posterior <- posterior0 / den_post$value

        # !!! Hier brauchen wir dringend eine Kontrollfunktion: wenn NaN, dann Fehlermeldung: "Die Daten sind bei deiner Gewählten gleichverteilten Priori (super starke annahme!!) unmöglich!"
        results[ , i] <- posterior
      }
      return(results)
    # })

  })



  # Index des Maximums in jeder Spalte finden
  index_maximum <- reactive(
    apply(return_list_uni2(), MARGIN = 2, which.max)
  )


  # Wert von 'x' für das Maximum von 'y' finden
  bayesWerte <- reactive(mu_hat()[index_maximum()])



  # over all mean Bayes
  mean_estBayes <- reactive(mean(bayesWerte()))


  #### Bayes mit nv Priori #####
  # Prior

  prior_densNV <- reactive(dnorm(mu_hat(), mean = mu_prior(), sd = tau_prior()))

  # return_list_nv1 <- reactive({
  #   norm_likelihood_nv = matrix(ncol = number(), nrow = lengthout())
  #
  #
  #   for (i in 1:number()) {
  #
  #     likelihood_function <-
  #       sapply(mu_hat(), FUN = function(i_mu){                                  # für jede Stichprobe wird die likelihood unter allen mu_hat werten ausgerechnet
  #         prod(dnorm(samp_df()[ , i], mean = i_mu, sd = sd(samp_df()[ , i]))) # prod = produkt (rechnet einzelne wahrscheinlichkeiten zu likelihood zusammen)
  #       })                                                                      # die SP nehmen wir nv an, deswegen hier unabhängig von prior dnorm()
  #
  #
  #
  #     # Normierung der Likelihood #ich glaube wir müssen das nicht normieren aber macht es trotzdem Sinn? nicht so kleine zahlen
  #     den_like <- Bolstad::sintegral(mu_hat(), likelihood_function)      # Normierungskonstante
  #     likelihood_function_norm <- likelihood_function / den_like$value # normierte Likelihood
  #
  #     # normierte likelihood speicher, für die plots
  #     norm_likelihood_nv[ , i] <- likelihood_function_norm
  #   }
  #   return(norm_likelihood_nv)
  # })



  return_list_nv2 <- reactive({

    resultsNV = matrix(ncol = number(), nrow = lengthout())
    # isolate({
      norm_likelihood_nv <- norm_likelihood() # likelihood_function_norm aus vorheriger for schleife dazu holen

      for (i in 1:number()) {


        # calculate posteriori
        posterior0 <- prior_densNV() * norm_likelihood_nv[ , i]

        # Posteriori normieren # hier nochmal
        den_post <- Bolstad::sintegral(mu_hat(), posterior0)
        posterior <- posterior0 / den_post$value

        resultsNV[ , i] <- posterior
      }

      return(resultsNV)
    # })

  })



  # Index des Maximums in jeder Spalte finden
  index_maximumNV <- reactive(
    apply(return_list_nv2(), MARGIN = 2, which.max)
  )


  # Wert von 'x' für das Maximum von 'y' finden
  bayesWerteNV <- reactive(mu_hat()[index_maximumNV()])

  # over all mean Bayes
  mean_estBayesNV <- reactive(mean(bayesWerteNV()))


  #### Plot ####
  #### definitions ####
  # min_coord <- reactive(mu() - 3 * std())
  # min(c(mu - 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, min_uni_priori))
  # max_coord <- reactive(mu() + 3 * std())
  # max(c(mu + 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, max_uni_priori))
  coord <- reactive(c(min_coord(), max_coord()))

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
  sampfdfspecific <- reactive(samp_df()[ ,specific()])
  bayeswertespecific <- reactive(bayesWerte()[specific()])
  resultsunispecific <- reactive(norm_likelihood()[ , specific()])

  # Anzahl der Klassen nach Sturges' Regel (für Histogram der einzelnen Stichprobe)
  num_classes <- reactive(ceiling(log2(n()) + 1))
  # Anzahl der Klassen nach Sturges' Regel (für Histogram der SKV)
  num_classesSKV <- reactive(ceiling(log2(number()) + 1))


  #### plots ####
  # x_layer <- reactive({scale_x_continuous(breaks = seq(min_coord(), max_coord(), std()), limits = coord())})

  # single sample
  # Single Sample Plot erstellen, aber NICHT ausgeben; für die Legende
  p_sample_basis <- reactive({
    ggplot(NULL, aes(x = sampfdfspecific())) +
      # Platzhalter
      geom_point(aes(x = bayeswertespecific(), y = 0, colour = "mean_est"), shape = 24, size = 1) +

      #        #
      #        # Verteilung
      geom_histogram(aes(y = after_stat(density)), fill = "lightgrey",
                     colour = "lightgrey", bins = (num_classes()*2), alpha = .99) +
      #

      #        # mu
      geom_point(aes(x = mu(), y = 0, colour = "mu"), shape = 17, size = 4) +
      geom_vline(aes(xintercept = mu(), colour = "mu"), linewidth = 1) +

      # #        # 2. y-Achse
      # scale_y_continuous(
      #   name = "Relative Häufigkeit",
      #   sec.axis = sec_axis( trans=~.*number(), name = "Anzahl TN")
      # ) +
      #
      xlim(coord()) +
      # x_layer() +

      labs(
        title = paste0("Einzelne Stichprobe (#", specific(),")"),
        x = "x") +
      # legende
      custom_colors +
      theme_bw()
  })

  ### Single Sample Plot anzeigen lassen ####
  ### Reaktiven Teil definieren
  ## Schätzer
  # Mean
  mean_layer <- reactive({
    if (input$p_mean == TRUE){
      list(geom_point(aes(x = estimators()[specific()], y = 0),
                      colour = "magenta", shape = 24,
                      fill = colours["est_mean"], size = 8),
           geom_vline(aes(xintercept = estimators()[specific()], colour = "est_mean"),
                      linetype = "dotted", linewidth = .75))
    } else NULL
  })

  # Minmax
  minmax_layer <- reactive({
    if (input$p_minmax == TRUE){
      list(geom_point(aes(x = minmax()[specific()], y = 0),
                      colour = "magenta", shape = 24, fill = colours["est_minmax"], size = 8),
           geom_vline(aes(xintercept = minmax()[specific()], colour = "est_minmax"), linetype = "dotted", linewidth = .75))

    } else NULL
  })

  # Bayes uni
  bayes_uni_layer <- reactive({
    if (input$p_bayes_uni == TRUE){
      list(geom_point(aes(x = bayesWerte()[specific()], y = 0),
                      colour = "magenta", shape = 24, fill = colours["est_bayes_uni"], size = 8),
           geom_vline(aes(xintercept = bayesWerte()[specific()], colour = "est_bayes_uni"), linetype = "dotted", linewidth = .75))
    } else NULL
  })

  # bayes nv
  bayes_nv_layer <- reactive({
    if (input$p_bayes_nv == TRUE){
      list(geom_point(aes(x = bayesWerteNV()[specific()], y = 0),
                      colour = "magenta", shape = 24, size = 8, fill = colours["est_bayes_nv"], ),
           geom_vline(aes(xintercept = bayesWerteNV()[specific()], colour = "est_bayes_nv"), linetype = "dotted", linewidth = .75))
    } else NULL
  })

  # Bayes Spezifika
  likelihood_layer <- reactive({
    if (input$show_likelihood == TRUE){
      list(     geom_line(aes(x = mu_hat(), y = resultsunispecific(), color = "likelihood")),
                geom_area(aes(x = mu_hat(), y = resultsunispecific()), alpha = .4, fill = colours["likelihood"]))
    } else NULL
  })

  # prior uni
  prior_uni_layer <- reactive({
    if (input$show_prior_uni == TRUE){
      list(
        geom_line(aes(x = mu_hat(), y = prior_dens()), color = colours["prior_uni"]),
        geom_area(aes(x = mu_hat(), y = prior_dens()), fill = colours["prior_uni"], alpha = .4)
      )
    } else NULL
  })

  # prior nv
  prior_nv_layer <- reactive({
    if (input$show_prior_nv == TRUE){
      list(
        geom_line(aes(x = mu_hat(), y = prior_densNV()), color = colours["prior_nv"]),
        geom_area(aes(x = mu_hat(), y = prior_densNV()),fill = colours["prior_nv"], alpha = .4)
      )
    } else NULL
  })



  # Pinke Umrandung
  pink_border <- reactive({
    if(any(c(input$p_mean, input$p_minmax, input$p_bayes_uni, input$p_bayes_nv))) {
      annotation_custom(
        grob = rectGrob(gp = gpar(col = "magenta", lwd = 5, fill = NA)),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    } else NULL
  })

  #### Legende ####
  output$legendontop <- renderUI({
    img(src='legende.png')
  })




  output$plot_samp <- renderPlot({
    if (!input$p_samp) return(NULL)

    p_sample_basis() +

      # Reaktive Schätzer
      mean_layer() +
      minmax_layer() +
      bayes_uni_layer() +
      bayes_nv_layer() +

      # Optionale Likelihood
      likelihood_layer() +

      # Priors
      prior_uni_layer() +
      prior_nv_layer() +

      pink_border() +

      theme(legend.position = "none")
  })


  # create data for the forest plot
  dat <- reactive(data.frame(
    Index = 1:4, # This provides an order to the data
    label = c("Arithmetisches Mittel", "Alternativer Schätzer", "Bayesschätzer mit gleichverteilter Priori", "Bayesschätzer mit normalverteilter Priori"),
    OR = c(mean(estimators()), mean(minmax()), mean(bayesWerte()), mean(bayesWerteNV())), # Odds Ration
    # Lower Border
    LL = c(
      mean(estimators()) - 1.96 * sd(estimators()), mean(minmax()) - 1.96 * sd(minmax()),
      mean(bayesWerte()) - 1.96 * sd(bayesWerte()), mean(bayesWerteNV()) - 1.96 * sd(bayesWerteNV())),
    # Upper border
    UL = c(
      mean(estimators()) + 1.96 * sd(estimators()), mean(minmax()) + 1.96 * sd(minmax()),
      mean(bayesWerte()) + 1.96 * sd(bayesWerte()), mean(bayesWerteNV()) + 1.96 * sd(bayesWerteNV())),
    frb = colours[c("est_mean", "est_minmax", "est_bayes_uni", "est_bayes_nv")])
  )

  #### Forestplot ####
  output$forest <- renderPlot({
    if (!input$p_forest) return(NULL)

    ggplot(dat(), aes(y = Index, x = OR)) +
      geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25, linewidth = 1, colour = dat()$frb) +
      geom_point(shape = 22, colour = "black", fill = "white") +
      geom_vline(aes(xintercept = mu()), colour = colours["mu"], linewidth = 1) +
      scale_y_continuous(breaks = 1:4, labels =
                           c(est_mean = "Arithmetisches \n Mittel",
                             est_minmax = "Alternativer \n Schätzer",
                             est_bayes_uni = "Bayesschätzer: \n gleichverteile \n Priori",
                             est_bayes_nv = "Bayesschätzer: \n normalverteile \n Priori"),
                         trans = "reverse") +
      xlim(coord()) +

      labs(
        title = "Konfidenzintervalle der einzelnen Schätzer",
        x = "Werte gemittelt",
        y = NULL
      ) +
      theme_bw()
  })



  #### Arithmetisches Mittel Plot ####
  output$plot_mean <- renderPlot({
    if (!input$p_mean) return(NULL)

    # Mean
    ggplot(NULL, aes(x = estimators())) +
      geom_histogram(aes(y = after_stat(density)), fill = colours["est_mean"],
                     bins = num_classesSKV(),
                     #binwidth = binweite(),
                     alpha = .5) +
      # every sample as triangle
      geom_point(aes(x = estimators(), y = 0), colour = colours["est_mean"], shape = 17, size = 4) +

      # frame selected sample
      geom_point(aes(x = estimators()[specific()], y = 0), colour = "magenta", fill = colours["est_mean"], shape = 24, size = 8) +
      # mu
      geom_point(aes(x = mu(), y = 0), colour = colours["mu"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mu()), colour = colours["mu"], linewidth = 1) +

      # mean over all samples
      geom_point(aes(x = mean_est(), y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mean_est()), colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +



      # Skalen, Theme, Labs etc.
      coord_cartesian(xlim = coord()) +
      # x_layer() +


      # # 2. y-Achse
      # scale_y_continuous(
      #   sec.axis = sec_axis( trans=~.*number())
      # ) +

      labs(
        title = "Arithmetisches Mittel",
        x = expression(bar(x)),
        y = NULL,
        colour = NULL) +
      theme_bw()
  })

  ## Definitionen für die Annotation der Formel
  kategorienbreite <- reactive({(max(minmax()) - min(minmax())) / num_classesSKV()})
  # y Wert für die Annotation
  anno_y <- reactive({max(density(minmax(), bw = kategorienbreite())$y)})



  #### Plot Minmax ####
  output$plot_minmax <- renderPlot({
    if (!input$p_minmax) return(NULL)

    # Alternativer Schätzer
    ggplot(NULL, aes(x = minmax())) +

      geom_histogram(aes(y = after_stat(density)), fill = colours["est_minmax"],
                     bins = num_classesSKV(), alpha = .5) +

      annotate("label",
               label = TeX(r"( $\frac{\max(x) + \min(x)}{2}$ )"),
               x = mu() - 2 * std(), y = anno_y()) +
      # every sample as triangle
      geom_point(aes(x = minmax(), y = 0), color = colours["est_minmax"], shape = 17, size = 4) +

      # frame selected sample
      geom_point(aes(x = minmax()[specific()], y = 0), colour = "magenta", fill = colours["est_minmax"], shape = 24, size = 8) +

      # mu
      geom_point(aes(x = mu(), y = 0), colour = colours["mu"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mu()), colour = colours["mu"], linewidth = 1) +

      # mean over all samples
      geom_point(aes(x = mean_minmax(), y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mean_minmax()), colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +



      # Skalen, Theme, Labs etc.
      coord_cartesian(xlim = coord()) +


      # # 2. y-Achse
      # scale_y_continuous(
      #   sec.axis = sec_axis( trans=~.*number())
      # ) +

      labs(
        title = "Alternativer Schätzer",
        x = expression(bar(x)),
        y = NULL,
        colour = NULL) +
      theme_bw()
  })


  binweite <- reactive({(max(bayesWerte()) - min(bayesWerte())) / 4})


  #### Plot Bayes Gleichverteilt ####
  output$plot_bayes_uni <- renderPlot({
    # plot_bayes()
    if (!input$p_bayes_uni) return(NULL)


    # Bayes Gleichverteilt
    ggplot(NULL, aes(x = bayesWerte())) +
      geom_histogram(aes(y = after_stat(density)), fill = colours["est_bayes_uni"],
                     # bins = num_classesSKV(),
                     binwidth = binweite(),
                     alpha = .5) +

      # Prior
      prior_uni_layer() +

      # every sample as triangle
      geom_point(aes(x = bayesWerte(), y = 0), color = colours["est_bayes_uni"], shape = 17, size = 4) +

      # frame selected sample
      geom_point(aes(x = bayesWerte()[specific()], y = 0), colour = "magenta", fill = colours["est_bayes_uni"], shape = 24, size = 8) +

      # mu
      geom_point(aes(x = mu(), y = 0), colour = colours["mu"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mu()), colour = colours["mu"], linewidth = 1) +

      # mean over all samples
      geom_point(aes(x = mean_estBayes(), y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mean_estBayes()),
                 colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +

      # Skalen, Theme, Labs etc.
      coord_cartesian(xlim = coord())  +

      labs(
        title = "Bayesschätzer mit gleichverteilter Priori",
        x = expression(bar(x)),
        y = NULL,
        colour = NULL) +

      theme_bw()
  })



  #### Bayes Normalverteilt ####
  output$plot_bayes_nv <- renderPlot({
    if (!input$p_bayes_nv) return(NULL)


    ggplot(NULL, aes(x = bayesWerteNV())) +
      geom_histogram(aes(y = after_stat(density)), fill = colours["est_bayes_nv"],
                     #bins = num_classesSKV(),
                     binwidth = binweite(),
                     alpha = .5) +

      prior_nv_layer() +

      # every sample as triangle
      geom_point(aes(x = bayesWerteNV(), y = 0), colour = colours["est_bayes_nv"], shape = 17, size = 4) +

      # frame selected sample
      geom_point(aes(x = bayesWerteNV()[specific()], y = 0), colour = "magenta", fill = colours["est_bayes_nv"], shape = 24, size = 8) +

      # mu
      geom_point(aes(x = mu(), y = 0), colour = colours["mu"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mu()), colour = colours["mu"], linewidth = 1) +

      # mean over all samples
      geom_point(aes(x = mean_estBayesNV(), y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mean_estBayesNV()), colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +



      # Skalen, Theme, Labs etc.
      coord_cartesian(xlim = coord())  +

      # # 2. y-Achse
      # scale_y_continuous(
      #   sec.axis = sec_axis( trans=~.*number())
      # ) +

      labs(
        title = "Bayesschätzer mit normalverteilter Priori",
        x = expression(bar(x)),
        y = NULL,
        colour = NULL) +
      theme_bw()

  })


}
