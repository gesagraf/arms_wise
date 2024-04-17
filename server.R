server <- function(input, output, session) {

  # browser() #debug
  # setup
  library(viridis)
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(cowplot)
  library(latex2exp)

  set.seed(12345)

  #functions
  which.max_custom<-function(...){
    x<-which.max(...)
    if(length(x) == 0) return(NA) else return(x)
  }


  ## constants

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
  ##



  #slider updaten
  observeEvent(input$number, {
    updateSliderInput(inputId = "specific", max = input$number)
  })

  #unecessary ??
  # # get input from ui
  # mu <- reactive(input$mu)                         # Population mean
  # std <- reactive(input$std)                       # Population sd
  # n  <- reactive(input$n)                          # sample size
  # number <- reactive(input$number)                 # number of samples
  # specific <- reactive(input$specific)             # specific sample
  # min_uni_priori <- reactive(input$rangePriori[1]) # minimum für gleichverteilte priori
  # max_uni_priori <- reactive(input$rangePriori[2]) # max für gleichverteilte priori
  # mu_prior <-  reactive(input$mu_prior)            # mittelwert der priori
  # tau_prior <- reactive(input$tau_prior)           # sd der priori

  plot_l<-reactiveValues()

  specific<-reactive(input$specific)
  norm_likelihood<-reactiveVal()
  estimators<-reactiveVal()
  minmax<-reactiveVal()
  bayesWerte<-reactiveVal()
  bayesWerteNV<-reactiveVal()
  resultsunispecific<-reactiveVal()
  # prior_dens<-reactiveVal()
  mu_hat<-reactiveVal()
  # p_sample_basis<-reactiveVal()
  number<-reactiveVal()
  lengthout<-reactiveVal()
  # prior_densNV<-reactive()
  # mean_estBayes<-reactiveVal()
  # mean_estBayesNV<-reactiveVal()
  coord<-reactiveVal()
  binweite<-reactiveVal()
  binweiteNV<-reactiveVal()
  done_computing<-reactiveVal()
  mu_layer<-reactiveVal()
  samp_df<-reactiveVal()
  mu_forest<-reactiveVal()
  num_classes<-reactiveVal()
  nas_NV<-reactiveVal()
  nas_uni<-reactiveVal()
  mu<-reactiveVal()



  prior_dens<-reactive({
    req(lengthout(), mu_hat())
    # density of prior
    dunif(mu_hat(), input$rangePriori[1], input$rangePriori[2])


  })
  prior_densNV<-reactive({
    req(lengthout, mu_hat())
    dnorm(mu_hat(), mean = input$mu_prior , sd = input$tau_prior)
  })




  observeEvent(input$compute, {
    # browser() #debug

    # get input from ui
    mu <- input$mu                        # Population mean
    std <- input$std                      # Population sd
    n  <- input$n                       # sample size
    number(input$number)                 # number of samples
    # specific()             # specific sample

    # mu_prior <-  input$mu_prior            # mittelwert der priori
    # tau_prior <- input$tau_prior          # sd der priori



    #### create data ####
    # draws samples of "number" iterations

    samp_df(
      # reactive(
      sapply(1:number(), FUN = function(i) {
        samp <- rnorm(n = n, mean = mu, sd = std)
      }
      )
    )
    samp_df<-samp_df()

    #### mean ####
    #colum wise
    estimators(apply(samp_df, MARGIN = 2, mean))


    # over all mean
    mean_est <- mean(estimators()) #prev reactive

    #### min/max ####
    # mean colum wise
    max_min_mean <-function(vec) {(max(vec) + min(vec)) / 2}

    minmax(apply(samp_df, MARGIN = 2, max_min_mean))

    # over all mean
    mean_minmax <- mean(minmax()) #prev reactive

    #### Bayes mit gleichverteilter Priori mit min max #####
    #take xlim coordinates of plots to get boundaries of mu_hat
    min_coord <- mu - 3 * std #prev reactive
    # # min(c(mu - 2 * sd, estimators, minmax, bayesWerte, bayesWerteNV, min_uni_priori))
    max_coord <- mu + 3 * std #prev reactive

    #prior reatio was 1000 points between -200 and 200
    #1000/400 = 2.5
    # use same ratio
    # max 50*2.5*6
    # browser() #debug


    lengthout(2.5*abs(min_coord-max_coord))

    # calculate likelihood
    mu_hat(seq(min_coord, max_coord, length.out = lengthout()))

    ## compute likelihood

    norm_likelihood(
      # reactive({
      sapply(1:number(), function(i){
        # isolate({
        # samp_df<-samp_df()
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
      )

    # })

    # return_list_uni2 <-
    #   # reactive({
    #
    #   results <- matrix(ncol = number(), nrow = lengthout())
    #
    #   # isolate({
    #   # norm_likelihood_uni <- norm_likelihood() # likelihood_function_norm aus vorheriger for schleife dazu holen
    #
    #   for (i in 1:number()) {
    #
    #     # calculate posteriori
    #     posterior0 <- prior_dens() * norm_likelihood()[ , i]
    #     #
    #     #   # Posteriori normieren
    #     den_post <- Bolstad::sintegral(mu_hat(), posterior0)
    #     posterior <- posterior0 / den_post$value
    #
    #     # !!! Hier brauchen wir dringend eine Kontrollfunktion: wenn NaN, dann Fehlermeldung: "Die Daten sind bei deiner Gewählten gleichverteilten Priori (super starke annahme!!) unmöglich!"
    #     results[ , i] <- posterior
    #   }
    #   # return(results)
    #   # })
    #
    # # })
    # # Index des Maximums in jeder Spalte finden
    # index_maximum <- apply(return_list_uni2, MARGIN = 2, which.max) #prev reactive
    # # Wert von 'x' für das Maximum von 'y' finden
    # bayesWerte(mu_hat()[index_maximum])
    #
    # # over all mean Bayes
    # mean_estBayes <- mean(bayesWerte) #prev reactive
    #

    #### Bayes mit nv Priori #####
    # Prior



    # return_list_nv2 <-
    #   # reactive({
    #
    #   resultsNV = matrix(ncol = number(), nrow = lengthout())
    #   # isolate({
    #   # norm_likelihood_nv <- norm_likelihood # likelihood_function_norm aus vorheriger for schleife dazu holen
    #
    #   for (i in 1:number()) {
    #
    #
    #     # calculate posteriori
    #     posterior0 <- prior_densNV() * norm_likelihood()[ , i]
    #
    #     # Posteriori normieren # hier nochmal
    #     den_post <- Bolstad::sintegral(mu_hat(), posterior0)
    #     posterior <- posterior0 / den_post$value
    #
    #     resultsNV[ , i] <- posterior
    #   }
    #
    #   # return(resultsNV)
    #   # })
    #
    # # })
    #
    #   # Index des Maximums in jeder Spalte finden
    #   index_maximumNV <- apply(return_list_nv2, MARGIN = 2, which.max) #prev reactive
    #   # Wert von 'x' für das Maximum von 'y' finden
    #   bayesWerteNV(mu_hat()[index_maximumNV])
    #
    #   # over all mean Bayes
    #   mean_estBayesNV <- mean(bayesWerteNV) #prev reactive

      #### Plot ####
      #### definitions ####

      coord(c(min_coord, max_coord)) #prev reactive





 # Anzahl der Klassen nach Sturges' Regel (für Histogram der einzelnen Stichprobe)
  num_classes(ceiling(log2(n) + 1))
  num_classes<-num_classes()
  # Anzahl der Klassen nach Sturges' Regel (für Histogram der SKV)
  num_classesSKV <- ceiling(log2(number()) + 1) #prev reactive


  #### plots ####
  # x_layer <- reactive({scale_x_continuous(breaks = seq(min_coord(), max_coord(), std()), limits = coord())})
  # browser() #debug
  # single sample
  # Single Sample Plot erstellen, aber NICHT ausgeben; für die Legende

  # })


    plot_l$plot_mean <-
      # renderPlot({
      # if (!input$p_mean) return(NULL)

      # Mean
      ggplot(NULL, aes(x = estimators())) +
        geom_histogram(aes(y = after_stat(density)), fill = colours["est_mean"],
                       bins = num_classesSKV,
                       #binwidth = binweite(),
                       alpha = .5) +
        # every sample as triangle
        geom_point(aes(x = estimators(), y = 0), colour = colours["est_mean"], shape = 17, size = 4) +
        # mu
        geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mu), colour = colours["mu"], linewidth = 1) +

        # mean over all samples
        geom_point(aes(x = mean_est, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_est), colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +



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
    # })
    # browser() #debug
    ## Definitionen für die Annotation der Formel
    kategorienbreite <- (max(minmax()) - min(minmax())) / num_classesSKV #prev reactive
    # y Wert für die Annotation
    anno_y <- max(density(minmax(), bw = kategorienbreite)$y) #prev reactive

    #### Plot Minmax ####
    # browser() #debug
    plot_l$plot_minmax <-
      # renderPlot({
      # if (!input$p_minmax) return(NULL)

      # Alternativer Schätzer
      ggplot(NULL, aes(x = minmax())) +

        geom_histogram(aes(y = after_stat(density)), fill = colours["est_minmax"],
                       bins = num_classesSKV, alpha = .5) +

        annotate("label",
                 label = TeX(r"( $\frac{\max(x) + \min(x)}{2}$ )"),
                 x = mu - 2 * std, y = anno_y) +
        # every sample as triangle
        geom_point(aes(x = minmax(), y = 0), color = colours["est_minmax"], shape = 17, size = 4)  +

        # mu
        geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mu), colour = colours["mu"], linewidth = 1) +

        # mean over all samples
        geom_point(aes(x = mean_minmax, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_minmax), colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +



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
    # })

    # browser() #debug

    mu_layer(list(
      geom_point(aes(x = mu, y = 0), colour = colours["mu"], shape = 17, size = 4),
        geom_vline(aes(xintercept = mu), colour = colours["mu"], linewidth = 1))
    )
    # browser() #debug

    mu_forest(list(
      geom_vline(aes(xintercept = mu), colour = colours["mu"], linewidth = 1)
    ))

    mu(mu)




    done_computing(TRUE)

  }) #end of observeEvent

  observe({
    req(done_computing())
    ## forest needs to get into observer or directly renderPlot
    # create data for the forest plot
    # browser() #debug



    #
    dat <- data.frame( #prev reactive
      Index = 1:4, # This provides an order to the data
      label = c("Arithmetisches Mittel", "Alternativer Schätzer", NA, NA),
      OR = c(mean(estimators()), mean(minmax()),
             NA, NA), # Odds Ration
      # Lower Border
      LL = c(
        mean(estimators()) - 1.96 * sd(estimators()), mean(minmax()) - 1.96 * sd(minmax()),
        NA, NA),
      # Upper border
      UL = c(
        mean(estimators()) + 1.96 * sd(estimators()), mean(minmax()) + 1.96 * sd(minmax()), NA, NA),
      frb = colours[c("est_mean", "est_minmax", NA, NA)])
    # browser() #debug

    p_forest<-
      # renderPlot({
      # if (input$p_forest) {
      ggplot(dat, aes(y = Index, x = OR, xmin = LL, xmax = UL)) +
      geom_errorbarh(height = 0.25, linewidth = 1, colour = dat$frb) +
      geom_point(shape = 22, colour = "black", fill = "white") +
      mu_forest() +

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
    # }


    if(all(is.na(bayesWerte()))){
      p_forest<-p_forest + geom_text(aes(y = 3, x = mu(), label = paste0(
        "All posteriori values are NA \nchange prior settings"
      )))

    } else{
      req(bayesWerte())
      bayes_mean<-mean(bayesWerte(), na.rm = TRUE)
      bayes_sd<-sd(bayesWerte(), na.rm = TRUE)
      bay_dat<-list(Index = 3,
                          label ="Bayesschätzer mit gleichverteilter Priori",
                          OR = bayes_mean,
                          LL = bayes_mean - 1.96 * bayes_sd,
                          UL = bayes_mean + 1.96 * bayes_sd,
                          frb = colours["est_bayes_uni"])
      dat[bay_dat$Index, ]<-bay_dat
      p_forest<-ggplot(dat, aes(y = Index, x = OR, xmin = LL, xmax = UL)) +
        geom_errorbarh(height = 0.25, linewidth = 1, colour = dat$frb) +
        geom_point(shape = 22, colour = "black", fill = "white") +
        mu_forest() +

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
    }


    if(all(is.na(bayesWerteNV()))){
      p_forest<-p_forest + geom_text(aes(y = 4, x = mu(), label = paste0(
        "All posteriori values are NA \nchange prior settings"
      )))

    } else{
      req(bayesWerteNV())
      bayes_meanNV<-mean(bayesWerteNV(), na.rm = TRUE)
      bayes_sdNV<-sd(bayesWerteNV(), na.rm = TRUE)
      bayNV_dat<-list(Index = 4,
                          label = "Bayesschätzer mit normalverteilter Priori",
                          OR = bayes_meanNV,
                          LL = bayes_meanNV - 1.96 * bayes_sdNV,
                          UL = bayes_meanNV + 1.96 * bayes_sdNV,
                          frb = colours["est_bayes_nv"])
      dat[bayNV_dat$Index, ]<-bayNV_dat
      p_forest<-ggplot(dat, aes(y = Index, x = OR, xmin = LL, xmax = UL)) +
        geom_errorbarh(height = 0.25, linewidth = 1, colour = dat$frb) +
        geom_point(shape = 22, colour = "black", fill = "white") +
        mu_forest() +

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
      #this needs to be double coded since %+% is not working here so we cant change the plot but redo it every time
      if(all(is.na(bayesWerte())))  p_forest<-p_forest + geom_text(aes(y = 3, x = mu(), label = paste0(
        "All posteriori values are NA \nchange prior settings"
      )))
    }
    plot_l$forest<-p_forest



  })



  p_sample_basis<-reactive({
    req(mu_layer())
    sampfdfspecific <- samp_df()[ , specific()]
    # browser() #debug
    bayeswertespecific <- bayesWerte()[specific()]
    resultsunispecific(norm_likelihood()[ , specific()])
    # reactive({
    ggplot(NULL, aes(x = sampfdfspecific)) +
      # Platzhalter
      geom_point(aes(x = bayeswertespecific, y = 0, colour = "mean_est"), shape = 24, size = 1, alpha = 0) +

      #        #
      #        # Verteilung
      geom_histogram(aes(y = after_stat(density)), fill = "lightgrey",
                     colour = "lightgrey", bins = (num_classes()*2), alpha = .99) +
      #

      #        # mu
      mu_layer() +

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
  ### Reaktiven Teil definieren
  ## Schätzer
  # Mean
  # browser() #debug
  mean_layer <-reactive({
    req(estimators(), specific()) #require this values to be not empty

    if (input$p_mean == TRUE){
      list(geom_point(aes(x = estimators()[specific()], y = 0),
                      colour = "magenta", shape = 24,
                      fill = colours["est_mean"], size = 8),
           geom_vline(aes(xintercept = estimators()[specific()], colour = "est_mean"),
                      linetype = "dotted", linewidth = .75))
    } else NULL
  })

  # Minmax
  minmax_layer <-
    reactive({
    req(minmax(), specific()) #require this values to be not empty

      if (input$p_minmax == TRUE){
      list(geom_point(aes(x = minmax()[specific()], y = 0),
                      colour = "magenta", shape = 24, fill = colours["est_minmax"], size = 8),
           geom_vline(aes(xintercept = minmax()[specific()], colour = "est_minmax"), linetype = "dotted", linewidth = .75))

    } else NULL
  })

  # Bayes uni
  bayes_uni_layer <-
    reactive({
      req(specific()) #require this values to be not empty
      # browser() #debug
      if(all(is.na(bayesWerte()))) return(NULL)
      if (input$p_bayes_uni == TRUE){
      list(geom_point(aes(x = bayesWerte()[specific()], y = 0),
                      colour = "magenta", shape = 24, fill = colours["est_bayes_uni"], size = 8),
           geom_vline(aes(xintercept = bayesWerte()[specific()], colour = "est_bayes_uni"), linetype = "dotted", linewidth = .75))
    } else NULL
  })

  # bayes nv
  bayes_nv_layer <-reactive({
    req(specific()) #require this values to be not empty
    if(all(is.na(bayesWerte()))) return(NULL)
    if (input$p_bayes_nv == TRUE){
      list(geom_point(aes(x = bayesWerteNV()[specific()], y = 0),
                      colour = "magenta", shape = 24, size = 8, fill = colours["est_bayes_nv"], ),
           geom_vline(aes(xintercept = bayesWerteNV()[specific()], colour = "est_bayes_nv"), linetype = "dotted", linewidth = .75))
    } else NULL
  })

  # Bayes Spezifika
  likelihood_layer <-reactive({
    req(mu_hat(), resultsunispecific()) #require this values to be not empty

    if (input$show_likelihood == TRUE){
      list(     geom_line(aes(x = mu_hat(), y = resultsunispecific(), color = "likelihood")),
                geom_area(aes(x = mu_hat(), y = resultsunispecific()), alpha = .4, fill = colours["likelihood"]))
    } else NULL
  })

  # prior uni
  prior_uni_layer <- reactive({
    req(mu_hat(), prior_dens()) #require this values to be not empty

    if (input$show_prior_uni == TRUE){
      list(
        geom_line(aes(x = mu_hat(), y = prior_dens()), color = colours["prior_uni"]),
        geom_area(aes(x = mu_hat(), y = prior_dens()), fill = colours["prior_uni"], alpha = .4)
      )
    } else NULL
  })

  # prior nv
  prior_nv_layer <-reactive({
    req(mu_hat(), prior_densNV()) #require this values to be not empty

    if (input$show_prior_nv == TRUE){
      list(
        geom_line(aes(x = mu_hat(), y = prior_densNV()), color = colours["prior_nv"]),
        geom_area(aes(x = mu_hat(), y = prior_densNV()),fill = colours["prior_nv"], alpha = .4)
      )
    } else NULL
  })



  # Pinke Umrandung
  pink_border <-
    reactive({
    if(any(c(input$p_mean, input$p_minmax, input$p_bayes_uni, input$p_bayes_nv))) {
      annotation_custom(
        grob = rectGrob(gp = gpar(col = "magenta", lwd = 5, fill = NA)),
        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
      )
    } else NULL
  })

  observe({
    req(p_sample_basis())
    # browser() #debug
    # reactive({
    # if (input$p_samp) {
    plot_l$plot_samp <-p_sample_basis() +

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



  ##### update bayes plots if prior changes
  ## uni
  observe({
    # req(lengthout())
    req(number(), lengthout(), mu_hat(), coord())
    # browser() #debug
    # if (!input$p_bayes_uni) return(NULL)

    results <- matrix(ncol = number(), nrow = lengthout())

    # isolate({
    # norm_likelihood_uni <- norm_likelihood() # likelihood_function_norm aus vorheriger for schleife dazu holen

    for (i in 1:number()) {

      # calculate posteriori
      posterior0 <- prior_dens() * norm_likelihood()[ , i]
      #
      #   # Posteriori normieren
      den_post <- Bolstad::sintegral(mu_hat(), posterior0)
      posterior <- posterior0 / den_post$value

      # !!! Hier brauchen wir dringend eine Kontrollfunktion: wenn NaN, dann Fehlermeldung: "Die Daten sind bei deiner Gewählten gleichverteilten Priori (super starke annahme!!) unmöglich!"
      results[ , i] <- posterior
    }



    # })
    # Index des Maximums in jeder Spalte finden
    index_maximum <- apply(results, MARGIN = 2, which.max_custom) #prev reactive
    # Wert von 'x' für das Maximum von 'y' finden
    bayesWerte(mu_hat()[index_maximum])
    # browser() #debug
    nas_uni(length(which(is.na(bayesWerte()))))

    binweite<-((max(bayesWerte()) - min(bayesWerte())) / 4)
    if(is.na(binweite)) binweite(NULL) else if(binweite>0)binweite(binweite)else if(binweite == 0) binweite(1) else binweite(NULL)





  # print("Hallo")

  })

  ##norm
  observe({
    req(number(), lengthout(), mu_hat(), coord())
    # if (input$p_bayes_nv == TRUE) return(NULL)



        resultsNV = matrix(ncol = number(), nrow = lengthout())
    # isolate({
    # norm_likelihood_nv <- norm_likelihood # likelihood_function_norm aus vorheriger for schleife dazu holen

    for (i in 1:number()) {


      # calculate posteriori
      posterior0 <- prior_densNV() * norm_likelihood()[ , i]

      # Posteriori normieren # hier nochmal
      den_post <- Bolstad::sintegral(mu_hat(), posterior0)
      posterior <- posterior0 / den_post$value

      resultsNV[ , i] <- posterior
    }



    # })
        # browser() #debug

    # Index des Maximums in jeder Spalte finden
    index_maximumNV <- apply(resultsNV, MARGIN = 2, which.max_custom) #prev reactive
    # Wert von 'x' für das Maximum von 'y' finden
    bayesWerteNV(mu_hat()[index_maximumNV])
    # browser() #debug
    nas_NV(length(which(is.na(bayesWerteNV()))))

    binweiteNV<-((max(bayesWerteNV()) - min(bayesWerteNV())) / 4)
    if(is.na(binweiteNV)) binweiteNV(NULL) else if(binweiteNV>0) binweiteNV(binweiteNV)else if(binweiteNV == 0) binweiteNV(1) else binweiteNV(NULL)



  })
 ################### Output ##########################
  output$forest<- renderPlot({
    plot_l$forest
  })

  output$plot_samp <- renderPlot({
    # req(bayesWerte(), specific(), plot_l$plot_samp)
    req(done_computing())
    # browser() #debug
    plot_l$plot_samp
  })

  output$plot_mean <- renderPlot({
    # req(estimators(), specific(), plot_l$plot_mean)
    req(done_computing())
    # browser() #debug
    plot_l$plot_mean +
      # frame selected sample
      geom_point(aes(x = estimators()[specific()], y = 0), colour = "magenta", fill = colours["est_mean"], shape = 24, size = 8)

  })
  #
  output$plot_minmax <- renderPlot({
    # req(minmax(), specific(), plot_l$plot_minmax)
    req(done_computing())
    plot_l$plot_minmax +
      # frame selected sample
      geom_point(aes(x = minmax()[specific()], y = 0), colour = "magenta", fill = colours["est_minmax"], shape = 24, size = 8)
  })
  #
  output$plot_bayes_uni <- renderPlot({
    # req(bayesWerte(), specific(), plot_l$plot_bayes_uni)
    req(done_computing())
    # browser() #debug
    # plot_l$plot_bayes_uni


    # over all mean Bayes
    mean_estBayes <- mean(bayesWerte(), na.rm = TRUE) #prev reactive
    # browser()

    if(all(is.na(bayesWerte()))){
      plot_l$plot_bayes_uni<-ggplot(NULL) + geom_text(aes(y = 0, x =0, label = paste0(
        "All posterior values are NA \n please choose a different prior"
      ))) + ggtitle("Bayesschätzer mit gleichverteilter Priori") + theme_minimal()
    } else{

    # browser() #debug
    plot_l$plot_bayes_uni <- ggplot(NULL, aes(x = bayesWerte())) +
      geom_histogram(aes(y = after_stat(density)), fill = colours["est_bayes_uni"],
                     # bins = num_classesSKV(),
                     binwidth = binweite(),
                     alpha = .5) +

      # every sample as triangle
      geom_point(aes(x = bayesWerte(), y = 0), color = colours["est_bayes_uni"], shape = 17, size = 4) +
      # frame selected sample
      geom_point(aes(x = bayesWerte()[specific()], y = 0), colour = "magenta", fill = colours["est_bayes_uni"], shape = 24, size = 8) +
      #mu
      mu_layer() +

      # mean over all samples
      geom_point(aes(x = mean_estBayes, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
      geom_vline(aes(xintercept = mean_estBayes),
                 colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +

      # Skalen, Theme, Labs etc.
      coord_cartesian(xlim = coord())  +

      labs(
        title = "Bayesschätzer mit gleichverteilter Priori",
        x = expression(bar(x)),
        y = NULL,
        colour = NULL) +

      theme_bw()
    # })












      if (input$show_prior_uni == TRUE) {
        isolate({
        plot_l$plot_bayes_uni<-plot_l$plot_bayes_uni + prior_uni_layer()
        })
      }
    }


    # })


    return(plot_l$plot_bayes_uni)
  })
  #
  output$plot_bayes_nv <- renderPlot({
    # req(bayesWerteNV(), specific(), plot_l$plot_bayes_nv)
    req(done_computing())
    # browser() #debug

    # over all mean Bayes
    mean_estBayesNV <- mean(bayesWerteNV(), na.rm = TRUE) #prev reactive

    if(all(is.na(bayesWerteNV()))){
      plot_l$plot_bayes_nv<-ggplot(NULL) + geom_text(aes(y = 0, x =0, label = paste0(
        "All posterior values are NA \n please choose a different prior"
      ))) + ggtitle("Bayesschätzer mit normalverteilter Priori") + theme_minimal()
    } else{
      plot_l$plot_bayes_nv <-
        # renderPlot({
        # if (!input$p_bayes_nv) return(NULL)


        ggplot(NULL, aes(x = bayesWerteNV())) +
        geom_histogram(aes(y = after_stat(density)), fill = colours["est_bayes_nv"],
                       #bins = num_classesSKV(),
                       binwidth = binweite(),
                       alpha = .5) +



        # every sample as triangle
        geom_point(aes(x = bayesWerteNV(), y = 0), colour = colours["est_bayes_nv"], shape = 17, size = 4)   +

        #sample specific
        geom_point(aes(x = bayesWerteNV()[specific()], y = 0), colour = "magenta", fill = colours["est_bayes_nv"], shape = 24, size = 8) +

        mu_layer() +

        # mean over all samples
        geom_point(aes(x = mean_estBayesNV, y = 0),  colour = colours["mean_est"], shape = 17, size = 4) +
        geom_vline(aes(xintercept = mean_estBayesNV), colour = colours["mean_est"], linetype = "dashed", linewidth = .75) +


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


      if (input$show_prior_nv == TRUE){
        isolate({
          plot_l$plot_bayes_nv <- plot_l$plot_bayes_nv +  isolate(prior_nv_layer())
        })
      }

    }



    #
    # # browser() #debug
    return(plot_l$plot_bayes_nv)
    })
  #
  #
  output$nas_uni<-renderText({
    req(nas_uni());
    paste0(nas_uni(),"/", number(), " posterior values are NAs")})
  output$nas_NV<-renderText({req(nas_NV()); paste0(nas_NV(),"/", number(), " posterior values are NAs")})

  #### Legende ####
  output$legendontop <- renderUI({
    img(src='legende.png')
  })

}


