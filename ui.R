# defininig UI
ui <- fluidPage(

  # Application title
  titlePanel("Überschrift"),

  # define sidebars

  sidebarLayout(
    sidebarPanel(

      #### Organiszing Plots ####
      # specific sample
     # checkboxInput("p_samp", "Zeige die spezifische Stichprobe", value = T),
      # forest plot
    #  checkboxInput("p_forest", "Zeige den Forestplot", value = F),
      # SKV Mean
   #   checkboxInput("p_mean", "Zeige die SKV des means", value = F),
      # SKV Minmax
  #    checkboxInput("p_minmax", "Zeige die SKV des alternativen Schätzers", value = F),
      # SKV Bayes uni
 #     checkboxInput("p_bayes_uni", "Zeige die SKV mit gleichverteiltem Bayes Schätzer", value = F),
      # SKV Bayes nv
#      checkboxInput("p_bayes_nv", "Zeige die SKV mit normalverteiltem Bayes Schätzer", value = F),

      # action Button
#      actionButton("go", "Go"),



      #### Population ####
      # Population mu
      sliderInput(inputId = "mu",
                  paste0("Populations-", expression(mu)),
                  min = -100,
                  max = 100,
                  value = 0),

      # Population sd
      sliderInput(inputId = "sd",
                  "Populations-Standardabweichung",
                  min = 0,
                  max = 100,
                  value = 1),


      #### Samples ####
      # Samplesize
      sliderInput(inputId = "n",
                  "Größe der einzelnen Stichproben",
                  min = 0,
                  max = 1000,
                  value = 100),

      # Number of Samples
      sliderInput(inputId = "number",
                  "Gesamtanzahl der Stichproben",
                  min = 0,
                  max = 1000,
                  value = 100),

      # Specific Sample
      sliderInput(inputId = "specific",
                  "Spezifische Stichprobe",
                  min = 0,
                  max = 100000,
                  value = 10),



      #### Priors ####
      # Gleichverteiler Prior
      # Minimum
      sliderInput(inputId = "min_uni_priori",
                  "Minimum der Gleichverteilten Priori",
                  min = -100,
                  max = 100,
                  value = -50),

      # Maximum
      sliderInput(inputId = "max_uni_priori",
                  "Maximum der Gleichverteilten Priori",
                  min = -100,
                  max = 100,
                  value = -10),


      # Normalverteilter Prior
      # Prior Mean
      sliderInput(inputId = "mu_prior",
                  label = "Prior Mean",
                  min = -100,
                  max = 100,
                  value = 50),

      # Sidebar with a slider input for Prior Tau
      sliderInput(inputId = "tau_prior",
                  label = "Prior Tau",
                  min = 1,
                  max = 100,
                  value = 10)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("skv_plot", width = "950px", height = "700px")
    )
  )
)
