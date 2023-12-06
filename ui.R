# defininig UI
ui <- fluidPage(

  # Application title
  titlePanel("Überschrift"),

  # define sidebars

  sidebarLayout(
    sidebarPanel(

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
                  value = 1000),

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
                  min = 0,
                  max = 100,
                  value = 10)

    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("skv_plot")
    )
  )
)
