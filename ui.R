# defininig UI
#install.packages("shinythemes")

ui <- fluidPage(
  #shinythemes::themeSelector(),

  # zum werte anschauen
  #tabPanel(title = "Penguin's life",
  #         textOutput("penguin_text")
  #),
  # define sidebars

#fluidRow(
#     # #### Organiszing Plots ####
# # # specific sample
    #column(2, checkboxInput("p_samp", "konkrete Stichprobe", value = T)),
    # forest plot
    #column(2,     checkboxInput("p_forest", "Forestplot", value = F)),
    # SKV Mean
    #column(2,     checkboxInput("p_mean", "SKV Arithmetisches Mittel", value = F)),
    # SKV Minmax
    #column(2,   checkboxInput("p_minmax", "SKV Alternativer Schätzer", value = F)),
    # SKV Bayes uni
    #column(2,   checkboxInput("p_bayes_uni", "SKV gleichverteilter Bayes Schätzer", value = F)),
    # SKV Bayes nv
    #column(2,  checkboxInput("p_bayes_nv", "SKV normalverteilter Bayes Schätzer", value = F)),
#),
    # # action Button
    #      actionButton("go", "Go"),

  sidebarPanel(width = 4,
               # selectInput(
               #   "anzeigen", "Welche Plots möchtest du gerne angezeigt bekommen?", possibilities,
               # multiple = TRUE
               # ),
               tabsetPanel(
                 tabPanel(title = "Simulationen",
    wellPanel(title = "hier kannst du dir eine bestimmte Stichprobe anzeigen lassen (plot 1 oben links)",
              strong("Überschrift"),
              p("Erlärungstext"),
                 # Specific Sample
    sliderInput(inputId = "specific",
                  "Spezifische Stichprobe",
                  min = 0,
                  max = 1000,
                  value = 3),
    checkboxInput("p_samp", "Plot der spezifischen Stichprobe", value = T)),

    wellPanel(title = "hier kannst du dir die Stichprobenkennwerte-Verteilungen
für die verschiedenen Schätzer in einem Forest-Plot anzeigen lassen (oben rechts)",
              strong("Überschrift"),
              p("Erlärungstext"),

    checkboxInput("p_forest", "Forestplot mit allen Schätzern", value = F)),
    wellPanel(title = "hier kannst du die Parameter für die Population einstellen)",
              strong("Überschrift"),
              p("Erlärungstext"),

    #### Population ####
    # Population mu
    sliderInput(inputId = "mu",
                paste0("Populations-Mittelwert"),
                min = -100,
                max = 100,
                value = 0),

    # Population sd
    sliderInput(inputId = "std",
                "Populations-Standardabweichung",
                min = 0,
                max = 50,
                value = 15),


    #### Samples ####
    # Samplesize
    sliderInput(inputId = "n",
                "Größe der einzelnen Stichproben",
                min = 0,
                max = 1000,
                value = 20),

    # Number of Samples
    sliderInput(inputId = "number",
                "Gesamtanzahl der Stichproben",
                min = 0,
                max = 1000,
                value = 100),
    checkboxInput("p_mean", "SKV Arithmetisches Mittel", value = F),
    checkboxInput("p_minmax", "SKV Alternativer Schätzer", value = F)),

    wellPanel(title = "Hier kannst du die Priori des gleichverteilten Bayes-Schätzers einstllen.
Der gleichverteilte Schätzer nimmt an, das alle Werte innerhalb der eingestellten Range gleich
wahrscheinlich sind, aber Werte außerhalb der Range unmöglich sind.",
              strong("Überschrift"),
              p("Erlärungstext"),
      checkboxInput("p_bayes_uni", "SKV gleichverteilter Bayes Schätzer", value = F),

    #### Priors ####
    # Gleichverteiler Prior
    sliderInput(inputId = "rangePriori",
                "Range des gleichverteilten Bayes Priors",
                min = -100,
                max = 100,
                value = c(-50,5))),

    wellPanel(title = "hier kannst du Mittelwert und Standardabweichung der normalverteilten Priori
einstellen",
              strong("Überschrift"),
              p("Erlärungstext"),

      checkboxInput("p_bayes_nv", "SKV normalverteilter Bayes Schätzer", value = F),
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

  )),
  tabPanel(title = "Einstellungen",
           checkboxInput("scale_true", "Skala an die Werte Anpassen", value = F)
  ))),
  # Show a plot of the generated distribution
  mainPanel(
        tabPanel("some title",
                 fluidRow(
                          column(6, plotOutput("plot_samp")),
                          column(4, plotOutput("forest")),
                          column(2, plotOutput("legende"))
                 ),
                fluidRow(
                         column(6, plotOutput("plot_mean")),
                         column(6, plotOutput("plot_minmax"))

                ),
                fluidRow(
                         column(6, plotOutput("plot_bayes_uni")),
                         column(6, plotOutput("plot_bayes_nv"))

        )
      )
  )
)









