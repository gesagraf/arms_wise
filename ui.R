# defininig UI
#install.packages("shinythemes")
# library(shinydashboard)

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
              tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #FF4500}")),
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
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red}")),
    sliderInput(inputId = "mu",
                paste0("Populations-Mittelwert"),
                min = -100,
                max = 100,
                value = 0),

    # Population sd
    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: green}")),
    sliderInput(inputId = "std",
                "Populations-Standardabweichung",
                min = 1, #0=error
                max = 50,
                value = 15),


    #### Samples ####
    # Samplesize
    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: green}")),
    sliderInput(inputId = "n",
                "Größe der einzelnen Stichproben",
                min = 2, # unter 2 error
                max = 200,
                value = 20),

    # Number of Samples
    tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: yellow}")),
    sliderInput(inputId = "number",
                "Gesamtanzahl der Stichproben",
                min = 1,#
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
    tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: green}")),
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
    tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: green}")),
    sliderInput(inputId = "mu_prior",
                label = "Prior Mean",
                min = -100,
                max = 100,
                value = 50),

    # Sidebar with a slider input for Prior Tau
    tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: green}")),
    sliderInput(inputId = "tau_prior",
                label = "Prior Tau",
                min = 1,
                max = 100,
                value = 10)

  )),
  tabPanel(title = "Aufgaben",
           strong("Aufgabe 1"),
           p("Finde eine Einstellung, für die im Plot der einzelnen Stichprobe die Verteilung (grau)"),
p("a: nur einen einzelnen Balken hat"),
p("b: den gesamten Plot abdeckt (-100 bis 100)"),
           strong("Aufgabe 2"),
           br(em("Aktiviere für diese Aufgabe die Plots der SKV des arithmetischen Mittels und des alternativen Schätzers")),
           p("Stelle die Stichprobengröße so ein, das die Plots der SKV vom arithmetischen Mittel und
             dem alternativen Schätzer identisch sind. (zur Erinnerung: der alternative Schätzer
             ist der Mittelwert des Minimums und Maximums)"),
           strong("Aufgabe 3"),
           br(em("Aktiviere für diese Aufgabe die Plots der SKV des arithmetischen Mittels und des gleichverteilten Bayes-Schätzers")),
           p("Finde eine Einstellung, für die die Plots der SKV des arithmetischen Mittels und
             des gleichverteilten Bayes-Schätzers identisch sind"),
           strong("Aufgabe 4"),
           br(em("Aktiviere für diese Aufgabe den Forest-Plot")),
           p("Vegleiche die verschiedenen Schätzer im Forest-Plot. Welches ist der beste Schätzer?"),
           p("Kannst du die Regler so verändern, das einer der Schätzer deutlich besser ist als die anderen?"),
           strong("Aufgabe 5"),
           p("Für Aufgabe 4 gibt es 2 unterschiedliche Lösungen, findest du beide? Welche der beiden
             Lösungen könntest du unter Umständen auch in der Forschung anwenden, und welche solltest du eher vermeiden?")),
      )),
  # Show a plot of the generated distribution
  mainPanel(
#    box(width = 12, height = 10, plotOutput("legendontop")),
        tabPanel("some title",

                fluidRow(
                          column(6, shinycssloaders::withSpinner(
                            plotOutput("plot_samp"), type = getOption("spinner.type", 4))),
                          column(6, shinycssloaders::withSpinner(
                                 plotOutput("forest"), type = getOption("spinner.type", 4)))
                          ),
                 strong("Überschrift"),
                 p("Die folgenden 4 Plots zeigen die Stichprobenkennwerteverteilungen der verschiedenen Schätzer.
Jedes Dreieck symbolisiert eine der XX generierten Stichproben. Das umrandete Dreieck zeigt die spezifische Stichprobe (Plot oben links)."),
                fluidRow(
                         column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_mean"), type = getOption("spinner.type", 4))),
                         column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_minmax"), type = getOption("spinner.type", 4)))

                ),
                fluidRow(
                         column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_bayes_uni"), type = getOption("spinner.type", 4))),
                         column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_bayes_nv"), type = getOption("spinner.type", 4)))

        )
      )
  ),

tags$head(tags$style(
  type="text/css",
  "#legendontop img {max-width: 100%; width: 100%; height: auto}"
)),

absolutePanel(
  top = 00, right = 00, width = 850,
  fixed = TRUE,
    fluidRow(column(12, height = 10, uiOutput("legendontop")))
  )


)









