ui <- fluidPage(

  # Hier wird die Sidebar (in grau) definiert
  sidebarPanel(width = 4,

               # Weil es verschiedene Tabs geben soll
               tabsetPanel(

                 # Tab 1: Für die Plots
                 tabPanel(title = "Simulation",
                          style = "overflow-y:scroll; max-height: 600px",

                          wellPanel(p("In diesem Tab kannst du grundsätzliche Einstellungen für die gesamte Simulation vornehmen.")),

                          # Kasten für die Populationsparameter
                          ## Überschriften & Text
                          wellPanel(title = "Einstellungen",
                                    strong("Pupulationsparameter einstellen"),
                                    # p("Erlärungstext"),

                                    # Slider Inputs
                                    ## Slider Populationsmittelwert
                                    ### Styleeinstellunge
                                    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red; border-top: red; border-bottom: red}")),
                                    ### Slider
                                    sliderInput(inputId = "mu",
                                                paste0("Populations-Mittelwert"),
                                                min = -100,
                                                max = 100,
                                                value = 0),

                                    ## Slider Populationsstandardabweichung
                                    ### Styleinstellungen
                                    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: red; border-top: red; border-bottom: red}")),
                                    ### Slider
                                    sliderInput(inputId = "std",
                                                "Populations-Standardabweichung",
                                                min = 1,
                                                max = 50,
                                                value = 15),

                                    ## Slider Stichprobengrößen
                                    ### Styleeinstellungen
                                    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #d3d3d3; border-top: #d3d3d3; border-bottom: #d3d3d3}")),
                                    ### Slider
                                    sliderInput(inputId = "n",
                                                "Größe der einzelnen Stichproben",
                                                min = 2,
                                                max = 200,
                                                value = 30),

                                    ## Slider Stichprobenanzahl
                                    ### Styleeinstellungen
                                    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #a9a9a9; border-top: #a9a9a9; border-bottom: #a9a9a9}")),
                                    ### Slider
                                    sliderInput(inputId = "number",
                                                "Gesamtanzahl der Stichproben",
                                                min = 1,
                                                max = 1000,
                                                value = 100))),

                 # Tab 1: Für die Plots
                 tabPanel(title = "Plots",
                          style = "overflow-y:scroll; max-height: 600px",

                          wellPanel(p("In diesem Tab kannst du dir die verschiedenen Plots anzeigen lassen.")),


                          # Kasten für die spezifische Stichprobe
                          ## Überschriften & Text
                          wellPanel(title = "hier kannst du dir eine bestimmte Stichprobe anzeigen lassen (Plot 1 oben links)",
                                    strong("Spezifische Stichprobe auswählen"),
                                    # p("Erlärungstext"),

                                    # Checkbox spezifische Stichprobe
                                    checkboxInput("p_samp", "Zeigen den Plot der spezifischen Stichprobe an", value = T),

                                    # Slider Input
                                    ## Styleeinstellungen für die Slider Inputs
                                    tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #d3d3d3; border-top: #d3d3d3; border-bottom: #d3d3d3}")),
                                    ## Slider Input
                                    sliderInput(inputId = "specific",
                                                "Wähle eine spezifische Stichprobe aus",
                                                min = 1,
                                                max = 1000,
                                                value = 3),


                                    # Checkbox Likelihood anzeigen
                                    checkboxInput("show_likelihood", "Zeigen die Likelihood der spezifischen Stichprobe an", value = F),




                                    ),

                          # Kasten für den Forestplot
                          ## Überschriften & Text
                          wellPanel(title = "hier kannst du dir die Stichprobenkennwerte-Verteilungen
                                    für die verschiedenen Schätzer in einem Forest-Plot anzeigen lassen (oben rechts)",
                                    strong("Forestplot"),
                                    # p("Erlärungstext"),

                                    # Checkbox Forestplot
                                    checkboxInput("p_forest", "Zeige den Forestplot mit allen Schätzern an", value = F)),


                          # Box für Mean und Minmax
                          wellPanel(title = "Hier kannst du dir die Stichprobenkennwerteverteilungen vom arithmetischen Mittel und dem alternativen Schätzer anzeigen lassen.",
                                    strong("Stichprobenkennwerteverteilungen"),
                                    p("Von Mittelwert und alternativen Schätzer"),


                                    checkboxInput("p_mean",
                                                  "Zeige die SKV des arithmetischen Mittels an", value = F),

    checkboxInput("p_minmax", "Zeige die SKV des alternativen Schätzers an", value = F)),

    wellPanel(title = "Hier kannst du die Priori des gleichverteilten Bayes-Schätzers einstllen.
Der gleichverteilte Schätzer nimmt an, das alle Werte innerhalb der eingestellten Range gleich
wahrscheinlich sind, aber Werte außerhalb der Range unmöglich sind.",
              strong("SKV von Bayesschätzern"),
              p("mit gleichverteilter Priori"),

      checkboxInput("p_bayes_uni", "Zeige die SKV dieses Bayesschätzers", value = F),

    #### Priors ####
    # Gleichverteiler Prior
    tags$style(HTML(".irs-from .irs-to .irs-from .js-irs-5 .irs-to .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #443A83FF; background-color: #443A83FF; border-top: #443A83FF; border-bottom: #443A83FF}")),

    sliderInput(inputId = "rangePriori",
                "Range des gleichverteilten Bayes Priors",
                min = -100,
                max = 100,
                value = c(-50,5)),

    checkboxInput("show_prior_uni", "Zeige den Prior des gleichverteilten Bayes Schätzers", value = F)



    ),








    wellPanel(title = "hier kannst du Mittelwert und Standardabweichung der normalverteilten Priori
einstellen",
              strong("SKV von Bayesschätzern"),
              p("mit normalverteiltem Prior"),

      checkboxInput("p_bayes_nv", "Zeige die SKV dieses Bayesschätzers", value = F),
    # Normalverteilter Prior
    # Prior Mean
    tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #440154FF; border-top: #440154FF; border-bottom: #440154FF}")),
    sliderInput(inputId = "mu_prior",
                label = "Prior Mean",
                min = -100,
                max = 100,
                value = 50),

    # Sidebar with a slider input for Prior Tau
    tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #440154FF; border-top: #440154FF; border-bottom: #440154FF}")),
    sliderInput(inputId = "tau_prior",
                label = "Prior Tau",
                min = 1,
                max = 100,
                value = 10),

    checkboxInput("show_prior_nv", "Zeige den Prior des normalverteilten Bayes Schätzers", value = F)


  )),
  tabPanel(title = "Aufgaben",
           style = "overflow-y:scroll; max-height: 600px",

           wellPanel(p("In diesem Tab findest du Aufgaben, die dich durch die Plots leiten und dir helfen können, sie gut zu verstehen.")),

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
        tabPanel("Stichprobenkennwerteverteilungen",
                 style = "overflow-x: scroll",
                 fluidRow(column(12, align="center", height = 20, br(".") , br("."), br("."))),
                fluidRow(
                          column(6, shinycssloaders::withSpinner(
                            plotOutput("plot_samp"), type = getOption("spinner.type", 4))),
                          column(6, shinycssloaders::withSpinner(
                                 plotOutput("forest"), type = getOption("spinner.type", 4)))
                          ),
                 h3("Stichprobenkennwerteverteilungen verschiedender Schätzer"),
                 # p("Die folgenden 4 Plots zeigen die Stichprobenkennwerteverteilungen der verschiedenen
                 # Schätzer. Jedes Dreieck symbolisiert eine der XX generierten Stichproben. Das umrandete
                 #   Dreieck zeigt die spezifische Stichprobe (Plot oben links)."),
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
  top = 00, right = 30, width = "64%",
  fixed = TRUE,
    fluidRow(column(12, height = 9, uiOutput("legendontop")))
  )


)
