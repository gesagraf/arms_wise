ui <- fluidPage(

  # Hier wird die Sidebar (in grau) definiert
  sidebarPanel(width = 4,

               # Weil es verschiedene Tabs geben soll
               tabsetPanel(

                 # Tab 1: Für die Plots
                 tabPanel(title = "Simulation",

                          wellPanel(p("In diesem Tab kannst du grundsätzliche Einstellungen für die gesamte Simulation vornehmen.")),

                          # Kasten für die Populationsparameter
                          ## Überschriften & Text
                          wellPanel(title = "Einstellungen",
                                    strong("Populationsparameter einstellen"),
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
                                                max = 150,
                                                value = 30),

                                    ## Slider Stichprobenanzahl
                                    ### Styleeinstellungen
                                    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #a9a9a9; border-top: #a9a9a9; border-bottom: #a9a9a9}")),
                                    ### Slider
                                    sliderInput(inputId = "number",
                                                "Gesamtanzahl der Stichproben (höhere Werte -> längere Rechenzeit)",
                                                min = 10,
                                                max = 1000,
                                                value = 100)),
                          actionButton("compute", "Draw Random Samples")),

                 # Tab 2: Für die Plots
                 tabPanel(title = "Plots",
                          #style = "overflow-y:scroll",

                          wellPanel(p("In diesem Tab kannst du dir die verschiedenen Plots anzeigen lassen.")),


                          # Kasten für die spezifische Stichprobe
                          ## Überschriften & Text
                          wellPanel(title = "hier kannst du dir eine bestimmte Stichprobe anzeigen lassen (Plot 1 oben links)",
                                    strong("Spezifische Stichprobe"),
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
                                    strong("Plot mit Konfidenzintervallen"),
                                    # p("Erlärungstext"),

                                    # Checkbox Forestplot
                                    checkboxInput("p_forest", "Zeige den Plot mit Konfidenzintervallen aller Schätzer an", value = F)),


                          # Box für Mean und Minmax
                          wellPanel(title = "Hier kannst du dir die Stichprobenkennwerteverteilungen vom arithmetischen Mittel und dem alternativen Schätzer anzeigen lassen.",
                                    strong("Stichprobenkennwerteverteilungen"),
                                    p("Von Mittelwert und alternativem Schätzer"),


                                    checkboxInput("p_mean",
                                                  "Zeige die SKV des arithmetischen Mittels an", value = F),

    checkboxInput("p_minmax", "Zeige die SKV des alternativen Schätzers an", value = F)),

    wellPanel(title = "Hier kannst du die Priori des gleichverteilten Bayes-Schätzers einstllen.
Der gleichverteilte Schätzer nimmt an, das alle Werte innerhalb der eingestellten Range gleich
wahrscheinlich sind, aber Werte außerhalb der Range unmöglich sind.",
              strong("SKV von Bayes-Schätzern"),
              p("mit gleichverteilter Priori"),

      checkboxInput("p_bayes_uni", "Zeige die SKV dieses Bayes-Schätzers", value = F),

    #### Priors ####
    # Gleichverteiler Prior
    tags$style(HTML(".irs-from .irs-to .irs-from .js-irs-5 .irs-to .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #443A83FF; background-color: #443A83FF; border-top: #443A83FF; border-bottom: #443A83FF}")),

    sliderInput(inputId = "rangePriori",
                "Range des gleichverteilten Bayes Priors",
                min = -100,
                max = 100,
                value = c(-50,5)),

    checkboxInput("show_prior_uni", "Zeige den Prior des gleichverteilten Bayes-Schätzers", value = F)#,
    # textOutput("nas_uni")



    ),








    wellPanel(title = "hier kannst du Mittelwert und Standardabweichung der normalverteilten Priori
einstellen",
              strong("SKV von Bayes-Schätzern"),
              p("mit normalverteiltem Prior"),

      checkboxInput("p_bayes_nv", "Zeige die SKV dieses Bayes-Schätzers", value = F),
    # Normalverteilter Prior
    # Prior Mean
    tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #440154FF; border-top: #440154FF; border-bottom: #440154FF}")),
    sliderInput(inputId = "mu_prior",
                label = "Prior Mean",
                min = -100,
                max = 100,
                value = 35),

    # Sidebar with a slider input for Prior Tau
    tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #440154FF; border-top: #440154FF; border-bottom: #440154FF}")),
    sliderInput(inputId = "tau_prior",
                label = "Prior Tau",
                min = 1,
                max = 100,
                value = 5),

    checkboxInput("show_prior_nv", "Zeige den Prior des normalverteilten Bayes-Schätzers", value = F)#,
    # textOutput("nas_NV")


  )),
  tabPanel(title = "Aufgaben",
          # style = "overflow-y:scroll; max-height: 1200px",

           wellPanel(p("In diesem Tab findest du Aufgaben, die dich durch die Plots leiten und dir helfen können, sie gut zu verstehen.")),
          # actionButton("pop_out", "pop-out-Aufgaben"),
          # p(""),
          wellPanel(
          h4("Lerne die App kennen"),
           strong("Aufgabe 1"),
           p("Finde eine Einstellung für den Mittelwert, wo der rote Strich (µ) genau auf der 50 liegt."),
          strong("Aufgabe 2"),
          p("Verändere den Mittelwert und die Standardabweichung so, dass die Zahlen {0, 50, 100} auf der X-Achse zu lesen sind."),
          strong("Aufgabe 3"),
          p("Verschiebe die Größe der einzelnen Stichproben, bis das Histogramm optisch einer Normalverteilung an nächsten kommt."),
          ),


          wellPanel(
          h4("Verändere die Populationsparameter"),
          strong("Aufgabe 4"),
          p("Schaue dir verschiedene spezifische Stichproben (unter Tab 'Plots') an, wieso unterscheiden sie sich?"),
          strong("Aufgabe 5"),
           br(em("Aktiviere für diese Aufgabe die Plots der SKV des arithmetischen Mittels und des alternativen Schätzers.")),
           p("Stelle die Stichprobengröße so ein, das die SKV vom arithmetischen Mittel und
             dem alternativen Schätzer identisch sind. (Zur Erinnerung: der alternative Schätzer
             ist der Mittelwert des Minimums und Maximums.)"),
           strong("Aufgabe 6"),
           br(em("Aktiviere für diese Aufgabe die Plots der SKV des arithmetischen Mittels und des gleichverteilten Bayes-Schätzers.")),
           p("Finde eine Einstellung, für die die Plots der SKV des arithmetischen Mittels und
             des gleichverteilten Bayes-Schätzers maximal ähnlich sind."),
          # p("")
          ),

          wellPanel(
          h4("Vergleiche die verschiedenen Schätzer"),
          strong("Aufgabe 7"),
          br(em("Lade für diese Aufgabe die Seite neu (um die Default-Werte wieder herzustellen; das ist der Pfeil oben links in der Ecke) und aktiviere den Plot des normalverteilten Bayes-Schätzers")),
          p("Der Mittelwert aller Bayes-Schätzer (schwarzer Strich) ist ziemlich eindeutig unterschiedlich zum tatsächlichen Poplulationsmittelwert (µ, roter Strich). Woran liegt das?"),
          p("Finde eine Einstellung, in dem der Mean aller Bayes-Schätzer (schwarzer Strich) und der tatsächliche Populationsmittelwert (roter Strich) direkt übereinander liegen."),



          strong("Aufgabe 8"),
           br(em("Aktiviere für diese Aufgabe den Konfidenzintervall-Plot.")),
           p("a) Vergleiche die verschiedenen Schätzer im Konfidenzintervall-Plot. Welcher Schätzer hat die beste Erwartungstreue, Effizienz und Konsistenz?"),
           p("b) Wie kannst du die Regler so einstellen, das einer der Schätzer den Populationsmittelwert (µ) nicht mehr mit einschließt? "),
          p("c) Kannst du die Regler so verändern, das einer der Schätzer deutlich effizienter ist als die anderen?"),
           strong("Aufgabe 9"),
           p("Für Aufgabe 8c) gibt es unterschiedliche Lösungen, findest du 2? Welche der
             Lösungen könntest du unter Umständen auch in der Forschung anwenden, und welche solltest du eher vermeiden?")

          ))
      )),
  # Show a plot of the generated distribution
  mainPanel(
    # fixedPanel(fluidRow(column(12, height = 9, uiOutput("legendontop")))),
    tabPanel("Stichprobenkennwerteverteilungen",
                 #style = "overflow-x: scroll",
                 fluidRow(column(12, height = 9, uiOutput("legendontop"))),
                 # fluidRow(column(12,
                 #                 # height = 4,
                 #                 plotOutput("legendontop"),style = "height:30vh")),
                 # fluidRow(column(12, align="center", height = 20, br("."), br(".") , br("."), br("."))),
                fluidRow(
                  conditionalPanel("input.p_samp == 1", column(6, shinycssloaders::withSpinner(
                            plotOutput("plot_samp"), type = getOption("spinner.type", 4)))),
                          conditionalPanel("input.p_forest == 1", column(6, shinycssloaders::withSpinner(
                                 plotOutput("forest"), type = getOption("spinner.type", 4))))
                          ),
                # h3("Stichprobenkennwerteverteilungen verschiedender Schätzer"),
                 # p("Die folgenden 4 Plots zeigen die Stichprobenkennwerteverteilungen der verschiedenen
                 # Schätzer. Jedes Dreieck symbolisiert eine der XX generierten Stichproben. Das umrandete
                 #   Dreieck zeigt die spezifische Stichprobe (Plot oben links)."),
                fluidRow(
                         conditionalPanel("input.p_mean == 1", column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_mean"), type = getOption("spinner.type", 4)))),
                         conditionalPanel("input.p_minmax == 1", column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_minmax"), type = getOption("spinner.type", 4))))

                ),
                fluidRow(
                         conditionalPanel("input.p_bayes_uni == 1", column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_bayes_uni"), type = getOption("spinner.type", 4)))),
                         conditionalPanel("input.p_bayes_nv == 1", column(6, shinycssloaders::withSpinner(
                                plotOutput("plot_bayes_nv"), type = getOption("spinner.type", 4))))

        )
      )
  ),

tags$head(tags$style(
  type="text/css",
  "#legendontop img {max-width: 100%; width: 100%; height: auto}"
)),

# absolutePanel(
#   top = 00, right = 30, width = "64%", height = 9,
  # fixed = TRUE,
  #   fluidRow(column(12, height = 9, uiOutput("legendontop")))
  # )
# ,
p("."),p("."),p(".")
,p("Erstellt im Rahmen des ARMS II Seminars
bei Prof. Dr. Florian Scharf. Projekt von Gesa Graf, Ian Buhmann und Constantin Wiegand.
")


)
