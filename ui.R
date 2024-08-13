navbarPage("Focos", id = "nav",

           tabPanel("Interactive map",
                    div(class = "outer",

                        tags$head(
                            # Include our custom CSS
                            includeCSS("styles.css"),
                            includeScript("gomap.js")
                        ),

                        # If not using custom CSS set height of
                        # leafletOutput to a number instead of percent
                        leafletOutput("map", width = "100%", height = "100%"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Filtros"),
                                      # Inputs
                                      selectizeInput(
                                          inputId = "city",
                                          label = "Cidade",
                                          choices = list("", "Jacareí", "São José dos Campos"),
                                          options = list(maxOptions = 5)
                                      ),
                                      dateRangeInput("daterange", "Filtro de intervalo:",
                                                     start = "2024-07-01",
                                                     end   = "2024-08-11"),
                                      # TODO: add filtro satelite de referencia
                                      # Plotss
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),

                        tags$div(id = "cite", "Trabalho de disciplina - Geoinformática")
                    )
           ),
           conditionalPanel("false", icon("crosshair"))
)
