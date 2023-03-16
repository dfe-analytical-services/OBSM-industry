# 
subject_by_industry_panel <- function() {
  tabPanel(
    value = "Subject by industry",
    "Subject by industry",
    
    # Define UI for application that draws a histogram
    
    # Sidebar with a slider input for number of bins
    gov_main_layout(
      gov_row(
        column(
          width=12,
          h1("Subject by industry"),
             ),
        column(
          width=12,
          div(
            class = "well",
            style = "min-height: 100%; height: 100%; overflow-y: visible",
            gov_row(
              column(
                width = 6,
                selectizeInput("selectSSA",
                               "Select Sector Subject Area",
                               choices = choicesSSATier1
                )),
              column(
                width = 6,
                selectizeInput(
                  inputId = "selectArea",
                  label = "Choose an area:",
                  choices = choicesAreas$area_name
                )
              ), 
              column(
                width = 12,
                paste("Download the underlying data for this dashboard:"), br(),
                downloadButton(
                  outputId = "download_data",
                  label= "Download data",
                  icon = shiny::icon("download"),
                  class = "downloadButton"
                )
              ))
          )
        ),
        
        column(
          width=12,
          tabsetPanel(id = "tabsetpanels",
                      tabPanel(
                        "Example panel 1",
                        fluidRow(
                          column(
                            width=12,
                            h2("Outputs 1 (h2)"),
                            valueBoxOutput("boxavgRevBal", width = 6),
                            valueBoxOutput("boxpcRevBal", width = 6),
                            box(
                              width=12,
                              plotlyOutput("lineRevBal")))
                        )
                      ),
                      tabPanel(
                        "Example panel 2",
                        fluidRow(
                          column(
                            width=12,
                            h2("Outputs 2 (h2)"),
                            p("This is the standard paragraph style for adding guiding info around data content."),
                            column(
                              width=6,
                              box(
                                width=12,
                                plotlyOutput("colBenchmark")
                              )
                            ),
                            column(
                              width=6,
                              div(
                                class = "well",
                                style = "min-height: 100%; height: 100%; overflow-y: visible",
                                fluidRow(
                                  column(
                                    width=12,
                                    selectizeInput("selectBenchLAs",
                                                   "Select benchamrk LAs",
                                                   choices = choicesLAs$area_name,
                                                   multiple=TRUE,
                                                   options = list(maxItems = 3)
                                    )
                                  )
                                )
                              ),
                              dataTableOutput("tabBenchmark")
                            ))
                        ))
          )
        )
        # add box to show user input
      )
    )
  )
}