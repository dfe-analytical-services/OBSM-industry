homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Further Education Outcomes Industry Dashboard"),
          intro_text(),
          br(),
          br()
        ),
        
        ## Left panel -------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Contents")
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "Industry by subject.",
                  h3(actionLink("link_to_ind_by_subj_tab", "Industry by subject table")),
                  ind_by_subj_text(),
                ),
               tags$div(
                 title = "Industry by subject.",
                 h3(actionLink("link_to_subj_by_ind_tab", "Subject by industry table")),
                 subj_by_ind_text(),
                
               ),
                br()
              )
            )
          ),
        ),
        
        ## Right panel ------------------------------------------------------
        
        column(
          6,
          div(
            div(
              class = "panel panel-info",
              div(
                class = "panel-heading",
                style = "color: white;font-size: 18px;font-style: bold; background-color: #1d70b8;",
                h2("Guidance")
              ),
              guidance_text(),
              # div(
              #   class = "panel-body",
              #   h3("Context and purpose (h3)"),
              #   p("This app is the DfE Analytical Service's R-Shiny template demonstration app and is being developed to provide a coherent styling for DfE dashboards alongside some useful example componenets that teams can adapt for their own uses."),
              #   p("DfE teams using this template should avoid changing the styling and layout, keeping the header, footer and side navigation list formats."),
              #                   p("You might want to add some relevant background information for your users here. For example some useful links to your EES publication, data sources and other relevant resources."),
              #   h3("Guidance sources (h3)"),
              #   p("For example, here we'll add some of the key resources we draw on to guide styling and vizualisation...")
              # )
            )
          )
        )
      )
    )
  )
  
}

  