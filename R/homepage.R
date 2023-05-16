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
             
            )
          )
        )
      )
    )
  )
  
}

  