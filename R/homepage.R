homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        column(
          12,
          heading_text(
            "Further Education Outcomes Industry Dashboard - 2022/23",
            size = "xl",
            level = 1
          ),
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
                shinyGovstyle::heading_text(
                  "Contents",
                  size = "l",
                  level = 2
                )
              ),
              div(
                class = "panel-body",
                tags$div(
                  title = "Industry by subject.",
                  shinyGovstyle::heading_text(
                    actionLink("link_to_ind_by_subj_tab", "Industry by subject table"),
                    size = "m",
                    level = 3
                  )
                ),
                ind_by_subj_text(),
              ),
              tags$div(
                title = "Industry by subject.",
                shinyGovstyle::heading_text(
                  actionLink("link_to_ind_by_subj_tab", "Industry by subject table"),
                  size = "m",
                  level = 3
                )
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
              shinyGovstyle::heading_text(
                "Guidance",
                size = "l",
                level = 2
              )
            ),
            guidance_text(),
          )
        )
      )
    )
  )
}
