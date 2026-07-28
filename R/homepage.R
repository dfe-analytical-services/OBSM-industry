homepage_panel <- function() {
  tabPanel(
    "Homepage",
    gov_main_layout(
      gov_row(
        heading_text(
          "Further Education Outcomes Industry Dashboard - 2022/23",
          size = "xl",
          level = 1
        ),
        intro_text()
      ),

      ## Left panel -------------------------------------------------------
      gov_row(
        bslib::layout_columns(
          card(
            card_header(
              shinyGovstyle::heading_text(
                "Contents",
                size = "l",
                level = 2
              )
            ),
            card_body(
              shinyGovstyle::heading_text(
                "Industry by subject table",
                size = "m",
                level = 3
              ),
              ind_by_subj_text(),
              shinyGovstyle::heading_text(
                "Subject by industry table",
                size = "m",
                level = 3
              ),
              subj_by_ind_text()
            )
          ),

          ## Right panel ------------------------------------------------------

          card(
            card_header(
              shinyGovstyle::heading_text(
                "Guidance",
                size = "l",
                level = 2
              )
            ),
            guidance_text(),
          ),
          col_widths = c(12, 12)
        )
      )
    )
  )
}
