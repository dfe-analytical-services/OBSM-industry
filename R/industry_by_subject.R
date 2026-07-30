###########  PAGE LAYOUT --------------

industry_by_subject_panel <- function() {
  tabPanel(
    tags$div(
      title = "This section is useful if you want to understand which industries your subject of study can lead to.",
      "Industry by subject"
    ),
    value = "IndustryBySubject",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          expandable(
            inputId = "details",
            label = textOutput("industry_by_subject_title"),
            contents = div(
              id = "div_a",
              gov_row(
                layout_columns(
                  # Volumes or proportions input
                  selectizeInput(
                    inputId = "selectType",
                    label = "Volumes or proportions of learners in each industry:",
                    choices = list(
                      "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
                    )
                  ),

                  # Provision input
                  selectizeInput(
                    inputId = "selectProvision",
                    label = "Select provision type:",
                    choices = choicesProvision$Provision
                  ),
                  # Data breakdown input
                  selectizeInput(
                    inputId = "selectBreakdown",
                    label = "Select breakdown:",
                    choices = list(
                      "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Level of Learning" = "LevelOfLearning",
                      "Sex" = "Sex"
                    )
                  ),

                  # SSA Tier 1 input. List of choices will be dependent on SSA Tier 1 selected above, so set to null for now
                  # Code in the server script will populate this list of choices dynamically
                  selectInput(
                    "selectSSA",
                    label = "Select Sector Subject Area Tier 1:",
                    choices = NULL
                  ),

                  # SSA Tier 2 input. List of choices will be dependent on SSA Tier 1 selected above, so set to null for now
                  # Code in the server script will populate this list of choices dynamically
                  selectInput(
                    "selectSSATier2",
                    label = "Select Sector Subject Area Tier 2",
                    choices = NULL
                  ),
                  col_widths = c(6, 6, 6, 6, 6)
                )
              )
            )
          ), # End of inputs gov_row

          ## Add main panel ----
          gov_row(
            column(
              width = 12,
              gov_text(textOutput("industry_by_subject_text")),
              # Add a button to download data as a csv
              shinyGovstyle::download_button(
                "downloadIndSub",
                "Download this table",
                file_type = "CSV",
                file_size = "max 5 KB"
              ),
              gt_output("industry_by_subject_crosstab")
            )
          )
        )
      )
    )
  )
}
