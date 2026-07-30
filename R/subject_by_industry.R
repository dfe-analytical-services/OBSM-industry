subject_by_industry_panel <- function() {
  tabPanel(
    tags$div(
      title = "This section is useful if you want to understand which industries your subject of study can lead to.",
      "Subject by industry"
    ),
    value = "SubjectByIndustry",
    gov_main_layout(
      gov_row(
        column(
          width = 12,
          expandable(
            inputId = "details",
            label = textOutput("subject_by_industry_title"),
            contents = div(
              id = "div_a",
              gov_row(
                layout_columns(
                  # Left column of filter options - start
                  # Data type input
                  selectizeInput(
                    inputId = "selectTypeSubj",
                    label = "Volumes or proportions of learners from each subject",
                    choices = list(
                      "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
                    )
                  ),

                  # Provision input
                  selectizeInput(
                    inputId = "selectProvisionSubj",
                    label = "Select provision type",
                    choices = choicesProvision$Provision
                  ),
                  # Industry input. List of choices will be dependent on provision type selected, so set to null for now
                  # Code in the server script will populate this list of choices dynamically
                  selectInput(
                    "selectIndustry",
                    label = "Select Industry",
                    choices = NULL
                  ),
                  # Level of detail input
                  selectizeInput(
                    inputId = "selectSSADetail",
                    label = "Select level of detail for sector subject area",
                    choices = list(
                      "General (Tier 1)" = "SSATier1",
                      "Detailed (Tier 2)" = "SSATier2"
                    )
                  ),

                  # Data breakdown input
                  selectizeInput(
                    inputId = "selectBreakdownSubj",
                    label = "Select breakdown",
                    choices = list(
                      "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Level of Learning" = "LevelOfLearning",
                      "Sex" = "Sex"
                    )
                  ),
                  # Add a button to download data as a csv
                  col_widths = c(6, 6, 6, 6, 6)
                )
              )
            )
          ),
          column(
            width = 12,
            gov_text(textOutput("subject_by_industry_text")),
            download_button(
              "downloadSubInd",
              "Download this table",
              file_type = "CSV",
              file_size = "max 13 KB"
            ),
            gt_output("subject_by_industry_crosstab")
          )
        )
      )
    )
  )
}
