subject_by_industry_panel <- function() {
  tabPanel(
    tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
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
                column(
                  width = 6,
                  selectizeInput(
                    inputId = "selectTypeSubj",
                    label = "View the volumes or proportions of learners from each subject",
                    choices = list(
                      "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
                    )
                  ),
                  # Industry input
                  selectizeInput(
                    inputId = "selectIndustry",
                    label = "Select industry",
                    choices = choicesIndustry$Industry,
                    selected = "All"
                  ),
                  # Level of detail input
                  selectizeInput(
                    inputId = "selectSSADetail",
                    label = "Select level of detail for sector subject area",
                    choices = list(
                      "General (Tier 1)" = "SSATier1",
                      "Detailed (Tier 2)" = "SSATier2"
                    )
                  )
                ),
                column(
                  width = 6,
                  # Provision input
                  selectizeInput(
                    inputId = "selectProvisionSubj",
                    label = "Select provision type",
                    choices = choicesProvision$Provision
                  ),


                  # Data breakdown input
                  selectizeInput(
                    inputId = "selectBreakdownSubj",
                    label = "Select breakdown",
                    choices = list(
                      "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Level of Learning" = "LevelOfLearning",
                      "Sex" = "Gender"
                    )
                  )
                )
              )
            )
          )
        ),
        column(
          width = 12,
          helpText("Download the table as a csv"),
          downloadButton("downloadSubInd", label = "Download this data table"),
          textOutput("subject_by_industry_text"),
          tableOutput("subject_by_industry_crosstab")
        )
      )
    )
  )
}
