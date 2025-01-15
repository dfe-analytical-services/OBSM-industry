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
                column( # Left column of filter options - start
                  width = 6,

                  # Data type input
                  selectizeInput(
                    inputId = "selectTypeSubj",
                    label = h4("View the volumes or proportions of learners from each subject:"),
                    choices = list(
                      "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
                    )
                  ),

                  # Provision input
                  selectizeInput(
                    inputId = "selectProvisionSubj",
                    label = h4("Select provision type:"),
                    choices = choicesProvision$Provision
                  ),
                  # Industry input. List of choices will be dependent on provision type selected, so set to null for now
                  # Code in the server script will populate this list of choices dynamically
                  selectInput("selectIndustry",
                    label = h4("Select Industry:"),
                    choices = NULL
                  )
                ), # Left column of filter options - end

                column( # Right column of filter options - start
                  width = 6,
                  # Level of detail input
                  selectizeInput(
                    inputId = "selectSSADetail",
                    label = h4("Select level of detail for sector subject area:"),
                    choices = list(
                      "General (Tier 1)" = "SSATier1",
                      "Detailed (Tier 2)" = "SSATier2"
                    )
                  ),

                  # Data breakdown input
                  selectizeInput(
                    inputId = "selectBreakdownSubj",
                    label = h4("Select breakdown:"),
                    choices = list(
                      "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Level of Learning" = "LevelOfLearning",
                      "Sex" = "Sex"
                    )
                  ),
                  # Add a button to download data as a csv
                  tags$button(
                    type = "button",
                    class = "govuk-button govuk-button--inverse",
                    `data-module` = "govuk-button",
                    download_link(
                      "downloadSubInd",
                      link_text = "Download this table",
                      file_size = "maximum 13 KB"
                    )
                  ),
                ) # Right column of filter options - end
              )
            )
          )
        ),
        column(
          width = 12,
          textOutput("subject_by_industry_text"),
          br(),
          br(),
          renderText("TEST"),
          gt_output("subject_by_industry_crosstab")
        )
      )
    )
  )
}
