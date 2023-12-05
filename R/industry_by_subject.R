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
                column( # Left column of filter options - start
                  width = 6,

                  # Volumes or proportions input
                  selectizeInput(
                    inputId = "selectType",
                    label = h4("View the volumes or proportions of learners in each industry:"),
                    choices = list(
                      "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
                    )
                  ),

                  # Provision input
                  selectizeInput(
                    inputId = "selectProvision",
                    label = h4("Select provision type:"),
                    choices = choicesProvision$Provision
                  ),
                  # Data breakdown input
                  selectizeInput(
                    inputId = "selectBreakdown",
                    label = h4("Select breakdown:"),
                    choices = list(
                      "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Level of Learning" = "LevelOfLearning",
                      "Sex" = "Sex"
                    )
                  ),

                  # Code to prevent text wrapping when selecting input from dropdowns
                  tags$head(
                    tags$style(HTML("

                                .selectize-dropdown {
                                    width: 500px !important;
                                }"))
                  )
                ), # Left column of filter options - end
                column( # Right column of filter options - start
                  width = 6,

                  # SSA Tier 1 input. List of choices will be dependent on SSA Tier 1 selected above, so set to null for now
                  # Code in the server script will populate this list of choices dynamically
                  selectInput("selectSSA",
                    label = h4("Select Sector Subject Area Tier 1:"),
                    choices = NULL
                  ),

                  # SSA Tier 2 input. List of choices will be dependent on SSA Tier 1 selected above, so set to null for now
                  # Code in the server script will populate this list of choices dynamically
                  selectInput("selectSSATier2",
                    label = h4("Select Sector Subject Area Tier 2:"),
                    choices = NULL
                  ),
                  helpText("Download the table as a csv"),
                  downloadButton("downloadIndSub", label = "Download this data table"),
                ) # Right column of filter options - end
              )
            )
          )
        )
      ), # End of inputs gov_row


      ## Add main panel ----
      gov_row(
        column(
          width = 12,
          textOutput("industry_by_subject_text"),
          gt_output("industry_by_subject_crosstab")
        )
      )
    )
  )
}
