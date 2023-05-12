

subject_by_industry_panel <- function(){tabPanel(
  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
  value = "SubjectByIndustry",
  
  ## Add sidebar panel ----
  
  sidebarPanel(
    width = 2,
    
    
    # Volumes or proportions input
    selectizeInput(
      inputId = "selectTypeSubj",
      label = "View the volumes or proportions of learners from each subject",
      choices = list( "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
      )
    ),
    
    # Level of detail input
    selectizeInput(
      inputId = "selectSSADetail",
      label = "Select level of detail for sector subject area",
      choices = list( "General (Tier 1)" = "SSATier1",
                      "Detailed (Tier 2)" = "SSATier2"
      )
    ),
    
    
    # Industry input
    selectizeInput(
      inputId = "selectIndustry",
      label = "Select industry",
      choices = choicesIndustry$Industry,
      selected = 'All'
    ),
    
    
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
      choices = list( "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Level of Learning" = "LevelOfLearning",
                      "Sex" = "Gender"
                    )
    ),
    
    
    
    # Code to prevent text wrapping when selecting input from dropdowns
    tags$head(
      tags$style(HTML('
                              
                                .selectize-dropdown {
                                    width: 500px !important;
                                }'
      )
      )
    )
    
  ),  #End of side panel
  
  
  ## Add main panel ----
  
  mainPanel(
    width = 10,
    style = "height: 90vh; overflow-y: auto; overflow-x: auto;",
    
   
    h4(textOutput("subject_by_industry_title")),
    textOutput("subject_by_industry_text"),
    helpText("Download the table as a csv"),
    downloadButton("downloadSubInd", label = "Download this data table"),
    tableOutput("subject_by_industry_crosstab")

    )

    )}

