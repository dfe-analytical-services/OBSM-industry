

industry_by_subject_panel <- function(){tabPanel(
  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Industry by subject"),
  value = "IndustryBySubject",
  
  ## Add sidebar panel ----
  
  # sidebarLayout(
  sidebarPanel(
    width = 2,
    
    # Instructions for users to appear at top of sidebar
    #helpText("Create your own table by selecting from the drop down boxes below."),
    
    # Volumes or proportions input
    selectizeInput(
      inputId = "selectTypeInd",
      label = "View the volumes or proportions of learners in each industry",
      choices = list( "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent"
      )
    ),
    
    # SSA Tier 1 input
    selectizeInput(
      inputId = "selectIndutsry",
      label = "Select industry",
      choices = choicesIndustry$Industry
    ),
    
    
    # Provision input
    selectizeInput(
      inputId = "selectProvisionInd",
      label = "Select provision type",
      choices = choicesProvision$Provision
    ),
    

    # Data breakdown input
    selectizeInput(
      inputId = "selectBreakdownInd",
      label = "Select breakdown",
      choices = list( "Age Group" = "AgeGroup",
                      "Ethnicity" = "Ethnicity",
                      "Gender" = "Gender",
                      "Level of Learning" = "LevelOfLearning"
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
    
    
    # h4(textOutput("subject_by_industry_title")),
    # paste("This table shows the industry of employment for learners with a sustained employment destination in 2020/21, after completing their aim in 2019/20."),
    # br(),
    # tableOutput("subject_by_industry_crosstab"),
    # helpText("Download the table as a csv"),
    # downloadButton("downloadSubInd", label = "Download this data table")
    # 


  ))}

