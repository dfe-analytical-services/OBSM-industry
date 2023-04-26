


###########  PAGE LAYOUT --------------


subject_by_industry_panel <- function(){tabPanel(
  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
  value = "subjectByIndustry",
  
## Add sidebar panel ----

 # sidebarLayout(
    sidebarPanel(
      width = 2,
    
  # Instructions for users to appear at top of sidebar
      #helpText("Create your own table by selecting from the drop down boxes below."),
  
  # Volumes or proportions input
  selectizeInput(
    inputId = "selectType",
    label = "View the volumes or proportions of learners in each industry",
    choices = list( "Volumes" = "NumberSustainedEmployment",
                    "Proportions" = "SustainedEmploymentPercent"
                    )
  ),
  
  # Provision input
  selectizeInput(
    inputId = "selectProvision",
    label = "Select provision type",
    choices = choicesProvision$Provision
  ),
  
  # SSA Tier 1 input 
    selectizeInput(
    inputId = "selectSSA",
    label = "Select Sector Subject Area",
    choices = choicesSSATier1$SSATier1
    ),
  

 # Data breakdown input
 selectizeInput(
   inputId = "selectBreakdown",
   label = "Select breakdown",
   choices = list( "Age Group" = "AgeGroup",
                   "Ethnicity" = "Ethnicity",
                   "Gender" = "Gender",
                  "Level of Learning" = "LevelOfLearning"
                  )
 ),
 

 # column(
 #   width = 12,
 #   paste("Download underlying data"), br(),
 #   downloadButton(
 #     outputId = "download_data",
 #     label= "Download data",
 #     icon = shiny::icon("download"),
 #     class = "downloadButton",
 #     style="display:inline-block"
 #   )),
 
 # helpText("Download the current table as a csv"),
 # downloadButton("downloadData", "Download table"),
 # #helpText("Note that the downloaded data will not retain the ordering in the displayed table and will instead order the data alphabetically by industry and sub-industry."),
    

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
paste("This table shows the industry that learners moved into after achieving their aim in 2019/20"),
tableOutput("subject_by_industry_crosstab"),
helpText("Download the table as a csv"),
downloadButton("downloadSubInd", label = "Download this data table")


#gt_output("subject_by_indsutry_crosstab")



))}

