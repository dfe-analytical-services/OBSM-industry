


###########  PAGE LAYOUT --------------


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
    label = "Select Sector Subject Area Tier 1",
    choices = choicesSSATier1$SSATier1
    ),
 
  # SSA Tier 2 input. List of choices will be dependent on SSA Tier 1 selected above, so set to null for now
  # Code in the server script will populate this list of choices dynamically
  selectInput("selectSSATier2",
              label = "Select Sector Subject Area Tier 2",
              choices = NULL),


 # Data breakdown input
 selectizeInput(
   inputId = "selectBreakdown",
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
  

h4(textOutput("industry_by_subject_title")),
paste("This table shows the industry of employment for learners with a sustained employment destination in 2020/21, after completing their aim in 2019/20."),
br(),
tableOutput("industry_by_subject_crosstab"),
helpText("Download the table as a csv"),
downloadButton("downloadIndSub", label = "Download this data table")

#

))}

