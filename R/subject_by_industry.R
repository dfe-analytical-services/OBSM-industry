
###########  PAGE LAYOUT --------------


subject_by_industry_panel <- function(){tabPanel(
  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
  value = "subjectByIndustry",
  
## Add sidebar panel ----

 # sidebarLayout(
    sidebarPanel(
      width = 2,
    
  # Instructions for users to appear at top of sidebar
      helpText("Create your own table by selecting from the drop down boxes below."),
  
  ### SSA Tier 1 input ----
  selectizeInput(
    inputId = "selectSSA",
    label = "Select Sector Subject Area",
    choices = choicesSSATier1$SSATier1
   # options = list(placeholder = "Select subject area")
    ),
  
 # Provision input
  selectizeInput(
    inputId = "selectProvision",
    label = "Select provision type",
    choices = choicesProvision$Provision
   # options = list(placeholder = "Select provision")
    ),
 

 # Data breakdown input
 selectizeInput(
   inputId = "selectBreakdown",
   label = "Select breakdown",
   choices = list("Sex" = "Sex",
                  "Ethnicity" = "Ethnicity",
                  "Level of Learning" = "LevelofLearning")
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
  
paste("Add text here"),
tableOutput("subject_by_industry_crosstab")

# dfInd %>% 
#   filter(SSATier1 == "Business, Administration and Law", SSATier2 == 'All', Provision == 'All',
#          LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All') %>%
#   select(SSATier1, IndustrySection, NumberSustainedEmployment )


))}
  


#################### CROSS-TABS ---- #######################################
## Main panel =========================================================


#textOutput("q")
