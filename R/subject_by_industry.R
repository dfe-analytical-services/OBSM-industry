
#########################  PAGE LAYOUT ---- ########################


subject_by_industry_panel <- function(){tabPanel(
  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
  value = "subjectByIndustry",
  
# Add sidebar panel

 # sidebarLayout(
    sidebarPanel(
      width = 2,
    
  # Instructions for users to appear at top of sidebar
      helpText("Create your own table by selecting from the drop down boxes below."),
  
  # SSA Tier 1 input ----
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
 
 #test
 
 textInput("Name", "Enter a name:"),

 
 
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
 
 
),
mainPanel(
  width = 10,
  style = "height: 90vh; overflow-y: auto; overflow-x: auto;",
  
paste("1. Outcome percentages are rounded to the nearest 0.1%.")
))}
  


#################### CROSS-TABS ---- #######################################
## Main panel =========================================================


#textOutput("q")
