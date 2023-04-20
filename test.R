test_panel <- function(){tabPanel(
  
  
  sidebarPanel(
    width = 2,
    
    # Instructions for users to appear at top of sidebar
    #helpText("Create your own table by selecting from the drop down boxes below."),
    
    # Volumes or proportions input
    selectizeInput(
      inputId = "selectType",
      label = "View the volumes or proportions of learners in each industry",
      choices = list( "Volumes" = "NumberSustainedEmployment",
                      "Proportions" = "SustainedEmploymentPercent",
      )))
  
  mainPanel(
 tableOutput("test_table")
  )
)}
