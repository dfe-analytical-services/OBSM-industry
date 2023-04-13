# ---------------------------------------------------------
# This is the server file.
# Use it to create interactive elements like tables, charts and text for your app.
#
# Anything you create in the server file won't appear in your app until you call it in the UI file.
# This server script gives an example of a plot and value box that updates on slider input.
# There are many other elements you can add in too, and you can play around with their reactivity.
# The "outputs" section of the shiny cheatsheet has a few examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
#
# This is the server logic of a Shiny web application. You can run th
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# ---------------------------------------------------------


server <- function(input, output, session) {

  # Loading screen ---------------------------------------------------------------------------
  # Call initial loading screen

  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")

## Test code ----
  # Simple server stuff goes here ------------------------------------------------------------
  reactiveRevBal <- reactive({
    dfRevBal %>% filter(
      area_name == input$selectArea | area_name == "England",
      school_phase == input$selectPhase
    )
  })

  # Define server logic required to draw a histogram
  output$lineRevBal <- renderPlotly({
    ggplotly(createAvgRevTimeSeries(reactiveRevBal(), input$selectArea)) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })

  reactiveBenchmark <- reactive({
    dfRevBal %>%
      filter(
        area_name %in% c(input$selectArea, input$selectBenchLAs),
        school_phase == input$selectPhase,
        year == max(year)
      )
  })

  output$colBenchmark <- renderPlotly({
    ggplotly(plotAvgRevBenchmark(reactiveBenchmark()) %>%
      config(displayModeBar = F),
    height = 420
    )
  })

  output$tabBenchmark <- renderDataTable({
    datatable(reactiveBenchmark() %>%
      select(
        Area = area_name,
        `Average Revenue Balance (£)` = average_revenue_balance,
        `Total Revenue Balance (£m)` = total_revenue_balance_million
      ),
    options = list(
      scrollX = TRUE,
      paging = FALSE
    )
    )
  })

  # Define server logic to create a box

  output$boxavgRevBal <- renderValueBox({

    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0("£", format((reactiveRevBal() %>% filter(
        year == max(year),
        area_name == input$selectArea,
        school_phase == input$selectPhase
      ))$average_revenue_balance,
      big.mark = ","
      )),
      # add subtitle to explain what it's hsowing
      paste0("This is the latest value for the selected inputs"),
      color = "blue"
    )
  })
  output$boxpcRevBal <- renderValueBox({
    latest <- (reactiveRevBal() %>% filter(
      year == max(year),
      area_name == input$selectArea,
      school_phase == input$selectPhase
    ))$average_revenue_balance
    penult <- (reactiveRevBal() %>% filter(
      year == max(year) - 1,
      area_name == input$selectArea,
      school_phase == input$selectPhase
    ))$average_revenue_balance

    # Put value into box to plug into app
    valueBox(
      # take input number
      paste0("£", format(latest - penult,
        big.mark = ","
      )),
      # add subtitle to explain what it's hsowing
      paste0("Change on previous year"),
      color = "blue"
    )
  })

  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
  })

  # Download the underlying data button
  output$download_data <- downloadHandler(
    filename = "shiny_template_underlying_data.csv",
    content = function(file) {
      write.csv(dfRevBal, file)
    }
  )

# # Actual code ----

## Subject by industry crosstab ---
  
  
  output$subject_by_industry_crosstab <- renderTable({

#If selected breakdown is ethnicity, select totals for all other options and output columns of interest
  if(input$selectBreakdown == 'Ethnicity'){
    dfInd %>%
      filter(SSATier1 == input$selectSSA, SSATier2 == 'All', Provision == input$selectProvision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
               IndustrySection != 'All') %>%
      select(IndustrySection, Ethnicity, input$selectType)     %>%
      rename(Industry = IndustrySection) %>%
      spread(Ethnicity, input$selectType) %>%
    arrange(desc(All))
  }
# If selected breakdown is gender, select totals for all other options and output columns of interest
    else if(input$selectBreakdown == 'Gender') {
    dfInd %>%
      filter(SSATier1 == input$selectSSA, SSATier2 == 'All', Provision == input$selectProvision,
             LevelOfLearning == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             IndustrySection != 'All') %>%
      select(IndustrySection, Gender, input$selectType)     %>%
       rename(Industry = IndustrySection) %>%
      spread(Gender, input$selectType) %>%
      arrange(desc(All))
    }
  # If selected breakdown is level of learning, select totals for all other options and output columns of interest
      else if(input$selectBreakdown == 'LevelOfLearning') {
      dfInd %>%
        filter(SSATier1 == input$selectSSA, SSATier2 == 'All', Provision == input$selectProvision,
               Gender == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
               IndustrySection != 'All') %>%
        select(IndustrySection, LevelOfLearning, input$selectType)     %>%
        rename(Industry = IndustrySection) %>%
        spread(LevelOfLearning, input$selectType) %>%
        arrange(desc(All))
      }
  # If selected breakdown is age group, select totals for all other options and output columns of interest
        else if(input$selectBreakdown == 'AgeGroup') {
      dfInd %>%
        filter(SSATier1 == input$selectSSA, SSATier2 == 'All', Provision == input$selectProvision,
               Gender == 'All', AppType == 'All', Ethnicity == 'All', LevelOfLearning == 'All',
               IndustrySection != 'All') %>%
        select(IndustrySection, AgeGroup, input$selectType)     %>%
        rename(Industry = IndustrySection) %>%
        spread(AgeGroup, input$selectType) %>%
        arrange(desc(All))
        }

    #If no breakdowns are selected show summary data for all
    else {dfInd %>% 
        filter(SSATier1 == input$selectSSA, SSATier2 == 'All', Provision == input$selectProvision,
               LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All',
               IndustrySection != 'All') %>%
        select(IndustrySection, Ethnicity, NumberSustainedEmployment)     %>%  
        rename(Industry = IndustrySection) %>%
        spread(Ethnicity, NumberSustainedEmployment) %>%
        arrange(desc(All)) }
  })
  
## Create a dynamic title for subject by industry page ----
  
## Reformat provision input - leave blank unless specifying type of provision
 provisioninput <- reactive({
   if(input$selectProvision == 'All'){
     ""
   }
   else{
      (tolower(input$selectProvision))}
   })

 ## Reformat subject input
 subjectinput <- reactive({
   if(input$selectSSA== 'All'){
     "all subjects"
   }
   else{
     (tolower(input$selectSSA))}
 })
 
 ## Reformat breakdown input
 breakdowninput <- reactive({
   if(input$selectBreakdown == 'AgeGroup'){
     "age group"
   }
   else if(input$selectBreakdown == 'LevelOfLearning'){
     "level of learning"
   }
      else{
     (tolower(input$selectSSA))}
 })
 
 ## Bring together variables as specified above to produce final dynamic title
  output$subject_by_industry_title <- renderText({
    paste(
      "Industry of employment for ", provisioninput(), " learners achieving in " ,  subjectinput(),  " in 2019/20, by ", breakdowninput()
      )
    })

 
  
  ##---- TEST CODE
  
 #  SSAinput <- reactive({input$selectSSA})
 #  
 # crosstab_test <- observe({     # If selected breakdown is ethnicity, select totals for all other options and output columns of interest
 #        dfInd %>% 
 #        filter(SSATier1 == 'All', SSATier2 == 'All', Provision == 'All',
 #               LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', 
 #               IndustrySection != 'All')})
 #   
 # 
 #   
 #   output$crosstab_test <-  renderReactable({reactable(crosstab_test)})
  
  

  # Stop app ---------------------------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
