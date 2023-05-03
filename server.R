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

# ## Test code ----
#   # Simple server stuff goes here ------------------------------------------------------------
#   reactiveRevBal <- reactive({
#     dfRevBal %>% filter(
#       area_name == input$selectArea | area_name == "England",
#       school_phase == input$selectPhase
#     )
#   })
# 
#   # Define server logic required to draw a histogram
#   output$lineRevBal <- renderPlotly({
#     ggplotly(createAvgRevTimeSeries(reactiveRevBal(), input$selectArea)) %>%
#       config(displayModeBar = F) %>%
#       layout(legend = list(orientation = "h", x = 0, y = -0.2))
#   })
# 
#   reactiveBenchmark <- reactive({
#     dfRevBal %>%
#       filter(
#         area_name %in% c(input$selectArea, input$selectBenchLAs),
#         school_phase == input$selectPhase,
#         year == max(year)
#       )
#   })
# 
#   output$colBenchmark <- renderPlotly({
#     ggplotly(plotAvgRevBenchmark(reactiveBenchmark()) %>%
#       config(displayModeBar = F),
#     height = 420
#     )
#   })
# 
#   output$tabBenchmark <- renderDataTable({
#     datatable(reactiveBenchmark() %>%
#       select(
#         Area = area_name,
#         `Average Revenue Balance (£)` = average_revenue_balance,
#         `Total Revenue Balance (£m)` = total_revenue_balance_million
#       ),
#     options = list(
#       scrollX = TRUE,
#       paging = FALSE
#     )
#     )
#   })
# 
#   # Define server logic to create a box
# 
#   output$boxavgRevBal <- renderValueBox({
# 
#     # Put value into box to plug into app
#     valueBox(
#       # take input number
#       paste0("£", format((reactiveRevBal() %>% filter(
#         year == max(year),
#         area_name == input$selectArea,
#         school_phase == input$selectPhase
#       ))$average_revenue_balance,
#       big.mark = ","
#       )),
#       # add subtitle to explain what it's hsowing
#       paste0("This is the latest value for the selected inputs"),
#       color = "blue"
#     )
#   })
#   output$boxpcRevBal <- renderValueBox({
#     latest <- (reactiveRevBal() %>% filter(
#       year == max(year),
#       area_name == input$selectArea,
#       school_phase == input$selectPhase
#     ))$average_revenue_balance
#     penult <- (reactiveRevBal() %>% filter(
#       year == max(year) - 1,
#       area_name == input$selectArea,
#       school_phase == input$selectPhase
#     ))$average_revenue_balance
# 
#     # Put value into box to plug into app
#     valueBox(
#       # take input number
#       paste0("£", format(latest - penult,
#         big.mark = ","
#       )),
#       # add subtitle to explain what it's hsowing
#       paste0("Change on previous year"),
#       color = "blue"
#     )
#   })
# 
#   observeEvent(input$link_to_app_content_tab, {
#     updateTabsetPanel(session, "navlistPanel", selected = "dashboard")
#   })
# 
#   # Download the underlying data button
#   output$download_data <- downloadHandler(
#     filename = "shiny_template_underlying_data.csv",
#     content = function(file) {
#       write.csv(dfRevBal, file)
#     }
#   )




  

# Industry by subject crosstab --------------------------------------------

  
# Call function which when proportions have been selected as data type, first creates a table of volumes with selected filters applied
# from which percentages will then be calculated. 
  vols_data_filtered <- reactive({filter_vols_data(input$selectBreakdown, input$selectType, input$selectSSA, input$selectProvision)})
  
  
# Call function which when proportions have been selected as data type, assign a grand total of learners for filtered data to use in 
#calculating percentages  
  total_val <- reactive({calc_learner_total(vols_data_filtered(), input$selectBreakdown, input$selectType)})

  
  
# Call function which when proportions have been selected as data type, divide initial volumes by grand total to create percentage, then format.
# If volumes are selected as data type, output filtered volume data.
  crosstab_data <- reactive({collate_crosstab_data(vols_data_filtered(), total_val(), input$selectBreakdown,input$selectType, 
                                         input$selectSSA, input$selectProvision)})
  
  
# Call function to format data as gt table
  crosstab_gt <- reactive({format_crosstab_gt(crosstab_data(), input$selectType)})
  
  # Output final table  
  output$industry_by_subject_crosstab <- render_gt({crosstab_gt()})
# output$industry_by_subject_crosstab <- renderTable({crosstab_data()})

# Download button for industry by subject data
  output$downloadIndSub <- downloadHandler(
    filename = "industry_by_subject.csv",
    content = function(file) {
      write.csv(crosstab_gt(), file)
    }
  )
  

# Industry by subject title -----------------------------------------------

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
     (tolower(input$selectBreakdown))}
 })
 
 ## Bring together variables as specified above to produce final dynamic title
  output$industry_by_subject_title <- renderText({
    paste(
      "Industry of employment for ", provisioninput(), " learners achieving in " ,  subjectinput(),  " in 2019/20, by ", breakdowninput()
      )
    })

# Subject by industry crosstab --------------------------------------------

  # Call function which when proportions have been selected as data type, first creates a table of volumes with selected filters applied
  # from which percentages will then be calculated.
  vols_data_filtered_subj <- reactive({  filter_vols_data_subj(input$selectBreakdownSubj, input$selectTypeSubj, input$selectIndustry, input$selectProvisionSubj) })
  
  # Call function which when proportions have been selected as data type, assign a grand total of learners for filtered data to use in 
  #calculating percentages  
  total_val_subj <- reactive({calc_learner_total(vols_data_filtered_subj(), input$selectBreakdownSubj, input$selectTypeSubj)})
  
  
  # Call function which when proportions have been selected as data type, divide initial volumes by grand total to create percentage, then format.
  # If volumes are selected as data type, output filtered volume data.
  crosstab_data_subj <- reactive({ collate_crosstab_data_subj(vols_data_filtered_subj(), total_val_subj(), 
                                    input$selectBreakdownSubj, input$selectTypeSubj, input$selectIndustry, input$selectProvisionSubj)})
 # Output crosstab as a gt object and apply formatting
  crosstab_gt_subj <- reactive({ 
    crosstab_data_subj() %>% 
      # Remove anly columns which are entirely NAs
      remove_empty(., which = "cols") %>%
    #  gt(groupname_col = "SSATier1") %>% 
      gt() %>% 
      # Add white borders to all cells
      tab_style(
        style = cell_borders(
          sides = ,
          color = "white",
          weight = px(1.5),
          style = "solid"
        ),
        locations = cells_body(
          columns = everything(),
          rows = everything()
        )
      ) %>%
      # Change font size
      tab_options(table.font.size = 13.5) %>%
      # Make Total column bold
      tab_style(cell_text(weight = "bold"), locations = cells_body(
        columns = Total,
        rows = everything()
      )) %>%
      # Make column headings bold
      tab_style(
        locations = cells_column_labels(columns = everything()),
        style     = list(
          cell_text(weight = "bold")
        )) %>%
      # Fix width of columns
      cols_width(SSATier1 ~ px(275), SSATier2 ~ px(275), everything() ~ px(105)) %>%
      # Format as either percentage or number depending on if volumes or proportions are selected
      {if (input$selectTypeSubj == "SustainedEmploymentPercent")
        fmt_percent(., columns = -SSATier1, decimals = 0)
        else fmt_number(., columns = -SSATier1, decimals = 0)
      } %>%
      # Apply colour coding to columns based on cell value
      data_color(., columns = -c(1:2), direction = "column",
                 palette = "Blues") %>%
      # Add footnotes
      {if (input$selectTypeSubj == "SustainedEmploymentPercent")
        tab_footnote(., "1. Proportions have been calculated using volume figures which have been rounded to the nearest 10")
        else tab_footnote(., "1. Learner volumes have been rounded to the nearest 10")
      } %>%
      tab_footnote(., "2. Where appropriate, data has been suppressed to protect confidentiality") %>%
      tab_footnote(., "3. This data provides is based on the industry in which a learner is employed, but does not tell us about their occupation within the company.") %>% 
      # Rename SSATier1 column
      cols_label(., SSATier1 = 'Sector Subject Area Tier 1', SSATier2 = 'Sector Subject Area Tier 2')
  })
  
  # Output final table  
  output$subject_by_industry_crosstab <- render_gt({crosstab_gt_subj()})
  
  
  # Download button for subject by industry data
  output$downloadSubInd <- downloadHandler(
    filename = "subject_by_industry.csv",
    content = function(file) {
      write.csv(crosstab_gt_subj(), file)
    }
  )
  

# Subject by industry title -----------------------------------------------

  
  ## Reformat provision input - leave blank unless specifying type of provision
  provisioninput <- reactive({
    if(input$selectProvisionSubj == 'All'){
      ""
    }
    else{
      (tolower(input$selectProvisionSubj))}
  })
  
  ## Reformat industry input
  industryinput <- reactive({
    if(input$selectIndustry== 'All'){
      "all industries"
    }
    else{
      (tolower(input$selectIndustry))}
  })
  
  ## Reformat breakdown input
  breakdowninput <- reactive({
    if(input$selectBreakdownSubj == 'AgeGroup'){
      "age group"
    }
    else if(input$selectBreakdownSubj == 'LevelOfLearning'){
      "level of learning"
    }
    else{
      (tolower(input$selectBreakdownSubj))}
  })
  
  ## Bring together variables as specified above to produce final dynamic title
  output$subject_by_industry_title <- renderText({
    paste(
      "Subjects studied by ", provisioninput(), " learners achieving in 19/20 with a sustained employment destination in",industryinput(),  ", by ", breakdowninput()
    )
  })
  
  # Stop app --------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
