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

  
  observe({
    if(input$navlistPanel == 'IndustryBySubject'){
      title_string <- paste(input$navlistPanel,input$selectBreakdown,sep=', ')
    } else if(input$navlistPanel  == 'SubjectByIndustry'){
      title_string <- paste(input$navlistPanel,input$selectBreakdownSubj,sep=', ')
    }
    title_string <- tolower(gsub('(?<=[a-z])(?=[A-Z])', ' ', title_string, perl = TRUE))
    change_window_title(session, paste0(site_title, ' - ', title_string))
  })

  # Homepage links to tabs --------------------------------------------------

  observeEvent(input$link_to_ind_by_subj_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "IndustryBySubject")
  })

  observeEvent(input$link_to_subj_by_ind_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "SubjectByIndustry")
  })


  # Dynamic filter options for SSA Tier 2 - Industry by Subject -------------



  # This code is used to generate dynamic filters, where the SSA Tier 2 options that appear are dependent
  # on SSA Tier 1 which has been selected

  # First create a dataset filtered by the SSATier1 which has been selected
  SSATier1 <- reactive({
    filter(dfInd, SSATier1 == input$selectSSA) %>%
      arrange(SSATier2 != "All", SSATier2) # Ensure All always appears at top of options list
  })

  # Then use this dataset to generate a list of possible SSA Tier 2 options for the SSA Tier 1 selected,
  # and use this to update the dynamic SSA Tier 2 input
  observeEvent(SSATier1(), {
    choices <- unique(SSATier1()$SSATier2)
    updateSelectInput(inputId = "selectSSATier2", choices = choices)
  })

  output$dropdown_label <- renderText({
    paste0("Current selections: ", input$selectProvisionSubj, ", ", input$selectBreakdownSubj)
  })

  # Industry by subject crosstab --------------------------------------------


  # Call function which when proportions have been selected as data type, first creates a table of volumes with selected filters applied
  # from which percentages will then be calculated.
  vols_data_filtered <- reactive({
    filter_vols_data(input$selectBreakdown, input$selectType, input$selectSSA, input$selectProvision, input$selectSSATier2)
  })


  # Call function which when proportions have been selected as data type, assign a grand total of learners for filtered data to use in
  # calculating percentages
  total_val <- reactive({
    calc_learner_total(vols_data_filtered(), input$selectBreakdown, input$selectType)
  })



  # Call function which when proportions have been selected as data type, divide initial volumes by grand total to create percentage, then format.
  # If volumes are selected as data type, output filtered volume data.
  crosstab_data <- reactive({
    collate_crosstab_data(
      vols_data_filtered(), total_val(), input$selectBreakdown, input$selectType,
      input$selectSSA, input$selectProvision, input$selectSSATier2
    )
  })


  # Call function to format data as gt table
  crosstab_gt <- reactive({
    format_crosstab_gt(crosstab_data(), input$selectType)
  })

  # Output final table
  output$industry_by_subject_crosstab <- render_gt({
    crosstab_gt()
  })


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
    if (input$selectProvision == "All") {
      ""
    } else {
      (tolower(input$selectProvision))
    }
  })

  ## Reformat subject input
  subjectinput <- reactive({
    if (input$selectSSA == "All") {
      "all subjects"
    }
    ## If no SSA Tier 2 filter is selected, use SSA Tier 1 to populate title
    else if (input$selectSSATier2 == "All") {
      tolower(input$selectSSA)
    }
    ## If SSA Tier 2 filter is selected, use SSA Tier 2 in title
    else {
      (tolower(input$selectSSATier2))
    }
  })

  ## Reformat breakdown input
  breakdowninput <- reactive({
    if (input$selectBreakdown == "AgeGroup") {
      "age group"
    } else if (input$selectBreakdown == "LevelOfLearning") {
      "level of learning"
    } else {
      (tolower(input$selectBreakdown))
    }
  })

  ## Bring together variables as specified above to produce final dynamic title
  output$industry_by_subject_title <- renderText({
    paste(
      "Industry of employment for ", provisioninput(), " learners achieving in ", subjectinput(), " in 2019/20, by ", breakdowninput()
    )
  })


  # Text for industry by subject page ---------------------------------------

  # Add text as an output otherwise it does not seem to be visible to a screen reader.
  output$industry_by_subject_text <- renderText({
    paste("This table shows the industry of employment for learners with a sustained employment destination in 2020/21, after completing their aim in 2019/20. Please note, this data provides information about the industry of the company that a learner works for but does not tell us about their occupation within the company")
  })

  # Subject by industry crosstab --------------------------------------------

  # Call function which when proportions have been selected as data type, first creates a table of volumes with selected filters applied
  # from which percentages will then be calculated.
  vols_data_filtered_subj <- reactive({
    filter_vols_data_subj(
      input$selectBreakdownSubj, input$selectTypeSubj, input$selectIndustry,
      input$selectProvisionSubj, input$selectSSADetail
    )
  })

  # Call function which when proportions have been selected as data type, assign a grand total of learners for filtered data to use in
  # calculating percentages
  total_val_subj <- reactive({
    calc_learner_total(vols_data_filtered_subj(), input$selectBreakdownSubj, input$selectTypeSubj)
  })


  # Call function which when proportions have been selected as data type, divide initial volumes by grand total to create percentage, then format.
  # If volumes are selected as data type, output filtered volume data.
  crosstab_data_subj <- reactive({
    collate_crosstab_data_subj(
      vols_data_filtered_subj(), total_val_subj(),
      input$selectBreakdownSubj, input$selectTypeSubj, input$selectIndustry, input$selectProvisionSubj, input$selectSSADetail
    )
  })

  # Call function to format data as gt table
  crosstab_gt_subj <- reactive({
    format_gt_subj(crosstab_data_subj(), input$selectTypeSubj, input$selectSSADetail)
  })

  # Output final table
  output$subject_by_industry_crosstab <- render_gt({
    crosstab_gt_subj()
  })


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
    if (input$selectProvisionSubj == "All") {
      ""
    } else {
      (tolower(input$selectProvisionSubj))
    }
  })

  ## Reformat industry input
  industryinput <- reactive({
    if (input$selectIndustry == "All") {
      "all industries"
    } else {
      (tolower(input$selectIndustry))
    }
  })

  ## Reformat breakdown input
  breakdowninput <- reactive({
    if (input$selectBreakdownSubj == "AgeGroup") {
      "age group"
    } else if (input$selectBreakdownSubj == "LevelOfLearning") {
      "level of learning"
    } else {
      (tolower(input$selectBreakdownSubj))
    }
  })

  ## Bring together variables as specified above to produce final dynamic title
  output$subject_by_industry_title <- renderText({
    paste(
      gsub("SustainedEmployment",'',input$selectTypeSubj),"of", 
      provisioninput(), "learners with a sustained employment destination in", industryinput(), 
      "split by subject completed in 19/20 and", breakdowninput()
      #      "Subjects studied by ", provisioninput(), " learners achieving in 19/20 with a sustained employment destination in", industryinput(), ", by ", breakdowninput()
    )
  })


  # Dynamic text for subject by industry page -------------------------------

  # Output text using industry input specified for title
  output$subject_by_industry_text <- renderText({
    paste("This table shows the subject studied by learners with a sustained employment destination in", industryinput(), "in 2020/21, after completing their aim in 2019/20.
        Please note, this data is based on the industry in which a learner is employed but does not tell us about their occupation within the company.")
  })

  # Stop app --------------------------------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
