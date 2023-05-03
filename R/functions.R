
# General functions -------------------------------------------------------

# Where proportions have been selected as data type, assign a grand total for filtered data to use in calculating percentages
calc_learner_total <- function(data, inputbreakdown, inputtype )({
  if(inputbreakdown == "Gender" &  inputtype == 'SustainedEmploymentPercent') {
    subset(data, Gender == "All") %>%
      {sum(.$NumberSustainedEmployment, na.rm = TRUE)}
  }
  else if(inputbreakdown == "AgeGroup" & inputtype == 'SustainedEmploymentPercent') {
    subset(data, AgeGroup == "All") %>%
      {sum(.$NumberSustainedEmployment, na.rm = TRUE)}
  }
  else if(inputbreakdown == "Ethnicity" & inputtype == 'SustainedEmploymentPercent') {
    subset(data, Ethnicity == "All") %>%
      {sum(.$NumberSustainedEmployment, na.rm = TRUE)}
  }
  else if(inputbreakdown == "LevelOfLearning" & inputtype == 'SustainedEmploymentPercent') {
    subset(data, LevelOfLearning == "All") %>%
      {sum(.$NumberSustainedEmployment, na.rm = TRUE)}
  }
})


# Format gt data

# 
# crosstab_gt_subj <- reactive({ 
#   crosstab_data_subj() %>% 
#     # Remove anly columns which are entirely NAs
#     remove_empty(., which = "cols") %>%
#     gt() %>%
#     # Add white borders to all cells
#     tab_style(
#       style = cell_borders(
#         sides = ,
#         color = "white",
#         weight = px(1.5),
#         style = "solid"
#       ),
#       locations = cells_body(
#         columns = everything(),
#         rows = everything()
#       )
#     ) %>%
#     # Change font size
#     tab_options(table.font.size = 13.5) %>%
#     # Make Total column bold
#     tab_style(cell_text(weight = "bold"), locations = cells_body(
#       columns = Total,
#       rows = everything()
#     )) %>%
#     # Make column headings bold
#     tab_style(
#       locations = cells_column_labels(columns = everything()),
#       style     = list(
#         cell_text(weight = "bold")
#       )) %>%
#     
#     # Fix width of columns
#     cols_width(Industry ~ px(275), everything() ~ px(105)) %>%
#     # Format as either percentage or number depending on if volumes or proportions are selected
#     {if (input$selectType == "SustainedEmploymentPercent")
#       fmt_percent(., columns = -Industry, decimals = 0)
#       else fmt_number(., columns = -Industry, decimals = 0)
#     } %>%
#     # Apply colour coding to columns based on cell value
#     data_color(., columns = -Industry, direction = "column",
#                palette = "Blues") %>%
#     # Add footnotes
#     {if (input$selectType == "SustainedEmploymentPercent")
#       tab_footnote(., "1. Proportions have been calculated using volume figures which have been rounded to the nearest 10")
#       else tab_footnote(., "1. Learner volumes have been rounded to the nearest 10")
#     } %>%
#     tab_footnote(., "2. Where appropriate, data has been suppressed to protect confidentiality") %>%
#     tab_footnote(., "3. This data provides information about the industry of the company that a learner works for, but does not tell us about their occupation within the company.")
#   
# 






# Industry by subject functions -------------------------------------------

# Where proportions have been selected as data type, need to first create table of volumes from which perecentages will be calculated

filter_vols_data <-  function(inputbreakdown, inputtype, inputSSA, inputprovision)({

  if(inputbreakdown == 'Gender' & inputtype == 'SustainedEmploymentPercent'){
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', AgeGroup == 'All', Ethnicity == 'All',
             Industry != 'All') %>%
      select(Industry, Gender,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', Ethnicity == 'All',
             Industry != 'All') %>%
      select(Industry, AgeGroup,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'Ethnicity' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, Ethnicity,  NumberSustainedEmployment) 
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision ==  inputprovision,
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, LevelOfLearning,  NumberSustainedEmployment)
  }
  else{
    dfInd %>%
      filter(SSATier1 == 'All', SSATier2 == 'All', Provision ==  'All',
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, NumberSustainedEmployment)
  }
})



# # Where proportions have been selected as data type, divide initial volumes by grand total to create percentage, then format
collate_crosstab_data <- function(data, totaldata, inputbreakdown, inputtype, inputSSA, inputprovision)({
  if(inputbreakdown == "Gender" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(Gender, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  else if(inputbreakdown == "AgeGroup" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(AgeGroup, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  else if(inputbreakdown == "Ethnicity" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(Ethnicity, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  else if(inputbreakdown == "LevelOfLearning" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(LevelOfLearning, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }


  # Where volumes have been selected as data type, select totals for all other options and then format
  else  if(inputbreakdown == 'Ethnicity' & inputtype == 'NumberSustainedEmployment'){
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
       relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  else if(inputbreakdown == 'Gender'& inputtype == 'NumberSustainedEmployment') {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, Gender, NumberSustainedEmployment)     %>%
      spread(Gender, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'NumberSustainedEmployment') {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, LevelOfLearning, NumberSustainedEmployment)     %>%
      spread(LevelOfLearning, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'NumberSustainedEmployment') {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', LevelOfLearning == 'All',
             Industry != 'All') %>%
      select(Industry, AgeGroup, NumberSustainedEmployment)     %>%
      spread(AgeGroup, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>% 
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame()
  }
  #If no breakdowns are selected show summary data for all
  else {dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == 'All', Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All',
             Industry != 'All') %>%
      select(Industry, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame() }
})


# Subject by industry -------------------------------------------

# Where proportions have been selected as data type, need to first create table of volumes from which perecentages will be calculated
filter_vols_data_subj <-  function(inputbreakdown, inputtype, inputindustry, inputprovision)({
  
  if(inputbreakdown == 'Gender' & inputtype == 'SustainedEmploymentPercent'){
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', AgeGroup == 'All', Ethnicity == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, Gender,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', Ethnicity == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, AgeGroup,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'Ethnicity' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, Ethnicity,  NumberSustainedEmployment) 
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision ==  inputprovision,
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, LevelOfLearning,  NumberSustainedEmployment)
  }
  else{
    dfInd %>%
      filter(Industry == 'All', SSATier2 == 'All', Provision ==  'All',
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, NumberSustainedEmployment)
  }
})

collate_crosstab_data_subj <- function(data, totaldata, inputbreakdown, inputtype, inputindustry, inputprovision)({
  
  
  
  
  
  if(inputbreakdown == "Gender" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata ) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(Gender, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == "AgeGroup" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(AgeGroup, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == "Ethnicity" & inputtype == 'SustainedEmploymentPercent') {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(Ethnicity, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if (inputbreakdown == "LevelOfLearning" & inputtype == 'SustainedEmploymentPercent')
  {
    data %>%
      mutate(PercentSustainedEmployment = NumberSustainedEmployment/totaldata) %>%
      mutate(PercentSustainedEmployment = round(PercentSustainedEmployment, digits = 2)) %>%
      select(-NumberSustainedEmployment) %>%
      spread(LevelOfLearning, PercentSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  
  
  
  # Where volumes have been selected as data type, select totals for all other options and then format
  else  if(inputbreakdown == 'Ethnicity' & inputtype == 'NumberSustainedEmployment'){
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'Gender'& inputtype == 'NumberSustainedEmployment') {
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, Gender, NumberSustainedEmployment)     %>%
      spread(Gender, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'NumberSustainedEmployment') {
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, LevelOfLearning, NumberSustainedEmployment)     %>%
      spread(LevelOfLearning, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'NumberSustainedEmployment') {
    dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', LevelOfLearning == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, AgeGroup, NumberSustainedEmployment)     %>%
      spread(AgeGroup, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  #If no breakdowns are selected show summary data for all
  else {dfInd %>%
      filter(Industry == inputindustry, SSATier2 == 'All', Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame() }
  
  
  
  
  
})