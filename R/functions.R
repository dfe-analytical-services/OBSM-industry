# Subject by industry functions -------------------------------------------



gt_hyperlink <- function(text, url) {
  htmltools::a(href = url, text, target = "_blank") %>%
    as.character() %>%
    gt::html()
}



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



