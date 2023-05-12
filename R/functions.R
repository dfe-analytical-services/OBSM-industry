
# GENERAL FUNCTIONS -------------------------------------------------------


## Calculate learner total -------------------------------------------------
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


# INDUSTRY BY SUBJECT FUNCTIONS -------------------------------------------


## Filter volumes data -----------------------------------------------------

# Where proportions have been selected as data type, need to first create table of volumes from which perecentages will be calculated
filter_vols_data <-  function(inputbreakdown, inputtype, inputSSA, inputprovision, inputSSATier2)({

  if(inputbreakdown == 'Gender' & inputtype == 'SustainedEmploymentPercent'){
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', AgeGroup == 'All', Ethnicity == 'All',
             Industry != 'All') %>%
      select(Industry, Gender,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', Ethnicity == 'All',
             Industry != 'All') %>%
      select(Industry, AgeGroup,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'Ethnicity' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             Industry != 'All') %>%
      select(Industry, Ethnicity,  NumberSustainedEmployment) 
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'SustainedEmploymentPercent')  {
    dfInd %>%
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision ==  inputprovision,
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



## Collate crosstab data ---------------------------------------------------

# Where proportions have been selected as data type, divide initial volumes by grand total to create percentage, then format
collate_crosstab_data <- function(data, totaldata, inputbreakdown, inputtype, inputSSA, inputprovision, inputSSATier2)({
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
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision == inputprovision,
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
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision == inputprovision,
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
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision == inputprovision,
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
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision == inputprovision,
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
      filter(SSATier1 == inputSSA, SSATier2 == inputSSATier2, Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All',
             Industry != 'All') %>%
      select(Industry, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>% 
      as.data.frame() }
})





## Format as gt table ------------------------------------------------------

#Format data into gt table
format_crosstab_gt <- function(data, inputtype)({
  data %>% 
    # Remove anly columns which are entirely NAs
    remove_empty(., which = "cols") %>% 
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
  cols_width(Industry ~ px(275), everything() ~ px(105))  %>%
    # Format as either percentage or number depending on if volumes or proportions are selected
    {if (inputtype == "SustainedEmploymentPercent")
      fmt_percent(., columns = -Industry, decimals = 0)
      else fmt_number(., columns = -Industry, decimals = 0)
    } %>%
    # Apply colour coding to columns based on cell value
    data_color(., columns = -Industry, direction = "column",
               palette = "Blues") %>%
    # Add footnotes
    {if (inputtype == "SustainedEmploymentPercent")
      tab_footnote(., "1. Proportions have been calculated using volume figures which have been rounded to the nearest 10")
      else tab_footnote(., "1. Learner volumes have been rounded to the nearest 10")
    } %>%
    tab_footnote(., "2. Where appropriate, data has been suppressed to protect confidentiality")


  
  
})


















# SUBJECT BY INDUSTRY FUNCTIONS --------------------------------------------


## Filter vols data - subject by industry ----------------------------------

# Where proportions have been selected as data type, need to first create table of volumes from which perecentages will be calculated
filter_vols_data_subj <-  function(inputbreakdown, inputtype, inputindustry, inputprovision, inputdetail)({
  
  if(inputbreakdown == 'Gender' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier2'){
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', AgeGroup == 'All', Ethnicity == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, Gender,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'Gender' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier1'){
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', AgeGroup == 'All', Ethnicity == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, Gender,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier2')  {
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', Ethnicity == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, AgeGroup,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier1')  {
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', Ethnicity == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, AgeGroup,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'Ethnicity' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier2')  {
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, Ethnicity,  NumberSustainedEmployment) 
  }
  else if(inputbreakdown == 'Ethnicity' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier1')  {
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier1 == 'All') %>%
      select(SSATier1, Ethnicity,  NumberSustainedEmployment) 
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier2')  {
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, LevelOfLearning,  NumberSustainedEmployment)
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'SustainedEmploymentPercent' & inputdetail == 'SSATier1')  {
    dfInd %>%
      filter(Industry == inputindustry, Provision ==  inputprovision,
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, LevelOfLearning,  NumberSustainedEmployment)
  }
  else{
    dfInd %>%
      filter(Industry == 'All', Provision ==  'All',
             Ethnicity == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All') %>%
      select(SSATier1, SSATier2, NumberSustainedEmployment)
  }
})



## Collate crosstab data - subject by industry -----------------------------

# Function to collate data for subject by industry crosstab
collate_crosstab_data_subj <- function(data, totaldata, inputbreakdown, inputtype, inputindustry, inputprovision, inputdetail)({
  
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
  else  if(inputbreakdown == 'Ethnicity' & inputtype == 'NumberSustainedEmployment' & inputdetail == 'SSATier2'){
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(SSATier1)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else  if(inputbreakdown == 'Ethnicity' & inputtype == 'NumberSustainedEmployment'  & inputdetail == 'SSATier1'){
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, Ethnicity, NumberSustainedEmployment)     %>%
      spread(Ethnicity, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(SSATier1)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'Gender'& inputtype == 'NumberSustainedEmployment'  & inputdetail == 'SSATier2') {
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, Gender, NumberSustainedEmployment)     %>%
      spread(Gender, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'Gender'& inputtype == 'NumberSustainedEmployment'  & inputdetail == 'SSATier1') {
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, Gender, NumberSustainedEmployment)     %>%
      spread(Gender, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'NumberSustainedEmployment' & inputdetail == 'SSATier2') {
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, LevelOfLearning, NumberSustainedEmployment)     %>%
      spread(LevelOfLearning, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'LevelOfLearning' & inputtype == 'NumberSustainedEmployment' & inputdetail == 'SSATier1') {
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', AgeGroup == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, LevelOfLearning, NumberSustainedEmployment)     %>%
      spread(LevelOfLearning, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'NumberSustainedEmployment' & inputdetail == 'SSATier2') 
    {
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', LevelOfLearning == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2, AgeGroup, NumberSustainedEmployment)     %>%
      spread(AgeGroup, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  else if(inputbreakdown == 'AgeGroup' & inputtype == 'NumberSustainedEmployment' & inputdetail == 'SSATier1') {
    dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             Gender == 'All', AppType == 'All', Ethnicity == 'All', LevelOfLearning == 'All',
             SSATier1 != 'All', SSATier2 == 'All') %>%
      select(SSATier1, AgeGroup, NumberSustainedEmployment)     %>%
      spread(AgeGroup, NumberSustainedEmployment) %>%
      relocate(All, .after = last_col()) %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame()
  }
  #If no breakdowns are selected show summary data for all
  else {dfInd %>%
      filter(Industry == inputindustry, Provision == inputprovision,
             LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All',
             SSATier1 != 'All', SSATier2 != 'All') %>%
      select(SSATier1, SSATier2,  NumberSustainedEmployment)     %>%
      arrange(desc(All)) %>%
      rename(Total = All) %>%
      as.data.frame() }
  
  
  
  
  
})


## Format data as gt table - subject by industry --------------------------------------------


# Function to format data as gt table - for when SSA Tier 2 is selected
format_gt_subj <- function(data, inputtype, inputdetail)({ 
  data %>% 
    # Remove any columns which are entirely NAs
    remove_empty(., which = "cols") %>%
   # gt(groupname_col = "SSATier1") %>% 
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
    # Add footnotes
    {if (inputtype == "SustainedEmploymentPercent")
      tab_footnote(., "1. Proportions have been calculated using volume figures which have been rounded to the nearest 10")
      else tab_footnote(., "1. Learner volumes have been rounded to the nearest 10")
    } %>%
    tab_footnote(., "2. Where appropriate, data has been suppressed to protect confidentiality") %>%
    # tab_footnote(., "3. This data is based on the industry in which a learner is employed, but does not tell us about their occupation within the company.") %>%
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
    # Format as either percentage or number depending on if volumes or proportions are selected
    {if (inputtype == "SustainedEmploymentPercent")
      fmt_percent(., columns = -SSATier1, decimals = 0)
      else fmt_number(., columns = -SSATier1, decimals = 0)
    } %>%
    

  # Apply next steps only if more detailed SSA Tier 2 info is selected
    {if (inputdetail == "SSATier2")
    # Fix width of columns    
      cols_width(., SSATier1 ~ px(225), SSATier2 ~ px(225), everything() ~ px(105)) %>%
    # Apply colour coding based on cell value  
      data_color(., columns = -c(1:2), direction = "column",
                 palette = "Blues") %>% 
    # Relabel SSA columns    
      cols_label(., SSATier1 = 'Sector Subject Area Tier 1', SSATier2 = 'Sector Subject Area Tier 2') 
      

   # Otherwise apply next steps if SSA Tier 1 is selected   
    else     
    # Fix width of columns    
    cols_width(., SSATier1 ~ px(275), everything() ~ px(105)) %>%
    # Apply colour coding based on cell value  
    data_color(., columns = -SSATier1, direction = "column",
                 palette = "Blues") %>% 
    # Relabel SSA column    
    cols_label(., SSATier1 = 'Sector Subject Area Tier 1')}
      
})    
    

# DASHBOARD TEXT FUNCTIONS ------------------------------------------------



# Introduction text -------------------------------------------------------

intro_text <- function() {
  div(
    h2("Introduction"),
    " SIC (UK Standard Industrial Classification of economic activities) codes have now been joined to LEO (Longitudinal Education Outcomes) data using the IDBR (Inter-Departmental Business Register) data. 
      This dashboard has been designed to allow users to explore this data and the relationship between subject studied and industry of employment after learning.",
    # a(
    #   href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-outcome-based-success-measures",
    #   "official statistics publication on Further Education Outcome Based Success Measures",
    #   .noWS = c("after")
    # ),
    # ".", br(), br(),
    # "This dashboard has been produced by the Department for Education to support the aims of the",
    # a(
    #   href = "https://www.gov.uk/government/groups/unit-for-future-skills",
    #   "Unit for Future Skills.", .noWS = c("after")
    # ),
  )
}

# Text for industry by subject table --------------------------------------

  ind_by_subj_text <- function() {
    
    div(
      "This table shows which industries learners from the selected subject area go on to work in.",
      "The following breakdowns are currently available:",
      br(),
      tags$ul(
        tags$li("Age group"),
        tags$li("Ethnicity"),
        tags$li("Level of learning"),
        tags$li("Sex"),
       )
    )
    
    
  }  
    

# Text for subject by industry table --------------------------------------
 
  subj_by_ind_text <- function() {
    
    div(
      "This table shows which subjects learners from the selected industry studied.
      The following breakdowns are currently available:",
      br(),
      tags$ul(
        tags$li("Age group"),
        tags$li("Ethnicity"),
        tags$li("Level of learning"),
        tags$li("Sex"),
      )
    )
  }


# Text for guidance box ---------------------------------------------------


  
guidance_text <- function() {
  
  div(
    class = "panel-body",
    h3("Learners in scope"),
    ("This dashboard covers learners who achieved apprenticeships, adult (19+) FE and Skills learners, and
      learners who completed a traineeship in 2019/20, and moved into sustained employment in the following academic year (2020/21)."),
    h3("Official Statistics"),
   
   "This dashboard uses data from the",
    a(
      href = "https://explore-education-statistics.service.gov.uk/find-statistics/further-education-outcome-based-success-measures",
      "official statistics publication on Further Education Outcome Based Success Measures",
      .noWS = c("after")
    ), 
    ", which present statistics on the employment, earnings and learning outcomes of further education learners.",
    h3("Definitions"),
    h4("Inter-departmental Business Register (IDBR)"),
    "IDBR data is a comprehensive list of UK businesses used by government for statistical purposes.
      The two main sources of input are Value Added Tax (VAT) and Pay As You Earn (PAYE) records from HMRC. Additional information comes from Companies House, Dun and Bradstreet and ONS business surveys.",
    h4("Longitudinal Education Outcomes (LEO)"),
    "The data uses the Longitudinal Education Outcomes (LEO) dataset, which looks at how learners move through education and into the labour market by bringing together schools,
    further and higher education information from the Department for Education (DfE), employment information from HM Revenue and Customs (HMRC), and benefit histories from the Department of Work and Pensions (DWP).",
    h4("Sector Subject Area (SSA)"),
    "All qualifications are categorised into Sector Subject Areas. This",
   a(
     href = "https://www.gov.uk/government/publications/types-of-regulated-qualifications/qualification-descriptions#sector",
     "qualifications guidance",
     .noWS = c("after")
   ), 
     " outlines these classifications at both Tier 1 (high level) and Tier 2 (low level).",
    h4("Standard Industrial Classification (SIC)"),
    "The UK Standard Industrial Classification (SIC) of economic activities is used to classify businesses by the type of activity they do. For 
      more information see the", 
   a(
        href = "https://onsdigital.github.io/dp-classification-tools/standard-industrial-classification/ONS_SIC_hierarchy_view.html",
        "Office for National Statistics interactive SIC hierarchy",
        .noWS = c("after")
      ),
   h4("Sustained employment"),
   "To be counted in a sustained employment, learners have to be recorded as being in employment for a 6 month period (October to March) in the year following study, or have submitted a self-assessment record.
   Further information is available in the",
   a(
     href = "https://explore-education-statistics.service.gov.uk/methodology/further-education-outcome-based-success-measures",
     "FE Outcome Based Success Measures methodology note",
     .noWS = c("after")
   ),
   
  )
   
}






