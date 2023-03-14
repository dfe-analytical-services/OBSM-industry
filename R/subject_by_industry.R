# crosstabs_test <- <- read_ind_data  %>% 
#   filter(SSATier1 == "Business, Administration and Law", SSATier2 == 'All', Provision == 'All',
#          LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All') %>%
#   select(SSATier1, IndustrySection, NumberSustainedEmployment )
# 
SubByIndPanel <- function() {
  tabPanel(value = "subject by industry", "Subject by industry")
  ## Create a static cross-tab
  # crosstab_test <- data_df %>% 
  #   filter(SSATier1 == "Business, Administration and Law", SSATier2 == 'All', Provision == 'All',
  #          LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All') %>%
  #   select(SSATier1, IndustrySection, NumberSustainedEmployment )
  }
