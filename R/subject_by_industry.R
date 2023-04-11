
col_formats <- function(data, footer_data, cellfunc, minWidth = NULL) {
  max <- data %>%
    ungroup() %>%
    select(-c(group_name, SECTIONNAME)) %>%
    mutate_all(funs(ifelse(. < 0, NA, .)))
  numeric_cols <- names(max)
  numeric_cols_def <- list()
  numeric_cols_def_nested <- list()
  for (column in numeric_cols) {
    script <- paste("
            // source: https://glin.github.io/reactable/articles/examples.html#grouped-cell-rendering-1
            function(rowInfo) {
              // source: https://stackoverflow.com/a/44134328/4856719
              function hslToHex(h, s, l) {
                l /= 100;
                const a = s * Math.min(l, 1 - l) / 100;
                const f = n => {
                  const k = (n + h / 30) % 12;
                  const color = l - a * Math.max(Math.min(k - 3, 9 - k, 1), -1);
                  return Math.round(255 * color).toString(16).padStart(2, '0');
                };
                return `#${f(0)}${f(8)}${f(4)}`;
              }
              var value = rowInfo.row['", column, "']
              var max = ", max(max, na.rm = TRUE), "
              var min = ", min(max, na.rm = TRUE), "
              // pct_value = (value - min) * 100 / (max - min)
              pct_value = (Math.min(value, max) - min) * 100 / (max - min)
              // If value equals 0, x, or c, set background to white.
              if (value < 0.001 || isNaN(value)) {
                var color = '#000000'
                var bg = '#FFFFFF'
              } else {
                var color = '#000000'
                var bg = hslToHex(209, 59, 100 - pct_value / 2)
              }
              return { color: color, backgroundColor: bg}
          }", sep = "")
    
    numeric_cols_def_nested[column] <- list(colDef(
      na = "x", style = JS(script), cell = cellfunc,
      minWidth = minWidth
    ))
    
    numeric_cols_def[column] <- list(colDef(
      na = "x", style = JS(script), cell = cellfunc,
      footer = format(round_any(sum(footer_data[column]), 5), big.mark = ",", scientific = FALSE, na.m = T),
      minWidth = minWidth
    ))
  }
  return(list(numeric_cols = numeric_cols, numeric_cols_def = numeric_cols_def, numeric_cols_def_nested = numeric_cols_def_nested, script = script))
}






###########  PAGE LAYOUT --------------


subject_by_industry_panel <- function(){tabPanel(
  tags$div(title = "This section is useful if you want to understand which industries your subject of study can lead to.", "Subject by industry"),
  value = "subjectByIndustry",
  
## Add sidebar panel ----

 # sidebarLayout(
    sidebarPanel(
      width = 2,
    
  # Instructions for users to appear at top of sidebar
      #helpText("Create your own table by selecting from the drop down boxes below."),
  
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
   choices = list( "Age Group" = "AgeGroup",
                   "Ethnicity" = "Ethnicity",
                   "Gender" = "Gender",
                  "Level of Learning" = "LevelOfLearning"
                  )
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
  
#paste("Add text here"),
#h2("Subject by industry"),
textOutput("subject_by_industry_title"),
#textOutput("crosstab_title"),
tableOutput("subject_by_industry_crosstab")

# dfInd %>%
#   filter(SSATier1 == "Business, Administration and Law", SSATier2 == 'All', Provision == 'All',
#          LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All') %>%
#   select(SSATier1, IndustrySection, NumberSustainedEmployment )


))}


#################### CROSS-TABS ---- #######################################
## Main panel =========================================================


# subj_ind_crosstab <-     dfInd %>% 
#   filter(SSATier1 == input$selectSSA, SSATier2 == 'All', Provision == input$selectProvision,
#          LevelOfLearning == 'All', AppType == 'All', Gender == 'All', AgeGroup == 'All', Ethnicity == 'All', IndustrySection != 'All') %>%
#   select(IndustrySection, NumberSustainedEmployment, SustainedEmploymentPercent)     %>%  
#   arrange(desc(NumberSustainedEmployment))


