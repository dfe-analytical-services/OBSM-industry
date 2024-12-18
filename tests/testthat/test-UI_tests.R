library(shinytest2)

test_that("Migrated shinytest test: UI_tests.R", {
  app <- AppDriver$new(
    height = 846,
    width = 1445,
    load_timeout = 120 * 1000,
    timeout = 20 * 1000,
    wait = TRUE,
    expect_values_screenshot_args = FALSE
  )

  listInputs <- c(
    "navlistPanel",
    "selectBreakdown",
    "selectBreakdownSubj",
    "selectIndustry",
    "selectProvision",
    "selectProvisionSubj",
    "selectSSA",
    "selectSSADetail",
    "selectSSATier2",
    "selectType",
    "selectTypeSubj"
  )

  listOutputs <- c(
    "industry_by_subject_text",
    "subject_by_industry_text"
  )

  app$expect_values(input = listInputs, output = listOutputs)
  app$set_inputs(link_to_ind_by_subj_tab = "click")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectType = "SustainedEmploymentPercent")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectProvision = "Apprenticeship")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectSSA = "Agriculture, Horticulture and Animal Care")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectSSATier2 = "Agriculture")
  app$set_inputs(selectSSATier2 = "Animal Care and Veterinary Science")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectBreakdown = "Ethnicity")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectSSA = "All")
  app$set_inputs(selectProvision = "All")
  app$set_inputs(selectType = "NumberSustainedEmployment")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectBreakdown = "LevelOfLearning")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectBreakdown = "Gender")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(navlistPanel = "SubjectByIndustry")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectTypeSubj = "SustainedEmploymentPercent")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectIndustry = "Accommodation and food service activities")
  app$set_inputs(selectSSADetail = "SSATier2")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectProvisionSubj = "Apprenticeship")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectBreakdownSubj = "Ethnicity")
  app$set_inputs(selectIndustry = "All")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectBreakdownSubj = "LevelOfLearning")
  app$expect_values(input = listInputs, output = listOutputs)

  app$set_inputs(selectBreakdownSubj = "Gender")
  app$expect_values(input = listInputs, output = listOutputs)
})
