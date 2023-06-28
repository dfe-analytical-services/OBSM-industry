app <- ShinyDriver$new("../../", loadTimeout = 1.e5)
app$snapshotInit("UI_tests", screenshot = FALSE)

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

app$snapshot(list(input = listInputs, output = listOutputs))
app$setInputs(link_to_ind_by_subj_tab = "click")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectType = "SustainedEmploymentPercent")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectProvision = "Apprenticeship")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectSSA = "Agriculture, Horticulture and Animal Care")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectSSATier2 = "Agriculture")
app$setInputs(selectSSATier2 = "Animal Care and Veterinary Science")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectBreakdown = "Ethnicity")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectSSA = "All")
app$setInputs(selectProvision = "All")
app$setInputs(selectType = "NumberSustainedEmployment")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectBreakdown = "LevelOfLearning")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectBreakdown = "Gender")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(navlistPanel = "SubjectByIndustry")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectTypeSubj = "SustainedEmploymentPercent")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectIndustry = "Accommodation and food service activities")
app$setInputs(selectSSADetail = "SSATier2")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectProvisionSubj = "Apprenticeship")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectBreakdownSubj = "Ethnicity")
app$setInputs(selectIndustry = "All")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectBreakdownSubj = "LevelOfLearning")
app$snapshot(list(input = listInputs, output = listOutputs))

app$setInputs(selectBreakdownSubj = "Gender")
app$snapshot(list(input = listInputs, output = listOutputs))
