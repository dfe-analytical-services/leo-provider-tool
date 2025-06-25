library(shinytest2)

app <- AppDriver$new(
  name = "leo_provider_app",
  load_timeout = 100000,
  timeout = 20000,
  shiny_args = list(test.mode = TRUE)
)

# Wait until Shiny is not busy for 500ms
app$wait_for_idle(500)

# Declare which inputs and outputs you're tracking
inputs <- c(
  "headlineGeog", "headlineYAG", "navlistPanel",
  "selectProviderGeography", "selectSubject", "selectCharType", "selectCharValue",
  "dashboardPanels"
)

outputs <- c(
  "lineRevBal", "colBenchmark", "boxavgRevBal", "boxpcRevBal"
)

# 1. App load test
test_that("App loads correctly", {
  app$expect_values(
    input = inputs,
    output = outputs
  )
})


# 2. Headline panel test
app$set_inputs(navlistPanel = "headline")
app$set_inputs(headlineGeog = "Arts University Bournemouth")
app$set_inputs(headlineYAG = "1")

test_that("Headline panel updates", {
  app$expect_values(
    input = inputs,
    output = outputs
  )
})


# 3. Dashboard panel - outcomes
app$set_inputs(navlistPanel = "dashboard")
app$set_inputs(dashboardPanels = "outcomes")
app$set_inputs(selectSubject = c("Total", "Chemistry"))

test_that("Dashboard outcomes panel renders", {
  app$expect_values(
    input = inputs,
    output = outputs
  )
})

# 4. Dashboard panel - earnings
app$set_inputs(dashboardPanels = "earnings")
test_that("Dashboard earnings panel renders", {
  app$expect_values(
    input = inputs,
    output = outputs
  )
})


app$stop()
