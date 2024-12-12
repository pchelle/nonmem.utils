library(shiny)
library(shinyWidgets)
library(miniUI)

# TODO
# launch the knit_params_ask to directly generate a report if asked
# rmarkdown::knit_params_ask("C:/WappsHemo/nonmem-utils/dataset-analysis/www/dataset-analysis.qmd")

get_app_repo <- function(app_name) {
  switch(app_name,
    "Dataset" = "pk-analysis",
    "Workbench" = "model-comparison",
    "Model Fit" = "model-fit",
    "Model Parameters" = "model-parameters",
    "Optimization" = "optimization-profile",
    "VPC" = "vpc-analysis",
    "Bootstrap" = "bootstrap-analysis"
  )
}

ui <- miniPage(
  gadgetTitleBar(span(icon("toolbox"), " Nonmem Utils")),
  miniContentPanel(
    pickerInput(
      "app_name",
      span(icon("file-circle-question"), " What do you need to review ?"),
      choices = c("Dataset", "Workbench", "Model Fit", "Model Parameters", "Optimization", "VPC", "Bootstrap"),
      multiple = FALSE,
      options = pickerOptions(iconBase = "fas"),
      choicesOpt = list(
        icon = c(
          "fa-database", "fa-table-list", "fa-chart-line", "fa-chart-simple",
          "fa-arrows-to-dot", "fa-chart-area", "fa-sliders"
        ),
        subtext = c(
          "Check Input PK dataset",
          "Compare Model Runs",
          "Check Model Fit",
          "Check Model Parameters",
          "Check Optimization Profile",
          "Visual Predictive Check",
          "Bootrstrap Analysis"
        )
      )
    ),
    br(),
    actionButton("run", "Run", icon = icon("play")),
    align = "center"
  )
)

server <- function(input, output, session) {
  # Handle the Done button being pressed.
  observeEvent(input$done, {
    stopApp(returnValue = get_app_repo(input$app_name))
  })
  observeEvent(input$run, {
    stopApp(returnValue = get_app_repo(input$app_name))
  })
  # Handle the Cancel button being pressed.
  observeEvent(input$cancel, {
    stopApp(returnValue = NULL)
  })
}

# Run the app on default viewer
app_value <- runGadget(
  ui, server,
  viewer = shiny::dialogViewer("Nonmem Utils", width = 400, height = 400)
)

if (!is.null(app_value)) {
  github_app_value <- paste0(
    "shiny::runGitHub(",
    "repo = \"nonmem-utils\", ",
    "subdir = \"", app_value, "\", ",
    "username = \"pchelle\", ref = \"main\")"
  )
  return(rstudioapi::sendToConsole(github_app_value, execute = TRUE, animate = TRUE))
}
