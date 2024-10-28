#' @title nonmem-review
#' @description A shiny app to review nonmem model results saved in .tab files

library(dplyr, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(mrgsolve, warn.conflicts = FALSE)

copyToInput <- function(currentInput, value) {
  if (any(is.na(currentInput), currentInput %in% "")) {
    return(value)
  }
  return(paste0(currentInput, ", ", value))
}

xBreaks <- rep(c(1, 5), 13) * 10^rep(-6:6, each = 2)
yBreaks <- rep(c(1, 2, 5), 13) * 10^rep(-6:6, each = 3)

ui <- dashboardPage(
  dashboardHeader(title = "Nonmem Model Review"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Get Started", tabName = "home", icon = icon("home")),
      menuItem(" Parameters distribution", tabName = "distribution", icon = icon("chart-simple")),
      menuItem(" Parameters correlation", tabName = "correlation", icon = icon("chart-line")),
      menuItem(" Individual Review", tabName = "indReview", icon = icon("person"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        box(
          title = span(icon("home"), "Get Started"),
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          fileInput(inputId = "newfile", label = "Upload Nonmem .tab file", accept = ".tab")
        ),
        box(
          title = span(icon("database"), "Dataset"),
          width = 12,
          solidHeader = TRUE,
          status = "success",
          DT::dataTableOutput("dataTable")
        )
      ),
      # Distribution Tab display distribution of a variable
      tabItem(
        tabName = "distribution",
        box(
          title = span(icon("gear"), "Settings"),
          solidHeader = TRUE,
          status = "warning",
          width = 4,
          selectInput("distributionVariableName", label = "Select Variable", choices = NA),
          selectInput("distributionGroupVariableName", label = "Select Grouping Variable", NA),
          sliderInput("bins", "Number of bins", value = 7, min = 1, max = 50),
          selectInput("distributionLine", label = "Display Statistic", choices = c("none", "mean", "median")),
          textInput("distributionSelection", "Data Selection", value = ""),
          helpText("The buttons below allows you to copy/paste the ", code("code"), " below into the data selection"),
          # html code to include a table with 3 columns
          actionButton(
            "distCopyFirstRowPerID",
            span("First row per ID: ", code("c(1, diff(ID)) != 0")),
            icon = icon("copy")
          ),
          br(),
          actionButton(
            "distCopyObs",
            span("Observations only: ", code("MDV == 0")),
            icon = icon("copy")
          ),
          br(),
          actionButton(
            "distCopyDose",
            span("Doses only: ", code("AMT > 0")),
            icon = icon("copy")
          )
        ),
        box(
          title = span(icon("chart-simple"), "Distribution"),
          solidHeader = TRUE,
          status = "primary",
          width = 8,
          plotlyOutput("distributionPlot"),
          br(),
          h3("Summary Statistics"),
          tableOutput("distributionTable"),
          align = "center"
        )
      ),
      tabItem(
        tabName = "correlation",
        box(
          title = span(icon("gear"), "Settings"),
          solidHeader = TRUE,
          status = "warning",
          width = 4,
          selectInput("yVariableName", label = "Select Y Variable", choices = NA),
          selectInput("xVariableName", label = "Select X Variable", choices = NA),
          selectInput("groupVariableName", label = "Select Grouping Variable", choices = NA),
          selectInput("xCorrelationScale", label = "X-Scale", choices = c("lin", "log")),
          selectInput("yCorrelationScale", label = "Y-Scale", choices = c("lin", "log")),
          selectInput("correlationSmoother", label = "Regression", choices = c("none", "loess", "lm")),
          selectInput("correlationLine", label = "Display Line", choices = c("none", "identity", "origin")),
          textInput("correlationSelection", "Data Selection", value = ""),
          helpText("The buttons below allows you to copy/paste the ", code("code"), " below into the data selection"),
          # html code to include a table with 3 columns
          actionButton(
            "corCopyFirstRowPerID",
            span("First row per ID: ", code("c(1, diff(ID)) != 0")),
            icon = icon("copy")
          ),
          br(),
          actionButton(
            "corCopyObs",
            span("Observations only: ", code("MDV == 0")),
            icon = icon("copy")
          ),
          br(),
          actionButton(
            "corCopyDose",
            span("Doses only: ", code("AMT > 0")),
            icon = icon("copy")
          )
        ),
        box(
          title = span(icon("chart-line"), "Correlation"),
          solidHeader = TRUE,
          status = "primary",
          width = 8,
          plotlyOutput("correlationPlot"),
          br(),
          h3("Correlation Coefficients"),
          tableOutput("correlationTable"),
          align = "center"
        )
      ),
      tabItem(
        tabName = "indReview",
        box(
          title = span(icon("gear"), "Settings"),
          solidHeader = TRUE,
          status = "warning",
          width = 4,
          selectInput("id", label = "Patient ID:", choices = NA),
          selectInput("indGroup", label = "Grouping:", choices = NA),
          selectInput("scale", label = "Scale", choices = c("linear", "log")),
          selectInput("dv", label = "Concentration", choices = c("Total", "Adjusted"))
        ),
        tabBox(
          width = 8,
          tabPanel(
            "Parameters",
            tableOutput("pkParameters"),
            style = "overflow-x: auto;",
            icon = icon("table")
          ),
          tabPanel(
            "Time Profile",
            plotlyOutput("timeProfile"),
            icon = icon("chart-line")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Define reactive values from loading the data
  inputData <- reactiveValues(
    pkData = NULL,
    variableNames = NULL
  )
  observeEvent(input$newfile, {
    inputData$pkData <- read.table(file = input$newfile$datapath, header = TRUE, skip = 1)
    inputData$variableNames <- names(inputData$pkData)
    # Update all the selectInput with the new variable names
    updateSelectInput(session, "distributionVariableName", choices = inputData$variableNames)
    updateSelectInput(session, "distributionGroupVariableName", choices = c("none", inputData$variableNames))
    updateSelectInput(session, "yVariableName", choices = inputData$variableNames)
    updateSelectInput(session, "xVariableName", choices = inputData$variableNames)
    updateSelectInput(session, "groupVariableName", choices = c("none", inputData$variableNames))
    updateSelectInput(session, "id", choices = unique(inputData$pkData$ID), selected = inputData$pkData$ID[1])
    updateSelectInput(session, "indGroup", choices = c("none", inputData$variableNames))
  })
  observeEvent(input$distCopyFirstRowPerID, {
    updateTextInput(
      session,
      "distributionSelection",
      value = copyToInput(input$distributionSelection, "c(1, diff(ID)) != 0")
    )
    # writeClipboard("c(1, diff(ID)) != 0")
  })
  observeEvent(input$corCopyFirstRowPerID, {
    updateTextInput(
      session,
      "correlationSelection",
      value = copyToInput(input$correlationSelection, "c(1, diff(ID)) != 0")
    )
  })
  observeEvent(input$distCopyObs, {
    updateTextInput(
      session,
      "distributionSelection",
      value = copyToInput(input$distributionSelection, "MDV == 0")
    )
  })
  observeEvent(input$corCopyObs, {
    updateTextInput(
      session,
      "correlationSelection",
      value = copyToInput(input$correlationSelection, "MDV == 0")
    )
  })
  observeEvent(input$distCopyDose, {
    updateTextInput(
      session,
      "distributionSelection",
      value = copyToInput(input$distributionSelection, "AMT > 0")
    )
  })
  observeEvent(input$corCopyDose, {
    updateTextInput(
      session,
      "correlationSelection",
      value = copyToInput(input$correlationSelection, "AMT > 0")
    )
  })

  output$dataTable <- DT::renderDataTable({
    if (is.null(inputData$pkData)) {
      return(data.frame("No data loaded" = NA, check.names = FALSE))
    }
    return(DT::datatable(inputData$pkData, options = list(scrollX = TRUE)))
  })

  output$correlationPlot <- renderPlotly({
    if (is.null(inputData$pkData)) {
      return()
    }
    eval(parse(text = paste0("data <- inputData$pkData |> filter(", input$correlationSelection, ")")))

    # In case name was not output
    if (!"ID" %in% names(data)) {
      data$ID <- ""
    }

    corPlot <- ggplot(
      data = data,
      mapping = aes(
        x = .data[[input$xVariableName]],
        y = .data[[input$yVariableName]],
        color = switch(input$groupVariableName,
          "none" = factor("data"),
          as.factor(.data[[input$groupVariableName]])
        ),
        group = switch(input$groupVariableName,
          "none" = factor("data"),
          as.factor(.data[[input$groupVariableName]])
        ),
        text = paste0(
          "ID: ", .data$ID,
          switch(input$groupVariableName,
            "none" = "",
            paste0("\n", input$groupVariableName, ": ", .data[[input$groupVariableName]])
          )
        )
      )
    ) +
      theme_bw(base_size = 12)

    if (input$correlationLine %in% "identity") {
      corPlot <- corPlot +
        geom_abline(intercept = 0, slope = 1, linetype = "longdash")
    }
    if (input$correlationLine %in% "origin") {
      corPlot <- corPlot +
        geom_hline(yintercept = 0, linetype = "longdash")
    }

    if (input$correlationSmoother %in% c("loess", "lm")) {
      corPlot <- corPlot +
        geom_smooth(
          formula = y ~ x,
          method = input$correlationSmoother
        )
    }
    corPlot <- corPlot +
      geom_point() +
      scale_colour_viridis_d() +
      labs(x = input$xVariableName, y = input$yVariableName, color = input$groupVariableName)

    if (input$xCorrelationScale %in% "log") {
      corPlot <- corPlot + scale_x_log10(breaks = xBreaks)
    }
    if (input$yCorrelationScale %in% "log") {
      corPlot <- corPlot + scale_y_log10(breaks = yBreaks)
    }
    return(ggplotly(corPlot, tooltip = c("x", "y", "text")))
  })
  output$correlationTable <- renderTable({
    if (is.null(inputData$pkData)) {
      return()
    }
    eval(parse(text = paste0("data <- inputData$pkData |> filter(", input$correlationSelection, ")")))

    data.frame(
      Pearson = cor(data[[input$xVariableName]], data[[input$yVariableName]], method = "pearson"),
      Kendall = cor(data[[input$xVariableName]], data[[input$yVariableName]], method = "kendall"),
      Spearman = cor(data[[input$xVariableName]], data[[input$yVariableName]], method = "spearman")
    )
  })

  output$distributionPlot <- renderPlotly({
    if (is.null(inputData$pkData)) {
      return()
    }
    eval(parse(text = paste0("data <- inputData$pkData |> filter(", input$distributionSelection, ")")))

    distPlot <- ggplot(
      data = data,
      mapping = aes(x = .data[[input$distributionVariableName]])
    ) +
      theme_bw(base_size = 12) +
      geom_histogram(fill = "grey", color = "black", bins = input$bins) +
      labs(x = input$distributionVariableName)

    distPlot <- distPlot + switch(input$distributionLine,
      "none" = geom_blank(),
      "mean" = geom_vline(aes(xintercept = mean(.data[[input$distributionVariableName]])), color = "dodgerblue"),
      "median" = geom_vline(aes(xintercept = median(.data[[input$distributionVariableName]])), color = "dodgerblue")
    )

    if (input$distributionGroupVariableName %in% "none") {
      return(distPlot)
    }
    return(distPlot + facet_wrap(~ as.factor(.data[[input$distributionGroupVariableName]]), ncol = 1))
  })
  output$distributionTable <- renderTable({
    if (is.null(inputData$pkData)) {
      return()
    }
    eval(parse(text = paste0("data <- inputData$pkData |> filter(", input$distributionSelection, ")")))

    as.data.frame(as.list(summary(data[[input$distributionVariableName]])), check.names = FALSE)
  })

  getIndData <- reactive({
    inputData$pkData |> filter(ID %in% input$id)
  })

  getSimData <- reactive({
    indData <- getIndData() |>
      mutate(indOcc = cumsum(EVID == 3) + 1)
    # Load appropriate model leveraging variable names
    modelFile <- "1cmt.cpp"
    if ("V2" %in% inputData$variableNames) {
      modelFile <- "2cmt.cpp"
    }
    if ("V3" %in% inputData$variableNames) {
      modelFile <- "3cmt.cpp"
    }
    pkModel <- mread_cache(file.path("www", modelFile), quiet = TRUE)
    simData <- NULL
    for (occ in unique(indData$indOcc)) {
      occData <- indData |>
        filter(indOcc %in% occ, EVID != 3) |>
        mutate(CMT = 1)
      occData <- pkModel |>
        data_set(data = occData) |>
        mrgsim_df(
          tgrid = seq(0, max(occData$TIME) + 24, 0.1),
          tad = TRUE,
          obsaug = TRUE,
          obsonly = TRUE
        )
      simData <- bind_rows(simData, c(OCC = occ, occData))
    }
    return(simData)
  })

  output$pkParameters <- renderTable({
    if (is.null(inputData$pkData)) {
      return()
    }
    if (input$indGroup %in% "none") {
      return(getIndData() |> summarise_all(first))
    }
    getIndData() |>
      group_by(.data[[input$indGroup]]) |>
      summarise_all(first)
  })

  output$timeProfile <- renderPlotly({
    if (is.null(inputData$pkData)) {
      return()
    }
    obsData <- getIndData() |>
      filter(MDV == 0)
    simData <- getSimData()
    # simData also includes tad
    indPlot <- ggplot() +
      theme_bw() +
      geom_line(data = simData, aes(x = TIME, y = DV, color = "Simulated", group = as.factor(OCC))) +
      geom_point(data = obsData, aes(x = TIME, y = DV, color = "Observed")) +
      geom_point(data = obsData, aes(x = TIME, y = PRED, color = "Pop. Predicted")) +
      geom_point(data = obsData, aes(x = TIME, y = IPRED, color = "Estimated")) +
      labs(
        x = "Time",
        y = "Concentration",
        color = "Data"
      )

    if (input$indGroup %in% "OCC") {
      indPlot <- indPlot + facet_wrap(~OCC)
    }

    if (input$scale %in% "log") {
      indPlot <- indPlot + scale_y_log10(breaks = yBreaks)
    }

    return(ggplotly(indPlot))
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
