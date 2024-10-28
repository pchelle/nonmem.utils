#' @title optimization-profile
#' @description
#' A shiny app to review nonmem model optimization saved in .ext files

library(dplyr, warn.conflicts = FALSE)
library(shinydashboard, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

ui <- dashboardPage(
  dashboardHeader(title = "Nonmem Optimization Review"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Get Started", tabName = "home", icon = icon("home")),
      menuItem(" Summary", tabName = "summary", icon = icon("table")),
      menuItem(" Profiles", tabName = "profiles", icon = icon("chart-line"))
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
          fileInput(inputId = "newfile", label = "Upload Nonmem .ext file", accept = ".ext")
        ),
        box(
          title = span(icon("database"), "Dataset"),
          width = 12,
          solidHeader = TRUE,
          status = "success",
          DT::dataTableOutput("dataTable")
        )
      ),
      tabItem(
        tabName = "summary",
        box(
          title = span(icon("table"), " Summary"),
          solidHeader = TRUE,
          status = "success",
          width = 12,
          textOutput("summaryIterations"),
          br(),
          tableOutput("summaryTable")
        )
      ),
      tabItem(
        tabName = "profiles",
        box(
          title = span(icon("gear"), "Settings"),
          solidHeader = TRUE,
          status = "warning",
          width = 4,
          selectInput("yVariableName", label = "Select Y Variable", choices = NA, multiple = TRUE),
          selectInput("displayMode", label = "Display Mode", choices = c("Absolute", "Relative")),
          selectInput("yScale", label = "Y-Scale", choices = c("lin", "log"))
        ),
        box(
          title = span(icon("chart-line"), " Profile"),
          solidHeader = TRUE,
          status = "primary",
          width = 8,
          plotlyOutput("profilePlot"),
          align = "center"
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
    inputData$pkData <- read.table(file = input$newfile$datapath, header = TRUE, skip = 1, check.names = FALSE)
    inputData$variableNames <- names(inputData$pkData)
    # Update all the selectInput with the new variable names
    updateSelectInput(session, "yVariableName", choices = inputData$variableNames, selected = "OBJ")
  })
  output$dataTable <- DT::renderDataTable({
    if (is.null(inputData$pkData)) {
      return(data.frame("No data loaded" = NA, check.names = FALSE))
    }
    return(DT::datatable(inputData$pkData, options = list(scrollX = TRUE)))
  })

  output$profilePlot <- renderPlotly({
    if (is.null(inputData$pkData)) {
      return()
    }
    if (length(input$yVariableName) == 0) {
      return()
    }

    profilePlots <- list()
    for (yVariableName in input$yVariableName) {
      profilePlot <- ggplot(
        data = inputData$pkData |> filter(ITERATION >= 0),
        mapping = aes(
          x = ITERATION,
          y = switch(input$displayMode,
            "Absolute" = .data[[yVariableName]],
            "Relative" = 100 * (.data[[yVariableName]] - first(.data[[yVariableName]])) / first(.data[[yVariableName]])
          ),
          group = 1,
          text = paste0(
            "Iteration: ", .data$ITERATION, "\n",
            "OFV: ", round(.data$OBJ, 3), "\n",
            yVariableName, ": ", .data[[yVariableName]], "\n",
            "Relative: ", round(100 * (.data[[yVariableName]] - first(.data[[yVariableName]])) / first(.data[[yVariableName]]), 1), " %"
          )
        )
      ) +
        theme_bw(base_size = 12) +
        geom_hline(
          aes(
            text = "Initial Estimate",
            yintercept = switch(input$displayMode,
              "Absolute" = first(.data[[yVariableName]]),
              "Relative" = 0
            )
          ),
          linetype = "dashed",
          color = "firebrick"
        ) +
        geom_hline(
          aes(
            text = "Final Estimate",
            yintercept = switch(input$displayMode,
              "Absolute" = last(.data[[yVariableName]]),
              "Relative" = 100 * (last(.data[[yVariableName]]) - first(.data[[yVariableName]])) / first(.data[[yVariableName]])
            )
          ),
          linetype = "dashed",
          color = "slateblue"
        ) +
        geom_point(aes(color = OBJ), size = 1) +
        geom_line(color = "darkgreen") +
        scale_color_viridis_c(option = "turbo") +
        labs(
          x = "Iterations",
          y = switch(input$displayMode,
            "Absolute" = yVariableName,
            "Relative" = paste("Variation from initial", yVariableName, "[%]")
          )
        )
      if (input$yScale %in% "log") {
        profilePlot <- profilePlot + scale_y_log10()
      }
      profilePlots[[yVariableName]] <- ggplotly(profilePlot, tooltip = "text")
    }
    finalProfilePlot <- subplot(
      profilePlots,
      nrows = length(input$yVariableName),
      shareX = TRUE,
      shareY = FALSE,
      titleY = TRUE
    )
    return(finalProfilePlot)
  })

  output$summaryIterations <- renderText({
    if (is.null(inputData$pkData)) {
      return()
    }
    return(paste("Nonmem ran", max(inputData$pkData$ITERATION), "iterations"))
  })

  output$summaryTable <- renderTable({
    if (is.null(inputData$pkData)) {
      return()
    }
    # Summary providing for each variable but iteration, initial and final values, their difference and variation
    extData <- inputData$pkData |> filter(ITERATION >= 0)
    summaryData <- bind_cols(
      Parameter = names(extData),
      extData |> summarise_all(first) |> t() |> `colnames<-`("Initial Estimate"),
      extData |> summarise_all(last) |> t() |> `colnames<-`("Final Estimate")
    ) |>
      mutate(
        Difference = `Final Estimate` - `Initial Estimate`,
        `Variation [%]` = round(100 * (`Final Estimate` - `Initial Estimate`) / `Initial Estimate`, 1)
      ) |>
      filter(Parameter != "ITERATION")
    return(summaryData)
  })

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
