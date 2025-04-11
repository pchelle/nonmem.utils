library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(pmplots)
library(plotly, warn.conflicts = FALSE)

add_col <- function(data, col_name) {
  data[[col_name]] <- data[[col_name]] %||% 0
  return(data)
}

#---- UI ----
ui <- page_navbar(
  title = span(icon("bullseye"), " Model Fit"),
  sidebar = sidebar(
    #---- Inputs ----
    accordion(
      accordion_panel(
        title = "Input Data",
        icon = icon("file-import"),
        card(
          fileInput(
            "dataset",
            span(icon("database"), "Import Data"),
            accept = c(".tab", ".csv", ".par", ".txt")
          )
        ),
        card(
          card_body(
            fileInput(
              "metadata",
              tooltip(
                span(icon("signs-post"), " Import dictionary"),
                span(icon("circle-info"), "Dictionary is a csv file indicating Name, Type, Label, Unit and Description for each dataset variable")
              ),
              accept = ".csv"
            )
          )
        )
      ),
      #---- Report ----
      accordion_panel(
        title = "Reporting",
        icon = icon("file-export"),
        downloadButton("dataset_report", icon = icon("file-word"))
      )
    )
  ),
  navset_card_tab(
    #---- Dataset ----
    nav_panel(
      title = "Data",
      icon = icon("database"),
      card(
        card_header(
          textInput(
            "data_filter",
            tooltip(
              span(icon("filter"), "Data Filter"),
              span(icon("circle-info"), "Filter is expected as dplyr expression")
            ),
            value = ""
          )
        ),
        card_body(DT::dataTableOutput("data"))
      )
    ),
    #---- Mapping ----
    nav_panel(
      title = "Mapping",
      icon = icon("signs-post"),
      card(DT::dataTableOutput("data_mapping"))
    ),
    #---- Goodness of Fit ----
    nav_panel(
      title = "Goodness of Fit",
      icon = icon("bullseye"),
      popover(
        span(icon("gear"), "Settings"),
        tagList(
          pickerInput("obs_vs_pred_scale", span(icon("ruler"), " Scale"), choices = c("linear", "log"))
        )
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("Observed vs Population Predictions"),
          plotlyOutput("obs_vs_pred")
        ),
        card(
          full_screen = TRUE,
          card_header("Observed vs Individual Predictions"),
          plotlyOutput("obs_vs_ipred")
        )
      )
    ),
    #---- Residuals ----
    nav_panel(
      title = "Residuals",
      icon = icon("chart-simple"),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("CWRES histogram"),
          plotlyOutput("res_hist")
        ),
        card(
          full_screen = TRUE,
          card_header("CWRES QQ-plot"),
          plotlyOutput("res_qq")
        )
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("CWRES vs Time"),
          plotlyOutput("res_vs_time")
        ),
        card(
          full_screen = TRUE,
          card_header("CWRES vs TAD"),
          plotlyOutput("res_vs_tad")
        ),
        card(
          full_screen = TRUE,
          card_header("CWRES vs Predicted"),
          plotlyOutput("res_vs_pred")
        )
      )
    ),
    #---- NPDE ----
    nav_panel(
      title = "NPDE",
      icon = icon("chart-simple"),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("NPDE histogram"),
          plotlyOutput("npde_hist")
        ),
        card(
          full_screen = TRUE,
          card_header("NPDE QQ-plot"),
          plotlyOutput("npde_qq")
        )
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header("NPDE vs Time"),
          plotlyOutput("npde_vs_time")
        ),
        card(
          full_screen = TRUE,
          card_header("NPDE vs TAD"),
          plotlyOutput("npde_vs_tad")
        ),
        card(
          full_screen = TRUE,
          card_header("NPDE vs Predicted"),
          plotlyOutput("npde_vs_pred")
        )
      )
    ),
    #---- Individual Profiles ----
    nav_panel(
      title = "Individual Profiles",
      icon = icon("person-circle-question"),
      card(
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            tagList(
              sliderInput("ind_page", span(icon("timeline"), "Page"), min = 1, value = 1, max = 1, step = 1),
              sliderInput("ind_rows", span(icon("layer-group"), " Rows per page"), min = 1, max = 5, value = 2, step = 1),
              sliderInput("ind_cols", span(icon("layer-group"), " Columns per page"), min = 1, max = 5, value = 3, step = 1)
            )
          )
        ),
        card_body(plotOutput("time_profile"))
      )
    )
  )
)


server <- function(input, output, session) {
  #---- Reactive Values ----
  get_metadata <- reactive({
    if (is.null(input$metadata)) {
      return()
    }
    read.csv(input$metadata$datapath)
  })
  mdv_name <- reactive({
    # Assumes column is MDV
    if (is.null(get_metadata())) {
      return("MDV")
    }
    mdv_row <- get_metadata() |> filter(Type %in% "mdv")
    if (nrow(mdv_row) == 1) {
      return(mdv_row$Name)
    }
    return("MDV")
  })
  blq_name <- reactive({
    # Assumes column is MDV
    if (is.null(get_metadata())) {
      return("BLQ")
    }
    blq_row <- get_metadata() |> filter(Type %in% "blq")
    if (nrow(blq_row) == 1) {
      return(blq_row$Name)
    }
    return("BLQ")
  })
  get_data <- reactive({
    if (is.null(input$dataset)) {
      return()
    }
    if (grepl("\\.csv$", input$dataset$datapath)) {
      data <- read.csv(input$dataset$datapath) |>
        add_col(mdv_name()) |>
        add_col(blq_name()) |>
        filter(.data[[mdv_name()]] == 0, .data[[blq_name()]] <= 0)
      return(data)
    }
    data <- read.table(input$dataset$datapath, skip = 1, header = TRUE) |>
      add_col(mdv_name()) |>
      add_col(blq_name()) |>
      filter(.data[[mdv_name()]] == 0, .data[[blq_name()]] <= 0)
    return(data)
  })

  enough_data <- reactive({
    !any(is.null(get_data()), is.null(get_metadata()))
  })

  #---- Selectors ----
  observeEvent(c(input$dataset, input$ind_rows, input$ind_cols), {
    updateSliderInput(
      session = session,
      "ind_page",
      max = ceiling(n_distinct(get_data()$ID) / (input$ind_rows * input$ind_cols)),
    )
  })


  #---- Tables ----
  output$data <- DT::renderDataTable({
    data <- get_data()
    if (!(input$data_filter %in% "")) {
      eval(parse(
        text = paste0(
          "data <- data |> filter(", input$data_filter, ")"
        )
      ))
    }
    data |> DT::datatable()
  })
  output$data_mapping <- DT::renderDataTable({
    get_metadata() |> DT::datatable()
  })

  #---- Figures ----
  output$obs_vs_pred <- renderPlotly({
    p <- dv_pred(df = get_data())
    p <- ggplotly(p, dynamicTicks = TRUE)
    if (input$obs_vs_pred_scale %in% "linear") {
      return(p)
    }
    return(p |> layout(xaxis = list(type = "log"), yaxis = list(type = "log")))
  })
  output$obs_vs_ipred <- renderPlotly({
    p <- dv_ipred(df = get_data())
    p <- ggplotly(p, dynamicTicks = TRUE)
    if (input$obs_vs_pred_scale %in% "linear") {
      return(p)
    }
    return(p |> layout(xaxis = list(type = "log"), yaxis = list(type = "log")))
  })
  output$res_hist <- renderPlotly({
    cwres_hist(df = get_data())
  })
  output$res_qq <- renderPlotly({
    cwres_q(df = get_data())
  })
  output$res_vs_pred <- renderPlotly({
    cwres_pred(df = get_data())
  })
  output$res_vs_time <- renderPlotly({
    cwres_time(df = get_data())
  })
  output$res_vs_tad <- renderPlotly({
    cwres_tad(df = get_data())
  })
  output$npde_hist <- renderPlotly({
    npde_hist(df = get_data())
  })
  output$npde_qq <- renderPlotly({
    npde_q(df = get_data())
  })
  output$npde_vs_pred <- renderPlotly({
    npde_pred(df = get_data())
  })
  output$npde_vs_time <- renderPlotly({
    npde_time(df = get_data())
  })
  output$npde_vs_tad <- renderPlotly({
    npde_tad(df = get_data())
  })

  output$time_profile <- renderPlot({
    p <- dv_pred_ipred(
      get_data(),
      # facets = "ID", TODO: could be changed according to meta
      id_per_plot = input$ind_rows * input$ind_cols,
      nrow = input$ind_rows,
      ncol = input$ind_cols
    )
    return(p[[input$ind_page]])
  })

  #---- Downloads ----
  output$dataset_report <- downloadHandler(
    filename = function() {
      "gof-report.docx"
    },
    content = function(file) {
      showModal(
        modalDialog(
          title = span(icon("file-pen"), " Writing report..."),
          size = "s",
          addSpinner(tableOutput("spinner"), spin = "fading-circle")
        ),
        session = session
      )
      rmarkdown::render(
        "www/gof-analysis.qmd",
        output_file = file,
        params = list(
          dataFile = input$dataset$datapath,
          metadataFile = input$metadata$datapath
        )
      )
      removeModal(session = session)
    }
  )

  #---- Clean closure ----
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
