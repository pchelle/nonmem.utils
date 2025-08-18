library(nonmem.utils)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

add_col <- function(data, col_name) {
  data[[col_name]] <- data[[col_name]] %||% 0
  return(data)
}

#TODO: add a setting to split by categorical or color by covariate

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
              "meta_data",
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
      card(
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            tagList(
              pickerInput("obs_vs_preds_scale", span(icon("ruler"), " Scale"), choices = c("linear", "log"))
            )
          )
        ),
        card_body(plotlyOutput("obs_vs_preds"))
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
              sliderInput("ind_cols", span(icon("layer-group"), " Columns per page"), min = 1, max = 5, value = 3, step = 1),
              pickerInput("ind_scale_y", span(icon("ruler"), " Y-axis scale"), choices = c("linear", "log"), selected = "Linear")
            )
          )
        ),
        card_body(plotlyOutput("time_profile"), spin = "fading-circle") #addSpinner()
      )
    )
  )
)


server <- function(input, output, session) {
  #---- Reactive Values ----
  get_meta_data <- reactive({
    if (is.null(input$meta_data)) {
      return()
    }
    readr::read_csv(input$meta_data$datapath)
  })
  mdv_name <- reactive({
    # Assumes column is MDV
    if (is.null(get_meta_data())) {
      return("MDV")
    }
    mdv_row <- get_meta_data() |> filter(Type %in% "mdv")
    if (nrow(mdv_row) == 1) {
      return(mdv_row$Name)
    }
    return("MDV")
  })
  blq_name <- reactive({
    # Assumes column is MDV
    if (is.null(get_meta_data())) {
      return("BLQ")
    }
    blq_row <- get_meta_data() |> filter(Type %in% "blq")
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
      data <- readr::read_csv(input$dataset$datapathna, na = c("NA", "N/A", "", "."))
      return(data)
    }
    data <- readr::read_table(
      input$dataset$datapath,
      na = c("NA", "N/A", "", "."),
      skip = 1
      ) |>
      # TODO: remove column after data is fixed
      mutate(LLOQ = 10)
    return(data)
  })
  get_tp_plots <- reactive({
    ind_time_profiles(
      get_data(),
      get_meta_data(),
      n_rows = input$ind_rows,
      n_cols = input$ind_cols
    )
  })
  # |> bindEvent(get_data(), get_meta_data(), input$ind_rows, input$ind_cols)

  enough_data <- reactive({
    !any(is.null(get_data()), is.null(get_meta_data()))
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
    get_meta_data() |> DT::datatable()
  })

  #---- Figures ----
  output$obs_vs_preds <- renderPlotly({
    p <- dv_preds(get_data(), get_meta_data())
    p <- ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
    if (input$obs_vs_preds_scale %in% "linear") {
      return(p)
    }
    return(plotly_log(p))
  })
  output$res_hist <- renderPlotly({
    p <- residual_hist(y_type = "cwres", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$res_qq <- renderPlotly({
    p <- residual_qq(y_type = "cwres", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$res_vs_pred <- renderPlotly({
    p <- residual_plot(x_type = "pred", y_type = "cwres", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$res_vs_time <- renderPlotly({
    p <- residual_plot(x_type = "time", y_type = "cwres", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$res_vs_tad <- renderPlotly({
    p <- residual_plot(x_type = "tad", y_type = "cwres", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$npde_hist <- renderPlotly({
    p <- residual_hist(y_type = "npde", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$npde_qq <- renderPlotly({
    p <- residual_qq(y_type = "cwres", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$npde_vs_pred <- renderPlotly({
    p <- residual_plot(x_type = "pred", y_type = "npde", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$npde_vs_time <- renderPlotly({
    p <- residual_plot(x_type = "time", y_type = "npde", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })
  output$npde_vs_tad <- renderPlotly({
    p <- residual_plot(x_type = "tad", y_type = "npde", get_data(), get_meta_data())
    ggplotly(p, dynamicTicks = TRUE, tooltip = "text")
  })

  output$time_profile <- renderPlotly({
    p <- get_tp_plots()
    p <- ggplotly(p[[input$ind_page]], dynamicTicks = TRUE, tooltip = "text")
    if (input$ind_scale_y %in% "linear") {
      return(p)
    }
    n_plots <- input$ind_rows*input$ind_cols
    p$x$layout$yaxis$type <- "log"
    for(plot_index in seq_len(n_plots)) {
      p$x$layout[[paste0("yaxis", plot_index)]]$type <- "log"
    }
    return(p)
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
      report_gof_analysis(
        data_path = input$dataset$datapath,
        meta_data_path = input$meta_data$datapath,
        report_path = file
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
