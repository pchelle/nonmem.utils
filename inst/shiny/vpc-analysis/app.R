library(nonmem.utils)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

options(shiny.maxRequestSize = 1e3 * 1024^2)

#---- UI ----
ui <- page_navbar(
  title = span(icon("chart-area"), " VPC Analyses"),
  sidebar = sidebar(
    #---- Inputs ----
    accordion(
      accordion_panel(
        title = "Input Data",
        icon = icon("file-import"),
        card(
          fileInput(
            "dataset",
            span(icon("database"), "Import dataset"),
            accept = c(".csv", ".tab")
          )
        ),
        card(
          card_body(
            fileInput(
              "meta_data",
              tooltip(
                span(icon("signs-post"), " Import dictionary"),
                span(icon("circle-info"), "Dictionary is a csv file indicating Name, Type, Label, Unit, Min and Max of each dataset variable")
              ),
              accept = ".csv"
            )
          ),
          card_footer(downloadButton("meta_data_template", "Template"))
        ),
        sliderInput("bins", span(icon("layer-group"), " Number of bins"), min = 1, max = 20, value = 5),
        sliderInput("ci", span(icon("arrows-left-right-to-line"), " Confidence Interval [%]"), min = 51, max = 99, value = 80, step = 0.5),
        pickerInput("time_group", span(icon("object-ungroup"), " Grouping"), choices = "All")
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
          downloadButton(
            "clean_dataset",
            icon = icon("file-csv"),
            label = "Clean csv Dataset"
          ),
          textInput(
            "data_filter",
            tooltip(
              span(icon("filter"), "Data Filter"),
              span(icon("circle-info"), "Filter is expected as dplyr expression")
            ),
            value = ""
          )
        ),
        card_body(DT::DTOutput("data"), spin = "fading-circle", color = "royalblue")
      )
    ),
    #---- Mapping ----
    nav_panel(
      title = "Mapping",
      icon = icon("signs-post"),
      card(DT::dataTableOutput("data_mapping"))
    ),
    #---- VPC ----
    nav_panel(
      title = HTML('<abbr title="Visual Predictive Check">VPC</abbr>'),
      icon = icon("chart-line"),
      card(
        card_header(
          pickerInput("vpc_y_scale", span(icon("ruler"), " Scale"), choices = c("Linear", "Log"))
        ),
        card_body(addSpinner(plotlyOutput("vpc_profile"), spin = "fading-circle", color = "royalblue"))
      )
    ),
    #---- pcVPC ----
    nav_panel(
      title = HTML('<abbr title="Prediction-corrected Visual Predictive Check">pcVPC</abbr>'),
      icon = icon("chart-line"),
      card(
        card_header(
          pickerInput("pc_vpc_y_scale", span(icon("ruler"), " Scale"), choices = c("Linear", "Log"))
        ),
        card_body(addSpinner(plotlyOutput("pc_vpc_profile"), spin = "fading-circle", color = "royalblue"))
      )
    ),
    #---- pvcVPC ----
    nav_panel(
      title = HTML('<abbr title="Prediction-variability-corrected Visual Predictive Check">pvcVPC</abbr>'),
      icon = icon("chart-line"),
      card(
        card_header(
          pickerInput("pvc_vpc_y_scale", span(icon("ruler"), " Scale"), choices = c("Linear", "Log"))
        ),
        card_body(addSpinner(plotlyOutput("pvc_vpc_profile"), spin = "fading-circle", color = "royalblue"))
      )
    ),
    #---- blqVPC ----
    nav_panel(
      title = HTML('<abbr title="Visual Predictive Check for BLQ data">BLQ VPC</abbr>'),
      icon = icon("chart-line"),
      card(
        card_body(addSpinner(plotlyOutput("blq_vpc_profile"), spin = "fading-circle", color = "royalblue"))
      )
    ),
    #---- npdeVPC ----
    nav_panel(
      title = HTML('<abbr title="Visual Predictive Check for NPDE">NPDE VPC</abbr>'),
      icon = icon("chart-line"),
      card(
        card_body(addSpinner(plotlyOutput("npde_vpc_profile"), spin = "fading-circle", color = "royalblue"))
      )
    ),
    #---- Summary ----
    nav_panel(
      title = "Summary",
      icon = icon("box-archive"),
      card(
        card_header(
          downloadButton(
            "sum_vpc",
            icon = icon("file-csv"),
            label = "VPC Summary"
          )
        ),
        card_body(addSpinner(DT::DTOutput("sum_vpc_table"), spin = "fading-circle", color = "royalblue"))
      )
    )
  )
)


server <- function(input, output, session) {
  #---- Reactive Values ----
  get_data <- reactive({
    if (is.null(input$dataset)) {
      return()
    }
    is_nonmem_file <- grepl(pattern = ".tab", input$dataset$datapath)
    if (is_nonmem_file) {
      sim_data_content <- readLines(input$dataset$datapath)
      # Remove header rows
      sim_data_file <- tempfile(fileext = ".tab")
      header_rows <- grepl(pattern = "ID", sim_data_content)
      data_header <- head(sim_data_content[header_rows], 1)
      sim_data_content <- sim_data_content[!header_rows]
      # Remove Table rows
      table_rows <- grepl(pattern = "TABLE", sim_data_content)
      sim_data_content <- sim_data_content[!table_rows]
      # Re-write temp file
      writeLines(c(data_header, sim_data_content), con = sim_data_file)
      sim_data <- readr::read_table(sim_data_file) |> filter(MDV==0)
      return(sim_data)
    }
    readr::read_csv(input$dataset$datapath, na = c("NA", "N/A", "", "."))
  })
  get_meta_data <- reactive({
    if (is.null(input$meta_data)) {
      return()
    }
    readr::read_csv(input$meta_data$datapath, na = c("NA", "N/A", "", "."))
  })
  enough_data <- reactive({
    !any(is.null(get_data()), is.null(get_meta_data()))
  })
  get_sum_data <- reactive({
    if (!enough_data()) {
      return()
    }
    tad <- pull_name("tad", get_meta_data())

    # Actual VPC analysis
    sum_data <- run_vpc(
      # Need to wrap meta_data mapping of categorical
      #map_cat_data(get_data(), get_meta_data()),
      get_data(),
      x = tad,
      group = input$time_group,
      bins = input$bins,
      ci = input$ci/100
      )
    return(sum_data)
  })

  get_vpc_plots <- reactive({
    if (!enough_data()) {
      return()
    }
    tad <- pull_name("tad", get_meta_data())
    all_vpc_plots <- vpc_plots(
      get_data(),
      get_meta_data(),
      x = tad,
      bins = input$bins,
      ci = input$ci/100
      )
    return(all_vpc_plots)
  })


  #---- Selectors ----
  observeEvent(input$meta_data, {
    updatePickerInput(
      session = session,
      "time_group",
      choices = c("All", pull_name("cat", get_meta_data()))
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

  output$sum_vpc_table <- DT::renderDataTable({
    if (!enough_data()) {
      return()
    }
    get_sum_data() |>
      mutate_if(is.numeric, round, 2) |>
      DT::datatable(rownames = FALSE)
  })

  #---- Figures ----
  output$vpc_profile <- renderPlotly({
    if (!enough_data()) {
      return()
    }
    all_vpc_plots <- get_vpc_plots()
    tp_plot <- ggplotly(all_vpc_plots$vpc, dynamicTicks = TRUE, tooltip = "text")

    if (input$vpc_y_scale %in% "Log") {
      return(tp_plot|> plotly_log(x = FALSE))
    }
    return(tp_plot)
  })

  output$pc_vpc_profile <- renderPlotly({
    if (!enough_data()) {
      return()
    }
    all_vpc_plots <- get_vpc_plots()
    tp_plot <- ggplotly(all_vpc_plots$pc_vpc, dynamicTicks = TRUE, tooltip = "text")

    if (input$pc_vpc_y_scale %in% "Log") {
      return(tp_plot|> plotly_log(x = FALSE))
    }
    return(tp_plot)
  })

  output$blq_vpc_profile <- renderPlotly({
    if (!enough_data()) {
      return()
    }
    all_vpc_plots <- get_vpc_plots()
    tp_plot <- ggplotly(all_vpc_plots$blq, dynamicTicks = TRUE, tooltip = "text")
    return(tp_plot)
  })

  output$npde_vpc_profile <- renderPlotly({
    if (!enough_data()) {
      return()
    }
    all_vpc_plots <- get_vpc_plots()
    tp_plot <- ggplotly(all_vpc_plots$npde, dynamicTicks = TRUE, tooltip = "text")
    return(tp_plot)
  })

  #---- Downloads ----
  output$meta_data_template <- downloadHandler(
    filename = function() {
      "meta_data.csv"
    },
    content = function(file) {
      write.csv(
        readr::read_csv(system.file("template-dictionary.csv", package = "nonmem.utils")),
        file,
        row.names = FALSE
      )
    }
  )

  output$dataset_report <- downloadHandler(
    filename = function() {
      "vpc-report.docx"
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
      report_vpc_analysis(
        data_path = input$dataset$datapath,
        meta_data_path = input$meta_data$datapath,
        report_path = file,
        bins = input$bins,
        ci = input$ci/100
      )
      removeModal(session = session)
    }
  )

  output$clean_dataset <- downloadHandler(
    filename = function() {
      "vpc-dataset.csv"
    },
    content = function(file) {
      write.csv(
        get_data(),
        file,
        row.names = FALSE
      )
    }
  )

  output$sum_vpc <- downloadHandler(
    filename = function() {
      "vpc-summary.csv"
    },
    content = function(file) {
      write.csv(
        get_sum_data(),
        file,
        row.names = FALSE
      )
    }
  )

  #---- Clean closure ----
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
