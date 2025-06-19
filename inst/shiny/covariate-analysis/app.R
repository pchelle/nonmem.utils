library(nonmem.utils)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
library(GGally)

get_field <- function(data, field_name, type_name) {
  data |>
    filter(Type %in% type_name) |>
    pull(field_name)
}

#---- UI ----
ui <- page_navbar(
  title = span(icon("database"), " Covariate Analysis"),
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
        card(
          fileInput(
            "result_file",
            span(icon("file-invoice"), "Import Result file (.res)"),
            accept = c(".res")
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
    #---- Eta Plot ----
    nav_panel(
      title = HTML("&eta;"),
      icon = icon("chart-line"),
      card(
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            pickerInput("select_eta_cat", "Grouping", choices = "All"),
            pickerInput("select_eta", HTML("Select &eta;"), multiple = TRUE, choices = NA)
          )
        ),
        card_body(plotlyOutput("eta_plot"))
      )
    ),
    #---- Covariates ----
    nav_panel(
      title = "Covariates",
      icon = icon("chart-simple"),
      card(
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            pickerInput("select_cov_cat", "Grouping", choices = "All"),
            pickerInput("select_eta_cov", HTML("Select &eta;"), multiple = TRUE, choices = NA),
            pickerInput("select_cov", "Select Covariate", multiple = TRUE, choices = NA)
          )
        ),
        card_body(plotlyOutput("cov_eta_plot"))
      )
    ),
    #---- Covariate correlation ----
    nav_panel(
      title = "Covariates",
      icon = icon("box-archive"),
      card(full_screen = TRUE, DT::DTOutput("eta_cov_table"))
    ),
    #---- Omega ----
    nav_panel(
      title = HTML("&Omega;"),
      icon = icon("box-archive"),
      card(DT::DTOutput("omega_table"))
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
    if(is_nonmem_file){
      return(readr::read_table(input$dataset$datapath, skip = 1))
    }
    readr::read_csv(input$dataset$datapath, na = c("NA", "N/A", "", "."))
  })
  get_meta_data <- reactive({
    if (is.null(input$meta_data)) {
      return()
    }
    readr::read_csv(input$meta_data$datapath, na = c("NA", "N/A", "", "."))
  })
  get_res_data <- reactive({
    if (is.null(input$result_file)) {
      return()
    }
    nonmem_res(input$result_file$datapath)
  })
  get_sorted_meta <- reactive({
    meta_data <- get_meta_data()
    if (is.null(meta_data)) {
      return()
    }
    sorted_meta <- split(meta_data, meta_data$Type)
    return(sorted_meta)
  })
  enough_data <- reactive({
    !any(is.null(get_data()), is.null(get_meta_data()))
  })

  get_eta_plots <- reactive({
    if (!enough_data()) {
      return(list())
    }
    meta_data <- get_meta_data() |>
      filter(Type %in% c("id", "cat") | Name %in% input$select_eta)
    all_eta_plots <- eta_plot(get_data(), meta_data)

    return(all_eta_plots)
  })
  get_cov_plots <- reactive({
    if (!enough_data()) {
      return(list())
    }
    meta_data <- get_meta_data() |>
      filter(Type %in% "id" | Name %in% c(input$select_cov, input$select_eta_cov))
    all_cov_plots <- eta_cov_plot(get_data(), meta_data)
    return(all_cov_plots)
  })

  #---- Selectors ----
  observeEvent(input$meta_data, {
    all_covariates <- get_meta_data() |>
      filter(Type %in% "cov")
    all_categoricals <- get_meta_data() |>
      filter(Type %in% "cat")
    all_etas <- get_meta_data() |>
      filter(Type %in% c("eta"))

    updatePickerInput(
      session = session,
      "select_cov",
      choices = list(Continuous = all_covariates$Name, Categorical = all_categoricals$Name),
      selected = first(all_covariates$Name),
      choicesOpt = list(subtext = c(all_covariates$Label, all_categoricals$Label))
    )

    updatePickerInput(
      session = session,
      "select_eta",
      choices = all_etas$Name,
      selected = first(all_etas$Name),
      choicesOpt = list(subtext = all_etas$Label)
    )
    updatePickerInput(
      session = session,
      "select_eta_cov",
      choices = all_etas$Name,
      selected = first(all_etas$Name),
      choicesOpt = list(subtext = all_etas$Label)
    )

    updatePickerInput(
      session = session,
      "select_eta_cat",
      choices = c("All", all_categoricals$Name),
      selected = "All",
      choicesOpt = list(subtext = c("All", all_categoricals$Label))
    )

    updatePickerInput(
      session = session,
      "select_cov_cat",
      choices = c("All", all_categoricals$Name),
      selected = "All",
      choicesOpt = list(subtext = c("All", all_categoricals$Label))
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
  output$data_mapping <- DT::renderDT({
    get_meta_data() |> DT::datatable()
  })
  output$omega_table <- DT::renderDT({
    res_data <- get_res_data()
    if (is.null(res_data)) {
      return()
    }
    omega_table <- as.data.frame(res_data$Omega) |>
      filter(Estimates > 0) |>
      select(-Initial) |>
      mutate(CV = 100*sqrt(Estimates)) |>
      mutate_if(is.numeric, round, 1)
    names(omega_table) <- c("Name", "Covariance", "RSE [%]", "Shrinkage [%]", "CV [%]")

    DT::datatable(omega_table, rownames = FALSE)
  })

  output$eta_cov_table <- DT::renderDT({
    if (!enough_data()) {
      return()
    }
    eta_cov_table <- eta_cor(get_data(), get_meta_data()) |>
      dplyr::mutate_all(gsub, pattern = "/", replacement="<br>")
    DT::datatable(eta_cov_table, rownames = FALSE, escape = FALSE)
  })

  #---- Figures ----
  output$eta_plot <- renderPlotly({
    all_eta_plots <- get_eta_plots()
    ggplotly(all_eta_plots[[input$select_eta_cat]])
  })

  output$cov_eta_plot <- renderPlotly({
    all_cov_plots <- get_cov_plots()
    ggplotly(all_cov_plots[[input$select_cov_cat]])
  })

  #---- Downloads ----
  output$meta_data_template <- downloadHandler(
    filename = function() {
      "meta_data.csv"
    },
    content = function(file) {
      write.csv(
        readr::read_csv(system.file("template-dictionary.csv", package = "nonmem.utils")),
        file, row.names = FALSE
        )
    }
  )

  output$dataset_report <- downloadHandler(
    filename = function() {
      "dataset-report.docx"
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
      report_dataset_analysis(
        data_path = input$dataset$datapath,
        meta_data_path = input$meta_data$datapath,
        report_path = file,
        bins = input$bins
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
