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
  theme = bs_theme(bootswatch = "zephyr"),
  title = span(icon("database"), " Dataset Analysis"),
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
        )
      ),
      #---- Report ----
      accordion_panel(
        title = "Reporting",
        icon = icon("file-export"),
        downloadButton("report_docx", icon = icon("file-word")),
        downloadButton("report_pdf", icon = icon("file-pdf"))
      )
    )
  ),
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
  #---- Time Profile ----
  nav_panel(
    title = "Time Profile",
    icon = icon("chart-line"),
    card(
      card_header(
        popover(
          span(icon("gear"), "Settings"),
          fluidPage(
            pickerInput("select_time", span(icon("timeline"), "Time Variable"), multiple = FALSE, choices = NA),
            sliderInput("bins", span(icon("layer-group"), " Number of bins"), min = 1, max = 20, value = 5),
            pickerInput("y_scale", span(icon("ruler"), " Scale"), choices = c("Linear", "Log", "Percent BLQ")),
            pickerInput("time_group", span(icon("object-ungroup"), " Grouping"), choices = "All")
          )
        )
      ),
      card_body(plotlyOutput("time_profile"))
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
          pickerInput("select_cov", "Select Covariate", multiple = TRUE, choices = NA)
        )
      ),
      card_body(plotlyOutput("fig_cov"))
    )
  ),
  #---- Categoricals ----
  nav_panel(
    title = "Categoricals",
    icon = icon("box-archive"),
    card(full_screen = TRUE, DT::DTOutput("cov_cor_table"))
  ),
  #---- Data Inventory ----
  nav_panel(
    title = "Data Inventory",
    icon = icon("box-archive"),
    card(
      full_screen = TRUE,
      card_header(
        popover(
          span(icon("gear"), "Settings"),
          pickerInput("select_inv_cat", "Select Group", multiple = FALSE, choices = "All")
        ),
        p("Data inventory")
      ),
      card_body(DT::dataTableOutput("sum_data"))
    )
  ),
  #---- Summary ----
  nav_panel(
    title = "Summary",
    icon = icon("box-archive"),
    layout_column_wrap(
      card(
        full_screen = TRUE,
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            pickerInput("select_sum_cat", "Select Group", multiple = FALSE, choices = "All")
          ),
          p("Continuous covariate statistics")
        ),
        card_body(DT::dataTableOutput("sum_cov"))
      ),
      card(
        full_screen = TRUE,
        card_header(" Count"),
        card_body(DT::dataTableOutput("sum_cat"))
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
  get_sum_data <- reactive({
    if (!enough_data()) {
      return()
    }
    id_var <- get_field(get_meta_data(), field_name = "Name", type_name = "id")
    sum_data <- get_data() |>
      group_by(.data[[id_var]]) |>
      summarise_all(first)
    return(sum_data)
  })

  #---- Selectors ----
  observeEvent(input$meta_data, {
    all_covariates <- get_meta_data() |>
      filter(Type %in% c("cov", "cat"))

    updatePickerInput(
      session = session,
      "select_cov",
      choices = list(
        Continuous = all_covariates$Name[all_covariates$Type %in% "cov"],
        Categorical = all_covariates$Name[all_covariates$Type %in% "cat"]
      ),
      selected = first(all_covariates$Name),
      choicesOpt = list(
        subtext = c(
          all_covariates$Label[all_covariates$Type %in% "cov"],
          all_covariates$Label[all_covariates$Type %in% "cat"]
        )
      )
    )
    all_time_names <- pull_name(c("time", "tad"), get_meta_data())
    updatePickerInput(
      session = session,
      "select_time",
      choices = all_time_names,
      selected = head(all_time_names, 1),
      choicesOpt = list(
        subtext = pull_label(all_time_names, get_meta_data())
      )
    )
    updatePickerInput(
      session = session,
      "time_group",
      choices = c("All", pull_name("cat", get_meta_data()))
    )

    updatePickerInput(
      session = session,
      "select_sum_cat",
      choices = names(cov_inventory(get_data(), get_meta_data())),
      selected = "All"
    )

    updatePickerInput(
      session = session,
      "select_inv_cat",
      choices = names(data_inventory(get_data(), get_meta_data())),
      selected = "All"
    )
  })

  #---- Tables ----
  output$data <- DT::renderDT({
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
  output$sum_data <- DT::renderDT({
    if (!enough_data()) {
      return()
    }
    inventories <- data_inventory(get_data(), get_meta_data())
    inventory <- data.frame(
      Parameter = names(inventories[[input$select_inv_cat]]),
      Value = t(inventories[[input$select_inv_cat]])
    )
    return(DT::datatable(inventory, rownames = FALSE))
  })
  output$sum_cov <- DT::renderDT({
    if (!enough_data()) {
      return()
    }
    covariates <- get_meta_data() |> filter(Type %in% "cov")
    if (nrow(covariates) == 0) {
      return()
    }
    cov_summaries <- cov_inventory(get_data(), get_meta_data())

    cov_summaries[[input$select_sum_cat]] |>
      mutate_if(is.numeric, round, 2) |>
      DT::datatable(rownames = FALSE)
  })
  output$sum_cat <- DT::renderDT({
    categoricals <- get_meta_data() |> filter(Type %in% "cat")

    if (nrow(categoricals) == 0) {
      return()
    }

    cat_table <- cat_inventory(get_data(), get_meta_data())
    cat_table <- lapply(
      cat_table,
      function(x) {
        if (!is.numeric(x)) {
          return(x)
        }
        round(x, 2)
      }
    ) |>
      data.frame(check.names = FALSE) |>
      DT::datatable(rownames = FALSE)
    return(cat_table)
  })
  output$cov_cor_table <- DT::renderDT({
    if (!enough_data()) {
      return()
    }
    cov_table <- cov_cor(get_data(), get_meta_data())
    DT::datatable(highlight_significant(cov_table), rownames = FALSE, escape = FALSE)
  })

  #---- Figures ----
  output$fig_cov <- renderPlotly({
    p <- ggpairs(
      map_cat_data(get_sum_data(), get_meta_data()),
      columns = input$select_cov,
      columnLabels = pull_label(input$select_cov, get_meta_data()),
      upper = list(
        continuous = "cor",
        combo = "box_no_facet", # summarise_by
        discrete = "colbar",
        na = "na"
      ),
      lower = list(
        continuous = wrap("smooth_loess", se = FALSE),
        combo = "box_no_facet",
        discrete = "colbar",
        na = "na"
      ),
      diag = list(
        continuous = wrap("barDiag", bins = 11),
        discrete = "barDiag",
        na = "naDiag"
      )
    ) + theme_bw()
    ggplotly(p)
  })

  output$time_profile <- renderPlotly({
    time_name <- pull_name("time", get_meta_data())
    tad_name <- pull_name("tad", get_meta_data())

    if (input$select_time %in% time_name) {
      tp_list <- time_profile(get_data(), get_meta_data(), bins = input$bins)
    }
    if (input$select_time %in% tad_name) {
      tp_list <- tad_profile(get_data(), get_meta_data(), bins = input$bins)
    }
    tp_plots <- tp_list[[input$time_group]]

    if (input$y_scale %in% "Log") {
      tp_plot <- ggplotly(
        tp_plots[["Linear"]],
        dynamicTicks = TRUE,
        tooltip = "text"
      ) |>
        plotly_log(x = FALSE)
      return(tp_plot)
    }
    tp_plot <- ggplotly(
      tp_plots[[input$y_scale]],
      dynamicTicks = TRUE,
      tooltip = "text"
    )

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

  output$report_docx <- downloadHandler(
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

  output$report_pdf <- downloadHandler(
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

      removeModal(session = session)
    }
  )

  #---- Clean closure ----
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
