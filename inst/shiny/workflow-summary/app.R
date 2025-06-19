library(nonmem.utils)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)
# library(GGally)

#---- UI ----
ui <- page_navbar(
  title = span(icon("list-check"), " Workflow Summary"),
  sidebar = sidebar(
    #---- Inputs ----
    accordion(
      accordion_panel(
        title = "Input Data",
        icon = icon("file-import"),
        card(
          actionButton("folder_path", label = "Select a directory", icon = icon("folder-open"))
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
    #---- Fixed Effects ----
    nav_panel(
      title = "Fixed Effects",
      icon = HTML("&Theta;"),
      card(
        card_header(
          selectInput(
            "select_model_theta",
            label = span(icon("magnifying-glass"), "Select a model"),
            choices = NA
          )
        ),
        card_body(DT::dataTableOutput("theta_data"))
      )
    ),
    #---- Random Effects Etas ----
    nav_panel(
      title = "Random Effects for BSV/BOV",
      icon = HTML("&Omega;"),
      card(
        card_header(
          selectInput(
            "select_model_omega",
            label = span(icon("magnifying-glass"), "Select a model"),
            choices = NA
          )
        ),
        card_body(DT::dataTableOutput("omega_data"))
      )
    ),
    #---- Random Effects Eps ----
    nav_panel(
      title = "Random Effects for RUV",
      icon = HTML("&Sigma;"),
      card(
        card_header(
          selectInput(
            "select_model_sigma",
            label = span(icon("magnifying-glass"), "Select a model"),
            choices = NA
          )
        ),
        card_body(DT::dataTableOutput("sigma_data"))
      )
    )
  )
)


server <- function(input, output, session) {
  #---- Reactive Values ----
  reviewed_directory <- reactiveValues(nonmem_results = NULL)
  get_data <- reactive({
    if (is.null(reviewed_directory$nonmem_results)) {
      return()
    }
    nonmem_data <- lapply(reviewed_directory$nonmem_results, format_result)
    nonmem_data <- do.call(rbind, nonmem_data) |>
      arrange(OFV)
    return(nonmem_data)
  })
  get_meta_data <- reactive({
    if (is.null(input$meta_data)) {
      return()
    }
    read.csv(input$meta_data$datapath, na = c("NA", "N/A", "", "."))
  })
  enough_data <- reactive({
    !any(is.null(get_data()), is.null(get_meta_data()))
  })

  #---- Selectors ----
  observeEvent(input$folder_path, {
    selected_directory <- rstudioapi::selectDirectory()
    if (is.null(selected_directory)) {
      return()
    }
    files_to_review <- list.files(selected_directory, full.names = TRUE, pattern = ".res")
    reviewed_directory$nonmem_results <- lapply(files_to_review, nonmem_res)
    names(reviewed_directory$nonmem_results) <- gsub(
      pattern = ".res",
      replacement = "",
      x = basename(files_to_review)
    )

    updateSelectInput(
      session = session,
      inputId = "select_model_theta",
      choices = get_data()$Name
    )
    updateSelectInput(
      session = session,
      inputId = "select_model_omega",
      choices = get_data()$Name
    )
    updateSelectInput(
      session = session,
      inputId = "select_model_sigma",
      choices = get_data()$Name
    )
  })

  #---- Tables ----
  output$data <- DT::renderDataTable({
    data <- get_data()
    if (is.null(data)) {
      return()
    }
    if (!(input$data_filter %in% "")) {
      eval(parse(
        text = paste0(
          "data <- data |> filter(", input$data_filter, ")"
        )
      ))
    }
    data |>
      select(-c(Theta, Omega, Sigma)) |>
      mutate_if(is.numeric, round, 2) |>
      DT::datatable(rownames = FALSE)
  })
  output$data_mapping <- DT::renderDataTable({
    get_meta_data() |> DT::datatable()
  })
  output$theta_data <- DT::renderDataTable({
    #selected_model <- which(get_data()$Name %in% input$select_model_theta)
    reviewed_directory$nonmem_results[[input$select_model_theta]]$Theta |>
      as.data.frame() |>
      mutate_if(is.numeric, round, 3) |>
      DT::datatable(rownames = FALSE)
  })
  output$omega_data <- DT::renderDataTable({
    #selected_model <- which(get_data()$Name %in% input$select_model_omega)
    reviewed_directory$nonmem_results[[input$select_model_omega]]$Omega |>
      as.data.frame() |>
      cov_to_cor() |>
      mutate_if(is.numeric, round, 3) |>
      DT::datatable(rownames = FALSE)
  })
  output$sigma_data <- DT::renderDataTable({
    #selected_model <- which(get_data()$Name %in% input$select_model_sigma)
    reviewed_directory$nonmem_results[[input$select_model_sigma]]$Sigma |>
      as.data.frame() |>
      cov_to_cor() |>
      mutate_if(is.numeric, round, 3) |>
      DT::datatable(rownames = FALSE)
  })


  #---- Figures ----
  output$fig_cov <- renderPlotly({
    cat_variables <- get_field(
      get_meta_data(),
      field_name = "Name",
      type_name = "cat"
    )

    p <- ggpairs(
      get_sum_data() |>
        mutate(across(all_of(cat_variables), as.factor)),
      columns = input$select_cov,
      columnLabels = get_label(input$select_cov, get_meta_data()),
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
    # evid_var <- get_field(get_meta_data(), field_name = "Name", type_name = "evid")
    mdv_var <- get_field(get_meta_data(), field_name = "Name", type_name = "mdv")
    dv_var <- get_field(get_meta_data(), field_name = "Name", type_name = "dv")
    blq_var <- get_field(get_meta_data(), field_name = "Name", type_name = "blq")
    lloq_var <- get_field(get_meta_data(), field_name = "Name", type_name = "lloq")
    id_var <- get_field(get_meta_data(), field_name = "Name", type_name = "id")
    blq_var <- ifelse(length(blq_var) > 0, blq_var, "BLQ_VAR")
    lloq_var <- ifelse(length(lloq_var) > 0, lloq_var, "LLOQ_VAR")

    time_breaks <- unique(quantile(
      get_data() |>
        filter(.data[[mdv_var]] == 0) |>
        pull(.data[[input$select_time]]),
      probs = seq(0, 1, length.out = input$bins + 1)
    ))
    bin_data <- get_data() |>
      filter(.data[[mdv_var]] == 0) |>
      mutate(
        TimeBins = cut(
          .data[[input$select_time]],
          breaks = time_breaks,
          include.lowest = TRUE
        ),
        # Creates a specific DV accounting for BLQ data
        LLOQ_VAR = .data[[dv_var]],
        BLQ_VAR = 0,
        DV_BLQ = ifelse(.data[[blq_var]] > 0, .data[[lloq_var]], .data[[dv_var]]),
        Data = ifelse(.data[[blq_var]] <= 0, "Observed", "BLQ")
      )

    if (input$y_scale %in% "blq") {
      vpc_data <- bin_data |>
        group_by(TimeBins) |>
        summarise(
          time = median(.data[[input$select_time]]),
          # nblq = sum(.data[[blq_var]] > 0),
          blq = 100 * sum(.data[[blq_var]] > 0) / n()
        )

      p <- ggplot(
        data = vpc_data,
        mapping = aes(x = time, y = blq)
      ) +
        theme_bw() +
        geom_line() +
        labs(
          x = get_label(input$select_time, get_meta_data()),
          y = "Proportion of BLQ data [%]"
        )
      return(ggplotly(p))
    }

    vpc_data <- bin_data |>
      group_by(TimeBins) |>
      summarise(
        time = median(.data[[input$select_time]]),
        dv_min = quantile(DV_BLQ, probs = 0.05),
        dv_max = quantile(DV_BLQ, probs = 0.95),
        dv = median(DV_BLQ)
      )

    p <- ggplot(
      data = vpc_data,
      mapping = aes(x = time, y = dv)
    ) +
      theme_bw() +
      geom_point(
        data = bin_data,
        mapping = aes(
          x = .data[[input$select_time]],
          y = .data[["DV_BLQ"]],
          group = .data[[id_var]],
          color = .data[["Data"]]
        )
      ) +
      geom_line(mapping = aes(y = dv_min, linetype = "5th-95th percentiles"), color = "dodgerblue") +
      geom_line(mapping = aes(y = dv_max, linetype = "5th-95th percentiles"), color = "dodgerblue") +
      geom_line(mapping = aes(linetype = "median"), color = "tomato") +
      scale_linetype_manual(values = rep("solid", 2)) +
      scale_color_manual(values = c("Observed" = "black", "BLQ" = "grey")) +
      labs(
        x = get_label(input$select_time, get_meta_data()),
        y = get_label(dv_var, get_meta_data()),
        linetype = "Statistics",
        color = "Data"
      )

    if (input$y_scale %in% "log") {
      p <- p + scale_y_log10()
    }

    ggplotly(p)
  })

  #---- Downloads ----
  output$meta_data_template <- downloadHandler(
    filename = function() {
      "meta_data.csv"
    },
    content = function(file) {
      write.csv(read.csv("www/template_mapping.csv"), file, row.names = FALSE)
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
