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
to_label <- function(label, unit) {
  no_unit <- (is.na(unit) | unit %in% "")
  return(paste0(label, ifelse(no_unit, "", paste0(" [", unit, "]"))))
}
get_label <- function(var_names, mapping) {
  selected_rows <- sapply(var_names, function(var_name){which(mapping$Name %in% var_name)})
  return(to_label(mapping$Label[selected_rows], mapping$Unit[selected_rows]))
}

#---- UI ----
ui <- page_navbar(
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
            accept = ".csv"
          )
        ),
        card(
          card_body(
            fileInput(
              "metadata",
              tooltip(
                span(icon("signs-post"), " Import meta-data"),
                span(icon("circle-info"), "Meta-data indicates type, label and unit for each dataset variable")
              ),
              accept = ".csv"
            )
          ),
          card_footer(downloadButton("metadata_template", "Template"))
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
    #---- Time Profile ----
    nav_panel(
      title = "Time Profile",
      icon = icon("chart-line"),
      card(
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            tagList(
              pickerInput("select_time", span(icon("timeline"), "Time Variable"), multiple = FALSE, choices = NA),
              sliderInput("bins", span(icon("layer-group"), " Number of bins"), min = 1, max = 20, value = 5),
              pickerInput("y_scale", span(icon("ruler"), " Scale"), choices = c("linear", "log", "blq"))
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
    #---- Summary ----
    nav_panel(
      title = "Summary",
      icon = icon("box-archive"),
      layout_column_wrap(
        card(
          full_screen = TRUE,
          card_header(" Statistics"),
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
)


server <- function(input, output, session) {
  #---- Reactive Values ----
  get_data <- reactive({
    if (is.null(input$dataset)) {
      return()
    }
    read.csv(input$dataset$datapath)
  })
  get_metadata <- reactive({
    if (is.null(input$metadata)) {
      return()
    }
    read.csv(input$metadata$datapath)
  })
  enough_data <- reactive({
    !any(is.null(get_data()), is.null(get_metadata()))
  })
  get_sum_data <- reactive({
    if (!enough_data()) {
      return()
    }
    id_var <- get_field(get_metadata(), field_name = "Name", type_name = "id")
    sum_data <- get_data() |>
      group_by(.data[[id_var]]) |>
      summarise_all(first)
    return(sum_data)
  })

  #---- Selectors ----
  observeEvent(input$metadata, {
    all_covariates <- get_metadata() |>
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
    updatePickerInput(
      session = session,
      "select_time",
      choices = get_metadata() |>
        filter(Type %in% c("time", "tad")) |>
        pull(Name),
      selected = get_metadata() |>
        filter(Type %in% "time") |>
        pull(Name),
      choicesOpt = list(
        subtext = get_metadata() |>
          filter(Type %in% c("time", "tad")) |>
          pull(Label)
      )
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
  output$sum_cov <- DT::renderDataTable({
    if (!enough_data()) {
      return()
    }
    covariates <- get_metadata() |>
      filter(Type %in% "cov")
    # arrange(Name)

    if (nrow(covariates) == 0) {
      return()
    }

    get_sum_data() |>
      select(all_of(covariates$Name)) |>
      pivot_longer(
        cols = everything(),
        names_to = "Covariate",
        values_to = "Values"
      ) |>
      group_by(Covariate) |>
      summarise(
        N = sum(!is.na(Values)),
        Mean = mean(Values, na.rm = TRUE),
        Median = median(Values, na.rm = TRUE),
        SD = sd(Values, na.rm = TRUE),
        Min = min(Values, na.rm = TRUE),
        Max = max(Values, na.rm = TRUE)
      ) |>
      mutate(Covariate = get_label(Covariate, covariates)) |>
      # Round values to 2 decimal
      mutate_if(is.numeric, round, 2) |>
      DT::datatable(rownames = FALSE)
  })
  output$sum_cat <- DT::renderDataTable({
    categoricals <- get_metadata() |>
      filter(Type %in% "cat")
    #|> arrange(Name)

    if (nrow(categoricals) == 0) {
      return()
    }

    get_sum_data() |>
      select(all_of(categoricals$Name)) |>
      mutate_all(as.factor) |>
      pivot_longer(
        cols = everything(),
        names_to = "Covariate",
        values_to = "Values"
      ) |>
      group_by(Covariate) |>
      summarise(
        Categories = paste(unique(Values), collapse = "<br>"),
        N = sum(!is.na(Values)),
        `Distinct Categories` = n_distinct(Values),
        Count = paste(sapply(
          unique(Values),
          function(category) {
            sum(Values %in% category)
          }
        ), collapse = "<br>"),
        Percent = paste(sapply(
          unique(Values),
          function(category) {
            round(100 * sum(Values %in% category) / n(), 1)
          }
        ), collapse = "<br>")
      ) |>
      mutate(Covariate = get_label(Covariate, categoricals)) |>
      DT::datatable(rownames = FALSE, escape = FALSE)
  })

  #---- Figures ----
  output$fig_cov <- renderPlotly({
    cat_variables <- get_field(
      get_metadata(),
      field_name = "Name",
      type_name = "cat"
    )

    p <- ggpairs(
      get_sum_data() |>
        mutate(across(all_of(cat_variables), as.factor)),
      columns = input$select_cov,
      columnLabels = get_label(input$select_cov, get_metadata()),
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
    # evid_var <- get_field(get_metadata(), field_name = "Name", type_name = "evid")
    mdv_var <- get_field(get_metadata(), field_name = "Name", type_name = "mdv")
    dv_var <- get_field(get_metadata(), field_name = "Name", type_name = "dv")
    blq_var <- get_field(get_metadata(), field_name = "Name", type_name = "blq")
    lloq_var <- get_field(get_metadata(), field_name = "Name", type_name = "lloq")
    id_var <- get_field(get_metadata(), field_name = "Name", type_name = "id")
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
          x = get_label(input$select_time, get_metadata()),
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
        x = get_label(input$select_time, get_metadata()),
        y = get_label(dv_var, get_metadata()),
        linetype = "Statistics",
        color = "Data"
      )

    if (input$y_scale %in% "log") {
      p <- p + scale_y_log10()
    }

    ggplotly(p)
  })

  #---- Downloads ----
  output$metadata_template <- downloadHandler(
    filename = function() {
      "metadata.csv"
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
      rmarkdown::render(
        "www/dataset-analysis.qmd",
        output_file = file,
        params = list(
          dataFile = input$dataset$datapath,
          metadataFile = input$metadata$datapath,
          bins = input$bins
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
