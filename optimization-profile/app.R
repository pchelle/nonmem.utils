library(tidyverse)
library(shiny)
library(shinyWidgets)
library(bslib, warn.conflicts = FALSE)
library(plotly, warn.conflicts = FALSE)

get_variables_with <- function(pattern, variable_names) {
  selected_var <- grepl(pattern, variable_names)
  return(variable_names[selected_var])
}

#---- UI ----
ui <- page_navbar(
  title = span(icon("arrow-trend-down"), " Optimization"),
  sidebar = sidebar(
    #---- Inputs ----
    accordion(
      accordion_panel(
        title = "Input Data",
        icon = icon("file-import"),
        card(
          fileInput(
            "dataset",
            span(icon("file-lines"), "Import .ext file"),
            accept = ".ext"
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
    #---- Iteration Profile ----
    nav_panel(
      title = "Iteration Profile",
      icon = icon("chart-line"),
      card(
        card_header(
          popover(
            span(icon("gear"), "Settings"),
            tagList(
              pickerInput("y_variables", span(icon("list-check"), "Select Variables"), multiple = TRUE, choices = NA),
              materialSwitch("is_relative", span(icon("percent"), " Relative Values"), status = "success")
            )
          )
        ),
        card_body(addSpinner(plotlyOutput("time_profile"), spin = "fading-circle"))
      )
    ),
    #---- Summary ----
    nav_panel(
      title = "Summary",
      icon = icon("box-archive"),
      card(
        full_screen = TRUE,
        card_header(value_box(
          title = "Number of Iterations",
          value = textOutput("iterations"),
          showcase = icon("arrows-spin")
        )),
        card_body(DT::dataTableOutput("sum_stat"))
        )
    )
  )
)

server <- function(input, output, session) {
  #---- Reactive Values ----
  input_data <- reactiveValues(data = NULL, variable_names = NULL)

  observeEvent(input$dataset, {
    input_data$data <- read.table(file = input$dataset$datapath, header = TRUE, skip = 1, check.names = FALSE)
    input_data$variable_names <- names(input_data$data)

    updatePickerInput(
      session = session,
      "y_variables",
      choices = list(
        "Objective Function" = "OBJ",
        "Fixed-Effects" = get_variables_with("THETA", input_data$variable_names),
        "Random-Effects" = get_variables_with("OMEGA", input_data$variable_names),
        "Residuals" = get_variables_with("SIGMA", input_data$variable_names)
      ),
      selected = "OBJ"
    )


    max(input_data$data$ITERATION)
  })

  #---- Tables ----
  output$data <- DT::renderDataTable({
    if (is.null(input_data$data)) {
      return()
    }
    data <- input_data$data
    if (!(input$data_filter %in% "")) {
      eval(parse(
        text = paste0(
          "data <- data |> filter(", input$data_filter, ")"
        )
      ))
    }
    data |> DT::datatable() |> DT::formatSignif(columns = 'OBJ', digits = 3)
  })

  output$sum_stat <- DT::renderDataTable({
    if (is.null(input_data$data)) {
      return()
    }

    # Summary providing for each variable but iteration, initial and final values, their difference and variation
    ext_data <- input_data$data |> filter(ITERATION >= 0)
    summary_data <- bind_cols(
      Parameter = names(ext_data),
      ext_data |> summarise_all(first) |> t() |> `colnames<-`("Initial Estimate"),
      ext_data |> summarise_all(last) |> t() |> `colnames<-`("Final Estimate")
    ) |>
      mutate(
        Difference = `Final Estimate` - `Initial Estimate`,
        `Variation [%]` = round(100 * (`Final Estimate` - `Initial Estimate`) / `Initial Estimate`, 1)
      ) |>
      filter(Parameter != "ITERATION")
    summary_table <- summary_data |> 
      DT::datatable() |> 
      DT::formatSignif(columns = c('Initial Estimate', 'Final Estimate', 'Difference'), digits = 3)
    return(summary_table)
  })

  #---- Figures  ----
  output$time_profile <- renderPlotly({
    if (is.null(input_data$data)) {
      return()
    }
    if (length(input$y_variables) == 0) {
      return()
    }

    profile_data <- input_data$data |> filter(ITERATION >= 0)
    profile_data_r <- profile_data |>
      mutate(across(all_of(input$y_variables), ~ 100 * (. - first(.)) / first(.)))

    profile_data <- left_join(
      profile_data,
      profile_data_r,
      by = "ITERATION",
      suffix = c("", "_REL")
    )

    profile_plots <- list()
    for (y_variable in input$y_variables) {
      profile_plot <- ggplot(
        data = profile_data,
        mapping = aes(
          x = ITERATION,
          y = .data[[ifelse(input$is_relative, paste0(y_variable, "_REL"), y_variable)]],
          group = 1,
          text = paste0(
            "Iteration: ", .data[["ITERATION"]], "\n",
            "OFV: ", round(.data[["OBJ"]], 3), "\n",
            y_variable, ": ", .data[[y_variable]], "\n",
            "Relative: ", .data[[paste0(y_variable, "_REL")]], " %"
          )
        )
      ) +
        theme_bw() +
        geom_hline(
          mapping = aes(
            text = "Initial Estimate",
            yintercept = ifelse(input$is_relative, 0, first(.data[[y_variable]]))
          ),
          linetype = "dashed",
          color = "firebrick"
        ) +
        geom_hline(
          aes(
            text = "Final Estimate",
            yintercept = ifelse(
              input$is_relative,
              last(.data[[paste0(y_variable, "_REL")]]),
              last(.data[[y_variable]])
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
          y = ifelse(
            input$is_relative,
            paste0(y_variable, " variation [%]"),
            y_variable
          )
        )
      profile_plots[[y_variable]] <- ggplotly(profile_plot, tooltip = "text")
    }
    final_profile_plot <- subplot(
      profile_plots,
      nrows = length(input$y_variables),
      shareX = TRUE,
      shareY = FALSE,
      titleY = TRUE
    )
    return(final_profile_plot)
  })

  #---- Text ----
  output$iterations <- renderText({
    if (is.null(input_data$data)) {
      return("")
    }
    max(input_data$data$ITERATION)
  })
  
  #---- Downloads ----
  output$dataset_report <- downloadHandler(
    filename = function() {
      "optimization-report.docx"
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
        "www/optimization-analysis.qmd",
        output_file = file,
        params = list(dataFile = input$dataset$datapath)
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
