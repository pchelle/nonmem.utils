#' @title shiny_toolbox
#' @description
#' Run Shiny Toolbox to select which Nonmem tool to use
#' @return A shiny app
#' @export
shiny_toolbox <- function() {
  # Gadget UI requires shiny and miniUI
  can_run <- all(
    requireNamespace("shiny", quietly = TRUE),
    requireNamespace("miniUI", quietly = TRUE),
    requireNamespace("rstudioapi", quietly = TRUE)
  )
  if (!can_run) {
    cat("Please install shiny, miniUI and rstudioapi packages to run this function")
    return()
  }
  # Shiny App UI
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar(shiny::span(shiny::icon("toolbox"), "Shiny Toolbox")),
    miniUI::miniContentPanel(
      shiny::selectInput(
        "app_type",
        shiny::span(shiny::icon("tag"), "Application type"),
        choices = c("Shiny App" = "run_shiny", "Quarto Report" = "report")
      ),
      shiny::selectInput(
        "app_name",
        shiny::span(shiny::icon("file-code"), "Available Analyses"),
        choices = list.files(system.file("shiny", package = "nonmem.utils"))
      ),
      shiny::conditionalPanel(
        condition = "input.app_type == 'report'",
        shiny::span(
          shiny::icon("database"),
          shiny::strong("Report dataset input")
        ),
        shiny::actionButton("data_path", "Select Dataset", icon = shiny::icon("file")),
        shiny::verbatimTextOutput("data_path_text"),
        shiny::span(
          shiny::icon("signs-post"),
          shiny::strong("Report dictionary input")
        ),
        shiny::actionButton("meta_data_path", "Select Dictionary", icon = shiny::icon("file")),
        shiny::verbatimTextOutput("meta_data_path_text")
      )
    )
  )
  # Shiny App Server
  server <- function(input, output, session) {
    quarto_input <- shiny::reactiveValues(
      data_path = NULL,
      meta_data_path = NULL
    )
    output$data_path_text <- shiny::renderText({
      quarto_input$data_path
    })
    output$meta_data_path_text <- shiny::renderText({
      quarto_input$meta_data_path
    })
    # Reactive check of combined dimensions
    shiny::observeEvent(input$app_type, {
      shiny::updateSelectInput(
        session = session,
        "app_name",
        choices = switch(input$app_type,
          "run_shiny" = list.files(system.file("shiny", package = "nonmem.utils")),
          "report" = gsub(
            pattern = "\\.qmd$",
            replacement = "",
            x = list.files(system.file("quarto", package = "nonmem.utils"))
          )
        )
      )
    })

    shiny::observeEvent(input$app_name, {
      if (input$app_name %in% "workflow-summary") {
        shiny::updateActionButton(
          session = session,
          "data_path",
          label = "Select Directory",
          icon = shiny::icon("folder-open")
        )
        return()
      }
      shiny::updateActionButton(
        session = session,
        "data_path",
        "Select Dataset",
        icon = shiny::icon("file")
      )
    })

    shiny::observeEvent(input$data_path, {
      if (input$app_name %in% "workflow-summary") {
        quarto_input$data_path <- rstudioapi::selectDirectory()
        return()
      }
      quarto_input$data_path <- rstudioapi::selectFile(
        caption = "Select Input Dataset",
        filter = "csv and tab files (*.csv|*.tab)"
      )
    })
    shiny::observeEvent(input$meta_data_path, {
      if (input$app_name %in% "workflow-summary") {
        return()
      }
      quarto_input$meta_data_path <- rstudioapi::selectFile(
        caption = "Select Input Dictionary",
        filter = "csv files (*.csv)"
      )
    })

    # Handle the Done button being pressed.
    shiny::observeEvent(input$done, {
      shiny::stopApp(
        returnValue = list(
          type = input$app_type,
          name = input$app_name,
          data_path = quarto_input$data_path,
          meta_data_path = quarto_input$meta_data_path
        )
      )
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp()
    })
  }
  # Run the app on default viewer
  app_value <- shiny::runGadget(
    ui, server,
    viewer = shiny::dialogViewer("Shiny Toolbox", width = 500, height = 400)
  )
  if (is.null(app_value)) {
    return(invisible())
  }
  if (app_value$type %in% "run_shiny") {
    return(rstudioapi::sendToConsole(
      paste0("nonmem.utils::", app_value$type, "(\"", app_value$name, "\")"),
      execute = FALSE, animate = TRUE
    ))
  }
  if (app_value$name %in% "workflow-summary") {
    return(rstudioapi::sendToConsole(
      paste0("nonmem.utils::report_workflow_summary(\"", app_value$data_path, "\")"),
      execute = FALSE, animate = TRUE
    ))
  }
  function_name <- paste0(
    "nonmem.utils::", app_value$type, "_",
    gsub(pattern = "-", replacement = "_", x = app_value$name)
  )
  return(rstudioapi::sendToConsole(
    paste0(function_name, "(\"", app_value$data_path, "\", \"", app_value$meta_data_path, "\")"),
    execute = FALSE, animate = TRUE
  ))
}

#' @title run_shiny
#' @description
#' Run a shiny app included in the package
#' @param app_name Name of app
#' @export
run_shiny <- function(app_name) {
  # list.files(system.file("shiny", package = "nonmem.utils"))
  shiny::runApp(system.file("shiny", app_name, package = "nonmem.utils"))
}
