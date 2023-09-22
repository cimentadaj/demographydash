#' The application server-side logic
#'
#' This function defines the server logic for the Shiny application, managing data processing, UI rendering, and routing.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @importFrom shiny reactiveVal renderUI req actionButton observeEvent observe
#' @importFrom shiny.router router_server change_page get_page
#' @importFrom shiny.semantic message_box
#' @importFrom shinyjs click
#' @importFrom utils read.csv
#' @noRd
app_server <- function(input, output, session) {
  router_server("index")

  data_processed <- reactiveVal(FALSE)

  output$more <- renderUI({
    req(input$csv_file)
    data <- read.csv(input$csv_file$datapath)
    Sys.sleep(4) # Emulating data processing delay
    data_processed(TRUE)
    actionButton("process", "Analysis button")
  })

  output$alert_content <- renderUI({
    if (!data_processed()) {
      message_box(class = "warning", header = "Warning", content = "Please upload and process the data first.")
    }
  })

  observeEvent(input$process, {
    change_page("analysis")
  })

  observe({
    current_page <- get_page()
    if (current_page == "analysis" && !data_processed()) {
      click("alert_content") # Activate the message_box
    }
  })
}
