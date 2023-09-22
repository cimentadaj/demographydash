library(shiny)
library(shiny.semantic)
library(shiny.router)
library(shinyjs)
library(shinycssloaders)

# Constants
background_color <- "#F4F4F4"
video_id <- "-EVgyaKO6vU"
logo_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/1205px-UN_emblem_blue.svg.png"

# Create Main Menu UI Component
menu_ui <- function() {
  div(
    style = "display: flex; justify-content: center; padding: 10px 0;",
    div(
      class = "ui secondary menu",
      tags$img(src = logo_url, style = "height: 50px; margin-right: 20px;"),
      a(class = "item", style = "color: black;", href = route_link("index"), icon("home"), "Home"),
      a(class = "item", style = "color: black;", href = route_link("data_loader"), "Data Loader"),
      a(class = "item", style = "color: black;", href = route_link("analysis"), "Analysis")
    )
  )
}

# Create Page UI Wrapper
page_ui <- function(content) {
  div(
    style = paste("background-color:", background_color, "; height: 100vh; display: flex; flex-direction: column;"),
    menu_ui(),
    div(
      style = "flex: 1; display: flex; justify-content: center; align-items: center;",
      div(
        class = "ui container",
        div(
          class = "ui grid",
          div(class = "twelve wide column centered", div(class = "ui segment", content))
        )
      )
    )
  )
}

# Create Home Page UI
home_page_ui <- function() {
  page_ui(
    tags$iframe(
      src = paste0("https://www.youtube.com/embed/", video_id),
      width = "560",
      height = "315",
      frameborder = "0",
      allowfullscreen = TRUE,
      style = "display: block; margin: 0 auto;"
    )
  )
}

# Create Data Loader Page UI
data_loader_page_ui <- function() {
  page_ui(
    div(fileInput("csv_file", "Upload Data", accept = ".csv"), withSpinner(uiOutput("more")))
  )
}

# Create Analysis Page UI
analysis_page_ui <- function() {
  page_ui(
    div(
      uiOutput("alert_content"),
      div(id = "analysis_content")
    )
  )
}

# Main UI
ui <- semanticPage(
  title = "UN-Themed Shiny App",
  router_ui(
    route("index", home_page_ui()),
    route("data_loader", data_loader_page_ui()),
    route("analysis", analysis_page_ui())
  )
)

# Server logic
server <- function(input, output, session) {
  router_server("index")

  data_processed <- reactiveVal(FALSE)

  output$more <- renderUI({
    req(input$csv_file)
    data <- read.csv(input$csv_file$datapath)
    Sys.sleep(4)  # Emulating data processing delay
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
      shinyjs::click("alert_content")  # Activate the message_box
    }
  })
}

shinyApp(ui, server)
