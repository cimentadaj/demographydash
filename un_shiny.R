library(shiny)
library(shiny.semantic)
library(shiny.router)
library(shinyjs)
library(shinycssloaders)

background_color <- "#F4F4F4"
video_id <- "-EVgyaKO6vU" # Replace with the ID of your YouTube video
logo_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/1205px-UN_emblem_blue.svg.png"

# Menu
menu <- div(
  style = paste("display: flex; justify-content: center; padding: 10px 0;"),
  div(
    class = "ui secondary menu",
    tags$img(src = logo_url, style = "height: 50px; margin-right: 20px;"),
    a(class = "item", style = "color: black;", href = route_link("index"), icon("home"), "Home"),
    a(class = "item", style = "color: black;", href = route_link("other"), icon("clock"), "Other")
  )
)

page1_ui <- function() {
  div(
    class = "ui big button",
    id = "start_button",
    "Start"
  )
}

page2_ui <- function() {
  div(
    fileInput("csv_file", "Upload Data", accept = ".csv"),
    withSpinner(uiOutput("more"))
  )
}

page3_ui <- function() {
  div(
    menu,  # Integrated menu
  )
}

page <- function(title, content) {
  div(
    style = paste("background-color:", background_color, "; height: 100vh; display: flex; flex-direction: column;"), # Flexbox styles
    menu, # Menu at the top
    div(
      style = "flex: 1; display: flex; justify-content: center; align-items: center;", # Content wrapper
      div(
        class = "ui container",
        div(
          class = "ui grid",
          div(
            class = "twelve wide column centered", # Centered content column
            div(
              class = "ui segment",
              p(content)
            )
          )
        )
      )
    )
  )
}

root_page <- page(
  "Home page",
  div(
    style = "display: flex; justify-content: center; align-items: center; height: 100%;",
    div(
      p("Welcome on sample routing page!"), # This is the text
      tags$iframe(
        src = paste0("https://www.youtube.com/embed/", video_id),
        width = "560",
        height = "315",
        frameborder = "0",
        allowfullscreen = TRUE,
        style = "display: block; margin: 0 auto;"
      )
    )
  )
)

## root_page <- page("Home page", "Welcome on sample routing page!")
other_page <- page("Some other page", "Lorem ipsum dolor sit amet.")


ui <- semanticPage(
  title = "Three-Step App",
  router_ui(
    route("page1", page1_ui()),
    route("page2", page2_ui()),
    route("page3", page3_ui()),
    route("index", root_page),
    route("other", other_page)
  )
)

server <- function(input, output, session) {
  router_server("page1")

  observeEvent(input$start_button, {
    change_page("page2")
  })

  output$more <- renderUI({
    req(input$csv_file)
    inFile <- input$csv_file
    data <- read.csv(inFile$datapath)
    Sys.sleep(4)
    actionButton("process", "Analysis button")
  })

  observeEvent(input$process, {
    change_page("page3")
  })
}

shinyApp(ui, server)

