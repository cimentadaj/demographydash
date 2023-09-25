## https://github.com/Appsilon/shiny.semantic/blob/develop/examples/grid/app.R

background_color <- "#FFFFFF"
video_id <- "-EVgyaKO6vU"
logo_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/UN_emblem_blue.svg/1205px-UN_emblem_blue.svg.png"
## logo_url <- here::here("inst/app/www/un_logo.png")


#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @return A shiny semantic UI for the application.
#' @importFrom shiny.semantic semanticPage
#' @importFrom shiny.router router_ui route
#' @noRd
app_ui <- function(request) {
  ui <- semanticPage(
    title = "UN-Themed Shiny App",
    router_ui(
      route("index", home_page_ui()),
      route("data_loader", data_loader_page_ui()),
      route("analysis", analysis_page_ui())
    )
  )

  ui
}

#' Create Main Menu UI Component
#'
#' @return A shiny UI div containing the main menu.
#' @importFrom shiny div tags a icon
#' @importFrom shiny.router route_link
menu_ui <- function() {
  shiny::div(
    style = "width: 75%; margin: auto;",  # Constrain and center the entire content
    shiny::div(
      style = "display: flex; flex-direction: column; align-items: center; padding: 10px 0; width: 100%;",
      shiny::div(
        style = "text-align: left; width: 100%; border-bottom: 1px solid lightgrey;",
        shiny::tags$img(src = logo_url, style = "height: 50px; margin-right: 20px;")
      ),
      shiny::div(
        class = "ui secondary menu",
        style = "width: 100%; justify-content: center; background-color: #333333;",
        shiny::tags$style(HTML("
          .item::after {
            content: '';
            position: absolute;
            right: 0;
            top: 0;
            height: 100%;
            border-right: 1px solid white;
          }
          .item:last-child::after {
            border-right: none;
          }
        ")),
        shiny::a(class = "item", style = "color: white; position: relative;", href = shiny.router::route_link("index"), "Home"),
        shiny::a(class = "item", style = "color: white; position: relative;", href = shiny.router::route_link("data_loader"), "Data Loader"),
        shiny::a(class = "item", style = "color: white; position: relative;", href = shiny.router::route_link("analysis"), "Analysis")
      )
    )
  )
}

#' Create Page UI Wrapper
#'
#' @param content Content to be wrapped in the page UI.
#' @return A shiny UI div for the page layout.
#' @importFrom shiny div
page_ui <- function(content) {
  shiny::div(
    style = paste("background-color:", background_color, "; height: 100vh; display: flex; flex-direction: column;"),
    menu_ui(),
    shiny::div(
      style = "flex: 1; display: flex; justify-content: center; align-items: center;",
      shiny::div(
        class = "ui container",
        shiny::div(
          class = "ui grid",
          shiny::div(class = "twelve wide column centered", content) # Removed 'ui segment' class here
        )
      )
    )
  )
}

#' Create Home Page UI
#'
#' @return A shiny UI for the home page.
#' @importFrom shiny tags
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

#' Create Data Loader Page UI
#'
#' @return A shiny UI for the data loader page.
#' @importFrom shiny div uiOutput
#' @importFrom shiny.semantic fileInput
#' @importFrom shinycssloaders withSpinner
data_loader_page_ui <- function() {
  page_ui(
    div(fileInput("csv_file", "Upload Data", accept = ".csv"), withSpinner(uiOutput("more")))
  )
}

#' Create Analysis Page UI
#'
#' @return A shiny UI for the analysis page.
#' @importFrom shiny div uiOutput
analysis_page_ui <- function() {
  page_ui(
    div(
      uiOutput("alert_content"),
      div(id = "analysis_content")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @importFrom shiny tags
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "demographydash"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
