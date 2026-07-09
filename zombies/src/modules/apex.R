#' Apex Charts
#'
#' @description An apexchart.js library integration
#' in shiny to provide interactive charts for PWA
#'
#'
#' @param id the namespace id of the apexchart
#'
#' @importFrom shiny uiOutput
#'
#' @export
apex_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("apex"))
  
}

#' Apex Charts
#'
#' @description An apexchart.js library integration
#' in shiny to provide interactive charts for PWA
#' on the server-side of an application
#'
#'
#' @param input list of inputs used in the shiny application session
#' @param output list of outputs used the shiny application session
#' @param session The shiny app session object
#' @param apexc the input function of an apex chart. Often times a defined reactive function
#' @param base_data the underlying apex data for refreshing the plot on the module side render
#'
#'@import apexcharter
#'
#'@importFrom shiny renderUI
#'@importFrom shiny req
#'@importFrom shiny is.reactive
#'
#'@export
apex_server <- function(input, output, session, apexc = NULL, base_data = NULL) {
  session$ns -> ns
  
  #Output of the visualization
  output[['viz']] <- apexcharter::renderApexchart({
    req(is.reactive(apexc))
    apexc()
  })
  
  #output of the UI to showcase the visualization
  output[['apex']] <- renderUI({
    req(apexc())
    apexcharter::apexchartOutput(outputId = ns("viz"))
  })
}
