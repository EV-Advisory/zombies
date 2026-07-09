gauge_ui <- function(id, ...) {
  ns <- NS(id)
  f7Gauge(id = ns("gauge"),...)  
}

gauge_server <- function(input,
                         output,
                         session,
                         trigger,
                         value_exp,
                         ...) {
  ns <- session$ns
  
  
  # update gauge
  observeEvent(trigger(), {
    # browser()
    req(is.numeric(value_exp()))
    # print("Update Gauge Value")
    # print(ns("gauge"))
    updateF7Gauge(id = ns('gauge'),
                  value = value_exp())
  }, ignoreInit = F,label = "Gauge UI")
}

# # Define the UI function
# gauge_ui <- function(id, ...) {
#   ns <- NS(id)
#   uiOutput(ns("gauge"),...)
# }
# 
# # Define the server function
# gauge_server <- function(input,
#                          output,
#                          session,
#                          trigger,
#                          value_exp,
#                          ...) {
#   ns <- session$ns
#   
#   observeEvent(trigger(), {
#     req(is.numeric(value_exp()))
#     
#     output[[ns("gauge")]] <- renderUI({
#       f7Gauge(id = ns("gauge_render"),
#               value = value_exp(),
#               ...)
#     })
#     
#   }, ignoreInit = F, label = "Gauge UI")
# }
