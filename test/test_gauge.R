library(shiny)
library(shinyMobile)

# Your UI definition
ui <- f7Page(f7TabLayout(navbar = f7Navbar(title = "Zombies Test"),f7Tabs(
  f7Tab(
    title = "Overview",
    tabName = "overview",
    icon = f7Icon("rays"),
    f7Block(
      f7BlockTitle("SP500") %>% f7Align(side = "center"),
      f7Row(f7Col(
        gauge_ui(
          id = "sp500_zombies",
          type  = "semicircle",
          value = 0,
          borderColor = "#2196f3",
          borderWidth = 10,
          valueFontSize = 41,
          valueTextColor = "#2196f3",
          labelText = "% Zombies"
        )
        
      ), f7Col(
        gauge_ui(
          id = "sp500_int",
          type  = "semicircle",
          value = 0,
          borderColor = "#2196f3",
          borderWidth = 10,
          valueFontSize = 41,
          valueTextColor = "#2196f3",
          labelText = "% Interest Expense are Zombies"
        )
      )),
      f7Slider(
        inputId = "slider",
        label = "New value:",
        min = 0,
        max = 100,
        value = 50
      ),
      f7Button("button", "Update Gauges")
    )
  )
)))

# Define server logic 
server <- function(input, output, session) {
  
  # Call the gauge server module for each gauge
  callModule(gauge_server,
               trigger = reactive({ input$button }),
               value_exp = reactive({ as.numeric(input$slider) }),
               id = "sp500_zombies")
  
  callModule(gauge_server,
               trigger = reactive({ input$button }),
               value_exp = reactive({ as.numeric(input$slider) }),
               id = "sp500_int")
}

# Run the application 
shinyApp(ui = ui, server = server)
