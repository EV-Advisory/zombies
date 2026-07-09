#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

suppressPackageStartupMessages({
  require(shiny)
  require(shinyMobile)
  require(magrittr)
})

invisible(sapply(
  list.files(
    "zombies/src/modules",
    full.names = T,
    include.dirs = F
  ),
  source,
  local = F
))


ui <-
  f7Page(f7TabLayout(
    #Only a single panel - LHS for configurations
    panels = tagList(f7Panel(
      side = "left",
      title = "Configuration",
      f7Align(f7Col(
        f7Select(
          "sector",
          "Sector",
          choices = c("All" = NA_character_, unique(s500_z_reports$sector)),
          selected = "All"
        ),
        f7Select("year",
                 "Year",
                 choices = unique(lubridate::year(
                   as.Date(s500_z_reports$date)
                 )) %>% .[order(.)])
      ), side = "center")
    )),
    navbar = f7Navbar(title = "Zombies", leftPanel = T),
    f7Tabs(
      f7Tab(
        title = "Overview",
        tabName = "overview",
        icon = f7Icon("rays"),
        f7Card(
          id = "overview-zombies",
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
            f7BlockFooter(text = uiOutput("500_zombies"))
          ),
          f7Block(
            f7BlockTitle("SP600") %>% f7Align(side = "center"),
            f7Row(f7Col(
              gauge_ui(
                "sp600_zombies",
                type  = "semicircle",
                value = 0,
                borderColor = "#2196f3",
                borderWidth = 10,
                valueFontSize = 41,
                valueTextColor = "#2196f3",
                labelText = "% Zombies"
              )
              # f7Gauge(id = "sp500_zombies",)
              
            ), f7Col(
              gauge_ui(
                id = "sp600_int",
                type  = "semicircle",
                value = 0,
                borderColor = "#2196f3",
                borderWidth = 10,
                valueFontSize = 41,
                valueTextColor = "#2196f3",
                labelText = "% Interest Expense are Zombies"
              )
              
            ))
          )
        )
        
      ),
      f7Tab(
        title = "Performance",
        tabName = "performance",
        icon = f7Icon("chart_pie_fill")
      ),
      f7Tab(
        title = "User Guide",
        tabName = "user_guide",
        active = T,
        icon = f7Icon("exclamationmark_shield_fill"),
        f7Align(
          f7ExpandableCard(
            title = "What are Zombies?",
            subtitle = "The financial distress phenomena",
            id = "overview-zombies",
            fullBackground = T,
            image = "https://cdn.pixabay.com/photo/2016/11/22/08/54/horror-1848697_1280.jpg",
            withMathJax(
              "$$\\text{Display formula in heading }X_n=X_{n-1}-\\varepsilon$$"
            )
            
          ),
          side = "center"
        )
      )
      
    )
  ))
# Define UI for application that draws a histogram
# ui <- fluidPage(# Application title
#   titlePanel("Old Faithful Geyser Data"),
#
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(sidebarPanel(
#     sliderInput(
#       "bins",
#       "Number of bins:",
#       min = 1,
#       max = 50,
#       value = 30
#     )
#   ),
#
#   # Show a plot of the generated distribution
#   mainPanel(plotOutput("distPlot"))))

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$toggle, {
    updateF7Navbar()
  })
  sector <- reactiveVal(value = NULL)
  ann <- reactiveVal(value = NULL)
  
  observe({
    req(!is.null(input$year), !is.null(input$sector))
    # print("Value made")
    sector(input$sector)
    ann(input$year)
  })
  
  s_500_year <- reactive({
    req(is.reactive(sector), is.reactive(ann))
    # print("Object made")
    df_out <- s500_z_reports
    if (!is.na(sector()) & nchar(sector()) > 0)
      df_out %<>% dplyr::filter(sector == sector())
    df_out %>% dplyr::filter(year(date) == ann())
  })
  
  s_600_year <- reactive({
    req(is.reactive(sector), is.reactive(ann))
    # print("Object made")
    df_out <- s600_z_reports
    if (!is.na(sector()) & nchar(sector()) > 0)
      df_out %<>% dplyr::filter(sector == sector())
    df_out %>% dplyr::filter(year(date) == ann())
  })
  
  
  # update gauge
  observeEvent(!is.null(sector()) | !is.null(ann()), {
    # browser()
    req(is.data.frame(s_500_year()))
    # print("Update Gauge Value")
    updateF7Gauge(id = "sp500_zombies",
                  value = round(
                    sum(s_500_year()$is_zombie, na.rm = T) /
                      nrow(s_500_year()) * 100
                  ))
  }, ignoreInit = F)
  
  callModule(
    gauge_server,
    id = "sp500_zombies",
    trigger = reactive(!is.null(sector()) |
                         !is.null(ann())),
    value_exp = reactive({
      req(is.data.frame(s_500_year()))
      round(sum(s_500_year()$is_zombie, na.rm = T) /
              nrow(s_500_year()) * 100)
    })
  )
  
  callModule(
    gauge_server,
    id = "sp600_zombies",
    trigger = reactive(!is.null(sector()) |
                         !is.null(ann())),
    value_exp = reactive({
      req(is.data.frame(s_600_year()))
      round(sum(s_600_year()$is_zombie, na.rm = T) /
              nrow(s_600_year()) * 100)
    })
  )
  
  callModule(
    gauge_server,
    id = "sp500_int",
    trigger = reactive(!is.null(sector()) |
                         !is.null(ann())),
    value_exp = reactive({
      req(is.data.frame(s_500_year()))
      round(
        sum(
          s_500_year() %>% dplyr::filter(is_zombie) %>% pull(interestExpense) %>% abs(.),
          na.rm = T
        ) /
          sum(s_500_year() %>% pull(interestExpense) %>% abs(.), na.rm = T) * 100
      )
    })
  )
  callModule(
    gauge_server,
    id = "sp600_int",
    trigger = reactive(!is.null(sector()) |
                         !is.null(ann())),
    value_exp = reactive({
      req(is.data.frame(s_600_year()))
      round(
        sum(
          s_600_year() %>% dplyr::filter(is_zombie) %>% pull(interestExpense) %>% abs(.),
          na.rm = T
        ) /
          sum(s_600_year() %>% pull(interestExpense) %>% abs(.), na.rm = T) * 100
      )
    })
  )
  
  output[['500_zombies']]<-renderUI({
    req(is.data.frame(s_500_year()))
  tags$p(paste0("In the S&P 500, there were ",sum(s_500_year()$is_zombie, na.rm = T)," companies that were officially classified as zombies spending more than ",scales::dollar(s_500_year() %>% dplyr::filter(is_zombie) %>% pull(interestExpense) %>% abs(.)%>%sum(.,na.rm=T)),collapse = " "))
    
    
    })
  
}

# Run the application
shinyApp(ui = ui, server = server)
