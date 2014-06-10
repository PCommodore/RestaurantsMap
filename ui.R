require(shiny)
require(rCharts)
require(googleVis)

#networks <- getNetworks()
shinyUI(pageWithSidebar( 
  #selectInput('network', '', sort(names(networks)), 'citibikenyc'),
  headerPanel(h1("Restaurant Map Singapore")),
  
  mainPanel(
    mapOutput('map_container'),
    h2("Data Table"),
    checkboxInput(inputId = "pageable", label = "Paginate?"),
    conditionalPanel("input.pageable==true",
                     numericInput(inputId = "pagesize",
                                  label = "Restaurants per page",5)),
    htmlOutput("myTable"),
    width = 8),
  
  sidebarPanel(
    h4("Restaurant Summary"),
    selectInput('restaurantsummary','Search Restaurant',sort(restaurantdf()$name)),
    htmlOutput("mySummary"),
    h4("Relationship Viewer"),
    htmlOutput("myChart"),
    selectInput('variablex','Variable X:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
    selectInput('variabley','Variable Y:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
    h2(""),
    tableOutput('hello'),
    width = 4)
    
    
  
  
))