require(shiny)
require(rCharts)
require(googleVis)

#networks <- getNetworks()
shinyUI(pageWithSidebar( 
  #selectInput('network', '', sort(names(networks)), 'citibikenyc'),
  headerPanel(h4("Dining Map Singapore - restaurant status in a glance..."), windowTitle = "RMap"),

  
  mainPanel(
    tabsetPanel(
      tabPanel("Map", mapOutput("map_container")),
      tabPanel("Data", checkboxInput(inputId = "pageable", label = "Paginate?"),
               conditionalPanel("input.pageable==true",
                                numericInput(inputId = "pagesize",
                                             label = "Restaurants per page",5)),
               htmlOutput("myTable")),
      tabPanel("Relationships", selectInput('variablex','Variable X:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
               selectInput('variabley','Variable Y:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
               htmlOutput("myChart"))
    ),
    
    
    
    #mapOutput('map_container'),
    #br(),
    #h4("Data Table"),
    #checkboxInput(inputId = "pageable", label = "Paginate?"),
    #conditionalPanel("input.pageable==true",
                     #numericInput(inputId = "pagesize",
                                  #label = "Restaurants per page",5)),
    #htmlOutput("myTable"),
    #h4("Relationship Viewer"),
    #selectInput('variablex','Variable X:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
    #selectInput('variabley','Variable Y:', c("Likes" = 'likes',"Talking" = 'talking', "Here" = 'here')),
    #htmlOutput("myChart"),
    width = 8),
  
    
  sidebarPanel(
    h4("Time"),
    h5(strong(textOutput("datetimenow"))),
    h4("Restaurant Summary"),
    checkboxInput(inputId = "centeronrestaurant", label = "Center Map on Restaurant?", value = FALSE),
    selectInput('restaurantsummary','Search Restaurant',sort(restaurantdf()$name)),
    htmlOutput("mySummary"),
    h2(""),
    #tableOutput('hello'),
    width = 4)
    
    
  
  
))