
require(shiny)
require(rCharts)
require(googleVis)
shinyServer(function(input, output, session){
  
    
  output$map_container <- renderMap({
    
    if (input$centeronrestaurant == FALSE) 
      {
        plotMapList()
      }
  
    
    
    else if (input$centeronrestaurant)      
      { 
        df = restaurantdf()
        latfocus = df[df$name==input$restaurantsummary,5]
        lngfocus = df[df$name==input$restaurantsummary,6]
        plotMapList(mapcenter = c(latfocus,lngfocus), mapzoom = 13)
      }
    
  })
  
  output$hello <- renderTable({
    restaurantdf()[,1:6]    
  })
  
  output$myChart <- renderGvis(
    { 
      df <- restaurantdf()[,1:6]
      ###Rcharts Plot Start
      #p1 <- rPlot(input$variablex, input$variabley, data = df, type = 'point')
      #p1$addParams(width = 880, height = 550, dom = 'myChart', title = "Scatter Plot")
      #p1$guides(x = list(title = "x values", min = 0, max = 15000))
      #p1$guides(y = list(title = "y values", min = 0, max = 15000))
      #return(p1) 
      ###Rcharts Plot End
      
      df1 = data.frame(xvar=df[,names(df)==input$variablex],yvar=df[,names(df)==input$variabley])
      
      gvisScatterChart(df1,
                       options=list(gvis.editor="Editor",width=820, height = 800,
                                    title = "Scatter Plot",backgroundColor="#ffffff"))
      
      
      
    })
  
  myTableOptions <- reactive({
    list(
      page=ifelse(input$pageable==TRUE,'enable','disable'),
      pageSize=input$pagesize,
      width=820,
      height=200
    )
  })
  output$myTable <- renderGvis({
    gvisTable(restaurantdf()[,1:6],options=myTableOptions())
  })
  
  output$mySummary <- renderGvis(
{ 
  df <- restaurantdf()
  ###Rcharts Plot Start
  #p1 <- rPlot(input$variablex, input$variabley, data = df, type = 'point')
  #p1$addParams(width = 880, height = 550, dom = 'myChart', title = "Scatter Plot")
  #p1$guides(x = list(title = "x values", min = 0, max = 15000))
  #p1$guides(y = list(title = "y values", min = 0, max = 15000))
  #return(p1) 
  ###Rcharts Plot End
  
  df1 = data.frame(name=df[df$name==input$restaurantsummary,1],
                   likes=df[df$name==input$restaurantsummary,2],
                   talking=df[df$name==input$restaurantsummary,3],
                   here=df[df$name==input$restaurantsummary,4])
  
  gvisBarChart(df1,
                   options=list(width=380,height=280,
                                title = input$restaurantsummary,backgroundColor="#f1f1f1",
                                hAxis="{title:'Numbers', titleTextStyle:{color:'blue'}}",
                                vAxis="{title:'Stats', titleTextStyle:{color:'blue'}}"
                                ))
  
  
  
})

  autoUpdate <- reactiveTimer(1000, session)
  
  output$datetimenow <- renderPrint({
    autoUpdate()
    datetime = Sys.time()
    writeLines(sprintf(as.character(datetime)))
  
})
  
  
})