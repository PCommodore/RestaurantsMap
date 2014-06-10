
require(shiny)
require(rCharts)
require(googleVis)
shinyServer(function(input, output, session){
  
  output$map_container <- renderMap({
    plotMap()
  })
  
  output$hello <- renderTable({
    restaurantdf()    
  })
  
  output$myChart <- renderGvis(
    { 
      df <- restaurantdf()
      ###Rcharts Plot Start
      #p1 <- rPlot(input$variablex, input$variabley, data = df, type = 'point')
      #p1$addParams(width = 880, height = 550, dom = 'myChart', title = "Scatter Plot")
      #p1$guides(x = list(title = "x values", min = 0, max = 15000))
      #p1$guides(y = list(title = "y values", min = 0, max = 15000))
      #return(p1) 
      ###Rcharts Plot End
      
      df1 = data.frame(xvar=df[,names(df)==input$variablex],yvar=df[,names(df)==input$variabley])
      
      gvisScatterChart(df1,
                       options=list(gvis.editor="Editor",width=380,height=380,
                                    title = "Google Chart",backgroundColor="#f1f1f1"))
      
      
      
    })
  
  myTableOptions <- reactive({
    list(
      page=ifelse(input$pageable==TRUE,'enable','disable'),
      pageSize=input$pagesize,
      width=880,
      height=200
    )
  })
  output$myTable <- renderGvis({
    gvisTable(restaurantdf(),options=myTableOptions())
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
  
  
})