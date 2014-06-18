
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
  
  output$mySummary <- renderGvis({ 
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
                   options=list(width=350,height=280,
                                title = paste("Quick Facebook Stats for", input$restaurantsummary),backgroundColor="#f1f1f1",
                                hAxis="{title:'Numbers', titleTextStyle:{color:'blue'}}",
                                vAxis="{title:'Stats', titleTextStyle:{color:'blue'}}"
                                ))
  
  
  
})

  autoUpdate <- reactiveTimer(1000, session)
  
  output$datetimenow <- renderPrint({
    autoUpdate()
    Sys.setenv(TZ='Asia/Kuala_Lumpur')
    datetime = Sys.time()
    writeLines(sprintf(as.character(datetime)))
  
})


output$myHours <- renderGvis({ 
  df <- restaurantdf()
  ###Rcharts Plot Start
  #p1 <- rPlot(input$variablex, input$variabley, data = df, type = 'point')
  #p1$addParams(width = 880, height = 550, dom = 'myChart', title = "Scatter Plot")
  #p1$guides(x = list(title = "x values", min = 0, max = 15000))
  #p1$guides(y = list(title = "y values", min = 0, max = 15000))
  #return(p1) 
  ###Rcharts Plot End
  
  df2 = df[df$name==input$restaurantsummary,7:34]
                   
  
  timelinedf=matrix(0,289,1)  
  timelinedf=data.frame(timelinedf)
  timelinedf$time=seq(strptime("0:00","%H:%M"),strptime("24:00","%H:%M"),by = "5 min")
  
  timelinedf$status = 0
  
  Sys.setenv(TZ='Asia/Kuala_Lumpur')
  
  if (weekdays(Sys.time()) == "Monday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$mon1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$mon1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$mon1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$mon2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$mon2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$mon2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  else if (weekdays(Sys.time()) == "Tuesday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$tue1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$tue1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$tue1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$tue2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$tue2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$tue2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  else if (weekdays(Sys.time()) == "Wednesday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$wed1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$wed1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$wed1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$wed2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$wed2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$wed2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  else if (weekdays(Sys.time()) == "Thursday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$thur1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$thur1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$thur1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$thur2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$thur2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$thur2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  else if (weekdays(Sys.time()) == "Friday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$fri1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$fri1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$fri1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$fri2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$fri2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$fri2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  else if (weekdays(Sys.time()) == "Saturday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$sat1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$sat1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$sat1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$sat2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$sat2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$sat2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  else if (weekdays(Sys.time()) == "Sunday") {
    for (i in 1:length(timelinedf$time)) {
      if (! is.na(strptime(df2$sun1open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$sun1open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$sun1close[1],"%H:%M")) {
          timelinedf$status[i] = 1
          
        }
      }
      if (! is.na(strptime(df2$sun2open[1],"%H:%M"))) {
        if (timelinedf$time[i] >= strptime(df2$sun2open[1],"%H:%M") & timelinedf$time[i] <= strptime(df2$sun2close[1],"%H:%M")){
          timelinedf$status[i] = 1
        }
      }
    }
  }
  
  
  
  
  gvisLineChart(
    data = timelinedf,
    xvar = "time",
    yvar = "status",
    options = list(
      title = paste("Today's Operating Hours for",input$restaurantsummary,"\n", "1 - Open | 0 - Close"),
      height = 300,
      width = 350,
      backgroundColor="#f1f1f1"
    )
  )
  
  
  
    
  
  
})



  
  
})


