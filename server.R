if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(googleVis)) install.packages("googleVis")
if(!require(data.table)) install.packages("data.table")
if(!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(lubridate)) install.packages("lubridate")
if(!require(sqldf)) install.packages("sqldf")
if(!require(parsedate)) install.packages("parsedate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(visNetwork)) install.packages("visNetwork")


library(shiny)
library(shinydashboard)
library(googleVis)
library(data.table)
library(dplyr)
library(DT)
library(sqldf)
library(ggplot2)
library(visNetwork)
library(RColorBrewer)

shinyServer(function(input, output, session) {
  
  ####-------- Reactive functions (data aquisition) --------#####
  
  
  getDataset <- reactive({
    library(data.table)
    
    dataset <- withProgress({
      if(is.null(input$inputFile)){
        dataset <- fread("data/sample.csv",stringsAsFactors = F)
      } else{
        dataset <- fread(input$inputFile$datapath,stringsAsFactors = F,na.strings=c("NA","N/A","null",""," "),showProgress = T)
      }
      
      if(is.null(dataset)){
        return(NULL)
      }
      if(!is.null(dataset$start)){
        if(!is.numeric(dataset$start)){
          dataset$start <- parsedate::parse_date(dataset$start)
        }
      }
      if(!is.null(dataset$end)){
        if(!is.numeric(dataset$end)){
          dataset$end <- parsedate::parse_date(dataset$end)
        }
      } else{
        dataset$end <- dataset$start + 1
      }
      
      if(is.null(dataset$sessionId)){
        dataset$sessionId <- 1
      }
      
      dataset$type <- as.factor(dataset$type)
      
      return(dataset)
      
      
      
    },message = "Loading dataset")
    
    print(paste('loaded',nrow(dataset),'records'))
    return(dataset)
  })
  
  getSessions <- reactive({
    dataset <- getDataset()
    if(!is.null(dataset)){
      sessions <- unlist(unique(dataset$sessionId))
      return(sessions)
    }
    else{
      return(NULL)
    }
  })
  
  
  getFilteredSession <- reactive({
    dataset <- getDataset()
    if(!is.null(dataset) & !is.null(input$slider) & !is.null(input$sessionSelect)){
      filtered <- dataset %>% filter(start >= input$slider[1], end <= input$slider[2], sessionId == input$sessionSelect) %>% data.frame()
      return(filtered)
    }
    return(NULL)
    
  })
  
  getFilteredEntireDataset <- reactive({
    dataset <- getDataset()
    if(!is.null(dataset) & !is.null(input$slider) & !is.null(input$sessionSelect)){
      filtered <- dataset %>% filter(start >= input$slider[1], end <= input$slider[2]) %>% data.frame()
      return(filtered)
    }
    return(NULL)
  })
  
  
  output$sessionSelect <- renderUI({
    input$inputFile
    sessions <- getSessions()
    if(is.null(sessions)){
      return(NULL)
    }
    selectInput("sessionSelect", "Choose Session:", as.list(sessions),selected = sessions[1]) 
  })
  
  output$slider <- renderUI({
    dataset <- getDataset()
    if(is.null(dataset)){
      s <- NULL
    } else{
      mini = min(dataset$start)
      maxi = max(dataset$end)
      s <- sliderInput("slider","Time range",min = mini,max = maxi,value = c(mini,maxi),step = 1)
    }
    s
  })
  
  ####-------- Timeline plot --------#####
  
  output$timeline <- googleVis::renderGvis({
    source("R/timelineUtils.R")
    sessionDataset = getFilteredSession()
    
    
    validate(
      need(!is.null(sessionDataset),
           "Welcome to the events visualization tool. This tools allows you to analyze events using a timeline.

            Please upload a valid CSV file with these columns: 
           - type (type of event, row in timeline)
           - label (value of event)
           - start (start time of event)
           - end (end time of event)
           - sessionId (optional, for example: userId, session, sensorId etc.)           

           Start and end should be dates in the format YYYY-MM-DD hh:mm:ss TZ"
      ),errorClass = "info"
      
    )
    
    if(is.null(sessionDataset)){
      return(NULL)
    }
    
    
    validate(
      need(!is.null(sessionDataset$sessionId),"session column missing"),
      need(!is.null(sessionDataset$type),"type column missing"),
      need(!is.null(sessionDataset$start),"start column missing"),
      need(!is.null(sessionDataset$end),"end column missing"),
      need(!is.null(sessionDataset$label),"label column missing"),
      errorClass = "validation"
    )
    
    tl <- getTimeline(sessionDataset = sessionDataset,joinNeighbors = input$groupAdjacent,joinGapInSeconds = input$minGap)
    
    tl
    
  })
  
  ####-------- DT (tabular) --------#####
  
  
  output$sql <- DT::renderDataTable({
    dataset = getFilteredSession()
    
    if(is.null(dataset)){
      return(data.frame())
    }
    library(sqldf)
    sqldf::sqldf(input$query)
  }, options = list(
    pageLength = 15,
    lengthMenu = c(5, 10, 15, 20,100)
  )
  )
  
  ####-------- across sessions plots --------#####
  
  output$distributions <- renderPlot({
    dataset = getFilteredEntireDataset()
    if(is.null(dataset)){
      return(NULL)
    }
    
    dataset$event <- paste0(dataset$type,": ",dataset$label)
    
    
    events <- unlist(sort(table(dataset$event),decreasing = T))
    if(!is.na(input$numEventsForDistribution)){
      print(input$numEventsForDistribution)
      if(input$numEventsForDistribution < length(events)){
        events <- events[1:input$numEventsForDistribution]
      }
    }
    #print(events)
    
    dataset <- dataset %>% filter(event %in% names(events)) %>% mutate(duration = as.integer(end) - as.integer(start))
    
    grouped <- dataset %>% group_by(sessionId,event) %>% summarize(totalDuration = sum(duration)) %>% mutate(Frequency = totalDuration)
    
    
    ggplot(grouped,aes(x = sessionId, y = Frequency,fill = event))+ ggtitle("Total duration per event across all sessions") + 
      geom_bar(position = "fill",stat = "identity") + coord_flip()
    
    #print(grouped)

  })

  ####-------- in session plots --------#####
  
  output$inSessionDistribution <- renderPlot({
    dataset = getFilteredSession()
    if(!is.null(dataset)){
      
      dataset$event <- paste0(dataset$type,": ",dataset$label)
      
      
      events <- unlist(sort(table(dataset$event),decreasing = T))
      if(!is.na(input$numEventsForDistribution)){
        print(input$numEventsForDistribution)
        if(input$numEventsForDistribution < length(events)){
          events <- events[1:input$numEventsForDistribution]
        }
      }
      #print(events)
      
      dataset <- dataset %>% filter(event %in% names(events)) %>% mutate(duration = as.integer(end) - as.integer(start))
      grouped <- dataset %>% group_by(event) %>% summarize(totalDuration = as.integer(sum(duration)))
      
      library(ggplot2)
      library(ggthemes)
      ggplot(grouped, aes(event, totalDuration)) + geom_col() + ggtitle(paste("Total duration per event in session",input$sessionSelect)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      
      
      
    } 
  })
  
  
  ####------ consecutive visNetwork ----- #####
  output$consecutives <- renderVisNetwork({
    source("R/networkUtils.R")
    input$sessionSelect
    dataset = getFilteredEntireDataset()
    if(!is.null(dataset)){
       if(input$consecutivePerSession==FALSE){
        session = input$sessionSelect
        getConsecutiveNetwork(events = dataset,sesId = session,maxTimeForConsecutiveInSeconds = input$maxTimeForConsecutiveInSeconds,byDuration = (input$byDuration=="Sum by duration"))
      } else{
        getConsecutiveNetwork(events = dataset,sesId = NULL, maxTimeForConsecutiveInSeconds = input$maxTimeForConsecutiveInSeconds,byDuration = (input$byDuration=="Sum by duration"))
      }  
    }
  })
  
  output$visNetworkTitle <- renderText({
    # We'll use the input$controller variable multiple times, so save it as x
    # for convenience.
    x <- input$consecutivePerSession
    
    # This will change the value of input$inText, based on x
    if(x==TRUE){
      paste("Which events occur after other events across all sessions?")
    } else{
      paste("Which events occur after other events? (session", input$sessionSelect,"only)")
    }
  })
  
  ####------ cooccurring events visNetwork ----- #####
  output$cooccurring <- renderVisNetwork({
    source("R/cooccurrenceUtils.R")
    input$sessionSelect
    dataset = getFilteredSession()
    if(!is.null(dataset)){
      getCooccurrenceGraph(sessionEvents = dataset,thresholdForCoccurring =input$cooccurrenceThreshold)
    }  
    
  })
  
  output$visNetworkCoocsTitle <- renderText({
    
    paste("Which events co-occur with other events? (session", input$sessionSelect,"only)")
    
  })
  
  
  
  
  
})