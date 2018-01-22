if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(googleVis)) install.packages("googleVis")
if(!require(data.table)) install.packages("data.table")
if(!require(dplyr)) install.packages("dplyr")
if(!require(DT)) install.packages("DT")
if(!require(lubridate)) install.packages("lubridate")
if(!require(sqldf)) install.packages("sqldf")
if(!require(parsedate)) install.packages("parsedate")
if(!require(ggvis)) install.packages("ggvis")

library(shiny)
library(shinydashboard)
library(googleVis)
library(data.table)
library(dplyr)
library(DT)
library(sqldf)
library(ggvis)

shinyServer(function(input, output, session) {
  
  getDataset <- reactive({
    library(data.table)
    
    
    
    
    
    dataset <- withProgress({
      if(is.null(input$inputFile)){
        dataset <- fread("data/sample.csv",stringsAsFactors = F)
      } else{
        dataset <- fread(input$inputFile$datapath,stringsAsFactors = F,na.strings=c("NA","N/A","null",""," "))
      }
      
      if(is.null(dataset)){
        return(NULL)
      }
      if(!is.null(dataset$start)){
        dataset$start <- parsedate::parse_date(dataset$start)
      }
      if(!is.null(dataset$end)){
        dataset$end <- parsedate::parse_date(dataset$end)
      }
      
      if(is.null(dataset$sessionId)){
        dataset$sessionId <- 1
      }
      
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
  
  
  getFilteredDataset <- reactive({
    dataset <- getDataset()
    if(!is.null(dataset) & !is.null(input$slider) & !is.null(input$sessionSelect)){
      filtered <- dataset %>% filter(start >= input$slider[1], end <= input$slider[2], sessionId == input$sessionSelect) %>% data.frame()
      return(filtered)
    }
    return(NULL)
    
  })
  
  
  output$sessionSelect <- renderUI({
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
  
  output$timeline <- googleVis::renderGvis({
    source("R/timelineUtils.R")
    sessionDataset = getFilteredDataset()
    
    
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
      need(!is.null(sessionDataset$type),"type column missing"),
      need(!is.null(sessionDataset$start),"start column missing"),
      need(!is.null(sessionDataset$end),"end column missing"),
      need(!is.null(sessionDataset$label),"label column missing"),
      errorClass = "validation"
    )
    
    tl <- getTimeline(sessionDataset = sessionDataset,joinNeighbors = input$groupAdjacent,joinGapInSeconds = input$minGap)
    
    tl
    
  })
  
  output$sql <- DT::renderDataTable({
    dataset = getFilteredDataset()
    
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
  
  distributionsVis <- reactive({
    dataset = getDataset()
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
    print(events)
    
    dataset <- dataset %>% filter(event %in% names(events))
    
    
    
    grouped <- dataset %>% group_by(sessionId,event) %>% summarize(count = n()) %>% mutate(freq = count / sum(count))

 
    print(grouped)
    
    grouped %>% 
      ggvis(y = ~sessionId, fill = ~event) %>%
      compute_stack(stack_var = ~freq, group_var = ~ sessionId) %>% 
      layer_rects(x = ~stack_lwr_, x2 = ~stack_upr_, height = band()) %>%
      add_axis("y", title = "Session") %>%
      add_axis("x", title = "Percentage") %>% add_tooltip(function(data){data$event}, "hover")
  })
  
  distributionsVis %>% bind_shiny("distributions")
  
  
  
  
  
  
})