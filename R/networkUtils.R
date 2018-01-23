library(visNetwork)
library(dplyr)
library(RColorBrewer)
getVisNetwork <- function(events, sesId, maxTimeForConsecutiveInSeconds = 1000,byDuration = TRUE){
  
  if(missing(events) | is.null(events)){
    return(NULL)
  }
  print(paste("By duration?",byDuration))
  events <- events %>% transmute(sessionId = sessionId, event = as.character(paste0(type,":",label)), start = start, end = end, duration = difftime(end,start,units = 'sec'))
  
  if(byDuration){
    eventsDf <- events %>% group_by(event) %>% summarize(value = sum(duration)) %>% mutate(id = 1:n())
  } else{
    #By count
    eventsDf <- events %>% group_by(event) %>% summarize(value = n()) %>% mutate(id = 1:n())
  }
  
  edges <- data.frame(from = as.numeric(NULL), to = as.numeric(NULL))
  
  if(is.null(sesId)){
    sessions <- unlist(unique(events$sessionId))
    
    for (ses in sessions){
      sessionEvents <- events %>% filter(sessionId == ses)
      df <- getConsequtiveInSession(sessionEvents,eventsDf,maxTimeForConsecutiveInSeconds)
      edges <- bind_rows(df,edges)
    }
      
  } else{
    print(paste0("Evaluating session ",sesId))
    sessionEvents <- events %>% filter(sessionId == sesId)
    if(byDuration){
      eventsDf <- sessionEvents %>% group_by(event) %>% summarize(value = sum(duration)) %>% mutate(id = 1:n())
    } else{
      #By count
      eventsDf <- sessionEvents %>% group_by(event) %>% summarize(value = n()) %>% mutate(id = 1:n())
    }
    edges <- getConsequtiveInSession(sessionEvents,eventsDf,maxTimeForConsecutiveInSeconds)
  }
  
  nodes <- eventsDf %>% rename(label = event)
  if(byDuration){
    nodes$title <- paste0("<p> Total duration:", nodes$value,"</p>")
  } else{
    nodes$title <- paste0("<p> Count:", nodes$value,"</p>")
  }
  

  
  
  edges <- edges %>% group_by(from, to) %>% summarize(value = n())
  if(nrow(edges) > 0){
    edges$title <- paste0("<p> Count:",edges$value,"</p>")
    #edges$color <- brewer.pal(nrow(edges),'BrBG')
  }
  if(nrow(nodes)>3){
    nodes$color <- brewer.pal(nrow(nodes),'BrBG')
  }
  
 # visNetwork::visNetwork(nodes,edges) %>%  visEdges(arrows = "middle") %>%
#    visOptions(highlightNearest = list(enabled = TRUE, degree = 2, hover = T))
  visNetwork(nodes, edges, height = "600px")  %>%  visEdges(arrows = "middle") %>%
    #visIgraphLayout() %>%
    visNodes(size = 40) %>%
    visOptions(selectedBy = "label", 
               highlightNearest = TRUE, 
               nodesIdSelection = F) %>%
    visInteraction(keyboard = TRUE,
                   dragNodes = T, 
                   dragView = T, 
                   zoomView = T)

  
}

getConsequtiveInSession <- function(sessionEvents, eventsDf, maxTimeForConsecutiveInSeconds = 60){
  
  consecutives <- sessionEvents %>% arrange(start) %>%
    mutate(
      nextStart = lead(start),
      nextEvent = lead(event),
      dist = as.numeric(difftime(lead(start),                                 # calculate timerange between adjacent events 
                                 end,           
                                 units = "secs"))) %>%
    filter(dist < maxTimeForConsecutiveInSeconds & dist > 0) %>%
    inner_join(eventsDf,by = c('event' = 'event')) %>%
    inner_join(eventsDf,by = c("nextEvent" = "event"),suffix = c(".prev", ".next")) %>% select(from = id.prev,to = id.next)
  
  consecutives
  
}