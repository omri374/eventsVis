library(visNetwork)
library(dplyr)
library(RColorBrewer)

getCooccurrenceGraph <- function(sessionEvents, thresholdForCoccurring = 50){
  
  if(missing(sessionEvents) | is.null(sessionEvents)| nrow(sessionEvents)==0){
    return(NULL)
  }
  
  events <- sessionEvents %>% transmute(sessionId = sessionId, event = as.character(paste0(type,":",label)), start = start, end = end, 
                                        duration = ifelse(is.numeric(start),end - start,difftime(end,start,units = 'sec')))
  eventsDf <- events %>% arrange(start) %>% group_by(event) %>% summarize(value = sum(duration), counter = n()) %>% mutate(id = 1:n())
  
  edges <- getCooccurringEvents(events,eventsDf,tolerance = thresholdForCoccurring)
  
  if(nrow(edges) > 0){
    edges <- edges %>% group_by(first, second) %>% summarize(value = n()) %>% rename(from = first, to = second)
    edges$title <- paste0("<p> <BR><b># of co-occurrences = </b>",edges$value,"</p>")
  }
  
  nodes <- eventsDf %>% mutate(label = event,title = paste0("<p> <U>",label,"</U><BR><b>Count</b> = ",counter,"</p>"))
  if(nrow(nodes)>3){
    if(nrow(nodes) > 12){
      colors <- rep(brewer.pal(12,'Paired'),nrow(nodes)/10)
      nodes$color <- colors[1:nrow(nodes)]
    } else{
      nodes$color <- brewer.pal(nrow(nodes),'Paired')
    }
  }
  
  visNetwork(nodes, edges)   %>%
    visNodes(size = 40) %>%
    visOptions(selectedBy = "label", 
               highlightNearest = TRUE, 
               nodesIdSelection = F) %>%
    visInteraction(keyboard = TRUE,
                   dragNodes = T, 
                   dragView = T, 
                   zoomView = T)
  
  
  
}

getCooccurringEvents <- function(sessionEvents,tolerance = 25){
  sessionEvents <- sessionEvents %>% mutate(event = as.character(paste0(type,":",label)), duration = end-start)
  eventsDf <- sessionEvents %>% arrange(start) %>% group_by(event) %>% summarize(value = sum(duration), counter = n()) %>% mutate(id = 1:n())
  getCooccurringEvents(sessionEvents,eventsDf,tolerance)
}

getCooccurringEvents <- function(sessionEvents, eventsDf, tolerance=25){
  library(lubridate)
  
  sessionEvents <- arrange(sessionEvents,start) %>% inner_join(eventsDf,by='event')
  
  startTimes <- as.numeric(sessionEvents$start)
  endTimes <- as.numeric(sessionEvents$end)
  ids <- as.factor(sessionEvents$id)
  len <- length(startTimes)
  
  coocs <- list()
  counter <- 0
  
  for(i in 1:(len-1)){
    for(j in (i+1):len){
      if(startTimes[j] < endTimes[i] + tolerance & startTimes[j] > startTimes[i]){
        #coocs <- list(coocs,list(first = i,second = j))
        counter <- counter + 1
        coocs[[counter]] <- data.frame(first = ids[i],second = ids[j])
      }
      if(endTimes[j] + tolerance > endTimes[i]){
        break
      }
    }
  }
  
  #Convert list of data frames to data frame
  cooccurrences <- do.call("rbind", coocs)
  #print(cooccurrences)
  if(is.null(cooccurrences)){
    return(data.frame())
  } else{
    return(cooccurrences)
  }
  
}