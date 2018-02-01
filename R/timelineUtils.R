library(data.table)
library(dplyr)
library(googleVis)


getTimeline = function(sessionDataset, joinNeighbors = TRUE, joinGapInSeconds = 10, plotWidth = "automatic"){
  if(missing(sessionDataset) | is.null(sessionDataset)){
    return(NULL)
  }
  
  sessionDataset <- sessionDataset %>% select(type,label,start,end)
  
  if(is.numeric(sessionDataset$start)){
    sessionDataset$start <- as.POSIXct(sessionDataset$start,origin = "1970-01-01",tz = "UTC")
    sessionDataset$end <- as.POSIXct(sessionDataset$end,origin = "1970-01-01",tz = "UTC")
  }
  
#  if(joinNeighbors & joinGapInSeconds > 0){
#    toTimeLine <- joinAdjacentEvents(sessionDataset,minGapInSeconds = joinGapInSeconds)
#  } else{
#    toTimeLine <- sessionDataset
#  }
  
 

  lanes = length(unique(sessionDataset$type))
  
  library(googleVis)
  tl <- gvisTimeline(data=sessionDataset, rowlabel="label",
                     start="start", end="end",
                     options=list(timeline="{showRowLabels:true, colorByRowLabel:true}",
                                  colors= "['#cbb69d', '#603913', '#c69c6e']",
                                  height = 200*lanes,
                                  width = plotWidth))
  
  tl
  
}





joinAdjacentEvents <- function(sessionEvents, minGapInSeconds=10){
  
  library(lubridate)
  
  colnames <- names(sessionEvents)
  
  joined <- sessionEvents %>% arrange(start) %>%
    group_by(type,label, sessionId) %>%                                                     # for each label and type
    mutate(
      prevEnd = lag(end),
      dist = difftime(start, lag(end), units = "secs")) %>%
    mutate(isDistinct = ifelse(is.na(dist) | dist > minGapInSeconds, 1, 0),                # flag distinct events: distances > minGapInSeconds''
           joinedEventId = cumsum(isDistinct)) %>%                             # create session id
    group_by(type,label, sessionId, joinedEventId) %>%                                      # group by adjacent types, labels and joints
    summarise(start = min(start),                                        
              end = max(end)) %>%
    ungroup()
  joined <- joined %>% data.frame()
  joined[,colnames]
  
}