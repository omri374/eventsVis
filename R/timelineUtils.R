library(data.table)
library(dplyr)
library(googleVis)


getTimeline = function(sessionDataset, joinNeighbors = TRUE, joinGapInSeconds = 10){
  if(missing(sessionDataset) | is.null(sessionDataset)){
    return(NULL)
  }
  
  sessionDataset <- sessionDataset %>% select(type,label,start,end)
  
  if(joinNeighbors & joinGapInSeconds > 0){
    toTimeLine <- joinAdjacentEvents(sessionDataset,minGapInSeconds = joinGapInSeconds)
  } else{
    toTimeLine <- sessionDataset
  }
  
 


  
  library(googleVis)
  tl <- gvisTimeline(data=toTimeLine, rowlabel="label",
                     start="start", end="end",
                     options=list(timeline="{showRowLabels:true, colorByRowLabel:true}",
                                  colors= "['#cbb69d', '#603913', '#c69c6e']",
                                  
                                  width = "automatic",
                                  height = 'automatic'))
  
  tl
  
}





joinAdjacentEvents <- function(sessionEvents, minGapInSeconds=10){
  library(lubridate)
  
  colnames <- names(sessionEvents)
  
  joined <- sessionEvents %>% arrange(start) %>%
    group_by(type,label) %>%                                                     # for each label and type
    mutate(
      prevEnd = lag(end),
      dist = as.numeric(difftime(start,                                 # calculate timerange between adjacent events 
                                      lag(end),           
                                      units = "secs"))) %>%
    mutate(isDistinct = ifelse(is.na(dist) | dist > minGapInSeconds, 1, 0),                # flag distinct events: distances > minGapInSeconds''
           joinedEventId = cumsum(isDistinct)) %>%                             # create session id
    group_by(type,label, joinedEventId) %>%                                      # group by adjacent types, labels and joints
    summarise(start = min(start),                                        
              end = max(end)) %>%
    ungroup()
  joined <- joined %>% data.frame()
  joined[,colnames]
  
}