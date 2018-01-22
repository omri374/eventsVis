#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#if(!require(shiny)) install.packages("shiny")
#if(!require(shinydashboard)) install.packages("shinydashboard")
#if(!require(googleVis)) install.packages("googleVis")
#if(!require(data.table)) install.packages("data.table")
#if(!require(dplyr)) install.packages("dplyr")
#if(!require(DT)) install.packages("DT")
#if(!require(lubridate)) install.packages("lubridate")
#if(!require(ggvis)) install.packages("ggvis")


library(shiny)
library(shinydashboard)
library(googleVis)
library(data.table)
library(dplyr)
library(DT)
library(ggvis)

ui <- dashboardPage(
  

  
  dashboardHeader(title = "Events Visualization Tool",titleWidth = "30%"),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-info {
                      color: black;
                      }
                      "))
      ),
    box(title = "Session selection",status = "info", solidHeader = TRUE,
        collapsible = TRUE, width = "18%",background = "black",
        fileInput("inputFile", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"),placeholder = ""),
        
        shiny::htmlOutput("sessionSelect"),
        uiOutput("slider",width = "100px")
    ), 
    
    box(title = "Group adjacent events",status = "info",solidHeader = TRUE,
        collapsible = TRUE, width = "18%", background = "black",
        checkboxInput("groupAdjacent",label = "Group adjacent events together",value = FALSE),
        numericInput("minGap","Minimum gap for grouping (seconds)",min = 0,value = 5)),
    
    width = '20%'),
  dashboardBody(    
    
    tabsetPanel(
      tabPanel("Intro",
               HTML("<H2>Welcome to the events visualization tool.<BR></H2><H3> This tools allows you to analyze events using a timeline. </H3><BR>

                    Next step is to upload a CSV file: <BR>
                    - <B>type</B> (type of event, row in timeline) <BR>
                    - <B>label</B> (value of event) <BR>
                    - <B>start</B> (start time of event) <BR>
                    - <B>end</B> (end time of event) <BR>
                    - <B><I>sessionId</I></B> (optional, for example: userId, session, sensorId etc.)            <BR> <BR>
                    
                    Start and end should be dates in the format <I>YYYY-MM-DD hh:mm:ss TZ</I>, although other formats might work as well<BR>")
               ),
      tabPanel("Events", 
               htmlOutput("timeline")
      , style = "overflow:scroll;"),
      tabPanel("Events distribution",
               h3("Compare event distribution in sessions:"),
               
               ggvis::ggvisOutput(plot_id = "distributions"),
               sliderInput("numEventsForDistribution","Number of event types to present",min = 1,value = 10, max = 50,step = 1)
               ),
      tabPanel("Raw Data",
               textInput("query",label = "Enter SQL Query",value = "SELECT * FROM dataset",width = '80%'),

               DT::dataTableOutput("sql")
      )
      
    )
  )
  
)
