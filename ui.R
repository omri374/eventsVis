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
library(visNetwork)

ui <- dashboardPage(
  
  
  
  dashboardHeader(title = "Events Visualization Tool",titleWidth = "350px"),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-info {
                      color: black;
                      }
                      "))
    ),
    sidebarMenu(
      fileInput("inputFile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),placeholder = ""),
      
      shiny::htmlOutput("sessionSelect"),
      uiOutput("slider",width = "100px"),
      
      
      
      checkboxInput("groupAdjacent",label = "Group adjacent events together",value = FALSE),
      numericInput("minGap","Minimum gap for grouping (seconds)",min = 0,value = 5)
      ,width = "250px")),
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
      tabPanel("Timeline", 
               htmlOutput("timeline")
               , style = "overflow:scroll;"),
      tabPanel("Events distribution",
               fluidRow(
                 sliderInput("numEventsForDistribution","Number of event types to present",min = 1,value = 10, max = 50,step = 1),
                 box(title = "Event distribution within session", width = 5, solidHeader = TRUE, status = "primary",
                     plotOutput("inSessionDistribution")),
                 box(title = "Compare event distribution between sessions", width = 5, solidHeader = TRUE, status = "primary",

                     ggvis::ggvisOutput(plot_id = "distributions"))
               ),
               fluidRow(
                 box(title = "Consecutive events analysis", solidHeader = TRUE, status = "primary", width = 10,
                     checkboxInput("consecutivePerSession",label = "All sessions",value = FALSE),
                     h2(textOutput('visNetworkTitle')),
                     radioButtons("byDuration",label="Aggregation type",choices = c("Sum by duration","Count events"),selected = "Sum by duration"),
                     numericInput("maxTimeForConsecutiveInSeconds",label = "Max time for events to be considered consecutive",min = 1,value = 50,width = '15%'),
                     
                     visNetwork::visNetworkOutput("consecutives"))
                 
                 
               )),
      tabPanel("Raw Data",
               textInput("query",label = "Enter SQL Query",value = "SELECT * FROM dataset",width = '80%'),
               
               DT::dataTableOutput("sql")
      )
      
    )
  )
  
)
