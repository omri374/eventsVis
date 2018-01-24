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
library(ggplot2)
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
    sidebarMenu(style = "position: fixed; ",

      fileInput("inputFile", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"),placeholder = ""),
      
      shiny::htmlOutput("sessionSelect"),
      uiOutput("slider"),
      
      
      
      checkboxInput("groupAdjacent",label = "Group adjacent events together",value = FALSE),
      numericInput("minGap","Minimum gap for grouping identical adjacent events",min = 0,value = 5)
      ),width = '320px'),
  dashboardBody(    
    
    tabsetPanel(
      tabPanel("Intro",
               HTML("<P><H1>Events visualization tool</H1></P>
                      <H3> This tools allows you to analyze temporal events using different visualization techniques:</H3>
<H4><B>
  <ul>
                    <li>Timeline</li>

                          <li>Distribution of events</li>
                          <li>Consecutive events analysis</li>
                          <li>Co-occurring events analysis</li></H4><BR>
</ul></B>
                    Next step is to upload a CSV file: <BR>
                    - <B>type</B> (type of event, row in timeline. For example: sensor Id, source Id, event category) <BR>
                    - <B>label</B> (value of event) <BR>
                    - <B>start</B> (start time of event) <BR>
                    - <B>end</B> (end time of event) - <i>optional, if missing, end will equal start + 1</i><BR>
                    - <B><I>sessionId</I></B> (optional, for example: userId, session, sensorId etc.)            <BR> <BR>
                    
                    Start and end should be either:

                    <b><ul><li>dates in the format <I>YYYY-MM-DD hh:mm:ss TZ</I> or other known formats (the tool will try to parse them)</li>
                    <li>integers</li></ul></b>
                    
                    Example input:
<table>
 <thead>
                    <tr>
                    <th> sessionId </th>
                    <th> type </th>
                    <th> label </th>
                    <th> start </th>
                    <th> end </th>
                    </tr>
                    </thead>
                    <tbody>
                    <tr>
                    <td> A    </td>
                    <td> Brake    </td>
                    <td> Harsh    </td>
                    <td> 2018-01-22 11:43:00 IST    </td>
                    <td> 2018-01-22 11:43:05 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Brake    </td>
                    <td> Harsh    </td>
                    <td> 2018-01-22 11:47:00 IST    </td>
                    <td> 2018-01-22 11:47:05 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Brake    </td>
                    <td> Harsh    </td>
                    <td> 2018-01-22 11:47:31 IST    </td>
                    <td> 2018-01-22 11:47:33 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Brake    </td>
                    <td> Medium    </td>
                    <td> 2018-01-22 11:49:00 IST    </td>
                    <td> 2018-01-22 11:49:05 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Brake    </td>
                    <td> Harsh    </td>
                    <td> 2018-01-22 11:43:09 IST    </td>
                    <td> 2018-01-22 11:43:15 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Brake    </td>
                    <td> Harsh    </td>
                    <td> 2018-01-22 11:58:00 IST    </td>
                    <td> 2018-01-22 11:58:05 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Accelerate    </td>
                    <td> Low    </td>
                    <td> 2018-01-22 11:44:05 IST    </td>
                    <td> 2018-01-22 11:45:30 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Accelerate    </td>
                    <td> Medium    </td>
                    <td> 2018-01-22 11:46:15 IST    </td>
                    <td> 2018-01-22 11:46:44 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Accelerate    </td>
                    <td> Medium    </td>
                    <td> 2018-01-22 11:48:25 IST    </td>
                    <td> 2018-01-22 11:48:30 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Accelerate    </td>
                    <td> Low    </td>
                    <td> 2018-01-22 11:50:35 IST    </td>
                    <td> 2018-01-22 11:51:12 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Turn    </td>
                    <td> Low    </td>
                    <td> 2018-01-22 11:50:39 IST    </td>
                    <td> 2018-01-22 11:51:12 IST    </td>
                    </tr>
                    <tr>
                    <td> A    </td>
                    <td> Turn    </td>
                    <td> High    </td>
                    <td> 2018-01-22 11:51:32 IST    </td>
                    <td> 2018-01-22 11:51:38 IST    </td>
                    </tr>
                    <tr>
                    <td> B    </td>
                    <td> Accelerate    </td>
                    <td> Medium    </td>
                    <td> 2018-01-22 11:46:15 IST    </td>
                    <td> 2018-01-22 11:46:44 IST    </td>
                    </tr>
                    <tr>
                    <td> B    </td>
                    <td> Accelerate    </td>
                    <td> Medium    </td>
                    <td> 2018-01-22 11:48:25 IST    </td>
                    <td> 2018-01-22 11:48:30 IST    </td>
                    </tr>
                    <tr>
                    <td> B    </td>
                    <td> Accelerate    </td>
                    <td> Low    </td>
                    <td> 2018-01-22 11:50:35 IST    </td>
                    <td> 2018-01-22 11:51:12 IST    </td>
                    </tr>
                    <tr>
                    <td> B    </td>
                    <td> Turn    </td>
                    <td> Low    </td>
                    <td> 2018-01-22 11:50:39 IST    </td>
                    <td> 2018-01-22 11:51:12 IST    </td>
                    </tr>
                    <tr>
                    <td> B    </td>
                    <td> Turn    </td>
                    <td> High    </td>
                    <td> 2018-01-22 11:51:32 IST    </td>
                    <td> 2018-01-22 11:51:38 IST    </td>
                    </tr>
                    <tr>
                    <td> C    </td>
                    <td> Turn    </td>
                    <td> High    </td>
                    <td> 2018-01-22 11:51:32 IST    </td>
                    <td> 2018-01-22 11:51:38 IST    </td>
                    </tr>
                    </tbody>
                    </table>
                    ")
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
                     
                     plotOutput("distributions"))
               )),
      tabPanel("Consecutive events analysis",
               fluidRow(
                 box(title = "Consecutive events analysis", solidHeader = TRUE, status = "primary", width = 10,
                     checkboxInput("consecutivePerSession",label = "All sessions",value = FALSE),
                     h2(textOutput('visNetworkTitle')),
                     radioButtons("byDuration",label="Aggregation type",choices = c("Sum by duration","Count events"),selected = "Sum by duration"),
                     numericInput("maxTimeForConsecutiveInSeconds",label = "Max time for events to be considered consecutive",min = 1,value = 50,width = '15%'),
                     
                     visNetwork::visNetworkOutput("consecutives"))
                 
                 
               )),
      tabPanel("Co-occurring events analysis",
               fluidRow(
                 box(title = "Co-occururing events analysis", solidHeader = TRUE, status = "primary", width = 10,
                     h2(textOutput('visNetworkCoocsTitle')),
                     numericInput("cooccurrenceThreshold",label = "Max gap for events to be considered co-occurring (co-occurrence threshold)",min = 0,value = 1,width = '15%'),
                     
                     visNetwork::visNetworkOutput("cooccurring"))
                 
                 
               )),
      tabPanel("Raw Data",
               textInput("query",label = "Enter SQL Query",value = "SELECT * FROM dataset",width = '80%'),
               
               DT::dataTableOutput("sql")
      )
      
    )
  )
  
)
