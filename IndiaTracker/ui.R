
#reading the data
require(rvest)
require(readr)
require(jsonlite)
require(shiny)
library(janitor)
require(shinydashboard)
require(dplyr)
require(tidyr)
require(highcharter)
library(readxl)
require(purrr)









#open connection to the file which is a efficient way.
myfile1 <- getURL('https://api.covid19india.org/csv/latest/state_wise.csv', 
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

#dataset with summariesed case counts of statese
StateCOVID_19 <- read.csv(textConnection(myfile1), header=T)
head(StateCOVID_19)


#dataset with time series data of Indian states
myfile2 <- getURL('https://api.covid19india.org/csv/latest/state_wise_daily.csv', 
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

State_time_series <- read.csv(textConnection(myfile2),header = T)
head(State_time_series)


#total Indian cases time series and daily changes
myfile3 <- getURL('https://api.covid19india.org/csv/latest/case_time_series.csv', 
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

Case_time_series <- read.csv(textConnection(myfile3),header = T)
head(Case_time_series)




dashboardPage(
  skin="black",
  dashboardHeader(title="COVID-19 INDIA TRACKER"),
  
  
  #dashboard sidebar
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Main Menu", tabName = "tab1" ,icon=icon("dashboard")),
      menuItem("Stateswise Cases", tabName = "tab2",icon= icon("globe")),
      menuItem("Samples tested across Country", tabName = "tab3",icon= icon("hospital")),
      menuItem("Time Series Analysis", tabName = "tab4",icon= icon("cog")),
      menuItem("Daily Change in Cases", tabName = "tab5",icon=icon("calendar")),
      menuItem("About", tabName = "tab6")
    ) #end sidebarmenu
    
  ), # end dashboardsidebar
  
  
  #dashboardBody
    dashboardBody(
      
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css?family=Merriweather|Playfair+Display|Raleway")
      ),
      
      #adding all the tabs
      tabItems(
        
        tabItem(tabName ="tab1",
                
                h2("COVID-19 India Tracker",align="center",style="margin-top:-5px;"),
                br(),
                
            fluidRow(
              
              box(
                
                h4("Active Cases:", align="left") , 
                textOutput("Active"), #end text Output
                width=3,
                tags$head(tags$style("#Active{
                                 color: blue;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                )) #end head
              ) ,
              
              
                box(
                  
                  h4("Confirmed Cases:", align="left") , 
                  width=3,
                  textOutput("Confirmed"), #end text Output
                  #adding custom CSS for the text
                  tags$head(tags$style("#Confirmed{
                                 font-size: 18px;
                                 color:black;
                                 font-family:'Raleway', sans-serif;
                                 }"
                  )
                  ) # end head
                  
                ), #end box
                
                box(
                  
                  h4("Total Deaths:", align="left") , 
                  textOutput("Deaths"), #end text Output
                  width=3,
                  #adding custom CSS for the text
                  tags$head(tags$style("#Deaths{
                                 color: red;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                  )) #end head
                ) , #end box
                
                box(
                  
                  h4("Total Recoveries:", align="left") , 
                  textOutput("Recoveries"), #end text Output
                  width=3,
                  tags$head(tags$style("#Recoveries{
                                 color: green;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                  )) #end head
                ), #end box
                
              
              box(
              
                width = 12,
                
                highchartOutput("stackedCovidIndia")
                  
                
              ),
              
               
                
                box(
                  width=12,
                  h3("Statewise data:"),
                  p("The data refreshes every day:"),
                  dataTableOutput("StateData")
                  
                ) #end box
            )#end fliudRow
        ), #end tab1
        
        tabItem(tabName = "tab2",
                
                fluidRow(
                  
                  box(width = 12,
                      
                      h3("Confirmed cases in Each Indian state:"),
                      br(),
                      highchartOutput("StateConfChart")
                    ) ,#end box
                  
                  box(
                    width = 12,
                    
                    selectInput("state", label = "Select State",choices = StateCOVID_19$State)
                    
                    
                  ), #end box
                  
                  box(
                    width = 6,
                    
                    highchartOutput("StateCityCases")
                    
                    
                  ), 
                  
                  box(
                    width = 6,
                    
                    highchartOutput("StateDate")
                    
                    
                  ), #end box
                  
                  
                  box(
                    width = 6,
                    align="center",
                    h3("Table of cumalative confirmed cases in each city of selected state till date:"),
                    p("Refreshes every 5 minutes"),
                    dataTableOutput("CityDatetable")
                    
                  ) ,
                  
                  box(
                    width = 6,
                    align="center",
                    h3("Table of cumalative confirmed cases in each state till date:"),
                    p("Refreshes every 5 minutes"),
                    dataTableOutput("statetable")
                    
                    
                  ) #end box
                  ) #end fluidRow
                ), #end tabItem
        
        
        
        tabItem(tabName ="tab3",
                
                fluidRow(
                  
                  
                       
                       box(
                         
                         width = 12,
                         h3("Time series plot of daily confirmed cases:"),
                         highchartOutput("TimeSeriesPlot")
                         
                        
                         
                         
                       ) #end box
                   
                  
                ) #end fluidRow
                
        ) #end tabitem
        
      ) #end tabitems
      
      
    ) #end dashboardBody
  
  
  
) #end dashboardPage

