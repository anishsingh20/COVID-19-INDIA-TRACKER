
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
require(RCurl)








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


myfile5 <- getURL('https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

State_tested<- read.csv(textConnection(myfile5), header = T)




myfile6 <- getURL('https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

Tested_ICMR <- read.csv(textConnection(myfile6),header = T)




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
                  
                  box(width = 6,
                      
                      h3("Confirmed cases in Each Indian state:"),
                      br(),
                      highchartOutput("StateConf")
                    ) ,#end box
                  
                  box(width = 6,
                      
                      h3("Active cases in Each India States"),
                      br(),
                      highchartOutput("StateActive")
                  ),
                  
                  box(width = 6,
                      
                      h3("Confirmed Deaths in Each Indian state:"),
                      br(),
                      highchartOutput("StateDeaths")
                  ) ,
                  
                  
                  box(width = 6,
                      
                      h3("Confirmed Recoveries in Each Indian state:"),
                      br(),
                      highchartOutput("StateRecoveries")
                  ) ,
                
                  br(),
                  #recovery and death rates statewise
                  box(
                    
                    width = 6,
                    h3("Recovery Rates for each state out of Total confirmed:"),
                    br(),
                    highchartOutput("ratesPlotRecovery")
                    
                    
                  ),
                  
                  
                  box(
                    
                    width = 6,
                    h3(" Death rates for each state out of Total confirmed:"),
                    br(),
                    highchartOutput("ratesPlotDeath")
                    
                    
                  ) #end box
              ) #end fluidRow
        ), #end tabItem
        
        
        #testing data tab
        tabItem(tabName = "tab3",
                
                fluidRow(
              
                  
                        box(
                          
                          width = 12,
                          h2("Total samples Tested till date in India"),
                          p("As per ICMR"),
                          br(),
                          textOutput("totalTested"),
                          tags$head(tags$style("#totalTested{
                                 color: black;
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                          )) #end head
                        ), 
                        
                        box(
                          width = 12,
                          h3("Samples tested daily as per ICMR"),
                          br(),
                          highchartOutput("TestingChart")
                        ), 
                        
                        box(
                          width = 12,
                          h3("COVID-19 Test Positivity rate and samples tested daily"),
                          p("Few days have missing data"),
                          br(),
                          dataTableOutput("RateTable")
                        )
                  
                ) #end fluidRow
                
        ) , #end tabitem
        
        
        tabItem(tabName = "tab4",
                
                
            fluidRow(
                
                box(
                  width = 12,
                  
                  selectInput("state", label = "Select State",choices = StateCOVID_19$State_code)
                  
                  
                ), #end box
                
                box(
                  width = 12,
                  h4("New confirmed cases in a Particular State:"),
                  br(),
                  highchartOutput("StateCasesTimeSeries")
               ),
               
               box(
                 width = 12,
                 h4("New confirmed recoveries in a Particular State:"),
                 br(),
                 highchartOutput("StateCasesTimeSeries_recover")
               ),
               
               box(
                 width = 12,
                 h4("New confirmed deaths in a Particular State:"),
                 br(),
                 highchartOutput("StateCasesTimeSeries_death")
               )
               
               
                
                
            )# end fluidRow 
          
        ), #end tab4
        
        tabItem(tabName ="tab5",
                
                fluidRow(
                  
                  
                  
                  box(
                    
                    width = 12,
                    h3("Confirmed cases reported Every day:"),
                    highchartOutput("ConfDaily")
                ), #end box
                
                box(
                  
                  width = 12,
                  h3("Deaths reported every day:"),
                  highchartOutput("DeathsDaily")
                ),
                
                
                box(
                  
                  width = 12,
                  h3("Recovered patients every day:"),
                  highchartOutput("RecoveredDaily")
                  
                )
                  
                  
                ) #end fluidRow
              
                
        ) #end tabitem
        
      ) #end tabitems
      
      
    ) #end dashboardBody
  
  
  
) #end dashboardPage

