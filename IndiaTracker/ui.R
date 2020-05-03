
#Loading the required packages
suppressMessages(library(rvest))
suppressMessages(library(readr))
suppressMessages(library(jsonlite))
suppressMessages(library(shiny))
suppressMessages(library(janitor))
suppressMessages(library(shinydashboard))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(highcharter))
suppressMessages(library(readxl))
suppressMessages(library(purrr))
suppressMessages(library(RCurl))
suppressMessages(library("rjson"))
suppressMessages(library(data.table))
suppressMessages(library(DT))




#reading the data


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


#DISTRICT WISE DATA(IN JSON FORMAT)

json_file <- "https://api.covid19india.org/state_district_wise.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))



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
      menuItem("District wise Analysis", tabName = "tab6",icon= icon("map")),
      menuItem("About", tabName = "tab7")
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
                                 font-size: 20px;
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
                                 font-size: 20px;
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
                                 font-size: 20px;
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
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                  )) #end head
                ), #end box
                
              box(
                
                h4("Total Tested:", align="left") , 
                textOutput("Tested"), #end text Output
                width=4,
                tags$head(tags$style("#Tested{
                                 color: black;
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                )) #end head
              ),
              
              box(
              
              h4("Recovery Rate %:", align="left") , 
              textOutput("RecRate"), #end text Output
              width=4,
              tags$head(tags$style("#RecRate{
                                 color: green;
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
              )) #end head
            ), 
            
            box(
            h4("Death Rate %:", align="left") , 
            textOutput("DeadRate"), #end text Output
            width=4,
            tags$head(tags$style("#DeadRate{
                                 color: red;
                                 font-size: 20px;
                                 font-family:'Raleway', sans-serif;
                                 }"
            )) #end head
        ), 
            
            
              box(
              
                width = 12,
                
                highchartOutput("stackedCovidIndia")
                  
                
              ),
              
               
                
                box(
                  
                  width=12,
                  h3("Statewise data:"),
                  p("The data refreshes every day:"),
                  DTOutput("StateData")
                  
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
                
                  hr(),
                  br(),
                  br(),
                  br(),
                  h2("Recovery and Death Rates for each Indian State",align="center"),
                  br(),
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
                          p("Few days have missing data"),
                          br(),
                          highchartOutput("TestingChart")
                        ), 
                        
                        br(),
                        br(),
                        
                        box(
                          width = 12,
                          h3("COVID-19 Test Positivity rate out of samples tested daily"),
                          p("Few days have missing data"),
                          br(),
                          highchartOutput("RateChartIndia")
                        ),
                        
                        #Statewise Tests done
                        h3("Statewise Testing being done:",align="center"),
                        br(),
                        
                        #SelectBox                      
                        box(
                        
                          width = 12,
                          selectInput("state_test", label = "Select State",choices = StateCOVID_19$State[-1])
                        
                        ),
                        
                        box(
                          width = 12,
                          br(),
                          h3("Total Statewise Testing Done(Cumalative Count)"),
                          p("Few days have missing data"),
                          br(),
                          highchartOutput("StateTestChart")
                        ), 
                        
                        #DAily statewise 
                        box(
                          width = 12,
                          br(),
                          h3("Daily Statewise Testing Done"),
                          p("Few days have missing data"),
                          br(),
                          highchartOutput("StateDailyTestChart")
                        ), 
                        
                        box(
                          width = 12,
                          br(),
                          h3("Test Positivity Rates for each state"),
                          p("Few days have missing data"),
                          br(),
                          highchartOutput("StatePositiveTestRate")
                        )
                  
                ) #end fluidRow
                
        ) , #end tabitem
        
        
        tabItem(tabName = "tab4",
                
            
                
                
            fluidRow(
              
              h2("Time Series analysis of New cases in Each Indian State",align="center"),
              br(),
              br(),
                
                box(
                  width = 12,
                  
                  selectInput("state", label = "Select State",choices = StateCOVID_19$State[-1])
                  
                  
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
                  
                  h2("Time Series analysis of New cases India",align="center"),
                  br(),
                  br(),
                  
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
              
                
        ), #end tabitem
        
        
        #district data tab
        tabItem(tabName ="tab6",
                
                fluidRow( 
                  
                  
                    h2("District wise analysis of Indian States",align="center"),
                    br(),
                    br(),
                    
                    box(
                      
                      width = 12,
                      selectInput("district_state", label = "Select State",choices = StateCOVID_19$State[-1])
                    ), #end box
                    
                    box(
                      
                    
                      width = 12,
                      selectInput("district", label = "Select District", choices = NULL)
                                    
                                    
                                    
                    ), #end box
                  
                    
                    
                    box(
                      
                      h3("Active Cases in each district",align="center"),
                      br(),
                      width = 6,
                      highchartOutput("district_active")
                      
                    ),
                    
                    
                    box(
                      
                      h3("Total Confirmed Cases in each district",align="center"),
                      br(),
                      width = 6,
                      highchartOutput("district_confirmed")
                      
                    ),
                    
                    
                    box(
                      
                      h3("Deaths in each district",align="center"),
                      br(),
                      width = 6,
                      highchartOutput("district_dead")
                      
                    ),
                    
                    box(
                      
                      h3("Recovered in each district",align="center"),
                      br(),
                      width = 6,
                      highchartOutput("district_recovered")
                      
                    ) ,
                    
                    
                    br(),
                    br(),
                    br(),
                    
                    h3("Deaths Rates and Recovery Rates of each District",align="center"),
                    
                    box(
                      
                      width = 12,
                      highchartOutput("district_recovery_rate")
                      
                      
                    ) ,
                    
                    br(),
                    br(),
                    
                    box(
                      
                      width = 12,
                      highchartOutput("district_death_rate")
                     
                      
                    ) ,
                    
                    
                    
                ) #end fluid row
                
        ) #end tabitem6  
                      
    
                  
                 
                  
       
        
      ) #end tabitems
      
      
    ) #end dashboardBody
  
  
  
) #end dashboardPage

