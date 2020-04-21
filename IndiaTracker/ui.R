
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










#State data(refreshes every day)
url_state <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSz8Qs1gE_IYpzlkFkCXGcL_BqR8hZieWVi-rphN1gfrO3H4lDtVZs4kd0C3P8Y9lhsT1rhoB-Q_cP4/pubhtml"




#setting the column names

StateCOVID_19 <- url_state %>%
  html() %>%
  html_nodes(xpath='//*[@id="1896310216"]/div/table') %>%
  html_table()

StateCOVID_19 <- StateCOVID_19[[1]]  

#Setting 1 row as column names:
StateCOVID_19 <- StateCOVID_19 %>%  row_to_names(row_number = 1)

#removing column 1 as it is not necessary:
StateCOVID_19[,3] <- NA
StateCOVID_19 <-remove_empty(StateCOVID_19,"cols")





#complete state cases dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pubhtml#"

#reading the table from the URL using rvest package

h1<- read_html(url)
html_text(h1)


tab <- h1 %>% html_nodes("table")
tab <- tab[[1]] %>% html_table


#setting the column names

tab <- tab %>%  row_to_names(row_number = 1)



#removing column 1 as it is not necessary:
tab$`1` <- NA
tab <-remove_empty(tab,"cols")





#removing the NA column
tab <- tab[colSums(!is.na(tab)) > 0]
#removing the NA rows
tab <- na.omit(tab)

#states and total cases in each state
state_data <- data.frame(table(tab$`Detected State`))
colnames(state_data) <- c("State","ConfCases")
state_data <- state_data %>% 
  arrange(desc(ConfCases))

#removing the NA column(Setting values of 3rd row as NA)
while(length(ind <- which(state_data$State == "")) > 0){
  state_data$State[ind] <- "Unconfirmed"
}
state_data <- na.omit(state_data)


#dataframe of Dates and cases on each date
date_cases <- data.frame(table(tab$`Date Announced`))
colnames(date_cases) <- c("Date","Total_confirmed")
#ordering the dataframe by date values
date_cases<- date_cases[order(as.Date(date_cases$Date, format="%d/%m/%Y")),]
date_cases[nrow(date_cases),] = NA #setting missing value as NA
date_cases <- date_cases[complete.cases(date_cases), ] #removing NA values



dashboardPage(
  skin="black",
  dashboardHeader(title="COVID-19 INDIA TRACKER"),
  
  
  #dashboard sidebar
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Main Menu", tabName = "tab1" ,icon=icon("dashboard")),
      menuItem("Stateswise Cases", tabName = "tab2",icon= icon("globe")),
      menuItem("Time Series Analysis", tabName = "tab3",icon= icon("cog")),
      menuItem("Daily Change in Cases", tabName = "tab4",icon=icon("calendar"))
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
                  
                  h4("Active Cases:", align="left") , 
                  textOutput("Active"), #end text Output
                  width=3,
                  tags$head(tags$style("#Active{
                                 color: green;
                                 font-size: 18px;
                                 font-family:'Raleway', sans-serif;
                                 }"
                  )) #end head
                ) ,
                
                
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
                    
                    selectInput("state", label = "Select State",choices = state_data[,1])
                    
                    
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

