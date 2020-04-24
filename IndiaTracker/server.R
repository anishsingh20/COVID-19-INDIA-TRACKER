

library(shiny)
#reading the data
require(rvest)
require(readr)
require(jsonlite)
require(janitor)
require(highcharter)
require(readxl)







#complete state cases dataset

#server logic
shinyServer(function(input, output) {
    
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
  
    
    
    
    
    
    output$Confirmed <- renderText({
        #no of rows in the raw dataset represents the total cases in India
      
       
    })
    
    output$Deaths <- renderText({
      
      
       
    })
    
    output$Recoveries <- renderText({
      
   
        
    })
    
    output$Active <- renderText({
      
       
    })
  
    
    output$StateData <- renderDataTable({
      
     
      
    })
  
    output$TimeSeriesPlot <- renderHighchart({
      
    
        
        
    })
   
    
    output$statetable <- renderDataTable({
        
        state_data
        
        
    })
    
    
    output$StateConfChart <- renderHighchart({
        
       
        
    })
    
    
    output$StateCityCases <- renderHighchart({
        
       
        
        
        
        
        
        
    })
    
    
    output$StateDate <- renderHighchart({
        
        
        
    })
    
    
    output$CityDatetable <- renderDataTable({
    
       
    })

})
