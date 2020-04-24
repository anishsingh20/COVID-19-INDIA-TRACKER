

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
