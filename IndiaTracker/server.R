

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
      
        sum(state_data$ConfCases)
    })
    
    output$Deaths <- renderText({
      
      StateCOVID_19$Deaths[1]
       
    })
    
    output$Recoveries <- renderText({
      
      StateCOVID_19$Recovered[1]
        
    })
    
    output$Active <- renderText({
      
        StateCOVID_19$Active[1]
    })
  
    
    output$StateData <- renderDataTable({
      
      StateCOVID_19
      
    })
  
    output$TimeSeriesPlot <- renderHighchart({
      
      #dataframe of Dates and cases on each date
      date_cases <- data.frame(table(tab$`Date Announced`))
      colnames(date_cases) <- c("Date","Total_confirmed")
      #ordering the dataframe by date values
      date_cases<- date_cases[order(as.Date(date_cases$Date, format="%d/%m/%Y")),]
      date_cases[nrow(date_cases),] = NA #setting missing value as NA
      date_cases <- date_cases[complete.cases(date_cases), ] #removing NA values
        
        hchart(date_cases, "spline", hcaes(x = Date,y = Total_confirmed), name="Confirmed cases on each date:",color="purple") %>% 
            hc_exporting(enabled = TRUE) %>%
            hc_title(text="Total COVID-19 confirmed cases each day:",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
        
        
    })
   
    
    output$statetable <- renderDataTable({
        
        state_data
        
        
    })
    
    
    output$StateConfChart <- renderHighchart({
        
        hchart(state_data, "column", hcaes(x = State,y = ConfCases), name="Confirmed cases for each state:",color="black") %>% 
            hc_exporting(enabled = TRUE) %>%
            hc_title(text="Total COVID-19 confirmed cases for each state:",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
        
        
    })
    
    
    output$StateCityCases <- renderHighchart({
        
        #dataframe of dates and cities
        state_city_data <- tab %>% 
            filter(`Detected State` == input$state) %>% 
            select(`Date Announced`,`Detected City`)
       
       # missing cities name are makred as unconfirmed cities
        while(length(ind <- which(state_city_data$`Detected City` == "")) > 0){
            state_city_data$`Detected City`[ind] <- "Unconfirmed"
        }
        
        #making a data frame of Cities and cases         
        df <- state_city_data %>% 
            group_by(`Detected City`) %>% 
            summarise(nCount=n()) %>% 
            arrange(desc(nCount))
     
        
        
        hchart(df, "column", hcaes(x = `Detected City`,y = nCount), name="Confirmed cases for each state's City",color="green") %>% 
            hc_exporting(enabled = TRUE) %>%
            hc_title(text="Total COVID-19 confirmed cases for each state's cities",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
        
        
        
        
        
        
    })
    
    
    output$StateDate <- renderHighchart({
        
        #dataframe of dates and cities
        state_city_data <- tab %>% 
            filter(`Detected State` == input$state) %>% 
            select(`Date Announced`,`Detected City`)
        
        #missing cities name are makred as unconfirmed cities
        while(length(ind <- which(state_city_data$`Detected City` == "")) > 0){
            state_city_data$`Detected City`[ind] <- "Unconfirmed"
        }
        
        #making a data frame of Cities and cases         
        df <-  df <- state_city_data %>% 
            group_by(`Date Announced`) %>% 
            summarise(nCount=n())
        
        df<- df[order(as.Date(df$`Date Announced`, format="%d/%m/%Y")),]
    
        
        hchart(df, "spline", hcaes(x = `Date Announced`,y = nCount), name="Cases Confirmed:",color="green") %>% 
            hc_exporting(enabled = TRUE) %>%
            hc_title(text="COVID-19 cases confirmed on each date in the selected state:",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
        
    })
    
    
    output$CityDatetable <- renderDataTable({
        
        state_city_data <- tab %>% 
            filter(`Detected State` == input$state) %>% 
            select(`Date Announced`,`Detected City`)
        
        #missing cities name are makred as unconfirmed cities
        while(length(ind <- which(state_city_data$City == "")) > 0){
            state_city_data$City[ind] <- "Unconfirmed"
        }
    
        df <- state_city_data %>% 
            group_by(`Detected City`,`Date Announced`) %>% 
            summarise(nCount = n())
        
        df<- df[order(as.Date(df$`Date Announced`, format="%d/%m/%Y")),]
        
        df
    })

})
