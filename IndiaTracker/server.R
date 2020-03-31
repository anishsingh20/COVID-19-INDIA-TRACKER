

library(shiny)
#reading the data
require(rvest)
require(readr)
require(jsonlite)
require(janitor)
require(highcharter)




#complete state cases dataset

#dataset of deceased cases
tab1_deceased<- tab %>% 
  filter(Current_status=="Deceased") %>% 
  select(`Date Announced`,`State Patient Number`,Gender,Age,City,State,Notes,Contracted_from,Nationality,`Status Change Date`)

tab1_recovered<- tab %>% 
    filter(Current_status=="Recovered") %>% 
    select(`Date Announced`,`State Patient Number`,Gender,Age,City,State,Notes,Contracted_from,Nationality,`Status Change Date`)
    
#server logic
shinyServer(function(input, output) {
    
    #reading the raw COVID-19 JSON data(refreshed every 5 minutes)
   # url1 <- "https://api.rootnet.in/covid19-in/unofficial/covid19india.org"
    #loading thhe JSON data fron the web
    #jsonDoc <- fromJSON(url1)
    
    #extracting data in data frame
    #India_data <- jsonDoc$data$rawPatientData
    
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
    
    colnames(tab)[11] = "Notes"
    colnames(tab)[12] = "Contracted_from"
    colnames(tab)[10] = "Current_status"
    colnames(tab)[1]  = "Patient_no"
    colnames(tab)[5]  =  "Age"
    colnames(tab)[7]  = "City"
    colnames(tab)[8]  = "District"
    colnames(tab)[9]  = "State"
    
    
    
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
    
    
    
    output$Confirmed <- renderText({
        #no of rows in the raw dataset represents the total cases in India
      
        sum(state_data$ConfCases)
    })
    
    output$Deaths <- renderText({
       
    })
    
    output$Recoveries <- renderText({
        
    })
    
    output$Active <- renderText({
        
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
            filter(State == input$state) %>% 
            select(`Date Announced`,`Detected City`)
       
        #missing cities name are makred as unconfirmed cities
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
            filter(State == input$state) %>% 
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
            filter(State == input$state) %>% 
            select(`Date Announced`,`Detected City`)
        
        #missing cities name are makred as unconfirmed cities
        while(length(ind <- which(state_city_data$City == "")) > 0){
            state_city_data$City[ind] <- "Unconfirmed"
        }
    
        df <- state_city_data %>% 
            group_by(City,`Date Announced`) %>% 
            summarise(nCount = n())
        
        df<- df[order(as.Date(df$`Date Announced`, format="%d/%m/%Y")),]
        
        df
    })

})
