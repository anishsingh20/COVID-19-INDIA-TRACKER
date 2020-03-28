

library(shiny)
#reading the data
require(rvest)
require(readr)
require(jsonlite)
require(janitor)
require(highcharter)




#complete state cases dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pubhtml#"

#reading the table from the URL using rvest package

h1<- read_html(url)
html_text(h1)


tab <- h1 %>% html_nodes("table")
tab <- tab[[1]] %>% html_table


#setting the column names

tab <- tab %>%  row_to_names(row_number = 1)

colnames(tab)[11] = "Notes"
colnames(tab)[12] = "Contracted_from"
colnames(tab)[10] = "Current_status"
colnames(tab)[2]  = "Patient_no"
colnames(tab)[5]  =  "Age"
colnames(tab)[7]  = "City"
colnames(tab)[8]  = "District"
colnames(tab)[9]  = "State"



#removing the NA column
    tab <- tab[colSums(!is.na(tab)) > 0]
    #removing the NA rows
    tab <- na.omit(tab)
    
    
    #removing column 1 as it is not necessary:
    #tab$`1` <- NA
    
    
    #states and total cases in each state
    state_data <- data.frame(table(tab$State))
    colnames(state_data) <- c("State","ConfCases")
    
    #dataframe of Dates and cases on each date
    date_cases <- data.frame(table(tab$`Date Announced`))
    colnames(date_cases) <- c("Date","Total_confirmed")
    #ordering the dataframe by date values
    date_cases<- date_cases[order(as.Date(date_cases$Date, format="%d/%m/%Y")),]
    date_cases[nrow(date_cases),] = NA #setting missing value as NA
    date_cases <- date_cases[complete.cases(date_cases), ] #removing NA values
    


#server logic
shinyServer(function(input, output) {
    
    #reading the raw COVID-19 JSON data(refreshed every 5 minutes)
   # url1 <- "https://api.rootnet.in/covid19-in/unofficial/covid19india.org"
    #loading thhe JSON data fron the web
    #jsonDoc <- fromJSON(url1)
    
    #extracting data in data frame
    #India_data <- jsonDoc$data$rawPatientData
    
    
    

    output$Confirmed <- renderText({
        #no of rows in the raw dataset represents the total cases in India
      
        sum(date_cases$Total_confirmed)
    })
    
    output$Deaths <- renderText({
        tab[2,4]
    })
    
    output$Recoveries <- renderText({
        tab[2,5]
    })
    
    output$Active <- renderText({
        tab[2,6]
    })

    
    output$TimeSeriesPlot <- renderHighchart({
        
        hchart(date_cases, "spline", hcaes(x = Date,y = Total_confirmed), name="Confirmed cases on each date:",color="purple") %>% 
            hc_exporting(enabled = TRUE) %>%
            hc_title(text="Total COVID-19 confirmed cases each day:",align="center") %>%
            hc_add_theme(hc_theme_elementary()) 
        
        
    })
   

})
