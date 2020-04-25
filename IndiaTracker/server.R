

library(shiny)
#reading the data
require(rvest)
require(readr)
require(jsonlite)
require(janitor)
require(highcharter)
require(readxl)
require(RCurl)







#complete state cases dataset

#server logic
shinyServer(function(input, output) {
    
  #open connection to the file which is a efficient way.
  myfile1 <- getURL('https://api.covid19india.org/csv/latest/state_wise.csv', 
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  #dataset with summariesed case counts of statese
  StateCOVID_19 <- read.csv(textConnection(myfile1), header=T)
 
  
  
  #dataset with time series data of Indian states
  myfile2 <- getURL('https://api.covid19india.org/csv/latest/state_wise_daily.csv', 
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  State_time_series <- read.csv(textConnection(myfile2),header = T)
  
  
  
  #total Indian cases time series and daily changes
  myfile3 <- getURL('https://api.covid19india.org/csv/latest/case_time_series.csv', 
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  Case_time_series <- read.csv(textConnection(myfile3),header = T)
  
  
  
  myfile5 <- getURL('https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv',
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  State_tested<- read.csv(textConnection(myfile5), header = T)
  
  
  
  
  myfile6 <- getURL('https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv',
                    ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  
  Tested_ICMR <- read.csv(textConnection(myfile6),header = T)
  
    
    
    
    
    
    output$Confirmed <- renderText({
        #no of rows in the raw dataset represents the total cases in India
        StateCOVID_19$Confirmed[1]
       
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
  
    
    output$stackedCovidIndia <- renderHighchart({
      
      highchart() %>% 
        hc_xAxis(categories=Case_time_series$Date) %>% 
        hc_add_series(name="Deaths", data=Case_time_series$Total.Deceased) %>% 
        hc_add_series(name="Recoveries",data=Case_time_series$Total.Recovered) %>% 
        hc_add_series(name="Confirmed Cases", data=Case_time_series$Total.Confirmed) %>% 
        hc_colors(c("red","green","black")) %>% 
        hc_add_theme(hc_theme_elementary()) %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Analysis of count of deaths,recoveries and cases for COVID-19 till date(Cumalative count) in India",align="center")
      
      
    })
     
    
    output$StateData <- renderDataTable({
      
      #selecting only first 6 columns of the data frame
      StateCOVID_19[1:6]
      
    })
    
    
    #tab 3 details
   output$totalTested <- renderText({
     
     val=Tested_ICMR$Total.Samples.Tested[nrow(Tested_ICMR)]
     val
     
   })
    
    
    output$RateTable<- renderDataTable({
      
      
      Tab <- Tested_ICMR %>% 
        select(Update.Time.Stamp,Total.Samples.Tested,Test.positivity.rate) %>% 
        mutate(Daily_tested = Total.Samples.Tested - lag(Total.Samples.Tested))
      
      Tab
      
      
      
    })
      
  
    output$TestingChart <- renderHighchart({
      
      #adding a new column of samples tested daily by calculating the moving differences
      
      Tested_ICMR <- Tested_ICMR %>%  mutate(Daily_tested = Total.Samples.Tested - lag(Total.Samples.Tested))
      
      Test_data <- Tested_ICMR %>% 
        select(Daily_tested,Update.Time.Stamp) 
      
      colnames(Test_data) <- c("Daily_tested","Date")
      Test_data <- na.omit(Test_data)
      
      hchart(Test_data, "column", hcaes(x = Date, y = Daily_tested), name="Count",color="green") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Daily Samples tested for COVID-19 in India as per ICMR",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_elementary()) 
      
    })
      
    
    
   
  
    output$ConfDaily <- renderHighchart({
      
      
      hchart(Case_time_series, "column", hcaes(x = Date, y = Daily.Confirmed), name="Confirmed Count",color="black") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New Confirmed COVID-19 cases everyday",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_elementary()) 
      
        
        
    })
    
    
    output$DeathsDaily <- renderHighchart({
      
      hchart(Case_time_series, "column", hcaes(x = Date, y = Daily.Deceased), name="Confirmed Deaths",color="red") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New deaths reported everyday",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_elementary()) 
      
      
    })
    
    
    output$RecoveredDaily <- renderHighchart({
      
      
      hchart(Case_time_series, "column", hcaes(x = Date, y = Daily.Recovered), name="Confirmed Recoveries",color="green") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New recoveries reported everyday",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_elementary()) 
      
    })
   


})


