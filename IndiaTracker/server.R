

library(shiny)
#reading the data
require(rvest)
require(readr)
require(jsonlite)
require(janitor)
require(highcharter)
require(readxl)
require(RCurl)
require(viridis)
require(dplyr)







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
        hc_add_theme(hc_theme_ffx()) %>%  
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
      
      #converting the char vector to Date vector
      Tab$Update.Time.Stamp =  as.Date(Tab$Update.Time.Stamp, format="%d/%m/%Y")
      
      #ordering by latest dates
      Tab = Tab %>% arrange(desc(Update.Time.Stamp))
      
      Tab
      
    })
      
  
    output$TestingChart <- renderHighchart({
      
      #adding a new column of samples tested daily by calculating the moving differences
      
      Tested_ICMR <- Tested_ICMR %>%  mutate(Daily_tested = Total.Samples.Tested - lag(Total.Samples.Tested))
      
      Test_data <- Tested_ICMR %>% 
        select(Daily_tested,Update.Time.Stamp) 
      
      colnames(Test_data) <- c("Daily_tested","Date")
      Test_data <- na.omit(Test_data)
      
      hchart(Test_data, "column", hcaes(x = Date, y = Daily_tested), name="Count",color="orange") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Daily Samples tested for COVID-19 in India as per ICMR",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
    })
      
    
    
   
  
    output$ConfDaily <- renderHighchart({
      
      
      hchart(Case_time_series, "column", hcaes(x = Date, y = Daily.Confirmed), name="Confirmed Count",color="black") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New Confirmed COVID-19 cases everyday",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
        
        
    })
    
    
    output$DeathsDaily <- renderHighchart({
      
      hchart(Case_time_series, "column", hcaes(x = Date, y = Daily.Deceased), name="Confirmed Deaths",color="red") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New deaths reported everyday",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    
    output$RecoveredDaily <- renderHighchart({
      
      
      hchart(Case_time_series, "column", hcaes(x = Date, y = Daily.Recovered), name="Confirmed Recoveries",color="purple") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New recoveries reported everyday",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
    })
   
    
    output$StateConf <- renderHighchart({
      
      
      #removing the first row as it has the totals
      chart_data <- StateCOVID_19[-1,]
      
      hchart(chart_data, "treemap", hcaes(x = State, value = Confirmed, color = Confirmed)) %>% 
        hc_add_theme(hc_theme_ffx())
  
      
    })
    
    
    output$StateActive <- renderHighchart({
      
      #removing the first row as it has the totals
      chart_data <- StateCOVID_19[-1,]
      
      hchart(chart_data, "treemap", hcaes(x = State, value = Active,color=Active)) %>% 
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    output$StateDeaths <- renderHighchart({
      
      #removing the first row as it has the totals
      chart_data <- StateCOVID_19[-1,]
      
      hchart(chart_data, "treemap", hcaes(x = State, value = Deaths, color = Deaths)) %>% 
        hc_add_theme(hc_theme_ffx())
      
      
    })
    
    
    output$StateRecoveries <- renderHighchart({
      
      #removing the first row as it has the totals
      chart_data <- StateCOVID_19[-1,]
      
      hchart(chart_data, "treemap", hcaes(x = State, value = Recovered, color = Recovered)) %>% 
        hc_add_theme(hc_theme_ffx())
      
    })
    
    
    output$ratesPlotRecovery <- renderHighchart({
      
      
      #making a dataframe with recovery and death rates
      State_rate_recover <- StateCOVID_19 %>% 
        select(State,Confirmed,Deaths,Recovered) %>% 
        mutate(Recover_rate = round((Recovered/Confirmed)*100,2))
      
      #converting NaNs produced to 0
      State_rate_recover <- na.omit(State_rate_recover)
      
      State_rate_recover <- State_rate_recover %>% arrange(desc(Recover_rate))
      
      
      highchart() %>% 
        hc_xAxis(categories=State_rate_recover$State) %>% 
        hc_add_series(name="Recovery Rate %", data=State_rate_recover$Recover_rate, type="column") %>% 
        hc_colors(c("orange")) %>% 
        hc_add_theme(hc_theme_ffx()) %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Recovery rates of each Indian State",align="center") %>% 
        hc_subtitle(text="Calculated out of total Confirmed case count",align="center")
      
      
    })
    
    output$ratesPlotDeath <- renderHighchart({
      
      State_rate_death <- StateCOVID_19 %>% 
        select(State,Confirmed,Deaths,Recovered) %>% 
        mutate(Death_rate = round((Deaths/Confirmed)*100,2))
      
      #converting NaNs produced to 0
      State_rate_death <- na.omit(State_rate_death)
      
      State_rate_death <- State_rate_death %>% arrange(desc(Death_rate))
      
      
      highchart() %>% 
        hc_xAxis(categories=State_rate_death$State) %>% 
        hc_add_series(name="Death Rate %",data=State_rate_death$Death_rate, type="column") %>% 
        hc_colors(c("red")) %>% 
        hc_add_theme(hc_theme_ffx()) %>%  
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Death rates of each Indian State",align="center") %>% 
        hc_subtitle(text="Calculated out of total Confirmed case count",align="center")
      
    })
    
    
    output$StateCasesTimeSeries  <- renderHighchart({
      
      State_time_series_wide <- State_time_series %>% 
        select(input$state,Status,Date) %>% 
        spread(Status,input$state) 
      
      State_time_series_wide$Date <- as.Date(State_time_series_wide$Date,format="%d-%B-%y")
      
      
      State_time_series_conf <- State_time_series_wide %>% 
        select(Date,Confirmed)
      
      State_time_series_Death <- State_time_series_wide %>% 
        select(Date,Deceased)
      
      State_time_series_Recovered <- State_time_series_wide %>% 
        select(Date,Recovered)
      
      hchart(State_time_series_conf, "column", hcaes(x = Date, y = Confirmed), name="Daily new confirmed",color="purple") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New confirmed cases",align="center") %>%
        hc_add_theme(hc_theme_ffx())
      
      
      
    })


})


