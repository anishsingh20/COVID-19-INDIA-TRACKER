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





#complete state cases dataset

#server logic
shinyServer(function(input, output, session) {
    
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
  
    
    
  #DISTRICT WISE DATA(IN JSON FORMAT)
  
  json_file <- "https://api.covid19india.org/state_district_wise.json"
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
    
    
    
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
    
    
    output$Tested <- renderText({
      
      val=Tested_ICMR$Total.Samples.Tested[nrow(Tested_ICMR)]
      val
      
    })
  
    
    output$RecRate <- renderText({
      
      State_rate_recover <- StateCOVID_19 %>% 
        select(State,Confirmed,Deaths,Recovered) %>% 
        mutate(Recover_rate = round((Recovered/Confirmed)*100,2))
      
      #getting value of first row of the column
      State_rate_recover$Recover_rate[1]
      
    })
    
    
    output$DeadRate <- renderText({
      
      
      State_rate_death <- StateCOVID_19 %>% 
        select(State,Confirmed,Deaths,Recovered) %>% 
        mutate(Death_rate = round((Deaths/Confirmed)*100,2))
      
      #getting value of first row of the column
      State_rate_death$Death_rate[1]
      
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
     
    
    output$StateData <- renderDT({
      
      #selecting only first 7 columns of the data frame
      StateCOVID_19[1:7]
      
    })
    
    
    #tab 3 details
   output$totalTested <- renderText({
     
     val=Tested_ICMR$Total.Samples.Tested[nrow(Tested_ICMR)]
     val
     
   })
    
    
    output$RateChartIndia <- renderHighchart({
      
      
      India_test_positive <- Tested_ICMR %>% 
        select(Update.Time.Stamp, Test.positivity.rate) %>% 
        group_by(Update.Time.Stamp) 
      
      India_test_positive$Test.positivity.rate <- as.character(India_test_positive$Test.positivity.rate)
      India_test_positive$Test.positivity.rate <- parse_number(India_test_positive$Test.positivity.rate)
      India_test_positive <- na.omit(India_test_positive)
    
      hchart(India_test_positive, "column", hcaes(x = Update.Time.Stamp , y = Test.positivity.rate), name="Rate",color="blue") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="Percentage of Tested Positive for COVID-19 out of Total Tested Daily in India",align="center") %>%
        hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
        hc_add_theme(hc_theme_ffx())
      
    
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
      
      #making a data frame of state codes
      df_State_codes <- StateCOVID_19 %>% 
        select(State,State_code) 
      
      #filtering the state code of the selected state
      State_code <- df_State_codes %>% 
        filter(State==input$state)
      
      State_time_series_wide <- State_time_series %>% 
        select(State_code$State_code,Status,Date) %>% 
        spread(Status,State_code$State_code) 
      
      State_time_series_wide$Date <- as.Date(State_time_series_wide$Date,format="%d-%B-%y")
      
      
      State_time_series_conf <- State_time_series_wide %>% 
        select(Date,Confirmed)
      
     
      
     
      
      hchart(State_time_series_conf, "column", hcaes(x = Date, y = Confirmed), name="Daily new confirmed",color="purple") %>% 
        hc_exporting(enabled = TRUE) %>%
        hc_title(text="New confirmed cases daily",align="center") %>%
        hc_add_theme(hc_theme_ffx())
      
      
      
    })

    
      output$StateCasesTimeSeries_recover <- renderHighchart({
        
        
        #making a data frame of state codes
        df_State_codes <- StateCOVID_19 %>% 
          select(State,State_code) 
        
        #filtering the state code of the selected state
        State_code <- df_State_codes %>% 
          filter(State==input$state)
        
        State_time_series_wide <- State_time_series %>% 
          select(State_code$State_code,Status,Date) %>% 
          spread(Status,State_code$State_code) 
        
        
        State_time_series_wide$Date <- as.Date(State_time_series_wide$Date,format="%d-%B-%y")
        
        State_time_series_Recovered <- State_time_series_wide %>% 
          select(Date,Recovered)
        
        hchart(State_time_series_Recovered, "column", hcaes(x = Date, y = Recovered), name="Daily new recovered",color="blue") %>% 
          hc_exporting(enabled = TRUE) %>%
          hc_title(text="New confirmed recoveries daily",align="center") %>%
          hc_add_theme(hc_theme_ffx())
        
      })
      
      
      output$StateCasesTimeSeries_death <- renderHighchart({
        
        
        #making a data frame of state codes
        df_State_codes <- StateCOVID_19 %>% 
          select(State,State_code) 
        
        #filtering the state code of the selected state
        State_code <- df_State_codes %>% 
          filter(State==input$state)
        
        
        State_time_series_wide <- State_time_series %>% 
          select(State_code$State_code,Status,Date) %>% 
          spread(Status,State_code$State_code) 
        
        State_time_series_wide$Date <- as.Date(State_time_series_wide$Date,format="%d-%B-%y")
        State_time_series_Death <- State_time_series_wide %>% 
          select(Date,Deceased)
        
        hchart(State_time_series_Death, "column", hcaes(x = Date, y = Deceased), name="Daily new deaths",color="red") %>% 
          hc_exporting(enabled = TRUE) %>%
          hc_title(text="New confirmed deaths daily",align="center") %>%
          hc_add_theme(hc_theme_ffx())
        
      })
      
      
      output$StateTestChart  <- renderHighchart({
        
        State_test_data <- State_tested   %>% 
          filter(State == input$state_test)  %>% 
          select(Updated.On,Total.Tested)
          
          
        hchart(State_test_data, "line", hcaes(x = Updated.On, y = Total.Tested), name="Total Samples Tested",color="green") %>% 
          hc_exporting(enabled = TRUE) %>%
          hc_title(text="Total Samples Tested Statewise(Cumalative Count)",align="center") %>%
          hc_add_theme(hc_theme_ffx())
        
      }) 
      
      
     output$StateDailyTestChart <- renderHighchart({
       
       #Daily tested data for each state selected. Adding a new column and adding Moving differences.
       State_test_data_daily <- State_tested   %>% 
         filter(State == input$state_test)  %>% 
         select(Updated.On,Total.Tested) %>%
         mutate(Daily_tested = Total.Tested - lag(Total.Tested))
       
       
       hchart(State_test_data_daily, "column", hcaes(x = Updated.On, y = Daily_tested), name="Daily Samples Tested",color="red") %>% 
         hc_exporting(enabled = TRUE) %>%
         hc_title(text="Daily Samples Tested Statewise",align="center") %>%
         hc_add_theme(hc_theme_ffx())
       
     })
     
     output$StatePositiveTestRate <- renderHighchart({
       
       
       State_Positive_rate <- State_tested   %>% 
         filter(State == input$state_test)  %>% 
         select(Updated.On,Total.Tested,Positive) %>%
         mutate(Positive_rate=round((Positive/Total.Tested)*100,2))
       
         
       
       #converting Rates to character vector and then converting it to Numeric(decimal) value to plot
       #State_Positive_rate$Test.positivity.rate <- as.character(State_Positive_rate$Test.positivity.rate)
       #State_Positive_rate$Test.positivity.rate <- readr::parse_number(State_Positive_rate$Test.positivity.rate)
       
       hchart(State_Positive_rate, "column", hcaes(x = Updated.On, y = Positive_rate), name="Rate%",color="blue") %>% 
         hc_exporting(enabled = TRUE) %>%
         hc_title(text="Percentage of Tested Positive for COVID-19 out of Total Tested Daily in the State",align="center") %>%
         hc_subtitle(text="Few days have missing data. Actual values may vary",align="center") %>% 
         hc_add_theme(hc_theme_ffx())
       
       
       
     })
     
     
     #nested selectInputs for district data analysis
     observe({
       
       #getting the value of the selected State
       State_district<-input$district_state
       #taking 1 value of the list which is the district name and making a dataframe of it.
       x<-as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 1)))
       
       #to use the rownames as a column of district names 
       setDT(x, keep.rownames = "District")[]
       x <- x %>% mutate_all(na_if,"") #removing NA values
       
       #adding district names in the nested selectInput
       updateSelectInput(session, "district", choices = x$District)
       
       
       output$district_active <- renderHighchart({
         
         #getting the value of the selected State
         State_district<-input$district_state
         
         
         State_active <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 2)))
         colnames(State_active)<- c("Active")
         #setting row names as columns values in a new column District
         setDT(State_active, keep.rownames = "District")[]
         
         State_active <- State_active %>% arrange(desc(Active))
         
         hchart(State_active,type="pie",hcaes(x=District,y=Active),name="Active") %>% 
           hc_exporting(enabled = TRUE) %>%
           hc_title(text="Pie Chart of Active COVID-19 cases in each District",align="center") %>% 
           hc_add_theme(hc_theme_ffx())
         
         
       })
       
       
       output$district_confirmed <- renderHighchart({
         
         State_conf <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 3)))
         colnames(State_conf)<- c("Confirmed")
         #setting row names as columns values in a new column District
         setDT(State_conf, keep.rownames = "District")[]
         
         State_conf <- State_conf %>% arrange(desc(Confirmed))
         
         hchart(State_conf,type="pie",hcaes(x=District,y=Confirmed),name="Confirmed") %>% 
           hc_exporting(enabled = TRUE) %>%
           hc_title(text="Pie Chart of Total confirmed COVID-19 cases in each District",align="center") %>% 
           hc_add_theme(hc_theme_ffx())
         
       })
       
       
       output$district_dead <- renderHighchart({
         
         State_death <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 4)))
         colnames(State_death)<- c("Death")
         #setting row names as columns values in a new column District
         setDT(State_death, keep.rownames = "District")[]
         
         State_death <- State_death %>% arrange(desc(Death))   
         
         hchart(State_death,type="pie",hcaes(x=District,y=Death),name="Deaths") %>% 
           hc_exporting(enabled = TRUE) %>%
           hc_title(text="Pie Chart of Deaths due to COVID-19 in each District",align="center") %>% 
           hc_add_theme(hc_theme_ffx())
         
       })
       
       
       
       output$district_recovered <- renderHighchart({
         
         State_recovered <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 5)))
         colnames(State_recovered)<- c("Recovered")
         #setting row names as columns values in a new column District
         setDT(State_recovered, keep.rownames = "District")[]
         
         State_recovered <- State_recovered %>% arrange(desc(Recovered))
         
         hchart(State_recovered,type="pie",hcaes(x=District,y=Recovered),name="Recovered") %>% 
           hc_exporting(enabled = TRUE) %>%
           hc_title(text="Pie Chart of Recovered COVID-19 cases in each District",align="center") %>% 
           hc_add_theme(hc_theme_ffx())
         
         
       })
       
       
       output$district_death_rate <- renderHighchart({
         
         State_conf <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 3)))
         colnames(State_conf)<- c("Confirmed")
         #setting row names as columns values in a new column District
         setDT(State_conf, keep.rownames = "District")[]
         
         
         State_death <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 4)))
         colnames(State_death)<- c("Death")
         #setting row names as columns values in a new column District
         setDT(State_death, keep.rownames = "District")[]
         
         
         #joining data to get the total confirmed and total deaths in each district to find rates
         df_district = merge(x = State_conf, y = State_death, by = "District", all = TRUE)
         
         df_district_DeathRate <- df_district %>% 
           mutate(Death_rate=round((Death/Confirmed)*100),2) %>%
           select(District,Death_rate) %>% 
           group_by(District) %>% 
           arrange(desc(Death_rate))
         
         
         highchart() %>% 
           hc_xAxis(categories=df_district_DeathRate$District) %>% 
           hc_add_series(name="Death Rate %", data=df_district_DeathRate$Death_rate, type="column") %>% 
           hc_colors(c("red")) %>% 
           hc_add_theme(hc_theme_ffx()) %>%  
           hc_exporting(enabled = TRUE) %>%
           hc_title(text="Death rates of each district",align="center") %>% 
           hc_subtitle(text="Calculated out of total Confirmed case count | Some data is missing or unknown(unaccounted for)",align="center")
         
       })
       
       
       
       output$district_recovery_rate <- renderHighchart({
         
         State_conf <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 3)))
         colnames(State_conf)<- c("Confirmed")
         #setting row names as columns values in a new column District
         setDT(State_conf, keep.rownames = "District")[]
         
         State_recovered <- as.data.frame(unlist(lapply(json_data[[State_district]]$districtData, `[[`, 5)))
         colnames(State_recovered)<- c("Recovered")
         #setting row names as columns values in a new column District
         setDT(State_recovered, keep.rownames = "District")[]
         
         
         #joining data to get the total confirmed and total recovered in each district to find rates
         df_district = merge(x = State_conf, y = State_recovered, by = "District", all = TRUE)
         
         
         df_district_RecRate <- df_district %>% 
           mutate(Rec_rate=round((Recovered/Confirmed)*100),2) %>% 
           select(District,Rec_rate) %>% 
           group_by(District) %>% 
           arrange(desc(Rec_rate))
         
         
         highchart() %>% 
           hc_xAxis(categories=df_district_RecRate$District) %>% 
           hc_add_series(name="Recovery Rate %", data=df_district_RecRate$Rec_rate, type="column") %>% 
           hc_colors(c("green")) %>% 
           hc_add_theme(hc_theme_ffx()) %>%  
           hc_exporting(enabled = TRUE) %>%
           hc_title(text="Recovery rates of each district",align="center") %>% 
           hc_subtitle(text="Calculated out of total Confirmed case count | Some data is missing or unknown(unaccounted for)",align="center")
         
         
       })
       
       
      })
     
     
     
      
})


