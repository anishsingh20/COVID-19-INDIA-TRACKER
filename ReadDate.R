#reading the raw COVID-19 JSON data(refreshed every 5 minutes)

library(RCurl)
require(lubridate)

#links to datasets used in APP:
#1) https://api.covid19india.org/csv/latest/state_wise_daily.csv #time series of states
#2) https://api.covid19india.org/csv/latest/state_wise.csv  #state summarised data
#3) https://api.covid19india.org/csv/latest/case_time_series.csv #cases time series
#4) https://api.covid19india.org/csv/latest/raw_data.csv # India's raw data
#5) https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv # Statewise tested 
#6) https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv  #tested ICMR data
 
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
 


#Raw data with all the information
myfile4 <- getURL('https://api.covid19india.org/csv/latest/raw_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

raw_India_data <- read.csv(textConnection(myfile4),header = T)
head(raw_India_data)


myfile5 <- getURL('https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

State_tested<- read.csv(textConnection(myfile5), header = T)




myfile6 <- getURL('https://api.covid19india.org/csv/latest/tested_numbers_icmr_data.csv',
                  ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)

Tested_ICMR <- read.csv(textConnection(myfile6),header = T)




Test_positive <- Tested_ICMR %>% 
  select(Test.positivity.rate,Update.Time.Stamp) 

Test_positive$Update.Time.Stamp =  as.Date(Test_positive$Update.Time.Stamp, format="%d/%m/%Y")

#ordering by latest dates
Test_positive = Test_positive %>% arrange(desc(Update.Time.Stamp))



colnames(Test_positive) <- c("Positive_rate","Date")
Test_positive <- na.omit(Test_positive)

Test_positive$Positive_rate <- as.double(Test_positive$Positive_rate)


#Finding Recovery Rate
recovery_rate <- Case_time_series %>% 
  mutate(Recover_rate = round((Total.Recovered/Total.Confirmed)*100,2),
         Death_rate = round((Total.Deceased/Total.Confirmed)*100,2))



State_rate <- StateCOVID_19 %>% 
  select(State,Confirmed,Deaths,Recovered) %>% 
  mutate(Recover_rate = round((Recovered/Confirmed)*100,2),
         Death_rate = round((Deaths/Recovered)*100,2))

  

#selecting the specific State and sprading the data
State_time_series_long <- State_time_series %>% 
  select(MH,Status,Date) %>% 
  spread(Status,MH) 

State_time_series_long$Date <- as.Date(State_time_series_long$Date,format="%d-%B-%y")
  
State_time_series_conf <- State_time_series_long %>% 
  select(Date,Confirmed)


State_time_series_Death <- State_time_series_long %>% 
  select(Date,Deceased)

State_time_series_Recovered <- State_time_series_long %>% 
  select(Date,Recovered)
  
  



District_data <- raw_India_data %>% 
  filter(Detected.State=="Uttarakhand") %>% 
  select(Detected.District) %>% 
  group_by(Detected.District) %>% 
  summarise(Count=n())


