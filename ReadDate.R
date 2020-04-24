#reading the raw COVID-19 JSON data(refreshed every 5 minutes)

library(RCurl)

#links to datasets used in APP:
#1) https://api.covid19india.org/csv/latest/state_wise_daily.csv #time series of states
#2) https://api.covid19india.org/csv/latest/state_wise.csv  #state summarised data
#3) https://api.covid19india.org/csv/latest/case_time_series.csv #cases time series
#4) https://api.covid19india.org/csv/latest/raw_data.csv # India's raw data
 
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


Pune_data <- raw_India_data %>% 
  filter(Detected.District=="Dehradun")
