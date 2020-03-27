
#reading the data
require(rvest)
require(readr)
require(jsonlite)




#reading the raw COVID-19 JSON data(refreshed every 5 minutes)
url1 <- "https://api.rootnet.in/covid19-in/unofficial/covid19india.org"
#loading thhe JSON data fron the web
jsonDoc <- fromJSON(url1)

#extracting data in data frame
India_data <- jsonDoc$data$rawPatientData


#complete state cases dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pubhtml#"

#reading the table from the URL using rvest package

h1<- read_html(url)
html_text(h1)


tab <- h1 %>% html_nodes("table")
tab <- tab[[1]] %>% html_table


#setting the column names
tab <- tab %>% setNames(c("S.No", "State","0", "Confirmed", "Recovered","Deaths","Active",
                          "Last_Updated_Time")) 

#removing the NA column
tab <- tab[colSums(!is.na(tab)) > 0]
#removing the NA rows
tab <- na.omit(tab)

