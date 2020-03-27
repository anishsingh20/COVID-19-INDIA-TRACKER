
#reading the data

#complete cases dataset
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSc_2y5N0I67wDU38DjDh35IZSIS30rQf7_NYZhtYYGU1jJYT6_kDx4YpF-qw0LSlGsBYP8pqM_a1Pd/pubhtml#"


require(rvest)


#reading the table from the URL using rvest package
h1<- read_html(url)
html_text(h1)




tab <- tab %>% setNames(c("S.No", "State", "Confirmed", "Recovered","Deaths","Active",
                          "Last_Updated_Time")) 

#removing the NA column
tab <- h1 %>% html_nodes("table")
tab <- tab[[1]] %>% html_table
tab <- na.omit(tab)


tab <- tab[colSums(!is.na(tab)) > 0]
#removing the NA rows
tab <- na.omit(tab)

