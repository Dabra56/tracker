library(tidyverse)

library(openxlsx)

gdp_nowcast_df <-  read.xlsx("https://www.frbatlanta.org/-/media/Documents/cqer/researchcq/gdpnow/GDPTrackingModelDataAndForecasts.xlsx",sheet="ContribHistory",detectDates = TRUE,cols = (2:50))


gdp_nowcast <- 
  gdp_nowcast_df %>% 
    filter(X1 == "GDP Nowcast")

#date_vector <- colnames(gdp_nowcast_df)

#gdp_nowcast_df <- gdp_nowcast_df %>% mutate(Date=date_vector)



cfnai_df <- read.xlsx("https://www.chicagofed.org/-/media/publications/cfnai/cfnai-data-series-xlsx.xlsx", sheet="data",
                      detectDates = TRUE)

cfnai <-   
  cfnai_df %>% 
   select(Date, CFNAI) %>% 
    filter(Date>"2008-01-01")



library(rvest)

employment_insurance<- read_html("https://www.canada.ca/en/employment-social-development/programs/ei/statistics.html")


table_data <- 
  employment_insurance %>% 
  html_nodes("td")%>% 
  html_text() 

ei_newclaim <-  table_data[5]
ei_regular_beneficiairies <- table_data[9]


title_column <- 
  employment_insurance %>% 
  html_nodes(".text-right :nth-child(2)")%>% 
  html_text() 

ei_week <- title_column[4]

ei_date <- word(ei_week,start=4,-1)

ei_table <- tibble(ei_date,ei_regular_beneficiairies,ei_newclaim)

write.csv(x = ei_table, file="data/businesses.csv")


library(tidyverse)  # data wrangling
library(RSelenium)  # activate Selenium server
library(rvest)      # web scrape tables
library(netstat)    # find unused port
library(data.table) # for the rbindlist function

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "109.0.5414.75",
                             verbose = F,
                             port = free_port())

#read the html content of the website
url <- read_html("https://www.canada.ca/en/employment-social-development/programs/ei/statistics.html")

#use html_nodes to extract the td elements from the website
td

#view the elements of the list
data_list

