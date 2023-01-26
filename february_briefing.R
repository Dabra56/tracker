library(tidyverse)
library(cansim)
library(rvest)
library(openxlsx)
library(lubridate)
library(openssl)
library(httpuv)
library(rtweet)
library(twitteR)
library(gtrendsR)

#Comment

gdp_nowcast_US_df <-  read.xlsx("https://www.frbatlanta.org/-/media/Documents/cqer/researchcq/gdpnow/GDPTrackingModelDataAndForecasts.xlsx",sheet="ContribHistory",detectDates = TRUE,cols = (2:50))


gdp_nowcast_US <- 
  gdp_nowcast_US_df %>% 
    filter(X1 == "GDP Nowcast")

#date_vector <- colnames(gdp_nowcast_df)

#gdp_nowcast_df <- gdp_nowcast_df %>% mutate(Date=date_vector)

write.csv(x = gdp_nowcast_US, file="gdp_nowcast.csv")

cfnai_df <- read.xlsx("https://www.chicagofed.org/-/media/publications/cfnai/cfnai-data-series-xlsx.xlsx", sheet="data",
                      detectDates = TRUE)

cfnai <-   
  cfnai_df %>% 
   select(Date, CFNAI) %>% 
    filter(Date>"2008-01-01")


write.csv(x = cfnai, file="cfnai.csv")

# US 

wei<-  read.xlsx("https://www.newyorkfed.org/medialibrary/research/interactives/wei/downloads/weekly-economic-index_data.xlsx",sheet="WEI for Recent Months",detectDates = TRUE, rows = (5:50))

wei <- 
  wei %>% 
    select(Date,WEI)

write.csv(x = wei, file="wei.csv")

# Getting weekly Employment insurance data 
ei_monthly <- get_cansim_vector("v64549350") %>% 
      filter(Date> "2018-12-01") %>% 
      select(Date,val_norm)
      
colnames(ei_monthly)[2] <- "Number of beneficiairies" 
  

# Creating starting file because weekly data is upgraded with no trace of historical data
ei_file <- read.csv("employment_insurance.csv")

ei_file <- ei_file [2:4]

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

Date <- word(ei_week,start=4,-1)

ei_table <- tibble(Date,ei_regular_beneficiairies,ei_newclaim)

if (mdy(ei_file[1,1])!=mdy(Date)) {
  
  ei_file <- 
    ei_file %>% bind_rows(ei_table)
} else {
  print("No new data")
}

# ei_file <- 
#   ei_file %>% 
#       mutate(Date= as.Date(dmy(Date)))

ei_datawrapper <- 
  ei_monthly %>% 
  bind_rows(ei_file %>% 
              mutate(Date= as.Date(mdy(Date))))

write.csv(ei_datawrapper, file = "employment_insurance_final.csv") 

business_conditions <-get_cansim("33-10-0398-01")

names(business_conditions)<-str_replace_all(names(business_conditions),
                                             c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

date_vector <- unique(business_conditions$Date)

city_vector <- c("Toronto, Ontario (0944)",
                 "Montreal, Quebec (0547)",
                 "Vancouver, British Columbia (0973)",
                 "Edmonton, Alberta (0252)",
                 "Winnipeg, Manitoba (1036)",
                 "Saskatoon, Saskatchewan (0738)",
                 "Moncton, New Brunswick (0539)",
                 "Halifax, Nova Scotia (0348)",
                 "St. John's, Newfoundland (792)")

last_date <- date_vector[length(date_vector)]
last_month <- date_vector[length(date_vector)-4]
six_month <- date_vector[length(date_vector)-26]
last_year <- date_vector[length(date_vector)-52]



business_conditions_last_date <- 
  business_conditions %>% 
    filter(Date==last_date) %>% 
    filter(GEO %in% city_vector) %>% 
    select(GEO, val_norm)%>%
rename(last_date = val_norm)

  
business_conditions_last_month <- 
  business_conditions %>% 
  filter(Date==last_month) %>% 
  filter(GEO %in% city_vector) %>% 
  select(GEO, val_norm) %>% 
  rename(last_month = val_norm)

business_conditions_six_month <- 
  business_conditions %>% 
  filter(Date==six_month) %>% 
  filter(GEO %in% city_vector) %>% 
  select(GEO, val_norm) %>% 
  rename(six_month = val_norm)

business_conditions_last_year <- 
  business_conditions %>% 
  filter(Date==last_year) %>% 
  filter(GEO %in% city_vector) %>% 
  select(GEO, val_norm) %>% 
  rename(last_year = val_norm)

business_conditions_city <- merge(x=business_conditions_last_date, y= business_conditions_last_month, by = "GEO")

business_conditions_city <-  merge(x=business_conditions_city, y= business_conditions_six_month, by = "GEO")

business_conditions_city <-  merge(x=business_conditions_city, y= business_conditions_last_year, by = "GEO")

reg_exp_city <- "(.*),"
  
business_conditions_city <- 
  business_conditions_city %>% 
    mutate(GEO= str_extract_all(GEO,pattern=reg_exp_city),
           GEO= str_sub(GEO,1, str_length(GEO)-1 ), 
           last_month = (last_date/last_month - 1)*100,
           six_month = (last_date/six_month -1)*100 ,
           last_year = (last_date/last_year -1)*100) 

colnames(business_conditions_city)[1] <-  ""
colnames(business_conditions_city)[2] <-  as.character(last_date)
colnames(business_conditions_city)[3] <-  "Variations since last month ^in %^"
colnames(business_conditions_city)[4] <-  "Variations for last 6 months ^in %^"
colnames(business_conditions_city)[5] <-  "Variations since previous year ^in %^"

write.csv(x = business_conditions_city, file="business_condition.csv")

# GOOGLE TRENDS 
# 
# install.packages("gtrendsR")

 
  # search_terms <- c("Recession", 
  #                   "Unemployment",
  #                   "Inflation")

gtrends(keyword = "Recession",
         geo = "CA",
         time = "today 12-m") -> recession_results

gtrends_recession <- recession_results %>%
  .$interest_over_time %>%
  select(date,hits) %>%
  rename(Recession = hits)

Sys.sleep(100)

gtrends(keyword = "Unemployment",
        geo = "CA",
        time = "today 12-m") -> unemployment_results

gtrends_unemployment <- unemployment_results %>%
  .$interest_over_time %>%
  select(date,hits) %>%
  rename(Unemployment = hits)

Sys.sleep(100)  

gtrends(keyword = "Inflation",
        geo = "CA",
        time = "today 12-m") -> inflation_results

gtrends_inflation <- inflation_results %>%
  .$interest_over_time %>%
  select(date,hits) %>%
  rename(Inflation = hits)


gtrends_all <- merge(x=gtrends_recession, y=gtrends_unemployment, by="date" )
gtrends_all <- merge(x=gtrends_all, y=gtrends_inflation, by="date" )

write.csv(x = gtrends_recession, file="gtrend_recession.csv")
write.csv(x = gtrends_unemployment, file="gtrend_unemployment.csv")
write.csv(x = gtrends_inflation, file="gtrends_inflation.csv")
write.csv(x = gtrends_all, file="gtrends_all.csv")

#Twitter

nowcast_gdp_file <- read.csv("nowcast_gdp.csv")


appname <- "Nowcast_CPACanada"
api_key <- "RImdmIWcLymaoMpNz4FRYuFHn"
api_secret_key <- "YODJ9F1vz1PFHzd5ORPoSHkWOwtCpoLLF2yJ1sLf7o1tOmJgPZ"

#Beared Token = "AAAAAAAAAAAAAAAAAAAAAEGwlQEAAAAA1KVzcV5965DQBJt%2BIGf%2FhRXNENo%3DpPtKvZvUkrWMMJZ7Egb8uB2NYcyqDLNptSyCtG1sQDWXabXh1h"

access_token <- "1218979300701102081-M9aFzFH5DVtj25VndfW12xcslQ6Gs0"

access_token_secret  <- "mKqGjEwFNLv8NmqcYDuW2HDZJbPE7Kw25KixMQwOEP3F0"

twitter_token <- create_token(
  app=appname, 
  consumer_key = api_key, 
  consumer_secret = api_secret_key, 
  access_token = access_token, 
  access_secret = access_token_secret
)

get_token()

tweet_nowcast_canada <- search_tweets("@IFSD_IFPD", n = 100, include_rts = FALSE)

nowcast_gdp <- 
  tweet_nowcast_canada %>% 
  mutate (nowcast_gdp =str_extract(text, "(\\d\\.\\d%)\\s\\(q\\/q, annualized\\)"),
          created_at = as.Date(created_at)) %>% 
  select(created_at,nowcast_gdp) %>% 
  rename(date= created_at) %>% 
  arrange(desc(date)) %>% 
  drop_na(nowcast_gdp) %>% 
  mutate(nowcast_gdp=substr(nowcast_gdp,1,4))


nowcast_gdp_file <- 
  nowcast_gdp_file %>% 
      select(-X) %>% 
      mutate(date=as.Date(date)) %>% 
      bind_rows(nowcast_gdp) %>% 
      arrange(date)

nowcast_gdp_file <- nowcast_gdp_file[!duplicated(nowcast_gdp_file), ]

write.csv(nowcast_gdp_file, file = "nowcast_gdp.csv", append = TRUE) 


# searchTwitter("Covid-19", n=3)
# 
# #APP ID = 26587201
# # API Key  = 2cst0jLqa7DG3UMBE0lvJSuDl
# # API Secret  = yRhZMjKARXlAAvygRfYe9l9jLi1o4Eoiu18CHHR5eOwW4dwtGo

# Bearer Token  = AAAAAAAAAAAAAAAAAAAAAEGwlQEAAAAASFGwRXQRv3pq7kLsDphjshSuYac%3DpI10YenudeWmQTwnaMjwiA96byUccfbxPoU9g2UqztEZPuLuTu





# 
# library(tidyverse)  # data wrangling
# library(RSelenium)  # activate Selenium server
# library(rvest)      # web scrape tables
# library(netstat)    # find unused port
# library(data.table) # for the rbindlist function
# 
# rs_driver_object <- rsDriver(browser = "chrome",
#                              chromever = "109.0.5414.75",
#                              verbose = F,
#                              port = free_port())
# 
# #read the html content of the website
# url <- read_html("https://www.canada.ca/en/employment-social-development/programs/ei/statistics.html")
# 
# #use html_nodes to extract the td elements from the website
# td
# 
# #view the elements of the list
# data_list
# 
