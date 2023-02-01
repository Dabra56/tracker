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
library(httr)

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
      filter(Date> "2021-12-01") %>% 
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
  bind_rows(ei_file %>% mutate(Date= as.Date(mdy(Date)))) %>% 
  mutate(ei_regular_beneficiairies= (gsub(",","",ei_regular_beneficiairies)))


for (i in 1:nrow(ei_datawrapper)){

      if (!is.na(ei_datawrapper$ei_regular_beneficiairies[i])) {
        year(ei_datawrapper$Date[i]) <- 2023
        
      }
}
#
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
#install.packages("gtrendsR")

 
  # search_terms <- c("Recession", 
#   #                   "Unemployment",
#   #                   "Inflation")
# Sys.sleep(100)
# 
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

# Might need to change URL
url <- "https://assets.ctfassets.net/gm6df3h7p862/5f8qkOUJDksXy7YOh0MgRj/338782b861e240a2ef17ae697bbcf0fb/MLS_HPI.zip"

zip_file <- GET(url)

writeBin(content(zip_file, "raw"), "MLS_HPI.zip")

unzip("MLS_HPI.zip")

MLS_HPI <- read.xlsx("Not Seasonally Adjusted.xlsx",detectDates = TRUE)

last_date=tail(MLS_HPI$Date)[1]

all_housing <- 
  MLS_HPI %>% 
    select(Date,Composite_HPI) %>% 
  mutate(last_month=lag(Composite_HPI,n=1)) %>% 
  filter(Date>"2021-12-01") %>% 
  mutate(housing_change_last_month = (Composite_HPI/last_month-1)*100) %>% 
  select(Date,housing_change_last_month)

all_housing_decrease <- 
  all_housing %>% 
      filter(Date>"2022-03-01") %>% 
      mutate(total_decrease = cumsum(housing_change_last_month)) %>% 
    select(Date,total_decrease)

housing_merge <- merge(x=all_housing,y=all_housing_decrease, by="Date",all=TRUE)

write.csv(all_housing, file = "housing.csv")     

single_family <- 
  MLS_HPI %>% 
  select(Date,Single_Family_HPI) %>% 
  mutate(last_month=lag(Single_Family_HPI,n=1)) %>% 
  filter(Date>"2021-12-01") %>% 
  mutate(sf_change_last_month = (Single_Family_HPI/last_month-1)*100)%>% 
  select(Date,sf_change_last_month)




# Single_Family_HPI
# 
# single_family <-   MLS_HPI %>% 
#   select(Date,Single_Family_HPI) %>% 
#   mutate(last_month=lag(Single_Family_HPI,n=1),
#          last_six_month=lag(Single_Family_HPI,n=6),
#          last_year=lag(Single_Family_HPI,n=12)) %>% 
#   filter(Date==last_date) %>% 
#   mutate(change_last_month = round((Single_Family_HPI/last_month-1)*100,digits = 1),
#          change_six_month = round((Single_Family_HPI/last_six_month-1)*100,digits = 1),
#          change_last_year = round((Single_Family_HPI/last_year-1)*100,digits=1)) %>% 
#   mutate(indicator = "Single family homes") %>% 
#   select(indicator,
#          change_last_month,
#          change_six_month,
#          change_last_year)


ft_employment <- get_cansim_vector("v2062812") %>% 
  select(Date,val_norm) %>% 
  rename(ft_employment = val_norm) %>% 
  mutate(last_month=lag(ft_employment,n=1))
# 
# last_date=tail(ft_employment$Date)[1]

ft_employment_change <- 
  ft_employment %>% 
    filter(Date>"2021-12-01") %>% 
      mutate(ft_change_last_month = (ft_employment - last_month)/1000) %>% 
  select(Date,ft_change_last_month)


pt_employment <- get_cansim_vector("v2062813") %>% 
  select(Date,val_norm) %>% 
  rename(pt_employment = val_norm) %>% 
  mutate(last_month=lag(pt_employment,n=1))

pt_employment_change <- 
 pt_employment %>% 
  filter(Date>"2021-12-01") %>% 
  mutate(pt_change_last_month = (pt_employment - last_month)/1000) %>% 
  select(Date,pt_change_last_month)


employment_change <- merge(x=ft_employment_change,y=pt_employment_change,by="Date")

write.csv(employment_change, file = "employment_change.csv")     


job_vacancy <- get_cansim_vector("v1212389364") %>% 
  select(Date,val_norm) %>% 
  mutate(last_month=lag(val_norm,n=1)) %>% 
  filter(Date>"2021-12-01") %>% 
  rename(job_vacancy = val_norm) %>% 
  mutate(job_vacancy_change = job_vacancy -last_month )

job_vacancy_change <- 
  job_vacancy %>% 
      mutate(job_vacancy_change=job_vacancy_change/1000) %>% 
      select(Date,job_vacancy_change)

employment_vacancy <- merge(x=employment_change,y=job_vacancy_change,by="Date",all=TRUE)

employment_vacancy_monthly <- 
  employment_vacancy %>% 
  rowwise() %>% 
    mutate(jobs = ft_change_last_month + pt_change_last_month,total=sum(job_vacancy_change,ft_change_last_month,pt_change_last_month,na.rm = TRUE)) %>% 
    select(Date,jobs,job_vacancy_change,total)

write.csv(employment_vacancy_monthly, file = "employment_vacancy_change.csv")    

last_date <- tail(employment_vacancy$Date)

employment_vacancy_cumulative <- 
  employment_vacancy %>% 
    mutate(jobs = ft_change_last_month + pt_change_last_month,total=sum(job_vacancy_change,ft_change_last_month,pt_change_last_month,na.rm = TRUE),
           cumulative_jobs = cumsum(jobs), 
           cumulative_vacancy = cumsum(job_vacancy_change)) %>% 
  fill(cumulative_vacancy,.direction = "downup") %>% 
  rowwise() %>% 
mutate(cumulative_total =sum(cumulative_jobs,cumulative_vacancy,na.rm = TRUE) ) %>% 
  select(Date,cumulative_jobs,cumulative_vacancy,cumulative_total)

write.csv(employment_vacancy_cumulative, file = "employment_vacancy_cumulative.csv")    


gdp_industry <-get_cansim("36-10-0434-02")

names(gdp_industry)<-str_replace_all(names(gdp_industry),
                                            c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))

gdp_all_industry <- 
  gdp_industry %>% 
    filter(Seasonal_adjustment=="Seasonally adjusted at annual rates",
           Prices=="Chained (2012) dollars",
           North_American_Industry_Classification_System__NAICS_=="All industries [T001]") %>% 
  mutate(last_month=lag(val_norm,n=1)) %>%
    filter(Date >"2021-12-01") %>% 
    mutate(changes_month = (val_norm/last_month - 1)*100) %>% 
  select(Date,changes_month) %>% 
  rename(all_industry = changes_month)

gdp_goods <- 
  gdp_industry %>% 
  filter(Seasonal_adjustment=="Seasonally adjusted at annual rates",
         Prices=="Chained (2012) dollars",
         North_American_Industry_Classification_System__NAICS_=="Goods-producing industries [T002]") %>% 
  mutate(last_month=lag(val_norm,n=1)) %>%
  filter(Date >"2021-12-01") %>% 
  mutate(changes_month = (val_norm/last_month - 1)*100) %>% 
  select(Date,changes_month) %>% 
  rename(goods = changes_month)

gdp_services <- 
  gdp_industry %>% 
  filter(Seasonal_adjustment=="Seasonally adjusted at annual rates",
         Prices=="Chained (2012) dollars",
         North_American_Industry_Classification_System__NAICS_=="Service-producing industries [T003]") %>% 
  mutate(last_month=lag(val_norm,n=1)) %>%
  filter(Date >"2021-12-01") %>% 
  mutate(changes_month = (val_norm/last_month - 1)*100) %>% 
  select(Date,changes_month)  %>% 
  rename(services = changes_month)


gdp_datawrapper <- merge(x=gdp_all_industry,y=gdp_goods,by="Date")
gdp_datawrapper <- merge(x=gdp_datawrapper,y=gdp_services,by="Date")


write.csv(gdp_datawrapper, file = "gdp_datawrapper.csv")   

retail <-get_cansim("20-10-0008-01")

names(retail)<-str_replace_all(names(retail),
                                     c(" " = "_" , "," = "_", "[(]" ="_","[)]"="_"))


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
