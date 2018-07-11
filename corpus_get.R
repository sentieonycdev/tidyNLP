library(tidyverse)
library(jsonlite)
library(tidyquant)

api_key="0e17c30b4fd5f6a39dae2718375b2885f902f18fc661c36fa79ec55369ba2b5c"

document8k_get <- function(x,api_key) {
  symbol<-tolower(x)
  #section<-tolower(section)
  #symbol<-"axp"
  section<-"news"
  s_url<-paste("https://app.sentieo.com/api/fetch_company_docs/?ticker=",symbol,"&start=0&sections=",section,"&size=30&apikey=",api_key,sep="")
  print(s_url)
  api_data <- try(fromJSON(s_url),silent=TRUE)
  df<-as.data.frame(api_data$result$news) %>%
    select(-highlights) %>%
    filter(grepl("Earnings",title))
  
  if(nrow(df)==0) {
    print(paste(symbol,"missing"))
    return()
  }
  
  df<-df %>% 
    mutate(date=mdy(filingdate)) %>%
    mutate(symbol=symbol) %>%
    select(symbol,date,title,id)
  
  #return(head(df,1))
}  

document_transcript_get <- function(x,api_key) {
  symbol<-tolower(x)
  #section<-tolower(section)
  #symbol<-"axp"
  section<-"transcripts"
  s_url<-paste("https://app.sentieo.com/api/fetch_company_docs/?ticker=",symbol,"&start=0&sections=",section,"&size=30&apikey=",api_key,sep="")
  print(s_url)
  api_data <- try(fromJSON(s_url),silent=TRUE)
  df<-as.data.frame(api_data$result$transcripts) %>%
    select(-highlights) %>%
    filter(grepl("Earnings",title))
  #df
  if(nrow(df)==0) {
    print(paste(symbol,"missing"))
    return()
  }
  
  df<-df %>% 
    mutate(date=mdy(filingdate)) %>%
    mutate(symbol=symbol) %>%
    select(symbol,date,title,id)
  #df
  #return(head(df,1))
}  


sp500<-tq_index("SP500")

safely_get_8k<-possibly(document8k_get,NULL)
safely_get_transcript<-possibly(document_transcript_get,NULL)


df_list<-map_df(sp500$symbol,safely_get_8k,api_key=api_key)
write_csv(as_data_frame(df_list),"sp500_earnings_8k_list.csv")

df_list<-map_df(sp500$symbol,safely_get_transcript,api_key=api_key)
write_csv(as_data_frame(df_list),"sp500_earnings_transcript_list.csv")
