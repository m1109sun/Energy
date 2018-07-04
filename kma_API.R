library(httr)
library(curl)
library(jsonlite)
library(dplyr)

# start_d : 불러올 시작일 ex) 20100101
# end_d : 불러올 종료일 ex) 20100102
# start_h : 불러올 시작 시간 ex) 00
# end_h : 불러올 종료 시간 ex) 23
# location : 지점 ex) 108(서울)
# personal_key : 개인 API key
# sample url : http://data.kma.go.kr/apiData/getData?type=xml&dataCd=ASOS&dateCd=HR&startDt=20100101&startHh=09&endDt=20100102&endHh=04&stnIds=108&schListCnt=10&pageIndex=1&apiKey=사용자api키

# 시간 자료 불러오기

get_time_kma <- function(start_d, end_d, start_h, end_h, location, personal_key){
  
  # 시간을 한달 간격으로 묶기(왜냐하면 최대로 뽑을 수 있는 data가 999개니까)
  date_dat <- data.frame(date = seq.Date(lubridate::ymd(start_d), lubridate::ymd(end_d), by = "day"))
  date_dat$year <- lubridate::year(date_dat$date)
  date_dat$month <- lubridate::month(date_dat$date)
  date_group <- date_dat %>%
    group_by(year, month) %>% 
    summarise(min_date = format(min(date), "%Y%m%d"), max_date = format(max(date), "%Y%m%d"))
  weather_info <- list()
  
  # for문
  for(i in 1:nrow(date_group)){
    # url_sub
    url_sub <- "http://data.kma.go.kr/apiData/getData?type=json&dataCd=ASOS&dateCd=HR&schListCnt=999&pageIndex=1"
    
    # 뽑아올 날짜 & 시간 & 장소
    startDt <- "&startDt="
    start_d <- date_group[i,3]
    endDt <- "&endDt="
    end_d <- date_group[i,4]
    startHh <- "&startHh="
    endHh <- "&endHh="
    stnIds <- "&stnIds="
    
    # API Key
    apiKey <- "&apiKey="
    
    # url
    url <- paste0(url_sub, startDt, start_d, endDt, end_d, startHh, start_h, endHh, end_h, stnIds, location, apiKey, personal_key)
    
    # R로 불러오기
    result <- httr::GET(url)
    json <- httr::content(result , as = "text")
    processed_json <- jsonlite::fromJSON(json)
    
    weather_info[[i]] <- processed_json$info[[4]]
  }
  weather_information <- data.table::rbindlist(weather_info, fill = TRUE)
  weather_information
}

Example <- get_time_kma("20160101", "20160203", "01", "05", "108", "personal_key")


# 네이버 Open API 이용 (기상청과는 무관)

library(httr)
library(curl)
library(jsonlite)
library(stringr)

query <- "명륜동"
enc_query <- URLencode(query)
url <- "https://openapi.naver.com/v1/map/geocode"
url_fed_to_get <- paste0(url, "?query=", enc_query)
query_result <- GET(url_fed_to_get, add_headers("X-Naver-Client-Id" = "personal_id",
                                                "X-Naver-Client-Secret" = "personal_secret"))
enc_query
URLdecode(enc_query)

json <-  content(query_result , as = "text")
processed_json <- fromJSON(json)

str(processed_json)
processed_json$result$items

str_trim(processed_json$result$items$address)



