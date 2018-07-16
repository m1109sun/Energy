library(dplyr)
library(tidyr)
library(lubridate)

# 설명 : id : 272개, date : 2016-01-01 00:00:00 ~ 2017-03-07 23:45:00, usage : 전력사용량

## date가 2016년인 것만 뽑은 후, 비어 있는 시간 채우기

energy_initial <- energy # 10772524개

energy_initial$date <- as.POSIXct(energy_initial$date)
energy_initial$year <- lubridate::year(energy_initial$date) # 2016 : 9127067개, 2017 : 1645457개

energy_initial <- energy_initial %>%
  arrange(id, date) %>%
  filter(year == "2016") %>%
  group_by(id) %>%
  tidyr::complete(date = seq.POSIXt(as.POSIXct("2016-01-01 00:00:00"), as.POSIXct("2016-12-31 23:45:00"), by = "15 min"))

table(is.na(energy_initial$usage)) # 430693개가 NA

# 각 id 당 35136개씩 있어야 하는데, id 별로 10000개 이상의 NA를 갖고 있는 애들은 삭제함(한 분기가 대략 11520 obs인데 한 분기가 NA이면 의미가 없다고 생각함). NA가 10000개 이상인 id가 14개이므로 14개 id를 제거함

NA_more_than10000 <- energy_initial %>% 
  group_by(id) %>%
  summarise(na_sum = sum(is.na(usage))) %>%
  filter(na_sum >= 10000)

remove_id <- NA_more_than10000$id

energy_initial <- energy_initial %>%
  filter(!id %in% remove_id)
energy_initial <- data.frame(energy_initial)

table(is.na(energy_initial$usage)) # 182564개가 NA

## get_time_kma function를 통해 2016년 날씨 변수 추가하기

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

kma_2016 <- get_time_kma("20160101", "20161231", "00", "23", "108", "Py9uJn2Iy3exjO25o2RFGHicZH6KFNQXbHjtVhC6D8d9Un6fL6Vt5nH6dK5Tb4c7")
kma_2016 <- kma_2016 %>%
  select("TM", "TA", "HM")
colnames(kma_2016) <- c("date", "temp", "humi")
kma_2016$date <- as.POSIXct(kma_2016$date)


## 15분 간격으로 값을 다 채우고 난 후, 평균을 냄

## Amelia  package 사용해보기

# Example

library(Amelia)
ex <- energy_initial %>%
  filter(id == "012746aa5a")
limit <- matrix(c(2, min(ex$usage, na.rm = TRUE), max(ex$usage, na.rm = TRUE)), nrow = 1, ncol = 3)
ex_amelia <- amelia(ex[,2:3], bounds = limit, ts = "date", splinetime = 3, polytime = 3, m = 5, tolerance = 0.0001)
compare.density(ex_amelia, "usage")

# 실제 energy_initial에 적용해보기

amd_id <- list()
energy_id <- unique(energy_initial$id)
for(i in 1:length(energy_id)){
  group <- energy_initial %>%
    filter(id == energy_id[i])
  limit <- matrix(c(2, min(group$usage, na.rm = TRUE), max(group$usage, na.rm = TRUE)), nrow = 1, ncol = 3)
  ame <- Amelia::amelia(group[,2:3], bounds = limit, ts = "date", splinetime = 3, polytime = 3, m = 5, tolerance= 0.0001)
  ame_mean <- list(ame[[1]]$imp1, ame[[1]]$imp2, ame[[1]]$imp3, ame[[1]]$imp4, ame[[1]]$imp5) %>%
    purrr::reduce(left_join, by = "date")
  ame_mean$mean_usage <- apply(ame_mean[,2:6], 1, mean)
  ame_mean <- ame_mean[,c(1,7)]
  amd_id[[i]] <- merge(group[,1:2], ame_mean, by = "date")
}

na_fill_energy <- data.table::rbindlist(amd_id)



## 15분 간격으로 돼있는거 1시간 단위로 평균 내기

mean_energy <- na_fill_energy %>% 
  group_by(id, date(na_fill_energy$date), hour(na_fill_energy$date)) %>% 
  summarise(mean = mean(mean_usage))
mean_energy <- data.frame(mean_energy)
colnames(mean_energy) <- c("id", "date", "hour", "usage")
mean_energy$time <- lubridate::ymd_h(paste(mean_energy$date, mean_energy$hour))
mean_energy <- mean_energy %>%
  select("id", "time", "usage")


# auto.arima는 Fit best ARIMA model to univariate time series
# dcast로 데이터 모양 바꿔주고 auto.arima 해보기
# 결과로는 ar_p가 1 : 46, 2 : 41, 3 : 62, 4 : 27, 5 : 82 이므로 따라서 ar(p)를 5로 선택함

mean_energy_dcast <- reshape2::dcast(mean_energy, time ~ id, value.var = "usage")

ar_order <- c()
for(i in 2:259){
  ar_id <- forecast::auto.arima(mean_energy_dcast[,i], d = 0, D = 0, max.q = 0)
  ar_order[[i]] <- arimaorder(ar_id)[1]
}

ar_p <- c(ar_order)
ar_p <- ar_p[-1]
ar_p_table <- data.frame(table(ar_p))
ggplot2::ggplot(ar_p_table, aes(ar_p, Freq, fill = ar_p)) + geom_bar(stat = "identity")

## ar(5)

### 정리된 코드 (converge가 안되므로 다른 방법을 사용해보자)
y_matrix <- t(as.matrix(mean_energy_dcast[,-1])) # row : id, column : usage

cov_mat <- diag(nrow(y_matrix))
x_matrix <- matrix(1, nrow(y_matrix), ncol(y_matrix))
ar_shift <- list()
lm_coef <- list()

cov_func <- function(initial, x_matrix, y_matrix, ar_order){
  lm_matrix <- lm(c(y_matrix) ~ c(x_matrix))
  for(l in 1:15){
    # residual
    lm_coef[[l]] <- coef(lm_matrix)[1]
    lm_residual <- stats::residuals(lm_matrix)
    res_matrix <- as.data.frame(matrix(lm_residual, nrow(y_matrix), byrow = FALSE))
    colnames(res_matrix) <- NULL
    cat(lm_coef[[l]], "\n")
    # AR
    t_res <- t(res_matrix) # 8784 * 258
    for(i in 1:(ar_order + 1)){
      ar_shift[[i]] <- c(t_res[i:(nrow(t_res) - ar_order + i - 1),])
    }
    ar_matrix <- data.frame(rlist::list.cbind(ar_shift))
    c_name <- colnames(ar_matrix)
    form <- paste0(c_name[ar_order + 1], " ~ ", paste0(c_name[-(ar_order+1)], collapse = " + "))
    e <- stats::residuals(lm(as.formula(form), data = ar_matrix))
    e_matrix <- matrix(e, nrow(res_matrix), byrow = TRUE) # 258 * 8779
    # cov matrix
    cov_mat <- e_matrix %*% t(e_matrix) / (ncol(y_matrix) - ar_order)
    initial <- cov_mat
    print(initial[1:6, 1:6])
    # chol %*% matrix
    half_chol <- t(solve(chol(initial)))
    y_matrix_up <- half_chol %*% y_matrix
    x_matrix_up <- half_chol %*% x_matrix
    lm_matrix <- lm(c(y_matrix_up) ~ c(x_matrix_up))
  }
}

cov_func(diag(258), x_matrix, y_matrix, 5)

