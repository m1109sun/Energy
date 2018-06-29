library(dplyr)
library(tidyr)
library(lubridate)

# 설명 : id : 272개, date : 2016-01-01 00:00:00 ~ 2017-03-07 23:45:00, usage : 전력사용량

## date가 2016년인 것만 뽑은 후, 비어 있는 시간 채우기

energy_initial <- energy

energy_initial$date <- as.POSIXct(energy_initial$date)

energy_initial <- energy_initial %>%
  arrange(id, date) %>%
  filter(lubridate::year(date) == 2016) %>%
  group_by(id) %>%
  tidyr::complete(date = seq.POSIXt(min(date), max(date), by = "15 min"))

table(is.na(energy_initial$usage)) # 430659개가 NA

# 각 id 당 35136개씩 있어야 하는데, id 별로 10000개 이상의 NA를 갖고 있는 애들은 삭제함(한 분기가 대략 11520 obs인데 한 분기가 NA이면 의미가 없다고 생각함). NA가 10000개 이상인 id가 14개이므로 14개 id를 제거함

NA_more_than10000 <- energy_initial %>% 
  group_by(id) %>%
  summarise(na_sum = sum(is.na(usage))) %>%
  filter(na_sum >= 10000)

remove_id <- NA_more_than10000$id

energy_initial <- energy_initial %>%
  filter(!id %in% remove_id)

table(is.na(energy_initial$usage)) # 182530개가 NA

