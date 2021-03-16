library(rnoaa)
library(rnoaahelpers)
library(readr)

start_date <- as.Date("2000-01-01")
end_date <- as.Date("2021-01-14")
station_id <- "GHCND:USW00003904" ## COLLEGE STATION EASTERWOOD FIELD, TX US
data_type_id <- "PRCP"
token <- Sys.getenv("noaakey")

df <- download_ncdc(start_date, end_date, station_id, data_type_id, token, progress = TRUE)
df

write_csv(df, here::here("Data/noaa_precip/easterwood_precip.csv"))


start_date <- as.Date("2013-01-01")
end_date <- as.Date("2021-01-14")
station_id <- "GHCND:US1TXBZS088" ## BRYAN 3.5 NNW, TX US
data_type_id <- "PRCP"
token <- Sys.getenv("noaakey")

df <- download_ncdc(start_date, end_date, station_id, data_type_id, token, progress = TRUE)
df

write_csv(df, here::here("Data/noaa_precip/bryan3_5nnw_precip.csv"))


start_date <- as.Date("2000-01-01")
end_date <- as.Date("2021-01-14")
station_id <- "GHCND:USW00003904" ## COLLEGE STATION EASTERWOOD FIELD, TX US
data_type_id <- "TMAX"
token <- Sys.getenv("noaakey")

df <- download_ncdc(start_date, end_date, station_id, data_type_id, token, progress = TRUE)
df

write_csv(df, here::here("Data/noaa_precip/easterwood_tmax.csv"))