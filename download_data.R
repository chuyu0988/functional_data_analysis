'''
空氣資料網址:https://history.colife.org.tw/?cd=%E7%A9%BA%E6%B0%A3%E5%93%81%E8%B3%AA/%E4%B8%AD%E7%A0%94%E9%99%A2_%E7%A9%BA%E5%93%81%E9%A0%90%E5%A0%B1%E6%A8%A1%E6%93%AC%E8%B3%87%E6%96%99#/
需要輸入網頁內的資訊 資訊在網頁的"network"的"?r=/getdir"(F12內)
'''
library(dplyr)
library(readr)
library(tidyr)
library(curl)
library(jsonlite)
library(stringr)
library(base64enc)

#輸入網頁資料
url <- ""
referer_value <- ""
csrf_token <- ""
cookie_value <- ""

h <- new_handle()
handle_setheaders(h,
  'Accept' = '',
  'Accept-Encoding' = '',
  'Accept-Language' = '',
  'Connection' = '',
  'Content-Type' = '',
  'Origin' = '',
  'Referer' = referer_value,
  'User-Agent' = '',
  'sec-ch-ua' = '',
  'sec-ch-ua-mobile' = '',
  'sec-ch-ua-platform' = '',
  'Sec-Fetch-Dest' = '',
  'Sec-Fetch-Mode' = '',
  'Sec-Fetch-Site' = '',
  'x-csrf-token' = csrf_token,
  'Cookie' = cookie_value
)
handle_setopt(h, postfields = '{}')

#發送 API request
conn <- curl_fetch_memory(url, handle = h)
result <- fromJSON(rawToChar(conn$content))

#解析出檔案清單
files <- result$data$files

# 先取 type 為 file
target_files <- files[files$type == "file", ]

# 再取出符合 "MOENV_OD_數字.zip" 格式的檔名
target_files <- target_files[grepl("^MOENV_OD_\\d+\\.zip$", target_files$name), ]
paths <- target_files$path

#產生下載網址
zip_url <- sapply(paths, function(path) {
  base64_path <- base64encode(charToRaw(path))
  url_encoded_path <- URLencode(base64_path, reserved = TRUE)
  paste0("https://history.colife.org.tw/?r=/download&path=", url_encoded_path)
})

#印出網址
for (url in zip_url) {
  cat(url, "\n")
}

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

df_list <- list()
station<-read.csv("c:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\station .csv")
site_list <- station$SITE_NAME
for (i in seq_along(zip_url)) {
    download_zip <- tempfile(fileext = ".zip")
    unzip_dir<-tempfile()
    download.file(zip_url[i], destfile = download_zip, mode = "wb")
    unzip(download_zip,exdir=unzip_dir)
    inner_folders <- list.dirs(unzip_dir, full.names = TRUE, recursive = FALSE)
    #判斷有沒有FOLDER
    if (length(inner_folders) > 0) {
        data_dir <- inner_folders[1]
       } 
        else {
            data_dir <- unzip_dir
            }
    #判斷有沒有多個zip
    inner_zips <- list.files(data_dir, pattern = "\\.zip$", full.names = TRUE)
    if (length(inner_zips) > 0) {
        for (j in seq_along(inner_zips)) {
            inner_unzip_dir <- tempfile()
            unzip(inner_zips[j], exdir = inner_unzip_dir)
            csv_files <- list.files(inner_unzip_dir, pattern = "\\.csv$", full.names = TRUE)
            for(jk in csv_files){
                tryCatch({
                    df <- read.csv(jk) %>% select(any_of(c("PublishTime", "SiteName", "O3", "O3_8hr", "PM10", "PM2.5")))
                    if (ncol(df) == 0) { 
                        cat("該CSV找不到任何欄位: ", jk, "\n")
                        } else {
                    df$O3 <- as.numeric(df$O3)
                    df$O3_8hr <- as.numeric(df$O3_8hr)
                    df$PM10 <- as.numeric(df$PM10)
                    df$PM2.5 <- as.numeric(df$PM2.5)
                    filtered_df <- df %>% filter(SiteName %in% site_list)
                    df_list[[length(df_list) + 1]] <- filtered_df
                        }
            }, error = function(e) {
                cat("內層讀取錯誤：", jk, "\n")
            })
            }
        }
    }
    else{
        csv_files <- list.files(data_dir ,pattern = "\\.csv$", full.names = TRUE)

        for(csv in csv_files){
            tryCatch({
                df<- read.csv(csv) %>% select(any_of(c("PublishTime", "SiteName", "O3", "O3_8hr", "PM10", "PM2.5")))
                if (ncol(df) == 0) { 
                        cat("該CSV找不到任何欄位: ", jk, "\n")
                        } else {
                df$O3 <- as.numeric(df$O3)
                df$O3_8hr <- as.numeric(df$O3_8hr)
                df$PM10 <- as.numeric(df$PM10)
                df$PM2.5 <- as.numeric(df$PM2.5)
                filtered_df <- df %>% filter(SiteName %in% site_list)
                df_list[[length(df_list) + 1]] <- filtered_df
                        }
                }, error = function(e) {
                     cat("外層讀取錯誤檔案：", csv, "\n")
                     })
        }
    }
}
full_df <- bind_rows(df_list)
o3_R <- full_df %>%
select(PublishTime, SiteName, O3) %>%
pivot_wider(names_from = SiteName, values_from = O3,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(o3_R, file = "C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\O3.RData")

O3_8hr_R <- full_df %>%
select(PublishTime, SiteName, O3_8hr) %>%
pivot_wider(names_from = SiteName, values_from = O3_8hr,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(O3_8hr_R, file = "C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\O3_8hr.RData")

PM10_R <- full_df %>%
select(PublishTime, SiteName, PM10) %>%
pivot_wider(names_from = SiteName, values_from = PM10,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(PM10_R, file = "C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\PM10.RData")

PM2_5_R <- full_df %>%
select(PublishTime, SiteName, PM2.5) %>%
pivot_wider(names_from = SiteName, values_from = PM2.5,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(PM2_5_R, file = "C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\PM2_5.RData")


load("C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\O3.RData")
load("C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\O3_8hr.RData")
load("C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\PM10.RData")
load("C:\\Users\\user\\Desktop\\HW-NCHU\\meeting\\air_data\\Rdata\\PM2_5.RData")

tail(o3_R, 5)
tail(O3_8hr_R, 5)
tail(PM10_R, 5)
tail(PM2_5_R, 5)

