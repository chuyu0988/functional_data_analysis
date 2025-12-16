# You need to input the information that is within the webpage under the 'network' tab, specifically the content of the ?r=/getdir request (found in the F12 Developer Tools).
library(dplyr)
library(readr)
library(tidyr)
library(curl)
library(jsonlite)
library(stringr)
library(base64enc)
library(lubridate)

# Input the web data from the F12 Developer Tools.
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

# Send an API request
conn <- curl_fetch_memory(url, handle = h)
result <- fromJSON(rawToChar(conn$content))

# List all URLs
files <- result$data$files
target_files <- files[files$type == "file", ]
# Extract the filenames that match the $\text{'MOENV\_OD\_[number].zip'}$ format.
target_files <- target_files[grepl("^MOENV_OD_\\d+\\.zip$", target_files$name), ]
paths <- target_files$path
zip_url <- sapply(paths, function(path) {
  base64_path <- base64encode(charToRaw(path))
  url_encoded_path <- URLencode(base64_path, reserved = TRUE)
  paste0("https://history.colife.org.tw/?r=/download&path=", url_encoded_path)
})
for (url in zip_url) {
  cat(url, "\n")
}

df_list <- list()
station<-read.csv("station .csv")
site_list <- station$SITE_NAME
for (i in seq_along(zip_url)) {
    download_zip <- tempfile(fileext = ".zip")
    unzip_dir<-tempfile()
    download.file(zip_url[i], destfile = download_zip, mode = "wb")
    unzip(download_zip,exdir=unzip_dir)
    inner_folders <- list.dirs(unzip_dir, full.names = TRUE, recursive = FALSE)
    # Determine if a folder exists
    if (length(inner_folders) > 0) {
        data_dir <- inner_folders[1]
       } 
        else {
            data_dir <- unzip_dir
            }
    # Determine if there are multiple zip files
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
                        cat("CSV找不到任何欄位: ", jk, "\n")
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
                        cat("CSV找不到任何欄位: ", jk, "\n")
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
save(o3_R, file = "O3.RData")

O3_8hr_R <- full_df %>%
select(PublishTime, SiteName, O3_8hr) %>%
pivot_wider(names_from = SiteName, values_from = O3_8hr,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(O3_8hr_R, file = "O3_8hr.RData")

PM10_R <- full_df %>%
select(PublishTime, SiteName, PM10) %>%
pivot_wider(names_from = SiteName, values_from = PM10,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(PM10_R, file = "PM10.RData")

PM2_5_R <- full_df %>%
select(PublishTime, SiteName, PM2.5) %>%
pivot_wider(names_from = SiteName, values_from = PM2.5,values_fn = mean)%>%
mutate(PublishTime = ymd_hms(PublishTime)) %>%
arrange(PublishTime)
save(PM2_5_R, file = "PM2_5.RData")


