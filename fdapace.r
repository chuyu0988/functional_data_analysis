#install.packages("fdapace")
library(tidyr)
library(dplyr)
library(lubridate)
library(fdapace)
library(refund)

data <- read.csv('C:/Users/user/Desktop/HW-NCHU/meeting/PM2.5.csv',fileEncoding = "UTF-8")

station <- "忠明"

cleaned_data <- data %>%
    mutate(PublishTime = ymd_hms(PublishTime)) %>%
    filter(year(PublishTime) >= 2020 & year(PublishTime) <= 2024) %>%
    dplyr::select(PublishTime, all_of(station)) %>%
    mutate(date = as.Date(PublishTime)) %>%
    arrange(PublishTime) %>%
    mutate(SubjectID = as.numeric(factor(date))) %>%
    mutate(Time = hour(PublishTime)) %>%
    dplyr::select(SubjectID, date, Time, all_of(station)) %>%
    drop_na()

unique_days <- unique(cleaned_data$SubjectID)
n_days <- length(unique_days)
n_observations <- rep(0, n_days)

for (i in 1:n_days) {
    day_data <- cleaned_data %>% filter(SubjectID == unique_days[i])
    n_observations[i] <- nrow(day_data)
}

# Count observations per subject
obs_counts <- cleaned_data %>%
group_by(SubjectID) %>%
summarise(n_obs = n())

# Filter subjects with at least 5 observations
valid_subjects <- obs_counts %>%
filter(n_obs >= 5) %>%
pull(SubjectID)


# Filter data
cleaned_data_filtered <- cleaned_data %>%
filter(SubjectID %in% valid_subjects)

"""
> head(cleaned_data_filtered, 5)
  SubjectID       date Time 忠明
1         1 2020-01-01    0    8
2         1 2020-01-01    1   15
3         1 2020-01-01    2    7
4         1 2020-01-01    3    5
5         1 2020-01-01    4   10
"""

first_day_2024_id <- cleaned_data_filtered %>%
filter(year(date) == 2024) %>%
slice(1) %>%
pull(SubjectID)

train_data <- cleaned_data_filtered %>%
    filter(SubjectID < first_day_2024_id)

test_data <- cleaned_data_filtered %>%
    filter(SubjectID >= first_day_2024_id)

train_Ly <- train_data %>%
    group_by(SubjectID) %>%
    summarise(y = list(.data[[station]])) %>%
    pull(y)

"""
> head(train_Ly, 3)
[[1]]
 [1]  8 15  7  5 10  8  9 10 10 11  6

[[2]]
[1] 21 14 21 21 21

[[3]]
[1] 56 64 62 63 58 57 60

"""

train_Lt <- train_data %>%
    group_by(SubjectID) %>%
    summarise(t = list(Time)) %>%
    pull(t)

"""
> head(train_Lt, 3)
[[1]]
 [1]  0  1  2  3  4  5  6  7  8  9 17

[[2]]
[1] 12 13 14 22 23

[[3]]
[1] 2 3 4 5 6 7 8
"""

test_Ly <- test_data %>%
    group_by(SubjectID) %>%
    summarise(y = list(.data[[station]])) %>%
    pull(y)

test_Lt <- test_data %>%
    group_by(SubjectID) %>%
    summarise(t = list(Time)) %>%
    pull(t)

fpca_train <- FPCA(
    Ly = train_Ly,
    Lt = train_Lt,
    optns = list(dataType = 'Sparse', FVEthreshold = 0.99, nRegGrid = 24)
)

pm25_train_smooth <- fitted(fpca_train)
pred_result <- predict(fpca_train,newLy = test_Ly,newLt = test_Lt)

scores <- pred_result$scores
mu <- fpca_train$mu
phi <- fpca_train$phi

pm25_test_smooth <- matrix(mu, nrow = nrow(scores), ncol = length(mu), byrow = TRUE) +
                        scores %*% t(phi[, 1:ncol(scores)])

X_train <- pm25_train_smooth[1:(nrow(pm25_train_smooth) - 1), ]
Y_train <- pm25_train_smooth[2:nrow(pm25_train_smooth), ]

X_test <- pm25_test_smooth[1:(nrow(pm25_test_smooth) - 1), ]
Y_test <- pm25_test_smooth[2:nrow(pm25_test_smooth), ]

test_subject_ids <- unique(test_data$SubjectID)
test_subject_ids_for_y <- test_subject_ids[2:length(test_subject_ids)]

n_test <- length(test_subject_ids_for_y)
Y_test_true <- matrix(NA, nrow = n_test, ncol = 24)

test_data_filtered <- test_data %>%
    filter(SubjectID %in% test_subject_ids_for_y) %>%
    mutate(
        row_idx = match(SubjectID, test_subject_ids_for_y),
        col_idx = Time + 1
    )

Y_test_true[cbind(test_data_filtered$row_idx, 
                    test_data_filtered$col_idx)] <- test_data_filtered[[station]]


time_grid <- 0:23
#PFFR
pffr_data <- data.frame(
Y = I(Y_train),  # Response functional data
X = I(X_train)   # Predictor functional data
)
fit <- pffr(Y ~ ff(X, xind=time_grid), yind=time_grid, data=pffr_data)
pffr_data2 <- data.frame(
X = I(X_test)   # Predictor functional data
)
Y_pred <- predict(fit, pffr_data2)

MAE <- rowMeans(abs(Y_pred - Y_test_true), na.rm = TRUE)
RMSE <- sqrt(rowMeans((Y_pred - Y_test_true)^2, na.rm = TRUE))

print(c(mean(RMSE), sd(RMSE)))
print(c(mean(MAE), sd(MAE)))
"""
> print(c(mean(RMSE), sd(RMSE)))
[1] 5.865903 3.242908
> print(c(mean(MAE), sd(MAE)))
[1] 4.912986 2.863254
"""



