library(LightLogR)
#these packages are needed for the examples as shown below.
library(flextable)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtsummary)
library(lubridate)
library(clipr)


path <- "D:/Areej/Thesis/112/files/"
files <- list.files(path, full.names = TRUE)
#show how many files are listes
length(files)

file_date <- c()
for (file in files){
  file_name <- basename(file)
  temp = unlist(strsplit(file_name,"_"))
  file_date <- c(file_date, substr(temp[4], 1, 8))
}
file_date <- as.Date(file_date, format = "%Y%m%d")
files <- files[order(file_date)]

data <- read.csv(files[1], skip = 32, sep = ';')
files_2 = files[2:length(files)]
for (file in files_2){
  data <- rbind(data, read.csv(file, skip = 32, sep = ';'))
}

data <- data[!duplicated(data), ]

time_vec <- dmy_hms(data$DATE.TIME)

start_time <- c()
end_time <- c()
reason <- c()
id <- c()
duration <- c()
dur_days <- c()
dur_hours <- c()
dur_minutes <- c()
i = 1

while(i < length(time_vec) - 1) {
  time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
  print(i)
  if(time_diff > 60 & year(time_vec[i+1]) == 2024 & year(time_vec[i]) == 2024){
    id <- c(id, temp[1])
    start_time <- c(start_time, data$DATE.TIME[i])
    end_time <- c(end_time, data$DATE.TIME[i + 1])
    reason <- c(reason, "missing data")
    #duration <- c(duration, capture.output(time_vec[i+1]- time_vec[i]))
    time_diff = as.numeric(time_vec[i+1]- time_vec[i], units = "secs")
    dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
    res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
    dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
    dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
    
    i <- i+1
  }
  else if(time_diff < 0 & year(time_vec[i + 1]) != 2024){
    id <- c(id, temp[1])
    start_time <- c(start_time, data$DATE.TIME[i])
    duration_start <- time_vec[i]
    print(i)
    print(data$DATE.TIME[i])
    while(year(time_vec[i + 1]) != 2024 & i < length(time_vec))
    {
      i<-i+1
    }
    end_time <- c(end_time, data$DATE.TIME[i + 1])
    reason <- c(reason, "false date 2000")
    #duration <- c(duration, capture.output(time_vec[i+1]- duration_start))
    time_diff = as.numeric(time_vec[i+1]- duration_start, units = "secs")
    dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
    res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
    dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
    dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
    i<-i+1
  }
  else{
    i<-i+1
  }
}

missing_time <- data.frame(id, start_time, end_time, reason, dur_days, dur_hours, dur_minutes)
write_clip(missing_time)



