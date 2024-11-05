library(LightLogR)
#these packages are needed for the examples as shown below.
library(flextable)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtsummary)
library(lubridate)
library(clipr)




get_fileName_Date <- function(files){
  file_date <- c()
  part_id <- c()
  for (file in files){
    file_name <- basename(file)
    temp = unlist(strsplit(file_name,"_"))
    file_date <- c(file_date, substr(tail(temp, n=1), 1, 8))
    part_id <- c(part_id, temp[1])
  }
  df <- data.frame(part_id, file_date, files)
  colnames(df) <- c("id","date", "file")
  return(df)
}

get_Data <- function(files){
  files$date <- as.Date(files$date, format = "%Y%m%d")
  files <- files[order(files$date),]
  data <- read.csv(files$file[1], skip = 32, sep = ';')
  files_2 = files$file[2:nrow(files)]
  for (file in files_2){
    data <- rbind(data, read.csv(file, skip = 32, sep = ';'))
  }
  
  data <- data[!duplicated(data), ]
  return(data)
}

find_missing_time <- function(data, participant_id){
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
  valid_date = 1
  
  while(i < length(time_vec) - 1) {
    time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
    print(i)
    con_missingData = time_diff > 60 & year(time_vec[i+1]) == 2024 & year(time_vec[i]) == 2024
    con_falseDate = time_diff < 0 & year(time_vec[i + 1]) != 2024
    if(con_missingData){
      id <- c(id, participant_id)
      start_time <- c(start_time, data$DATE.TIME[i])
      end_time <- c(end_time, data$DATE.TIME[i + 1])
      reason <- c(reason, "missing data")
      duration <- c(duration, capture.output(time_vec[i+1]- time_vec[i]))
      time_diff = as.numeric(time_vec[i+1]- time_vec[i], units = "secs")
      dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
      res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
      dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
      dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
      
      i <- i+1
    }
    else if(con_falseDate){
      id <- c(id, participant_id)
      start_time <- c(start_time, data$DATE.TIME[i])
      duration_start <- time_vec[i]
      while(year(time_vec[i + 1]) != 2024 & i < length(time_vec))
      {
        i<-i+1
      }
      end_time <- c(end_time, data$DATE.TIME[i + 1])
      reason <- c(reason, "false date 2000")
      duration <- c(duration, capture.output(time_vec[i+1]- duration_start))
      time_diff = as.numeric(time_vec[i+1]- duration_start, units = "secs")
      dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
      res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
      dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
      dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
      i<-i+1
    }
    else{
      id <- c(id, participant_id)
      start_time <- c(start_time, data$DATE.TIME[i])
      duration_start <- time_vec[i]
      while((!con_falseDate) & (!con_missingData) & i < length(time_vec)){
        i<-i+1
        time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
        con_missingData = time_diff > 60 & year(time_vec[i+1]) == 2024 & year(time_vec[i]) == 2024
        con_falseDate = time_diff < 0 & year(time_vec[i + 1]) != 2024
      }
      end_time <- c(end_time, data$DATE.TIME[i])
      reason <- c(reason, "data present")
      time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
      duration <- c(duration, capture.output(time_vec[i]- duration_start))
      dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
      res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
      dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
      dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
    }
  }
  
  missing_time <- data.frame(id, start_time, end_time, reason, duration, dur_days, dur_hours, dur_minutes)
  return(missing_time)
}


path <- "D:/Thesis/all_files/"
files <- list.files(path, full.names = TRUE)

fileName_date <- get_fileName_Date(files)
unique_id <- sort(unique(fileName_date$id))

missing_data <- c()
for(id in unique_id)
{
  data <- get_Data(fileName_date[fileName_date$id == id,])
  missing_data <- rbind(missing_data, find_missing_time(data, id))
}
colnames(missing_data) <- c("id","start_time", "end_time", "missing_data", "duration", "duration_days", "duration_hours", "duration_minutes")
missing_data$duration <- gsub('Time difference of','',missing_data$duration)
missing_data$duration[is.na(missing_data$end_time)] <- NA
write_clip(missing_data)



