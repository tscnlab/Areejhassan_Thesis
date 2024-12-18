library(LightLogR)
#these packages are needed for the examples as shown below.
library(flextable)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtsummary)
library(lubridate)
library(clipr)
library(ggplot2)
library(tidyr)




get_fileName_Date <- function(files){
  file_date <- c()
  part_id <- c()
  log_id <- c()
  for (file in files){
    file_name <- basename(file)
    temp = unlist(strsplit(file_name,"_"))
    file_date <- c(file_date, substr(tail(temp, n=1), 1, 8))
    part_id <- c(part_id, temp[1])
    log_id <- c(log_id, temp[3])
  }
  df <- data.frame(part_id, log_id, file_date, files)
  colnames(df) <- c("id","device_id", "date", "file")
  return(df)
}

get_Data <- function(files){
  files$date <- as.Date(files$date, format = "%Y%m%d")
  files <- files[order(files$date),]
  data <- read.csv(files$file[1], skip = 32, sep = ';')
  data$device_id <- files$device_id[1]
  files_2 = files[2:nrow(files),]
  for(i in 1:nrow(files_2)){
    file <- files_2[i,]
    temp_data <- read.csv(file$file, skip = 32, sep = ';')
    temp_data$device_id <- file$device_id
    data <- rbind(data, temp_data)
  }
  
  data <- data[!duplicated(data[,-34]), ]
  return(data)
}

find_missing_time <- function(data, participant_id){
  time_vec <- dmy_hms(data$DATE.TIME)
  data$reason <- ""
  data$activity_diff <- c(0, diff(as.numeric(data$PIM)))
  start_time <- c()
  end_time <- c()
  start_deviceId <- c()
  end_deviceId <- c()
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
    con_falseDate = year(time_vec[i + 1]) != 2024
    con_inactivity = abs(data$activity_diff[i]) <= 0
    if(con_missingData){
      #data$reason[i] <- "missing"
      #data$reason[i+1] <- "missing"
      id <- c(id, participant_id)
      start_time <- c(start_time, data$DATE.TIME[i])
      start_deviceId <- c(start_deviceId, data$device_id[i])
      end_time <- c(end_time, data$DATE.TIME[i + 1])
      end_deviceId <- c(end_deviceId, data$device_id[i + 1])
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
      if(time_diff < 0 )
      {
        id <- c(id, participant_id)
        start_time <- c(start_time, data$DATE.TIME[i])
        start_deviceId <- c(start_deviceId, data$device_id[i])
        duration_start <- time_vec[i]
        #data$reason[i] <- "false"
        while(year(time_vec[i + 1]) != 2024 & i < length(time_vec))
        {
          i<-i+1
          #data$reason[i] <- "false"
        }
        #data$reason[i +1] <- "false"
        end_time <- c(end_time, data$DATE.TIME[i + 1])
        end_deviceId <- c(end_deviceId, data$device_id[i + 1])
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
        start_time <- NA
        start_deviceId <- c(start_deviceId, data$device_id[i])
        while(year(time_vec[i]) != 2024 & i < length(time_vec))
        {
          i<-i+1
        }
        end_time <- c(end_time, data$DATE.TIME[i])
        end_deviceId <- c(end_deviceId, data$device_id[i])
        reason <- c(reason, "false date 2000")
        duration <- c(duration, NA)
        dur_days <- c(dur_days, NA)
        dur_hours <- c(dur_hours, NA)
        dur_minutes <- c(dur_minutes, NA)
      }
    }
    else{
      if(!con_inactivity)
      {
        id <- c(id, participant_id)
        start_time <- c(start_time, data$DATE.TIME[i])
        #data$reason[i] <- "present"
        start_deviceId <- c(start_deviceId, data$device_id[i])
        duration_start <- time_vec[i]
        while((!con_falseDate) & (!con_missingData) & i < length(time_vec) & (!con_inactivity)){
          i<-i+1
          #data$reason[i] <- "present"
          time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
          con_missingData = time_diff > 60 & year(time_vec[i+1]) == 2024 & year(time_vec[i]) == 2024
          con_falseDate = time_diff < 0 & year(time_vec[i + 1]) != 2024
          con_inactivity = abs(data$activity_diff[i]) <= 0
        }
        end_time <- c(end_time, data$DATE.TIME[i])
        end_deviceId <- c(end_deviceId, data$device_id[i])
        reason <- c(reason, "data present")
        time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
        duration <- c(duration, capture.output(time_vec[i]- duration_start))
        dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
        res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
        dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
        dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
      }
      else{
        start_index <- i
        start <- data$DATE.TIME[i]
        duration_start <- time_vec[i]
        count <- 0
        while((!con_falseDate) & (!con_missingData) & i < length(time_vec) & (con_inactivity))
        {
          #print(paste("no activity happening at index:" ,i))
          i <- i + 1
          count <- count + 1
          time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
          con_missingData = time_diff > 60 & year(time_vec[i+1]) == 2024 & year(time_vec[i]) == 2024
          con_falseDate = time_diff < 0 & year(time_vec[i + 1]) != 2024
          con_inactivity = abs(data$activity_diff[i]) <= 0
        }
        if(count > 10)
        {
          #print("activity counter is more than 10")
          #data$reason[start_index:i] <- "inactive"
          start_time <- c(start_time, start)
          end_time <- c(end_time, data$DATE.TIME[i])
          id <- c(id, participant_id)
          end_deviceId <- c(end_deviceId, data$device_id[i])
          reason <- c(reason, "non-wearable")
          duration <- c(duration, capture.output(time_vec[i]- duration_start))
          time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
          dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
          res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
          dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
          dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
        }
        else{
          #data$reason[start_index:i] <- "present"
          start_time <- c(start_time, start)
          end_time <- c(end_time, data$DATE.TIME[i])
          id <- c(id, participant_id)
          end_deviceId <- c(end_deviceId, data$device_id[i])
          reason <- c(reason, "data present")
          duration <- c(duration, capture.output(time_vec[i]- duration_start))
          time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
          dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
          res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
          dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
          dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
        }
      }
    }
  }
  
  missing_time <- data.frame(id, start_time, end_time, end_deviceId, reason, duration, dur_days, dur_hours, dur_minutes)
  #return(list(missing_time = missing_time, data = data))
  return(missing_time)
}

calc_inactivity <- function(data_Id, participant_id) {
  #data_Id <- data_Id[data_Id$reason == "present",]
  time_vec <- dmy_hms(data_Id$DATE.TIME)
  data_Id$activity_diff <- c(0, diff(as.numeric(data_Id$PIM)))
  #data_Id$reason <- ""
  i <- 1
  count <- 0
  
  start_time <- c()
  end_time <- c()
  end_deviceId <- c()
  reason <- c()
  id <- c()
  duration <- c()
  dur_days <- c()
  dur_hours <- c()
  dur_minutes <- c()
  
  while(i < nrow(data_Id) - 1){
    print(i)
    if(abs(data_Id[i,]$activity_diff) <= 0)
    {
      print("inside if loop")
      start_index <- i
      start <- data_Id$DATE.TIME[i]
      duration_start <- time_vec[i]
      while(abs(data_Id[i,]$activity_diff) <= 0 )
      {
        print(paste("no activity happening at index:" ,i))
        i <- i + 1
        count <- count + 1
      }
      if(count > 10)
      {
        print("activity counter is more than 10")
        data_Id$reason[start_index:i] <- "inactive"
        start_time <- c(start_time, start)
        end_time <- c(end_time, data_Id$DATE.TIME[i])
        id <- c(id, participant_id)
        end_deviceId <- c(end_deviceId, data_Id$device_id[i])
        reason <- c(reason, "non-wearable")
        duration <- c(duration, capture.output(time_vec[i]- duration_start))
        time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
        dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
        res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
        dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
        dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
        count <- 0
      }
      else{
        print("activity counter is less than 10")
        data_Id$reason[start_index:i] <- "present"
        start_time <- c(start_time, start)
        end_time <- c(end_time, data_Id$DATE.TIME[i])
        id <- c(id, participant_id)
        end_deviceId <- c(end_deviceId, data_Id$device_id[i])
        reason <- c(reason, "present")
        duration <- c(duration, capture.output(time_vec[i]- duration_start))
        time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
        dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
        res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
        dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
        dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
        count <- 0
      }
    }
    else{
      print(paste("activity happening at index:" ,i))
      start_index <- i
      start <- data_Id$DATE.TIME[i]
      duration_start <- time_vec[i]
      while(abs(data_Id[i,]$activity_diff) > 0 )
      {
        print(paste("no activity happening at index:" ,i))
        i <- i + 1
      }
      data_Id$reason[start_index:i] <- "present"
      start_time <- c(start_time, start)
      end_time <- c(end_time, data_Id$DATE.TIME[i])
      id <- c(id, participant_id)
      end_deviceId <- c(end_deviceId, data_Id$device_id[i])
      reason <- c(reason, "present")
      duration <- c(duration, capture.output(time_vec[i]- duration_start))
      time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
      dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
      res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
      dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
      dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
    }
  }
  missing_activity <- data.frame(id, start_time, end_time, end_deviceId, reason, duration, dur_days, dur_hours, dur_minutes)
  return(missing_activity)
}

generate_missing_months <- function(missing) {
  # Generate a sequence of months between start_date and end_date
  start_date <- missing$start_time
  end_date <- missing$end_time
  months_in_range <- seq.POSIXt(start_date, end_date, by = "month")
  missing_months <- c()
  if(length(months_in_range) <= 1){
    missing_months <- missing
  }
  else{
    for(i in 1:(length(months_in_range) - 1))
    {
      temp <- missing
      temp$start_time <- months_in_range[i]
      temp$end_time <- months_in_range[i + 1]
      missing_months <- rbind(missing_months, temp)
    }
  }
  return(missing_months)
}

path <- "F:/all_files/"
files <- list.files(path, full.names = TRUE)
fileName_date <- get_fileName_Date(files)
unique_id <- sort(unique(fileName_date$id))

missing_data <- c()
inactive_data <- c()

for(id in unique_id)
{
  data <- get_Data(fileName_date[fileName_date$id == id,])
  missing_id <- find_missing_time(data, id)
  missing_data <- rbind(missing_data, missing_id)
}

colnames(missing_data) <- c("id","start_time", "end_time", "deviceId", "missing_data", "duration", "duration_days", "duration_hours", "duration_minutes")
missing_data$duration <- gsub('Time difference of','',missing_data$duration)
missing_data$duration[is.na(missing_data$end_time)] <- NA
write_clip(missing_data)


ggplot(na.omit(missing_data)) + geom_segment(aes(x=dmy_hms(start_time), y=id, xend=dmy_hms(end_time), yend=id, color = missing_data, linetype = missing_data, size = missing_data)) + 
  scale_color_manual(values = c("missing data" = "#E57373", "false date 2000" = "#f7d6d6", "data present" = "#81C784", "non-wearable" = "#eef7ee"))+
  scale_size_manual(values = c("missing data" = 2.5, "false date 2000" = 2.5, "data present" = 3.5, "non-wearable" = 2.5)) +  # Customize line width
  scale_linetype_manual(values = c("missing data" = "solid", "false date 2000" = "solid", "data present" = "solid", "non-wearable" = "solid")) + 
  theme_minimal()
ggsave("missing_data.png", bg = "white")


missing <- missing_data
missing$start_time <- as.POSIXct(missing$start_time, format="%d/%m/%Y %H:%M:%S")
missing$end_time <- as.POSIXct(missing$end_time, format="%d/%m/%Y %H:%M:%S")
missing <- missing %>%
  filter(complete.cases(.))
all_missing_months <- lapply(1:nrow(missing), function(i) {
  print(i)
  generate_missing_months(missing[i,])
})

# Combine all the missing months data
missing <- do.call(rbind, all_missing_months)

# Extract the month and year from start_time to group by month
missing$month <- month(missing$start_time)

# Calculate the number of missing data points per month
missing_data_monthly <- missing %>%
  mutate(missing_flag = ifelse(missing_data == "missing data", end_time - start_time, 0)) %>%
  mutate(present_flag = ifelse(missing_data == "data present", end_time - start_time, 0)) %>%
  mutate(inactivity_flag = ifelse(missing_data == "non-wearable", end_time - start_time, 0)) %>%
  mutate(false_flag = ifelse(missing_data == "false date 2000", end_time - start_time, 0)) %>%
  group_by(month, id) %>%
  summarise(
    missing_data = sum(missing_flag),
    data_present = sum(present_flag),
    non_wearable = sum(inactivity_flag),
    false_date = sum(false_flag)
  )

df_long <- missing_data_monthly %>%
  pivot_longer(cols = c(missing_data, data_present, non_wearable, false_date),
               names_to = "missing_data", values_to = "value") %>%
  # Calculate the total for each month
  group_by(month, id) %>%
  mutate(total_entries = sum(value)) %>%
  ungroup() %>%
  # Calculate the percentage for each entry type
  mutate(percentage = (value / total_entries) * 100)

ggplot(df_long, aes(x = as.factor(month), y = percentage, fill = missing_data)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black") +  # Add a dashed line at 50%
  facet_wrap(~ id) +  # Facet by ID
  labs(
    title = "Percentage of Missing and Non-Missing light data by Month and ID",
    x = "Month",
    y = "Percentage (%)"
  ) +
  scale_fill_manual(
    values = c("missing_data" = "#E57373", "false_date" = "#f7d6d6","data_present" = "#81C784", "non_wearable" = "#eef7ee"),  # Shades of red and green
    labels = c("missing_data" = "missing","false_date" = "false date" ,"data_present" = "non missing", "non_wearable" = "inactive")) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )
ggsave("percetage_plot.png", bg = "white")
