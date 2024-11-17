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
    if(con_missingData){
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
        while(year(time_vec[i + 1]) != 2024 & i < length(time_vec))
        {
          i<-i+1
        }
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
      id <- c(id, participant_id)
      start_time <- c(start_time, data$DATE.TIME[i])
      start_deviceId <- c(start_deviceId, data$device_id[i])
      duration_start <- time_vec[i]
      while((!con_falseDate) & (!con_missingData) & i < length(time_vec)){
        i<-i+1
        time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
        con_missingData = time_diff > 60 & year(time_vec[i+1]) == 2024 & year(time_vec[i]) == 2024
        con_falseDate = time_diff < 0 & year(time_vec[i + 1]) != 2024
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
  }
  
  missing_time <- data.frame(id, start_time, end_time, end_deviceId, reason, duration, dur_days, dur_hours, dur_minutes)
  return(missing_time)
}

generate_missing_months <- function(start_date, end_date, missing_data_value, id) {
  # Generate a sequence of months between start_date and end_date
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  months_in_range <- seq.Date(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "month")
  
  # Create a data frame for these months
  missing_months <- data.frame(
    id = rep(id, length(months_in_range)),
    month = month(months_in_range),
    missing_data = rep(missing_data_value, length(months_in_range)),
    stringsAsFactors = FALSE
  )
  return(missing_months)
}


path <- "F:/all_files/"
files <- list.files(path, full.names = TRUE)
fileName_date <- get_fileName_Date(files)
unique_id <- sort(unique(fileName_date$id))

missing_data <- c()

for(id in unique_id)
{
  data <- get_Data(fileName_date[fileName_date$id == id,])
  missing_data <- rbind(missing_data, find_missing_time(data, id))
}
colnames(missing_data) <- c("id","start_time", "end_time", "deviceId", "missing_data", "duration", "duration_days", "duration_hours", "duration_minutes")
missing_data$duration <- gsub('Time difference of','',missing_data$duration)
missing_data$duration[is.na(missing_data$end_time)] <- NA
write_clip(missing_data)


ggplot(na.omit(missing_data)) + geom_segment(aes(x=dmy_hms(start_time), y=id, xend=dmy_hms(end_time), yend=id, color = missing_data, linetype = missing_data, size = missing_data)) + 
scale_color_manual(values = c("missing data" = "red", "false date 2000" = "blue", "data present" = "green"))+
scale_size_manual(values = c("missing data" = 2.5, "false date 2000" = 2.5, "data present" = 1.5)) +  # Customize line width
scale_linetype_manual(values = c("missing data" = "solid", "false date 2000" = "solid", "data present" = "solid")) + 
theme_minimal()
ggsave("missing_data.png", bg = "white")



missing <- missing_data
missing_data <- missing_data %>%
  filter(complete.cases(.))
missing_data$start_time <- dmy_hms(missing_data$start_time)
missing_data$end_time <- dmy_hms(missing_data$end_time)

all_missing_months <- lapply(1:nrow(missing_data), function(i) {
  generate_missing_months(missing_data$start_time[i], missing_data$end_time[i], missing_data$missing_data[i], missing_data$id[i])
})
# Combine all the missing months data
missing_months_data <- do.call(rbind, all_missing_months)

# Add a column for duration calculations (optional)
missing_months_data <- missing_months_data %>%
  group_by(id, month) %>%
  mutate(percentage = ifelse(missing_data == "data present", 0, 100)) %>%
  ungroup()

# Now calculate percentage of missing data by id and month
missing_data_percentage <- missing_months_data %>%
  group_by(id, month, missing_data) %>%
  count() %>%
  group_by(id, month) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()


ggplot(missing_data_percentage, aes(x = factor(month), y = percentage, fill = missing_data)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ id, scales = "free", strip.position = "top") +
  labs(title = "Percentage of Missing Data by ID and Month",
       x = "Month", y = "Percentage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +  # Optional: change color palette
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10))
ggsave("percetage_plot.png", bg = "white")
