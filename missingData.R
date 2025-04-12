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
library(corrplot)
library(reshape2)




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
    print(file$file)
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
  #data$activity_diff <- c(0, diff(as.numeric(data$PIM)))
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
    time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "mins"))
    print(i)
    con_missingData <- time_diff > 60 & (year(time_vec[i + 1]) %in% c(2024, 2025)) & (year(time_vec[i]) %in% c(2024, 2025))
    con_falseDate = !(year(time_vec[i + 1]) %in% c(2024, 2025))
    con_inactivity = data$PIM[i] <= 0
    if(con_missingData){
      #data$reason[i] <- "missing"
      #data$reason[i+1] <- "missing"
      if(1){
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
      }

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
        while(!(year(time_vec[i + 1]) %in% c(2024, 2025)) & i < length(time_vec))
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
        start_time <- c(start_time, NA)
        start_deviceId <- c(start_deviceId, data$device_id[i])
        while(!(year(time_vec[i]) %in% c(2024, 2025)) & i < length(time_vec))
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
        data$reason[i] <- "present"
        start_deviceId <- c(start_deviceId, data$device_id[i])
        duration_start <- time_vec[i]
        while((!con_falseDate) & (!con_missingData) & i < length(time_vec) & (!con_inactivity)){
          i<-i+1
          data$reason[i] <- "present"
          time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "mins"))
          con_missingData <- time_diff > 60 & (year(time_vec[i + 1]) %in% c(2024, 2025)) & (year(time_vec[i]) %in% c(2024, 2025))
          con_falseDate = !(year(time_vec[i + 1]) %in% c(2024, 2025))
          con_inactivity = data$PIM[i] <= 0
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
          time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "mins"))
          con_missingData <- time_diff > 60 & (year(time_vec[i + 1]) %in% c(2024, 2025)) & (year(time_vec[i]) %in% c(2024, 2025))
          con_falseDate = !(year(time_vec[i + 1]) %in% c(2024, 2025))
          con_inactivity = data$PIM[i] <= 0
        }
        if(count > 240)
        {
          #print("activity counter is more than 10")
          #data$reason[start_index:i] <- "inactive"
          start_time <- c(start_time, start)
          end_time <- c(end_time, data$DATE.TIME[i])
          id <- c(id, participant_id)
          end_deviceId <- c(end_deviceId, data$device_id[i])
          reason <- c(reason, "non-wear")
          duration <- c(duration, capture.output(time_vec[i]- duration_start))
          time_diff = as.numeric(time_vec[i]- duration_start, units = "secs")
          dur_days <- c(dur_days, floor(time_diff / (24 * 3600)))
          res_hrs <- floor((time_diff %% (24 * 3600)) / 3600)
          dur_hours <- c(dur_hours, floor((time_diff %% (24 * 3600)) / 3600))
          dur_minutes <- c(dur_minutes, floor(((time_diff %% (24 * 3600)) / 3600 - res_hrs) * 60))
        }
        else{
          data$reason[start_index:i] <- "present"
          start_time <- c(start_time, start)
          #print(paste("start time is:", start_time))
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
  return(list(missing_time = missing_time, data = data))
  #return(missing_time)
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

path <- "F:/all_files_2025/"
files <- list.files(path, full.names = TRUE)
fileName_date <- get_fileName_Date(files)
unique_id <- 112#sort(unique(fileName_date$id))

missing_data <- c()
inactive_data <- c()
present_data <- c()

for(id in unique_id)
{
  data <- get_Data(fileName_date[fileName_date$id == id,])
  data$id <- id
  missing <- find_missing_time(data, id)
  missing_data <- rbind(missing_data, missing$missing_time)
  present_data <- rbind(present_data, missing$data)
  #present_data <- rbind(present_data, missing$data[missing$data$reason == "present",] )
}

colnames(missing_data) <- c("id","start_time", "end_time", "deviceId", "missing_data", "duration", "duration_days", "duration_hours", "duration_minutes")
missing_data$duration <- gsub('Time difference of','',missing_data$duration)
missing_data$duration[is.na(missing_data$end_time)] <- NA
write_clip(missing_data)

x11()
ggplot(na.omit(missing_data)) + geom_segment(aes(x=dmy_hms(start_time), y=id, xend=dmy_hms(end_time), yend=id, color = missing_data, linetype = missing_data, size = missing_data)) +
  scale_color_manual(values = c("missing data" = "#E57373", "false date 2000" = "#f7d6d6", "data present" = "#81C784", "non-wear" = "#eef7ee"))+
  scale_size_manual(values = c("missing data" = 2.5, "false date 2000" = 2.5, "data present" = 3.5, "non-wear" = 2.5)) +  # Customize line width
  scale_linetype_manual(values = c("missing data" = "solid", "false date 2000" = "solid", "data present" = "solid", "non-wear" = "solid")) +
  theme_minimal() +
  scale_x_datetime(breaks = date_breaks("1 month"), # Key change: Set breaks every month
                 labels = date_format("%b %Y"))  # Optional: Format date labels
ggsave("missing_data.png", bg = "white")




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
  mutate(present_flag = ifelse(missing_data == "data present", end_time - start_time, 0)) %>%
  mutate(inactivity_flag = ifelse(missing_data == "non-wear", end_time - start_time, 0)) %>%
  group_by(month, id) %>%
  summarise(
    data_present = sum(present_flag),
    non_wear = sum(inactivity_flag)
  )

df_long <- missing_data_monthly %>%
  pivot_longer(cols = c(data_present, non_wear),
               names_to = "missing_data", values_to = "value") %>%
  # Calculate the total for each month
  group_by(month, id) %>%
  mutate(total_entries = sum(value)) %>%
  ungroup() %>%
  # Calculate the percentage for each entry type
  mutate(percentage = (value / total_entries) * 100)

x11()
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
    values = c("data_present" = "#81C784", "non_wear" = "#E57373"),  # Shades of red and green
    labels = c("data_present" = "wear", "non_wear" = "non-wear")) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )
ggsave("percetage_plot.png", bg = "white")





present_data_filtered <- present_data[present_data$reason == "present",]

file_cortisol <- "TUM_EcoSleep_Results_MEL&CORT_240921.csv"
data_cortisol <- read.csv(file_cortisol, sep = ',')

calculate_auc <- function(cortisol) {
  auc <- 0
  for (i in 1:(length(cortisol) - 1)) {
    auc <- auc + (0.5 * (cortisol[i] + cortisol[i + 1]) * (30))
  }
  return(auc)
}

unique_id <- sort(unique(data_cortisol$Participant.ID))
df_correlation <- c()
for(ID in unique_id)
{
  data_cortisol_id <- data_cortisol %>% filter(Participant.ID == ID)
  unique_month <- sort(unique(data_cortisol_id$Month.1))
  melatonin_date <- unique(data_cortisol_id[data_cortisol_id$Timestamp == "e1",]$Date)
  for(unique_date in melatonin_date)
  {
    date <- dmy(unique_date)
    data_month_id <- data_cortisol_id %>% filter(dmy(Date) == date | dmy(Date) == date + days(1))
    target_date <- date
   # start_time <- ymd_hms(paste(target_date - days(1), "08:00:00"))
    start_time <- ymd_hms(paste(target_date, "08:00:00"))
    end_time <- ymd_hms(paste(target_date, "23:00:00"))
    
    light_data_month <- present_data_filtered[dmy_hms(present_data_filtered$DATE.TIME) > start_time &
                                     dmy_hms(present_data_filtered$DATE.TIME) < end_time,]
    # light_data_month <- present_data %>% filter(as.Date(dmy_hms(DATE.TIME)) == dmy(data_month_id[data_month_id$Timestamp == "e1",]$Date)
    #                                             & id == ID)
    if (nrow(light_data_month) > 0) {
      light_data_month <- light_data_month %>% rename(MEDI = MELANOPIC.EDI)
      light_data_month <- light_data_month %>% rename(Datetime = DATE.TIME )
      light_data_month$Datetime <- dmy_hms(light_data_month$Datetime) 
      data_month = light_data_month %>%
        dplyr::reframe(duration_above_threshold(MEDI, Datetime, threshold = 5000, as.df = TRUE))
      
      freq_cross_month = light_data_month %>%
        dplyr::reframe(frequency_crossing_threshold(MEDI, threshold = 5000, as.df = TRUE))
      
      interdaily_var_month = light_data_month %>%
        dplyr::reframe(intradaily_variability(MEDI, Datetime, as.df = TRUE))
      
      per_above_thr_month = light_data_month %>%
        dplyr::reframe(period_above_threshold(MEDI, Datetime, "above", threshold = 5000))
      
      pulse_above_thr_month = light_data_month %>%
        dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = 5000, as.df = TRUE))
      
      data_disparity_idex_month = light_data_month %>%
        dplyr::reframe(disparity_index(MEDI))
      
      time_abv_thr_month = light_data_month %>%
        dplyr::reframe(timing_above_threshold(MEDI, Datetime, "above", 5000, as.df = TRUE))
      
      cortisol_value <- as.numeric(tail(data_month_id$Cortisol..ng.mL.,4))
      auc <- calculate_auc(cortisol_value)
      
      new_row <- list(c(cortisol_value, 
                        auc,
                        data_month$duration_above_5000,
                        freq_cross_month$frequency_crossing_5000,
                        interdaily_var_month$intradaily_variability,
                        per_above_thr_month$`period_above_threshold(MEDI, Datetime, "above", threshold = 5000)`,
                        pulse_above_thr_month$n_pulses_above_5000,
                        pulse_above_thr_month$mean_level_pulses_above_5000,
                        pulse_above_thr_month$mean_duration_pulses_above_5000,
                        pulse_above_thr_month$total_duration_pulses_above_5000,
                        data_disparity_idex_month$`disparity_index(MEDI)`,
                        time_abv_thr_month$mean_timing_above_5000,
                        time_abv_thr_month$first_timing_above_5000,
                        time_abv_thr_month$last_timing_above_5000))
      temp_df <- c()
      temp_df <- data.frame(do.call(rbind, new_row))
      df_correlation <- bind_rows(df_correlation, temp_df)
      
    }
  }
}
colnames(df_correlation) <- c("m1","m2","m3","m4", "AUC",
                              "dur_abv_thr","freq", "IV", "per_above_250", "pulse_above_250", "pulse_above_250_mean_level", "pulse_above_250_mean_duration", "pulse_above_250_total_duration",
                              "disparity_index", "mean_time_above_250", "first_timing_above_250", "last_timing_above_250")
df_correlation[df_correlation == "sample not received"] <- NA
df_correlation[df_correlation == "Sample not received"] <- NA
df_correlation[df_correlation == "etremly low volume (<50µL)"] <- NA
df_correlation[df_correlation == "empty"] <- NA
df_correlation[df_correlation == ">1000"] <- NA

df_correlation <- as.data.frame(lapply(df_correlation, type.convert, as.is = TRUE))
#df_correlation <- na.omit(df_correlation)
cor_matrix <- cor(na.omit(df_correlation))

x11()
corrplot(cor_matrix, method = "color")
ggsave("correlation_plot.png", bg = "white")



df_long <- melt(na.omit(df_correlation[,18:21]), variable.name = "variable", value.name = "value")

# Create the box plot with superimposed line plot
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +  # Box plot
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 1) +  # Line plot for means
  stat_summary(fun = mean, geom = "point", color = "red", size = 3) +  # Points for means
  labs(title = "Box Plot with Mean Line", x = "Variable", y = "Value") +
  theme_minimal()
ggsave("box_plot.png", bg = "white")