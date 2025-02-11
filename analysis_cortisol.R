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

path <- "F:/all_files/"
files <- list.files(path, full.names = TRUE)
#show how many files are listes
length(files)

tz <- "Europe/Berlin"
pattern <- "^(\\d{3})"
data_original <- import$ActLumus(files, tz = tz, dst_adjustment = TRUE, auto.plot = FALSE, auto.id = pattern, remove_duplicates = TRUE)
data <- data_original %>% filter_Date(start = "2024-01-01")
data %>% gg_overview()
data <- data %>% aggregate_Datetime(unit = "1 min")
data %>% gg_overview()

present_data <- c()
unique_id <- sort(unique(data$Id))
for(id in unique_id)
{
  data_Id <- data %>% filter(Id == id)
  data_Id$reason <- ""
  i <- 1
  count <- 0
  while(i < nrow(data_Id) - 1){
    if(data_Id$PIM[i] <= 0)
    {
      start_index <- i
      while((data_Id$PIM[i] <= 0) & (i < (nrow(data_Id) - 1)))
      {
        i <- i + 1
        count <- count + 1
      }
      if(count > 120)
      {
        print(i)
        count <- 0
      }
      else{
        count <- 0
        data_Id$reason[start_index:i] <- "present"
      }
    }
    else{
      data_Id$reason[i] <- "present"
      i <- i + 1
    }
  }
  present_data <- rbind(present_data, data_Id)
}

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
light_threshold <- seq(from = log(10), to = log(400000), length.out = 20)
for(ID in unique_id)
{
  data_cortisol_id <- data_cortisol %>% filter(Participant.ID == ID)
  unique_month <- sort(unique(data_cortisol_id$Month.1))
  melatonin_date <- unique(data_cortisol_id[data_cortisol_id$Timestamp == "e1",]$Date)
  for(unique_date in melatonin_date)
  {
    date <- dmy(unique_date)
    data_month_id <- data_cortisol_id %>% filter(dmy(Date) == date | dmy(Date) == date + days(1))
    # start_time <- ymd_hms(paste(target_date - days(1), "08:00:00"))
    start_time <- dmy_hms(paste(unique_date, "08:00:00"))
    end_time <- dmy_hms(paste(unique_date, "23:00:00"))
    
    light_data_month <- present_data_filtered[present_data_filtered$Datetime > start_time &
                                                present_data_filtered$Datetime < end_time &
                                                present_data_filtered$Id == ID,]
    # light_data_month <- present_data %>% filter(as.Date(dmy_hms(DATE.TIME)) == dmy(data_month_id[data_month_id$Timestamp == "e1",]$Date)
    #                                             & id == ID)
    if (nrow(light_data_month) > 0) {
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
      
      new_row <- list(c(
                        auc,
                        as.numeric(data_month[2]),
                        as.numeric(freq_cross_month[2]),
                        as.numeric(interdaily_var_month[2]),
                        as.numeric(per_above_thr_month[2]),
                        as.numeric(pulse_above_thr_month[2]),
                        as.numeric(pulse_above_thr_month[3]),
                        as.numeric(pulse_above_thr_month[4]),
                        as.numeric(pulse_above_thr_month[5]),
                        as.numeric(data_disparity_idex_month[2]),
                        as.numeric(time_abv_thr_month[2]),
                        as.numeric(time_abv_thr_month[3]),
                        as.numeric(time_abv_thr_month[4])))
      temp_df <- c()
      temp_df <- data.frame(do.call(rbind, new_row))
      df_correlation <- bind_rows(df_correlation, temp_df)
      
    }
  }
}
colnames(df_correlation) <- c("AUC",
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

