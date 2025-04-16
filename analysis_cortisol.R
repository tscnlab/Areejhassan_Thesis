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
library(purrr)
library(gridExtra)
#library(dlmoR)
library(readxl)

path <- "D:/all_files_2025/"
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
file_cortisol <- "output_updated_data_v3.xlsx"
#data_cortisol <- read.csv(file_cortisol, sep = ';')
data_cortisol <- read_excel(file_cortisol)
calculate_auc <- function(cortisol) {
  auc <- 0
  for (i in 1:(length(cortisol) - 1)) {
    auc <- auc + (0.5 * (cortisol[i] + cortisol[i + 1]) * (30))
  }
  return(auc)
}

cor_nested <- function(dataset, 
                       nesting.variable, 
                       cor.variable.1, 
                       cor.variable.2) {
  #requirements:I
  #1. dataset has to be a datatable
  #2. nesting.variable has to be a string, referring to the nesting.variable in
  #   the dataset. This nesting variable has to be of type character or factor
  #3. cor.variable.1 and .2 have to be strings, referring to a numeric variable
  #   in the dataset.
  #print(cor.variable.2)
  #grouping and normalizing the variables
  dataset[[cor.variable.1]] <- as.numeric(dataset[[cor.variable.1]])
  dataset[[cor.variable.2]] <- as.numeric(dataset[[cor.variable.2]])
  dataset <- 
    dataset |> 
    group_by(.data[[nesting.variable]], .add = TRUE) |> 
    mutate(
      across(
        all_of(c(cor.variable.1, cor.variable.2)), 
        \(x) {
          x_clean <- ifelse(is.nan(x), NA, x)
          as.numeric(scale(x_clean)) 
        }#x |> \(x) ifelse(is.nan(x), NA, x) |> scale() |> as.numeric())
    )
    )|> 
    ungroup()
  
  cor.test(dataset[[cor.variable.1]], dataset[[cor.variable.2]], use = "complete.obs")
  
}


unique_id <- sort(unique(data_cortisol$ID))
df_correlation <- c()
light_threshold <- seq(from = log(10), to = log(250), length.out = 20)
time_duration <- seq(from = 0, to = 19, length.out = 20)
# Initialize an empty dataframe to store correlations
correlation_df <- data.frame()
pValue_df <- data.frame()
#threshold <- 7
# for (day in time_duration) {
#   # Initialize a temporary dataframe to store AUC and light metrics for this threshold
#   temp_df <- data.frame()
# 
#   for (ID in unique_id) {
#     data_cortisol_id <- data_cortisol %>% filter(Participant.ID == ID)
#     unique_month <- sort(unique(data_cortisol_id$Month.1))
#     melatonin_date <- unique(data_cortisol_id[data_cortisol_id$Timestamp == "e1",]$Date)
# 
#     for (unique_date in melatonin_date) {
#       date <- dmy(unique_date)
#       data_month_id <- data_cortisol_id %>% filter(dmy(Date) == date | dmy(Date) == date + days(1))
#       start_time <- dmy_hms(paste(unique_date, "08:00:00")) - days(day)
#       end_time <- dmy_hms(paste(unique_date, "23:00:00"))
# 
#       light_data_month <- present_data_filtered[present_data_filtered$Datetime > start_time &
#                                                   present_data_filtered$Datetime < end_time &
#                                                   present_data_filtered$Id == ID,]
# 
#       if (nrow(light_data_month) > 0) {
#         # Calculate light metrics using the current threshold
#         data_month = light_data_month %>%
#           dplyr::reframe(duration_above_threshold(MEDI, Datetime, threshold = exp(threshold), as.df = TRUE))
# 
#         freq_cross_month = light_data_month %>%
#           dplyr::reframe(frequency_crossing_threshold(MEDI, threshold = exp(threshold), as.df = TRUE))
# 
#         interdaily_var_month = light_data_month %>%
#           dplyr::reframe(intradaily_variability(MEDI, Datetime, as.df = TRUE))
# 
#         per_above_thr_month = light_data_month %>%
#           dplyr::reframe(period_above_threshold(MEDI, Datetime, "above", threshold = exp(threshold)))
# 
#         pulse_above_thr_month = light_data_month %>%
#           dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = exp(threshold), as.df = TRUE))
# 
#         data_disparity_idex_month = light_data_month %>%
#           dplyr::reframe(disparity_index(MEDI))
# 
#         time_abv_thr_month = light_data_month %>%
#           dplyr::reframe(timing_above_threshold(MEDI, Datetime, "above", exp(threshold), as.df = TRUE))
# 
#         # Calculate cortisol AUC
#         cortisol_value <- as.numeric(tail(data_month_id$Cortisol..ng.mL., 4))
#         auc <- calculate_auc(cortisol_value)
# 
#         #Store the metrics and AUC in a temporary dataframe
#         new_row <- list(c(
#           ID,
#           auc,
#           as.numeric(data_month[2]),
#           as.numeric(freq_cross_month[2]),
#           as.numeric(interdaily_var_month[2]),
#           as.numeric(per_above_thr_month[2]),
#           as.numeric(pulse_above_thr_month[2]),
#           as.numeric(pulse_above_thr_month[3]),
#           as.numeric(pulse_above_thr_month[4]),
#           as.numeric(pulse_above_thr_month[5]),
#           as.numeric(data_disparity_idex_month[2]),
#           as.numeric(time_abv_thr_month[2]),
#           as.numeric(time_abv_thr_month[3]),
#           as.numeric(time_abv_thr_month[4])
#         )
#         )
#         temp_df <- bind_rows(temp_df, data.frame(do.call(rbind, new_row)))
#       }
#     }
#   }
# 
#   # Assign column names to the temporary dataframe
#   colnames(temp_df) <- c(
#     "ID",
#     "AUC",
#     "dur_abv_thr", "freq", "IV", "per_above_250",
#     "pulse_above_thr", "pulse_above_thr_mean_level",
#     "pulse_above_thr_mean_duration", "pulse_above_thr_total_duration",
#     "disparity_index", "mean_time_above_thr",
#     "first_timing_above_thr", "last_timing_above_thr"
#   )
# 
#   # Calculate correlations of each metric with AUC for this threshold
#   #correlation_row <- sapply(temp_df[-1], function(metric) cor(temp_df$AUC, metric, use = "complete.obs"))
#   correlation_row_1 <- sapply(temp_df %>% names() %>% .[3:length(.)], function(col) {
#     cor_result <- cor_nested(temp_df, "ID", "AUC", col)
#     c(correlation = cor_result$estimate, p_value = cor_result$p.value)
#   }) %>% 
#   # Convert matrix to data frame and add adjusted p-values
#   as.data.frame() %>% 
#   t() %>% 
#   as.data.frame() %>% 
#   mutate(
#     p_adjusted = p.adjust(p_value, method = "BH")  # Benjamini-Hochberg
#   ) %>% 
#   t()
# 
#   # Append the correlation row to the correlation dataframe
#   #correlation_df <- rbind(correlation_df, correlation_row)
#   correlation_df <- rbind(correlation_df, correlation_row_1[1,])
#   
#   significant <- correlation_row_1[3,] < 0.05
#   pValue_df <- rbind(pValue_df, significant)
# }
# 
# colnames(correlation_df) <- temp_df %>% names() %>% .[3:length(.)]
# colnames(pValue_df) <- temp_df %>% names() %>% .[3:length(.)]
# 
# # Add row names corresponding to the thresholds
# rownames(correlation_df) <- paste0("Threshold_", seq_along(time_duration))
# 
# # The resulting `correlation_df` contains correlations of each metric with AUC for all thresholds.
# 
# 
# # Convert correlation_df to long format
# correlation_long <- correlation_df %>%
#   mutate(duration = time_duration) %>%
#   pivot_longer(cols = -duration, names_to = "metric", values_to = "correlation")
# 
# pValue_long <- pValue_df %>%
#   mutate(duration = time_duration) %>%
#   pivot_longer(cols = -duration, names_to = "metric", values_to = "pValue")
# 
# correlation_long$pValue <- pValue_long$pValue
# 
# x11()
# # Create a list of plots, one for each metric
# plots <- correlation_long %>%
#   split(.$metric) %>%
#   map(function(df) {
#     ggplot(df, aes(x = duration, y = correlation)) +
#       geom_line() +
#       geom_point(aes(color = pValue)) +
#       labs(title = unique(df$metric),
#            x = "days",
#            y = "Correlation with AUC",
#            color = "p-value significant") +
#       theme_minimal()
#   })
# n_cols <- min(3, length(plots))  # Adjust the number of columns as needed
# n_rows <- ceiling(length(plots) / n_cols)
# 
# grid.arrange(grobs = plots, ncol = n_cols, nrow = n_rows)


light_data_id <- data.frame()

for (threshold in light_threshold) {
  # Initialize a temporary dataframe to store AUC and light metrics for this threshold
  temp_df <- data.frame()

  for (participant_ID in unique_id) {
    print(participant_ID)
    data_cortisol_id <- data_cortisol %>% filter(ID == participant_ID)
    data_cortisol_id$date <- as.character(data_cortisol_id$date)
    cortisol_date <- na.omit(unique(data_cortisol_id[data_cortisol_id$instance == "m1",]$date))
    for (unique_date in cortisol_date) {
      unique_date <- ymd(unique_date, tz = "Europe/Berlin")
      print(unique_date)
      #date_unique <- ymd(unique_date)
      data_month_id <- data_cortisol_id %>% filter(ymd(date, tz = "Europe/Berlin") == unique_date)
      #start_time <- dmy_hms(paste(unique_date, "00:00:00"), tz = tz)
      #end_time <- dmy_hms(paste(unique_date, "23:59:00"), tz = tz)
      cortisol_day <- data_month_id[data_month_id$instance == "m1",]$date
      extracted_time_start <- data_month_id[data_month_id$instance == "m1",]$time_final
      if (is.na(extracted_time_start)) {
        extracted_time_start <- "08:00:00"
      }
      extracted_time_end <- data_month_id[data_month_id$instance == "m4",]$time_final
      if (is.na(extracted_time_end)) {
        extracted_time_end <- "10:00:00"
      }
      
      start_cortisol_time <- ymd_hms(paste(cortisol_day, extracted_time_start), tz = "Europe/Berlin")
      end_cortisol_time <- ymd_hms(paste(cortisol_day, extracted_time_end), tz = "Europe/Berlin")
      if (is.na(start_cortisol_time)) {
        start_cortisol_time <- ymd_hms(paste(cortisol_day, "08:00:00"), tz = "Europe/Berlin")
      }
      if (is.na(end_cortisol_time)) {
        end_cortisol_time <- ymd_hms(paste(cortisol_day, "10:00:00"), tz = "Europe/Berlin")
      }
      
      
      start_time <- start_cortisol_time - hours(2)
      end_time <- end_cortisol_time

      light_data_month <- present_data_filtered[present_data_filtered$Datetime > start_time &
                                                  present_data_filtered$Datetime < end_time &
                                                  present_data_filtered$Id == participant_ID,]
      
      
      if (nrow(light_data_month) > 1) {
        light_data_id <- rbind(light_data_id, light_data_month)
        # Calculate light metrics using the current threshold
        data_month = light_data_month %>%
          dplyr::reframe(duration_above_threshold(MEDI, Datetime, threshold = exp(threshold), as.df = TRUE))

        freq_cross_month = light_data_month %>%
          dplyr::reframe(frequency_crossing_threshold(MEDI, threshold = exp(threshold), as.df = TRUE))

        #interdaily_var_month = light_data_month %>%
        #  dplyr::reframe(intradaily_variability(MEDI, Datetime, as.df = TRUE))

        per_above_thr_month = light_data_month %>%
          dplyr::reframe(period_above_threshold(MEDI, Datetime, "above", threshold = exp(threshold)))

        pulse_above_thr_month = light_data_month %>%
          dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = exp(threshold), as.df = TRUE))
      
       data_disparity_idex_month = light_data_month %>%
          dplyr::reframe(disparity_index(MEDI))
       
       data_barroso = light_data_month %>%
         dplyr::reframe(barroso_lighting_metrics(MEDI, Datetime, as.df = TRUE))
       
       data_centroid = light_data_month %>%
         dplyr::reframe(centroidLE(MEDI, Datetime, "2 hours",  as.df = TRUE))
       
       data_midpoint = light_data_month %>%
         dplyr::reframe(midpointCE(MEDI, Datetime, as.df = TRUE))

        time_abv_thr_month = light_data_month %>%
          dplyr::reframe(timing_above_threshold(MEDI, Datetime, "above", exp(threshold), as.df = TRUE))

        # Calculate cortisol AUC
        cortisol_value <- as.numeric(tail(data_month_id$`cortisol (ng/mL)`, 4))
        auc <- calculate_auc(cortisol_value)

        # Store the metrics and AUC in a temporary dataframe
        new_row <- list(c(
          participant_ID,
          auc,
          as.numeric(data_month[2]),
          as.numeric(freq_cross_month[2]),
          as.numeric(per_above_thr_month[2]),
          as.numeric(pulse_above_thr_month[2]),
          as.numeric(pulse_above_thr_month[3]),
          as.numeric(pulse_above_thr_month[4]),
          as.numeric(pulse_above_thr_month[5]),
          as.numeric(time_abv_thr_month[2]),
          as.numeric(time_abv_thr_month[3]),
          as.numeric(time_abv_thr_month[4]), 
          as.numeric(data_centroid[2]), 
          as.numeric(data_midpoint[2])
        )
        )
        temp_df <- bind_rows(temp_df, data.frame(do.call(rbind, new_row)))
      }
    }
  }

  # Assign column names to the temporary dataframe
  colnames(temp_df) <- c(
    "ID",
    "AUC",
    "duration_above_threshold", "frequency", "period_above_threshold",
    "pulse_above_threshold", "pulse_above_threshold_mean_level",
    "pulse_above_threshold_mean_duration", "pulse_above_threshold_total_duration", "mean_time_above_threshold",
    "first_timing_above_threshold", "last_timing_above_threshold", "centroid", "mid_point"
  )

  correlation_row_1 <- sapply(temp_df %>% names() %>% .[3:length(.)], function(col) {
    cor_result <- cor_nested(temp_df, "ID", "AUC", col)
    c(correlation = cor_result$estimate, p_value = cor_result$p.value)
  }) %>% 
  # Convert matrix to data frame and add adjusted p-values
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(
    p_adjusted = p.adjust(p_value, method = "BH")  # Benjamini-Hochberg
  ) %>% 
  t()

  # Append the correlation row to the correlation dataframe
  correlation_df <- rbind(correlation_df, correlation_row_1[1,])

  significant <- correlation_row_1[3,] < 0.05
  pValue_df <- rbind(pValue_df, significant)
}

# Assign column names to the correlation dataframe
colnames(correlation_df) <- temp_df %>% names() %>% .[3:length(.)]
colnames(pValue_df) <- temp_df %>% names() %>% .[3:length(.)]

# Add row names corresponding to the thresholds
rownames(correlation_df) <- paste0("Threshold_", seq_along(light_threshold))

# The resulting `correlation_df` contains correlations of each metric with AUC for all thresholds.


# Convert correlation_df to long format
correlation_long <- correlation_df %>%
  mutate(threshold = light_threshold) %>%
  pivot_longer(cols = -threshold, names_to = "metric", values_to = "correlation")

pValue_long <- pValue_df %>%
  mutate(threshold = light_threshold) %>%
  pivot_longer(cols = -threshold, names_to = "metric", values_to = "pValue")

correlation_long$pValue <- pValue_long$pValue

x11()
# Create a list of plots, one for each metric
plots <- correlation_long %>%
  split(.$metric) %>%
  map(function(df) {
    ggplot(df, aes(x = threshold, y = correlation)) +
      geom_line() +
      geom_point(aes(color = pValue)) +
      labs(title = unique(df$metric),
           x = "Light Threshold",
           y = "Correlation with AUC",
           color = "p-value significant") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_x_continuous(
        trans = "log", # Keep the scale in log,
        labels = function(x) format(round(exp(x)), scientific = FALSE)
      ) +
      scale_y_continuous(limits = c(-1, 1))
  })

# Arrange plots in a grid
n_cols <- min(3, length(plots))  # Adjust the number of columns as needed
n_rows <- ceiling(length(plots) / n_cols)

grid.arrange(grobs = plots, ncol = n_cols, nrow = n_rows)

plot_grid <- arrangeGrob(grobs = plots, ncol = n_cols, nrow = n_rows)
ggsave("results_15_04/corrPlotvsLightThresh_250.png", plot_grid, width = 15, height = 10, units = "in")
ggsave("corrPlotvsLightThresh_2025.png", bg = "white")

# file_melatonin <- "TUM_EcoSleep_Results_MEL&CORT.csv"
# filename <- system.file(file_melatonin, package = "dlmoR")
# dlmo_result <- calculate_dlmo(file_path = file_melatonin, threshold = 3, fine_flag = TRUE)
# 
# 
#  
 
save_plot_for_id <- function(id) {
    plot <- light_data_id %>%
      filter(Id == id) %>%
      gg_day(aes_col = MEDI < 250, size = 0.75) + 
      geom_line(aes(y = PIM), color = "red", alpha = 0.2) +
      theme(legend.position = "bottom")
    
    # Save the plot with the ID in the filename
    ggsave(filename = paste0("plot_", id, ".png"), plot = plot, width = 8, height = 6, dpi = 300, bg = "white")
}
unique_ids <- unique(light_data_id$Id)
walk(unique_ids, save_plot_for_id)
