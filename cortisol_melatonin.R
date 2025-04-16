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
library(reshape2)
library(stringr)

file_cortisol <- "output_updated_data_v3.xlsx"
data_cortisol <- read_excel(file_cortisol)

present_data <- readRDS("present_data_2025.rds")
present_data_filtered <- present_data[present_data$reason == "present",]

######################## Violin Plot for present data #################################
present_data_filtered$season <- factor(quarters(present_data_filtered$Datetime), 
                    levels = c("Q1", "Q2", "Q3", "Q4"),
                    labels = c("Spring", "Summer", "Autumn", "Winter"))


present_data_filtered <- present_data_filtered %>% 
  mutate(week = floor_date(Datetime, "week")) %>% # Create a "week" column
  group_by(Id, week) %>%  # Group by ID and week
  mutate(mean_MEDI = mean(MEDI, na.rm = TRUE)) %>%
  ungroup()


present_data_filtered_unique <- present_data_filtered %>%
  group_by(Id, season) %>% 
  distinct(mean_MEDI, .keep_all = TRUE) %>% # Keep only unique mean_MEDI values
  ungroup()


ggplot(present_data_filtered, aes(x = season, fill = season)) +
  labs(color = "Participant ID") + 
  geom_violin(aes(y = MEDI), 
              color = "black",
              fill = "#d6eaf8",
              size = 0.8,
              adjust = 2) + 
  geom_boxplot(aes(y = MEDI),
               width = 0.1, 
               position = position_nudge(x = 0.2),
               outlier.shape = NA,
               color = "black", 
               fill = "white",
               size = 0.8) + 
  geom_point(data = present_data_filtered_unique, # Use filtered data for points
             aes(x = season, y = mean_MEDI, color = Id), 
             position = position_jitter(width = 0.1), 
             alpha = 0.7, 
             size = 2) +
  scale_y_continuous(trans = "log10",      # Keep the log10 transformation
                     labels = scales::comma_format(accuracy = 0.001)) +
  labs(title = "Distribution of Melanopic EDI by Season",
       x = "Season", 
       y = "Melanopic EDI (lux)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill = "none")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
        axis.title.y = element_text(face = "bold", margin = margin(r=10))
  )

ggsave("violin_plot_light.png", bg = "white")

################################ Cortisol Data ##################################################################

unique_id <- sort(unique(data_cortisol$ID))
df_correlation <- c()
time_duration <- seq(from = 0, to = 19, length.out = 20)
# Initialize an empty dataframe to store correlations
correlation_df <- data.frame()
temp_df <- data.frame()
df_box_plot <- data.frame()
threshold <- log(250)
light_data_id <- data.frame()

calculate_auc <- function(cortisol) {
  auc <- 0
  for (i in 1:(length(cortisol) - 1)) {
    auc <- auc + (0.5 * (cortisol[i] + cortisol[i + 1]) * (30))
  }
  return(auc)
}

get_mode <- function(x) {
  uniq_x <- unique(x)  # Get unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Find the value that occurs most
}

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
      
      
      #melatonin_value <- as.numeric(data_month_id$melatonin[1:11])
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
      df_box_plot <- bind_rows(df_box_plot, data.frame(do.call(rbind, list(c(participant_ID, cortisol_value, auc,
                                                                           mean(light_data_month$MEDI),
                                                                           median(light_data_month$MEDI),
                                                                           get_mode(light_data_month$MEDI))))))
    }
  }
}
# colnames(df_box_plot) <- c("ID", "m1" , "m2", "m3", "m4", "AUC", "mean", "median", "mode", 
#                            "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11")

colnames(df_box_plot) <- c("ID", "m1" , "m2", "m3", "m4", "AUC", "mean", "median", "mode")

colnames(temp_df) <- c(
  "ID",
  "AUC",
  "duration_above_threshold", "frequency", "period_above_threshold",
  "pulse_above_threshold", "pulse_above_threshold_mean_level",
  "pulse_above_threshold_mean_duration", "pulse_above_threshold_total_duration", "mean_time_above_threshold",
  "first_timing_above_threshold", "last_timing_above_threshold", "centroid", "mid_point"
)
ggsave("correlation_plot_cortisol_time.png", bg = "white")

####### Correlation Matrix ########################################################################################
correlation_df <- as.data.frame(lapply(temp_df, type.convert, as.is = TRUE))
correlation_df <- na.omit(correlation_df)
cor_matrix <- cor(correlation_df)
corrplot(cor_matrix, method = "color")

############# Box Plots #########################################################################################
df_box_plot[df_box_plot == "NA"] <- NA
df_box_plot <- na.omit(df_box_plot)
df_box_plot[] <- lapply(df_box_plot, function(x) as.numeric(as.character(x)))
df_box_plot$ID <- as.factor(df_box_plot$ID) 

# Pivot longer for easier plotting
df_long <- df_box_plot[1:5] %>%
  pivot_longer(cols = c(m1, m2, m3, m4), names_to = "variable")
ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(color = "black",
              fill = "#d6eaf8",
              size = 0.8,
              trim = FALSE
  ) + 
  geom_boxplot(width = 0.1, 
               position = position_nudge(x = 0.2),
               outlier.shape = NA,
               color = "black", 
               fill = "white",
               size = 0.8) + # White boxplots with black outlines
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 1, position = position_nudge(x = 0.2)) +  # Line plot for means
  stat_summary(fun = mean, geom = "point", color = "red", size = 3, position = position_nudge(x = 0.2)) +  # Points for means
  geom_point(aes(color = ID), 
             position = position_jitter(width = 0.1), # Add jitter to avoid overlap
             alpha = 0.7,  # Make points slightly transparent
             size = 2) +
  labs(title = "Distribution of Cortisol Samples",
       x = "Morning Samples", 
       y = "Cortisol (ng/mL)") +
  guides(fill = "none")+
  theme_minimal() + # Optional: Apply a clean theme 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
        axis.title.y = element_text(face = "bold", margin = margin(r=10))
        )
ggsave("violet_plot_cortisol.png", bg = "white")



df_long <- df_box_plot[,c("ID", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11")] %>%
  pivot_longer(cols = c(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11), names_to = "variable")
df_long$variable <- factor(df_long$variable, 
                           levels = paste0("e", 1:11)) # Specify the exact order

ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(color = "black",
              fill = "#d6eaf8",
              size = 0.8,
              trim = FALSE
  ) + 
  geom_boxplot(width = 0.1, 
               position = position_nudge(x = 0.2),
               outlier.shape = NA,
               color = "black", 
               fill = "white",
               size = 0.8) + # White boxplots with black outlines
  geom_point(aes(color = ID), 
             position = position_jitter(width = 0.1), # Add jitter to avoid overlap
             alpha = 0.7,  # Make points slightly transparent
             size = 2) +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red", size = 1, position = position_nudge(x = 0.2)) +  # Line plot for means
  stat_summary(fun = mean, geom = "point", color = "red", size = 3, position = position_nudge(x = 0.2)) +  # Points for means
  labs(title = "Distribution of Melatonin Samples",
       x = "Evening Samples", 
       y = "Melatonin (pg/mL)
") +
  guides(fill = "none")+
  theme_minimal() + # Optional: Apply a clean theme 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
        axis.title.y = element_text(face = "bold", margin = margin(r=10))
  )
ggsave("violet_plot_melatonin_mean.png", bg = "white")



df <- df_box_plot[,7:9]
df_long <- df %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.6, outlier.shape = NA) +  # Box plot
  scale_y_log10() +
  labs(title = "Box plot for Light Statistics", x = "Mean, Median and Mode", y = "MEDI") +
  theme_minimal()
# Create the box plot with superimposed line plot
ggsave("box_plot_Medi.png", bg = "white")


###### Light Profiling ################################################################################
save_plot_for_id <- function(id) {
  plot <- light_data_id %>%
    filter(Id == id) %>%
    gg_day(aes_col = MEDI < 250, size = 0.75) + 
    geom_line(aes(y = PIM), color = "red", alpha = 0.2) +
    theme(legend.position = "bottom")
  
  # Save the plot with the ID in the filename
  ggsave(filename = paste0("results_15_04/plot_", id, ".png"), plot = plot, width = 8, height = 16, dpi = 300, bg = "white")
}
unique_ids <- unique(light_data_id$Id)
walk(unique_ids, save_plot_for_id)




