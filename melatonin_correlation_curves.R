# Load the package
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
library(dlmoR)
library(hms)
library(patchwork)

# Load sample data (provided with the package)
file_cortisol <- "output_data/output_updated_data_v4.xlsx"
data_cortisol <- read_excel(file_cortisol)

present_data <- readRDS("input_data/lightLogR/present_data_2025.rds")
present_data_filtered <- present_data[present_data$reason == "present",]


unique_id <- sort(unique(data_cortisol$ID))
time_duration <- seq(from = 0, to = 19, length.out = 20)
# Initialize an empty dataframe to store correlations
correlation_df_onset_time <- data.frame()
pValue_df_onset_time <- data.frame()
correlation_df_onset_value <- data.frame()
pValue_df_onset_value <- data.frame()
light_threshold <- log(seq(from = 0, to = 3500, length.out = 15))
light_threshold[1] <- log(10)
  # log(c(10,  50,  100,  150, 
  #                    250, 300,  400, 500,  750,
  #                    1000,1500,2000, 2500, 3500))
  #seq(from = log(10), to = log(3500), length.out = 20)
melatonin_threshold <- 3
melatonin_df <- data.frame(
  ID    = factor(),    # integer column
  date  = ymd(character()),  # character column
  melatonin_onset_time  = numeric(),    # logical (TRUE/FALSE) column
  melatonin_onset_value = numeric() 
)

get_mode <- function(x) {
  uniq_x <- unique(x)  # Get unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Find the value that occurs most
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

process_once <- TRUE
for (threshold in light_threshold) {
  print(threshold)
  temp_df <- data.frame()
  for (participant_ID in unique_id) {
    print(participant_ID)
    data_saliva_id <- data_cortisol %>% filter(ID == participant_ID)
    data_saliva_id$date <- as.character(data_saliva_id$date)
    cortisol_date <- na.omit(unique(data_saliva_id[data_saliva_id$instance == "e1",]$date))
    for (unique_date in cortisol_date) {
      unique_date <- ymd(unique_date, tz = "Europe/Berlin")
      print(unique_date)
      data_month_id <- data_saliva_id %>% filter(ymd(date, tz = "Europe/Berlin") == unique_date)
      data_month_id$datetime <- paste(data_month_id$date, data_month_id$time_final, sep = " ")
      
      data_month_id_melatonin <- data_month_id[, c("datetime", "melatonin")]
      data_month_id_melatonin$datetime <- ymd_hms(data_month_id_melatonin$datetime, tz = "Europe/Berlin")
      
      start_melatonin_time <- data_month_id_melatonin$datetime[1]
      end_melatonin_time <- data_month_id_melatonin$datetime[11]    
      if (is.na(start_melatonin_time)) {
        start_melatonin_time <- unique_date + hours(17)
      }
      if (is.na(end_melatonin_time)) {
        end_melatonin_time <- unique_date + hours(23)
      }
      
      
      
      start_time <- start_melatonin_time - hours(2)
      end_time <- end_melatonin_time
      
      light_data_month <- present_data_filtered[present_data_filtered$Datetime > start_time &
                                                  present_data_filtered$Datetime < end_time &
                                                  present_data_filtered$Id == participant_ID,]
      
      melatonin_complete_cases <- data_month_id_melatonin[ complete.cases(data_month_id_melatonin), ]
      
      melatonin_valid <- !any(is.na(melatonin_complete_cases)) &
        max(melatonin_complete_cases$melatonin) > melatonin_threshold
      
      if (nrow(light_data_month) > 2 & melatonin_valid == TRUE) {
        #light_data_id <- rbind(light_data_id, light_data_month)
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
        melatonin_onset_time <- 0
        melatonin_onset_value <- 0
        if(process_once == TRUE)
        {
          dlmo_result <- calculate_dlmo(data = melatonin_complete_cases, threshold = melatonin_threshold, fine_flag = FALSE)
          melatonin_onset_time <- as.numeric(dlmo_result$dlmo$coarse$time)
          melatonin_onset_value <- dlmo_result$dlmo$coarse$fit_melatonin
        }
        else
        {
          melatonin_onset_time <- melatonin_df[melatonin_df$ID == participant_ID & melatonin_df$date == unique_date,]$melatonin_onset_time
          melatonin_onset_value <- melatonin_df[melatonin_df$ID == participant_ID & melatonin_df$date == unique_date,]$melatonin_onset_value
        }
        melatonin_value <- as.numeric(data_month_id_melatonin$melatonin[1:11])

        # Store the metrics and AUC in a temporary dataframe
        new_row <- list(c(
          participant_ID,
          melatonin_onset_time,
          melatonin_onset_value,
          as.numeric(data_month[2]),
          as.numeric(freq_cross_month[2]),
          as.numeric(per_above_thr_month[2]),
          as.numeric(pulse_above_thr_month[2]),
          as.numeric(pulse_above_thr_month[3]),
          as.numeric(pulse_above_thr_month[4]),
          as.numeric(pulse_above_thr_month[5]),
          as.numeric(as_hms(time_abv_thr_month[2][[1]])),
          as.numeric(as_hms(time_abv_thr_month[3][[1]])),
          as.numeric(as_hms(time_abv_thr_month[4][[1]])), 
          as.numeric(as_hms(data_centroid[2][[1]])), 
          as.numeric(as_hms(data_midpoint[2][[1]]))
        )
        )
        temp_df <- bind_rows(temp_df, data.frame(do.call(rbind, new_row)))
        if(process_once == TRUE){
          new_row_melatonin <- data.frame(
            ID                    = participant_ID,   # ensure the level exists
            date                  = unique_date,
            melatonin_onset_time  = as.numeric(dlmo_result$dlmo$coarse$time),     # e.g. 23.5 hours
            melatonin_onset_value = dlmo_result$dlmo$coarse$fit_melatonin,
            stringsAsFactors      = FALSE     # already factors handled above
          )
          melatonin_df <- bind_rows(melatonin_df, new_row_melatonin)
        }
      }
    }
  }
  process_once <- FALSE
  #colnames(melatonin_df) <- c("ID", "date", "melatonin_onset_time", "melatonin_onset_value")
  colnames(temp_df) <- c(
    "ID",
    "melatonin_onset_time","melatonin_onset_value",
    "duration_above_threshold", "frequency", "period_above_threshold",
    "pulse_above_threshold", "pulse_above_threshold_mean_level",
    "pulse_above_threshold_mean_duration", "pulse_above_threshold_total_duration", "mean_time_above_threshold",
    "first_timing_above_threshold", "last_timing_above_threshold", "centroid", "mid_point"
  )
  
  correlation_row_1 <- sapply(temp_df %>% names() %>% .[4:length(.)], function(col) {
    cor_result <- cor_nested(temp_df, "ID", "melatonin_onset_time", col)
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
  correlation_df_onset_time <- rbind(correlation_df_onset_time, correlation_row_1[1,])
  
  significant <- correlation_row_1[3,] < 0.05
  pValue_df_onset_time <- rbind(pValue_df_onset_time, significant)
  
  
  correlation_row_2 <- sapply(temp_df %>% names() %>% .[4:length(.)], function(col) {
    cor_result <- cor_nested(temp_df, "ID", "melatonin_onset_value", col)
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
  correlation_df_onset_value <- rbind(correlation_df_onset_value, correlation_row_2[1,])
  
  significant <- correlation_row_2[3,] < 0.05
  pValue_df_onset_value <- rbind(pValue_df_onset_value, significant)
  
}


# Assign column names to the correlation dataframe
colnames(correlation_df_onset_time) <- temp_df %>% names() %>% .[4:length(.)]
colnames(pValue_df_onset_time) <- temp_df %>% names() %>% .[4:length(.)]

# Add row names corresponding to the thresholds
rownames(correlation_df_onset_time) <- paste0("Threshold_", seq_along(light_threshold))

# The resulting `correlation_df` contains correlations of each metric with AUC for all thresholds.


# Convert correlation_df to long format
correlation_long <- correlation_df_onset_time %>%
  mutate(threshold = light_threshold) %>%
  pivot_longer(cols = -threshold, names_to = "metric", values_to = "correlation")

pValue_long <- pValue_df_onset_time %>%
  mutate(threshold = light_threshold) %>%
  pivot_longer(cols = -threshold, names_to = "metric", values_to = "pValue")

correlation_long$pValue <- pValue_long$pValue

x11()
plots <- correlation_long %>%
  split(.$metric) %>%
  map(function(df) {
    
    xvals <- exp(df$threshold)
    ggplot(df, aes(x = xvals, y = correlation)) +
      geom_line() +
      geom_point(aes(color = pValue)) +
      
      # force exactly one major grid line per point
      scale_x_continuous(
        breaks      = xvals,       # one break at each x‐value
        labels      = xvals, # or whatever formatting you like
        minor_breaks = NULL,       # no minor grid lines
        expand      = c(0, 0)      # optional: snug to min/max
      ) +
      
      # 2) force the major x‐grid on
      theme_minimal() +
      theme(
        panel.grid.major.x = element_line(color = "grey80"),
        panel.grid.minor.x = element_blank(),
        
        plot.title      = element_text(hjust = 0.5),
        axis.text.x     = element_text(angle = 90, hjust = 1)
      ) +
      
      scale_y_continuous(limits = c(-1, 1)) +
      labs(
        title = unique(df$metric),
        x     = "Light Threshold",
        y     = "Correlation with Habitual Melatonin Onset",
        color = "Significant(p<0.05)"
      )
  })


# Arrange plots in a grid
n_cols <- min(3, length(plots))  # Adjust the number of columns as needed
n_rows <- ceiling(length(plots) / n_cols)



# Suppose 'plots' is your list of ggplot objects
combined <- wrap_plots(plots, ncol = n_cols) +
  plot_layout(axis_titles = "collect") & 
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

# Add a top‐level title
combined + 
  plot_annotation(
    title = "Correlation Curves(Habitual Melatonin Onset and Light Metrics)",
    theme = theme(
      plot.title = element_text(
        size   = 16,
        face   = "bold",
        hjust  = 0.5,
        margin = margin(b = 20)    # <-- adds 20 pts of space below the title
      )
  )
  )

ggsave(
  filename = "result_26/correlation_habitual_melatonin_onset.png",  # file name (extension decides format)
  plot     = last_plot(),  
  width    = 20,     # in inches
  height   = 12,      # in inches
  dpi      = 300     # resolution
)