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

present_data <- readRDS("present_data_2025.rds")
present_data_filtered <- present_data[present_data$reason == "present",]


# --- Define analysis parameters ---
daytime_start_hour <- 8
daytime_end_hour <- 18 # Up to, but not including 18:00
evening_start_hour <- 18
evening_end_hour <- 23 # Up to, but not including 23:00

threshold_low <- 10
threshold_medium <- 100
threshold_high <- 250

# Define minimum required data per day for analysis (e.g., 80% of 1440 minutes)
min_valid_minutes_per_day <- 0.8 * 24 * 60


# Add Date and Hour columns for grouping and filtering
light_df_processed <- present_data_filtered %>%
  filter(!is.na(MEDI)) %>% # Remove rows with NA MEDI if any
  mutate(
    Date = date(Datetime),
    Hour = hour(Datetime)
  )

# Calculate daily summaries
daily_summary <- light_df_processed %>%
  group_by(Id, Date) %>%
  summarise(
    # Basic stats
    Daily_Mean_MEDI = mean(MEDI, na.rm = TRUE),
    Daily_Median_MEDI = median(MEDI, na.rm = TRUE),
    Daily_SD_MEDI = sd(MEDI, na.rm = TRUE),    # Intra-day variability
    Daily_Max_MEDI = max(MEDI, na.rm = TRUE),
    
    # Windowed Means
    Daytime_Mean_MEDI = mean(MEDI[Hour >= daytime_start_hour & Hour < daytime_end_hour], na.rm = TRUE),
    Evening_Mean_MEDI = mean(MEDI[Hour >= evening_start_hour & Hour < evening_end_hour], na.rm = TRUE),
    
    # Time above thresholds (in minutes, since data is 1-min sampled)
    Minutes_Above_Low = sum(MEDI > threshold_low, na.rm = TRUE),
    Minutes_Above_Medium = sum(MEDI > threshold_medium, na.rm = TRUE),
    Minutes_Above_High = sum(MEDI > threshold_high, na.rm = TRUE),
    
    # Windowed Time Above Threshold
    Daytime_Mins_Above_Medium = sum(MEDI[Hour >= daytime_start_hour & Hour < daytime_end_hour] > threshold_medium, na.rm = TRUE),
    Evening_Mins_Above_Low = sum(MEDI[Hour >= evening_start_hour & Hour < evening_end_hour] > threshold_low, na.rm = TRUE),
    
    # Add count of valid minutes for QC
    Valid_Minutes = n(),
    .groups = 'drop' # Drop grouping after summarising
  )
  # Quality Control: Keep only days with sufficient data
  #filter(Valid_Minutes >= min_valid_minutes_per_day)

print(head(daily_summary))


participant_summary <- daily_summary %>%
  group_by(Id) %>%
  summarise(
    # Averages of daily stats
    Avg_Daily_Mean_MEDI = mean(Daily_Mean_MEDI, na.rm = TRUE),
    Avg_Daily_Median_MEDI = mean(Daily_Median_MEDI, na.rm = TRUE),
    Avg_Daytime_Mean_MEDI = mean(Daytime_Mean_MEDI, na.rm = TRUE),
    Avg_Evening_Mean_MEDI = mean(Evening_Mean_MEDI, na.rm = TRUE),
    Avg_Minutes_Above_Low = mean(Minutes_Above_Low, na.rm = TRUE),
    Avg_Minutes_Above_Medium = mean(Minutes_Above_Medium, na.rm = TRUE),
    Avg_Minutes_Above_High = mean(Minutes_Above_High, na.rm = TRUE),
    Avg_Daytime_Mins_Above_Medium = mean(Daytime_Mins_Above_Medium, na.rm = TRUE),
    Avg_Evening_Mins_Above_Low = mean(Evening_Mins_Above_Low, na.rm = TRUE),
    Avg_Daily_Max_MEDI = mean(Daily_Max_MEDI, na.rm = TRUE),
    
    # Variability metrics
    Avg_Intra_Day_SD = mean(Daily_SD_MEDI, na.rm = TRUE),  # Average within-day variability
    Inter_Day_SD = sd(Daily_Mean_MEDI, na.rm = TRUE),     # Variability of daily means across days
    
    # Overall max for the participant across all valid days
    Overall_Max_MEDI = max(Daily_Max_MEDI, na.rm = TRUE),
    
    # Count number of valid days summarized
    Num_Valid_Days = n(),
    .groups = 'drop'
  )
print(participant_summary)


# Calculate group summaries (Mean, SD, Median, IQR, Min, Max)
group_summary <- participant_summary %>%
  summarise(
    N_Participants = n(), # Count total participants included
    across(
      # Select numeric columns to summarize, exclude counts/IDs if needed
      .cols = where(is.numeric) & !matches("Num_Valid_Days"),
      .fns = list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE),
        Median = ~median(.x, na.rm = TRUE),
        Q1 = ~quantile(.x, 0.25, na.rm = TRUE), # For IQR
        Q3 = ~quantile(.x, 0.75, na.rm = TRUE), # For IQR
        Min = ~min(.x, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE)
      ),
      # Create column names like 'Avg_Daily_Mean_MEDI_Mean', 'Avg_Daily_Mean_MEDI_SD'
      .names = "{.col}|{.fn}"
    )
  )

# (Assuming 'group_summary' data frame exists from the previous step)

# --- Format the table as requested (Rows = Stats, Cols = Summaries) ---
# Revised pivoting strategy using names_pattern
formatted_group_table <- group_summary %>%
  select(-N_Participants) %>% # Keep N separate
  pivot_longer(
    cols = everything(),
    names_to = c("Statistic", "SummaryFn"), # Assign names based on pattern capture groups
    # SummaryFn will hold 'Mean', 'SD', 'Median', 'Q1', 'Q3', 'Min', 'Max'
    names_pattern = "(.*)\\|(.*)",          # Regex: Group 1: (.*) captures the Stat name before |
    #        Group 2: (.*) captures the Summary func name after |
    values_to = "Value"                     # Explicitly name the column holding the numeric values
  ) %>%
  # Now pivot wider using the explicitly created columns
  pivot_wider(
    names_from = "SummaryFn", # Use the column containing Mean, SD, etc. for new col names
    values_from = "Value"     # Use the column containing the numeric values
  ) %>%
  # Now 'Mean', 'SD', 'Median', 'Q1', 'Q3', 'Min', 'Max' should exist as columns
  # Create the combined summary strings
  mutate(
    `Mean ± SD` = paste0(round(Mean, 1), " ± ", round(SD, 1)),
    `Median [IQR]` = paste0(round(Median, 1), " [", round(Q1, 1), "-", round(Q3, 1), "]"),
    `Min - Max` = paste0(round(Min, 1), " - ", round(Max, 1))
  ) %>%
  # Select and order the final columns
  select(Statistic, `Mean ± SD`, `Median [IQR]`, `Min - Max`) %>%
  # Optional: Arrange rows logically (Ensure names match the 'Statistic' column)
  arrange(factor(Statistic, levels = c(
    "Avg_Daily_Mean_MEDI", "Avg_Daily_Median_MEDI",
    "Avg_Daytime_Mean_MEDI", "Avg_Evening_Mean_MEDI",
    "Avg_Minutes_Above_Low", "Avg_Minutes_Above_Medium", "Avg_Minutes_Above_High",
    "Avg_Daytime_Mins_Above_Medium", "Avg_Evening_Mins_Above_Low",
    "Avg_Intra_Day_SD", "Inter_Day_SD",
    "Avg_Daily_Max_MEDI", "Overall_Max_MEDI"
    # Add any other statistic names here if calculated
  )))

# Print the final table
print(paste("Summary based on N =", group_summary$N_Participants, "participants"))
print(formatted_group_table)

write_xlsx(formatted_group_table, path = "results_15_04/my_tibble.xlsx")
