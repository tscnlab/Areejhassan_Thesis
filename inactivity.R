library(LightLogR)
#these packages are needed for the examples as shown below.
library(flextable)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtsummary)

path <- "F:/all_files/"
files <- list.files(path, full.names = TRUE)
#show how many files are listes
length(files)

tz <- "Europe/Berlin"
pattern <- "^(\\d{3})"
data_lightLogR <- import$ActLumus(files, tz = tz, dst_adjustment = TRUE, auto.plot = FALSE, auto.id = pattern, remove_duplicates = TRUE)

data_lightLogR <- data_lightLogR %>% filter_Date(start = "2024-01-01")
data_lightLogR %>% gg_overview()

unique_id <- 101#sort(unique(data$Id))


start_time <- c()
end_time <- c()
non_activity_id <- c()

for(id in unique_id)
{
  data_Id <- data_lightLogR %>% filter(Id == id)
  data_Id$activity_diff <- c(0, diff(data_Id$PIM))
  i <- 1
  count <- 0
  while(i < nrow(data_Id) - 1){
    if(abs(data_Id[i,]$activity_diff) <= 1)
    {
      start <- data_Id$Datetime[i]
      while(abs(data_Id[i,]$activity_diff) <= 1 )
      {
        i <- i + 1
        count <- count + 1
      }
      if(count > 10)
      {
        print(i)
        start_time <- c(start_time, start)
        end_time <- c(end_time, data_Id$Datetime[i])
        non_activity_id <- c(non_activity_id, id)
        count <- 0
      }
      else{
        count <- 0
      }
    }
    else{
      i <- i + 1
    }
  }
}

missing_activity <- data.frame(id, start_time, end_time)
missing_activity$start_time <- as.POSIXct(missing_activity$start_time, origin = "1970-01-01", tz = tz)
missing_activity$end_time <- as.POSIXct(missing_activity$end_time, origin = "1970-01-01", tz = tz)

ggplot(na.omit(missing_activity)) + geom_segment(aes(x=start_time, y=id, xend=end_time, yend=id), size = 2) + 
  theme_minimal()
ggsave("missing_activity.png", bg = "white")

#data_Id %>%
#  gg_days(y.axis = PIM, y.axis.label = "Activity (PIM)")

ggplot(data = data_Id, aes(x = Datetime, y = PIM)) +
  geom_line() +
  theme_minimal()
ggsave("missing_activity_PIM.png", bg = "white")
