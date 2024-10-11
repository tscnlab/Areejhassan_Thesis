library(LightLogR)
#these packages are needed for the examples as shown below.
library(flextable)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gtsummary)
library(lubridate)


path <- "D:/Thesis/106/all_files/"
files <- list.files(path, full.names = TRUE)
#show how many files are listes
length(files)

data <- read.csv(files[1], skip = 32, sep = ';')
files_2 = files[2:length(files)]
for (file in files_2){
  data <- rbind(data, read.csv(file, skip = 32, sep = ';'))
}


#tz <- "Europe/Berlin"
# pattern <- "^(\\d{3})"
# data <- import$ActLumus(files, tz = tz, auto.id = pattern, dst_adjustment = TRUE,
#                         auto.plot = TRUE, silent = TRUE, remove_duplicates = TRUE)



#data$DATE.TIME <- dmy_hms(data$DATE.TIME, tz = "Europe/Berlin")
time_vec <- dmy_hms(data$DATE.TIME)

start_time <- c()
end_time <- c()
reason <- c()
i = 1

while(i < length(time_vec) - 1) {
  time_diff = as.numeric(difftime(time_vec[i+1], time_vec[i], units = "secs"))
  print(i)
  if(time_diff > 60 & year(time_vec[i+1]) != 2000 & year(time_vec[i]) != 2000){
    start_time <- c(start_time, data$DATE.TIME[i])
    end_time <- c(end_time, data$DATE.TIME[i + 1])
    reason <- c(reason, "missing time")
    i <- i+1
  }
  else if(time_diff < 0 & year(time_vec[i + 1]) == 2000){
    start_time <- c(start_time, data$DATE.TIME[i])
    print(i)
    print(data$DATE.TIME[i])
    while(year(time_vec[i + 1]) == 2000)
    {
      i<-i+1
    }
    end_time <- c(end_time, data$DATE.TIME[i + 1])
    reason <- c(reason, "false time")
    i<-i+1
  }
  else{
    i<-i+1
  }
}

missing_time <- data.frame(start_time, end_time, reason)



