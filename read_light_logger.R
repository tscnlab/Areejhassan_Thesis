library(LightLogR)
#these packages are needed for the examples as shown below.
library(flextable)
library(dplyr)
library(ggplot2)

path <- "C:/Workspace/R/thesis/data"
files <- list.files(path, full.names = TRUE)
#show how many files are listes
length(files)

tz <- "Europe/Berlin"
pattern <- "^(\\d{3})"
data <- import$ActLumus(files, tz = tz, auto.id = pattern, print_n=33)
data %>% gg_overview()

data_Id101 <- data %>% filter(Id == 101 & date(Datetime) == "2024-07-11")
data_Id101 %>% gg_day()