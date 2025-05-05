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
library(hms)
library(ggtext)
library(lme4)
library(sjPlot)
library(patchwork)
library(cowplot)
library(scales)

file_cortisol <- "output_data/output_updated_data_v3.xlsx"
data_cortisol <- read_excel(file_cortisol)

# path <- "E:/all_files_2025/"
# files <- list.files(path, full.names = TRUE)
# tz <- "Europe/Berlin"
# pattern <- "^(\\d{3})"
# data_original <- import$ActLumus(files, tz = tz, dst_adjustment = TRUE, auto.plot = FALSE, auto.id = pattern, remove_duplicates = TRUE)
# present_data <- data_original %>% filter_Date(start = "2024-01-01")


present_data <- readRDS("input_data/lightLogR/present_data_2025.rds")
present_data_filtered <- present_data[present_data$reason == "present",]

######################## Violin Plot for present data #################################
present_data_filtered <- subset(present_data_filtered, MEDI > 0.1)
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

n_per_season <- present_data_filtered %>%
  group_by(season) %>%
  summarize(n = n(), .groups="drop")
#############################Lmer Model########################################
df2 <- present_data_filtered %>%
  filter(!is.na(MEDI), MEDI > 0)  
df2$season <- factor(df2$season)
df2$season <- relevel(df2$season, ref = "Winter")
m1 <- lmer(log10(MEDI) ~ season + (1 | Id), data = df2)
my_aic <- AIC(m1)
# then append a note in your report or HTML:
summary(m1)
tab_model(
  m1,
  show.aic   = FALSE,
  file     = "result_26/lmer_logmedi_season.html",   # output file on disk
  title    = "LMM Results: log(MEDI) ~ season + (1|Id)"
)
cat("AIC:", my_aic, file="result_26/lmer_logmedi_season.html", append=TRUE)
#######################Without Log#######################################
m1 <- lmer(MEDI ~ season + (1 | Id), data = df2)
my_aic <- AIC(m1)
# then append a note in your report or HTML:
summary(m1)
tab_model(
  m1,
  show.aic   = FALSE,
  file     = "result_26/lmer_medi_season.html",   # output file on disk
  title    = "LMM Results: MEDI ~ season + (1|Id)"
)
cat("AIC:", my_aic, file="result_26/lmer_medi_season.html", append=TRUE)
########################log Photo Period#################################################
coordinates <- c(48.1351, 11.5820)
present_data_photo_period <- present_data_filtered |> add_photoperiod(coordinates)
df2 <- present_data_photo_period %>%
  filter(!is.na(MEDI), MEDI > 0)
m1 <- lmer(log10(MEDI) ~ photoperiod + (1 | Id), data = df2)
my_aic <- AIC(m1)
summary(m1)
tab_model(
  m1,
  show.aic   = FALSE,
  file     = "result_26/lmer_logMedi_photoperiod.html",   # output file on disk
  title    = "LMM Results: log(MEDI) ~ photoperiod + (1|Id)"
)
cat("AIC:", my_aic, file="result_26/lmer_logMedi_photoperiod.html", append=TRUE)
########################Photo Period#################################################
m1 <- lmer(MEDI ~ photoperiod + (1 | Id), data = df2)
my_aic <- AIC(m1)
summary(m1)
tab_model(
  m1,
  show.aic   = FALSE,
  file     = "result_26/lmer_Medi_photoperiod.html",   # output file on disk
  title    = "LMM Results: MEDI ~ photoperiod + (1|Id)"
)
cat("AIC:", my_aic, file="result_26/lmer_Medi_photoperiod.html", append=TRUE)
###############################################################################
ggplot(present_data_filtered, aes(x = season, fill = season)) +
  # your violin + boxplot
  geom_violin(aes(y = MEDI),
              color = "black", fill = "#d6eaf8", size = 0.8, adjust = 2) +
  geom_boxplot(aes(y = MEDI),
               width = 0.1, position = position_nudge(x = 0.2),
               outlier.shape = NA,
               color = "black", fill = "white", size = 0.8) +
  
  # add mean points (stat_summary with geom = "point")
  stat_summary(
    aes(y = MEDI), 
    fun = mean, 
    geom = "point", 
    shape = 23,            # filled diamond
    size = 3, 
    fill = "red", 
    position = position_nudge(x = 0.2)
  ) +
  
  # add line connecting the mean points across seasons
  stat_summary(
    aes(y = MEDI, group = 1),  # group=1 so it connects across the x‐axis in order
    fun = mean,
    geom = "line",
    size = 0.8,
    color = "red",
    position = position_nudge(x = 0.2)
  ) +
  
  # (optional) still show individual means per Id
  geom_point(data = present_data_filtered_unique,
             aes(x = season, y = mean_MEDI, color = Id),
             position = position_jitter(width = 0.1), alpha = 0.7, size = 2) +

  # scale_y_continuous(
  #   trans  = pseudo_log_trans(sigma = 0.1,base = 10),     # log‐tails, linear around zero
  #   breaks = c(0, 10^(0:5)),                  # your exact breaks
  #   labels = c("0", format(10^(0:5), scientific = FALSE))
  #               ) +
  scale_y_log10(
    breaks = c(0.1, 10^(0:5)),                  # your exact breaks
    labels = c("0.1", format(10^(0:5), scientific = FALSE))
  )+
  geom_text(
    data = n_per_season,
    aes(x = season, y = max(present_data_filtered$MEDI)*1.1,   # e.g. just above the top
        label = paste0("N=", n)),
    position = position_nudge(x = 0.3),
    size = 3
  ) +
  labs(title = "Distribution of Melanopic EDI by Season ",
       subtitle = "(Colored points represent weekly mean of each participant)",
       x = "Season",
       y = "Melanopic EDI (lux) in Log Scale",
       color = "Participant ID") +
  theme_minimal() +
  theme(
    plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  ) +
  guides(fill = "none")


ggsave(
  filename = "result_26/figure6.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 6,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
  )

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
  cortisol_date <- na.omit(unique(data_cortisol_id[data_cortisol_id$instance == "e1",]$date))
  for (unique_date in cortisol_date) {
    unique_date <- ymd(unique_date, tz = "Europe/Berlin")
    print(unique_date)
    #date_unique <- ymd(unique_date)
    data_month_id <- data_cortisol_id %>% filter(ymd(date, tz = "Europe/Berlin") == unique_date + days(1))
    data_month_id_melatonin <- data_cortisol_id %>% filter(ymd(date, tz = "Europe/Berlin") == unique_date)
    start_time <- dmy_hms(paste(unique_date, "00:00:00"), tz = tz)
    end_time <- dmy_hms(paste(unique_date, "23:59:00"), tz = tz)
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
    
     #start_time <- unique_date
     #end_time <- start_time + days(2)
    
    light_data_month <- present_data_filtered[present_data_filtered$Datetime > start_time &
                                                present_data_filtered$Datetime < end_time &
                                                present_data_filtered$Id == participant_ID,]
    
    # light_data_month <- present_data[present_data$Datetime > start_time &
    #                                    present_data$Datetime < end_time &
    #                                    present_data$Id == participant_ID,]
    
    if (nrow(light_data_month) > 1) {
    # if (1) {
      light_data_month$month <- data_month_id_melatonin[data_month_id_melatonin$instance == "e1",]$month
      light_data_month$monthYear <- format(light_data_month[light_data_month$Id == participant_ID,]$Datetime, "%m-%y")
      light_data_id <- rbind(light_data_id, light_data_month)
      # # Calculate light metrics using the current threshold
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
        as.numeric(as_hms(time_abv_thr_month[2][[1]])),
        as.numeric(as_hms(time_abv_thr_month[3][[1]])),
        as.numeric(as_hms(time_abv_thr_month[4][[1]])),
        as.numeric(as_hms(data_centroid[2][[1]])),
        as.numeric(as_hms(data_midpoint[2][[1]]))
      )
      )
      temp_df <- bind_rows(temp_df, data.frame(do.call(rbind, new_row)))
      df_box_plot <- bind_rows(df_box_plot, data.frame(do.call(rbind, list(c(participant_ID, cortisol_value, auc,
                                                                           mean(light_data_month$MEDI),
                                                                           median(light_data_month$MEDI),
                                                                           get_mode(light_data_month$MEDI))))))
      # melatonin_value <- as.numeric(data_month_id_melatonin[1:11,]$melatonin)
      # df_box_plot <- bind_rows(df_box_plot, data.frame(do.call(rbind, list(c(participant_ID, melatonin_value)))))
      # df_box_plot <- bind_rows(df_box_plot, data.frame(do.call(rbind, list(c(participant_ID, cortisol_value, auc,
      #                                                                        mean(light_data_month$MEDI),
      #                                                                        median(light_data_month$MEDI),
      #                                                                        get_mode(light_data_month$MEDI),
      #                                                                        data_month_id_melatonin[1:11,]$melatonin)))))
    }
  }
}
#colnames(df_box_plot) <- c("ID", "m1" , "m2", "m3", "m4", "AUC", "mean", "median", "mode", 
#                             "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11")

colnames(df_box_plot) <- c("ID", "m1" , "m2", "m3", "m4", "AUC", "mean", "median", "mode")

colnames(temp_df) <- c(
  "ID",
  "AUC",
  "duration_above_threshold", "frequency", "period_above_threshold",
  "pulse_above_threshold", "pulse_above_threshold_mean_level",
  "pulse_above_threshold_mean_duration", "pulse_above_threshold_total_duration", "mean_time_above_threshold",
  "first_timing_above_threshold", "last_timing_above_threshold", "centroid", "mid_point"
)



########################### Correlation Matrix #################################
cor.mtest <- function(mat, conf.level = 0.95) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for(i in 1:(n-1)) for(j in (i+1):n) {
    tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
    p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

correlation_df <- as.data.frame(lapply(temp_df, type.convert, as.is = TRUE))
correlation_df <- na.omit(correlation_df)
cor_matrix <- cor(correlation_df[ , -1 ])
p.mat <- cor.mtest(correlation_df[,-1])

alpha <- 0.05
png(
  filename = "result_26/correlation_matrix_AUC_new.png",
  width    = 10,     # width in inches
  height   = 8,     # height in inches
  units    = "in",
  res      = 300    # resolution in dots per inch
)

# 2. Plot the full matrix
par(mar = c(1,1,0,1))  # make room for title/subtitle
corrplot(
  cor_matrix,
  method = "color",
  type   = "lower",       # draw both triangles
  tl.cex = 0.8,
  cl.cex = 0.8,
  tl.col = "black",       # make all text labels black
  tl.font = 2,  
  cl.font = 2,   
  main   = "\nCorrelation Matrix for AUC and Light Metrics",
  mar    = c(0, 0, 0, 0) 
)

# 3. Overlay stars on significant cells
n <- ncol(cor_matrix)
sig.pos <- which(p.mat < alpha, arr.ind = TRUE)
# remove the diagonal (i == j) if you don’t want to mark those
sig.pos <- sig.pos[sig.pos[,1] != sig.pos[,2], , drop = FALSE]
# **keep only lower‐triangle pairs** (row index > col index)
sig.pos <- sig.pos[sig.pos[,1] > sig.pos[,2], , drop = FALSE]

for(k in seq_len(nrow(sig.pos))) {
  i <- sig.pos[k,1]
  j <- sig.pos[k,2]
  # corrplot plots row i, col j at (x = j, y = n - i + 1)
  text(
    x   = j, 
    y   = n - i + 1,
    labels = "*",
    cex    = 1.5,
    col    = "black"
  )
}

# 4. Legend for the asterisk
legend(
  "bottomleft",
  legend   = "* = p < 0.05",
  bty      = "n",
  cex      = 1.2,
  x.intersp= 0.5,
  y.intersp= 0.7,
  inset     = c(0, 0.05) 
)

dev.off()

#############################Lmer Model########################################
m1 <- lmer(AUC ~ pulse_above_threshold_mean_level + (1 | ID), data = correlation_df)
summary(m1)
tab_model(
  m1,
  file     = "result_26/lmer_auc_pulse_above_threshold_mean_level.html",   # output file on disk
  title    = "LMM Results: AUC ~ pulse_above_threshold_mean_level + (1|Id)"
)

############# Box Plots #########################################################################################
df_box_plot[df_box_plot == "NA"] <- NA
df_box_plot <- na.omit(df_box_plot)
df_box_plot[] <- lapply(df_box_plot, function(x) as.numeric(as.character(x)))
df_box_plot$ID <- as.factor(df_box_plot$ID) 

# Pivot longer for easier plotting
df_long <- df_box_plot[1:5] %>%
  pivot_longer(cols = c(m1, m2, m3, m4), names_to = "variable")

n_per_variable <- df_long %>%
  group_by(variable) %>%
  summarize(n = n(), .groups="drop")
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
  labs(title = paste0("Distribution of Cortisol Samples (N = ", 4*n_per_variable$n, ")"),
       x = "Morning Samples", 
       y = "Cortisol (ng/mL)") +
  guides(fill = "none")+
  theme_minimal() + # Optional: Apply a clean theme 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
        axis.title.y = element_text(face = "bold", margin = margin(r=10))
        )
ggsave(
  filename = "result_26/vioin_cortisol.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 6,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)
########################### Per Id plot #############################################################
ids <- unique(df_long$ID)
plot_list <- vector("list", length(ids))
names(plot_list) <- ids

for(i in seq_along(ids)) {
  this_id <- ids[i]
  df_i    <- subset(df_long, ID == this_id)
  n_i     <- nrow(df_i)
  
  p <- ggplot(df_i, aes(x = variable, y = value, fill = variable)) +
    geom_violin(color="black", fill="#d6eaf8", size=0.8, trim=FALSE) +
    geom_boxplot(width=0.1, position=position_nudge(x=0.2),
                 outlier.shape=NA, color="black", fill="white", size=0.8) +
    stat_summary(fun=mean, geom="line", aes(group=1),
                 color="red", size=1, position=position_nudge(x=0.2)) +
    stat_summary(fun=mean, geom="point", color="red", size=3,
                 position=position_nudge(x=0.2)) +
    geom_point(aes(color=ID), position=position_jitter(width=0.1),
               alpha=0.7, size=2) +
    labs(
      title = paste0("ID=", this_id, "  (N=", n_i, ")"),
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      legend.position  = "none"
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_textbox_simple(size = 14,face = "bold",fill= "#0c93d2", halign = 0.5),
      axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
      axis.title.y = element_text(face = "bold", margin = margin(r=10)),
      legend.position  = "none"            # ← remove legend
    )
  
  plot_list[[i]] <- p
}
combined <- wrap_plots(plot_list[5:10], ncol = 2) +
  plot_layout(axis_titles = "collect")


combined <- combined & 
  labs(
    x = "Morning Samples",
    y = "Cortisol (ng/mL)"
  ) & 
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

# now add the main title via plot_annotation()
combined_final <- combined +
  plot_annotation(
    title = "Per-Participant Cortisol Profiles",
    theme = theme(
      plot.title = element_text(
        size  = 16,
        face  = "bold",
        hjust = 0.5,
        margin = margin(b = 10)
      )
    )
  )

ggsave(
  filename = "result_26/cortisol5-8.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 8,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)



####################################################################################################



df_long <- df_box_plot[,c("ID", "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11")] %>%
  pivot_longer(cols = c(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11), names_to = "variable")

n_per_variable <- df_long %>%
  group_by(variable) %>%
  summarize(n = n(), .groups="drop")
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
  stat_summary(fun = median, geom = "line", aes(group = 1), color = "red", size = 1, position = position_nudge(x = 0.2)) +  # Line plot for means
  stat_summary(fun = median, geom = "point", color = "red", size = 3, position = position_nudge(x = 0.2)) +  # Points for means
  labs(title  = paste0("Distribution of Melatonin Samples (N = ", 11 * n_per_variable$n, ")"),
       x = "Evening Samples", 
       y = "Melatonin (pg/mL)
") +
  guides(fill = "none")+
  theme_minimal() + # Optional: Apply a clean theme 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
        axis.title.y = element_text(face = "bold", margin = margin(r=10))
  )
ggsave(
  filename = "result_26/melatonin_mean.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 6,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)
################################ Per ID Melatonin Plot###################################
ids <- unique(df_long$ID)
plot_list <- vector("list", length(ids))
names(plot_list) <- ids

for(i in seq_along(ids)) {
  this_id <- ids[i]
  df_i    <- subset(df_long, ID == this_id)
  n_i     <- nrow(df_i)
  
  p <- ggplot(df_i, aes(x = variable, y = value, fill = variable)) +
    geom_violin(color="black", fill="#d6eaf8", size=0.8, trim=FALSE) +
    geom_boxplot(width=0.1, position=position_nudge(x=0.2),
                 outlier.shape=NA, color="black", fill="white", size=0.8) +
    stat_summary(fun=median, geom="line", aes(group=1),
                 color="red", size=1, position=position_nudge(x=0.2)) +
    stat_summary(fun=median, geom="point", color="red", size=3,
                 position=position_nudge(x=0.2)) +
    geom_point(aes(color=ID), position=position_jitter(width=0.1),
               alpha=0.7, size=2) +
    labs(
      title = paste0("ID=", this_id, "  (N=", n_i, ")"),
      axis.title.x     = element_blank(),
      axis.title.y     = element_blank(),
      legend.position  = "none"
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_textbox_simple(size = 14,face = "bold",fill= "#0c93d2", halign = 0.5),
      axis.title.x = element_text(face = "bold", margin = margin(t=10)),  # Make x-axis label bold
      axis.title.y = element_text(face = "bold", margin = margin(r=10)),
      legend.position  = "none"            # ← remove legend
    )
  
  plot_list[[i]] <- p
}
combined <- wrap_plots(plot_list[5:10], ncol = 2) +
  plot_layout(axis_titles = "collect")


combined <- combined & 
  labs(
    x = "Evening Samples",
    y = "Melatonin (pg/mL)"
  ) & 
  theme(
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )

# now add the main title via plot_annotation()
combined_final <- combined +
  plot_annotation(
    title = "Per-Participant Melatonin Profiles",
    theme = theme(
      plot.title = element_text(
        size  = 16,
        face  = "bold",
        hjust = 0.5,
        margin = margin(b = 10)
      )
    )
  )

ggsave(
  filename = "result_26/melatonin5-10.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 8,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)

########################################################################################
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
    filter_Time(start = "08:00:34", end = "09:00:00")%>%
    gg_day(aes_col = MEDI < 250, size = 0.75) + 
    geom_line(aes(y = PIM), color = "red", alpha = 0.2) +
    theme(legend.position = "bottom")
  
  # Save the plot with the ID in the filename
  ggsave(filename = paste0("figure/plot_", id, ".png"), plot = plot, width = 8, height = 16, dpi = 300, bg = "white")
}
unique_ids <- c(101:104,106,107,109:112)
walk(unique_ids, save_plot_for_id)





df_summary <- light_data_id |>
  filter(Id %in% unique_ids) |>
  aggregate_Datetime(
    Datetime.colname  = Datetime,
    unit              = "60 mins",
    type              = "floor",
    # leave numeric.handler = mean (or median) if you don’t care about the default,
    # but we will compute the exact summaries you asked for via ...
    med      = median(MEDI,        na.rm = TRUE),
    q50_low  = quantile(MEDI, 0.25,    na.rm = TRUE),
    q50_high = quantile(MEDI, 0.75,    na.rm = TRUE),
    q75_low  = quantile(MEDI, 0.125,   na.rm = TRUE),   # 12.5th percentile
    q75_high = quantile(MEDI, 0.875,   na.rm = TRUE),   # 87.5th percentile
    q95_low  = quantile(MEDI, 0.025,   na.rm = TRUE),
    q95_high = quantile(MEDI, 0.975,   na.rm = TRUE)
  )


months_to_plot <- c(9:12)
global_y_axis_breaks <- c(0, 10, 1000, 100000)
global_y_lim <- c(0, max(global_y_axis_breaks) * 1.1) # e.g., 0 to 110000

make_single_plot <- function(data_subset, m, ylim_vals, is_first_col, is_first_row) {
  
  # decide whether we want a month‐title here or not
  title_text <- if (is_first_row) as.character(m) else NULL
  
  p <- gg_days(
    dataset        = data_subset,
    x.axis         = Datetime,
    geom           = "blank",
    facetting      = TRUE,
    x.axis.breaks  = \(x) Datetime_breaks(x, by = "6 hours", shift = 0),
    x.axis.format  = "%H:%M"
  ) +
    geom_ribbon(aes(x = Datetime, ymin = q50_low, ymax = q50_high),
                fill = "red", alpha = 0.3) +
    geom_ribbon(aes(x = Datetime, ymin = q75_low, ymax = q75_high),
                fill = "red", alpha = 0.2) +
    geom_ribbon(aes(x = Datetime, ymin = q95_low, ymax = q95_high),
                fill = "red", alpha = 0.1) +
    geom_line(aes(x = Datetime, y = med), size = 0.6) +
    scale_y_continuous(
      trans = pseudo_log_trans(sigma = 0.1, base = 10), # Use pseudo-log to handle 0
      breaks = global_y_axis_breaks,                 # Apply the global breaks
      labels = label_number(accuracy=1),              # Format labels nicely
      limits = global_y_lim,                          # Apply global limits
      expand = expansion(mult = c(0, 0.05))           # Control expansion (optional)
    ) +
    
    # only give labs(title=…) if title_text is non‐NULL
    labs(
      title = title_text,
      x     = NULL,
      y     = NULL
    ) +
    
    theme_minimal(base_size = 9) +
    theme(
      strip.text       = element_blank(),
      strip.background = element_blank(),
      axis.text.x      = element_text(angle = 0, hjust = 0.5, size = rel(0.8)),
      plot.title       = element_textbox_simple(
        size    = 10,
        face    = "bold",
        halign  = 0.5,
        fill    = "#0c93d2",
        color   = "white",
        padding = margin(2, 2, 2, 2),
        margin  = margin(b = 4)
      ),
      plot.margin      = margin(1, 1, 1, 1),
      axis.text.y      = element_text(size = rel(0.8))
    )
  
  
  return(p)
}

# 4. Generate all plots or placeholders in the correct order (row by row)
is_first_row <- TRUE 
plot_list <- list()
for (id_val in unique_ids) {
  # Iterate through each month defined for plotting
  for (m in months_to_plot) {
    # Check if this is the first column for the current row
    is_first_col <- (m == months_to_plot[1])
    
    # Filter the main dataframe for data matching the current ID and month
    current_data <- df_summary %>%
      filter(Id == id_val, month == m)
    
    # Check if any data exists for this specific ID-month combination
    if (nrow(current_data) > 0) {
      # Data exists: create the plot using the helper function
      p <- make_single_plot(current_data, m, current_ylim_row, is_first_col,is_first_row)
    } else {
      title_text <- if (is_first_row) as.character(m) else NULL
      # No data: create a placeholder plot
      p_blank <- ggplot() +
        # Set limits to match the row for alignment, but keep it blank
        # Use scale_y_log10 here too if using it in make_single_plot for consistency
        # coord_cartesian(ylim = current_ylim_row, expand = FALSE) +
        # Add "No Data" text in the center
        labs(title = title_text, x = NULL, y = NULL) + # Keep title for alignment, remove axis labels
        theme_void() + # Start with a completely blank theme
        theme(
          # Add a subtle background and border to indicate missing data visually
          plot.background = element_rect(fill = "grey95", color = "grey85", linetype = "dashed"),
          # Style the title identically to the actual plots for alignment
          plot.title = element_textbox_simple(
            size = 10, face = "bold", fill = "#0c93d2", color = "white",
            halign = 0.5, padding = margin(2, 2, 2, 2), margin = margin(b = 4)
          ),
          # Reduce margins for the placeholder plot as well
          plot.margin = margin(1, 1, 1, 1)
        )
      
      # Apply column-specific theme adjustments for axis alignment
      if (is_first_col) {
        # First column placeholder: Mimic y-axis space but keep labels/ticks invisible
        p_blank <- p_blank +
          theme(
            axis.text.y = element_text(color = "transparent", size=rel(0.8)), # Match size, make invisible
            axis.ticks.y = element_line(color = "transparent"), # Invisible ticks
            # Add a left margin to simulate axis title space if needed for perfect alignment
            plot.margin = margin(1, 1, 1, 5) # Add left margin
          )
      } else {
        # Not first column: Ensure no y-axis elements are present
        p_blank <- p_blank + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
      }
      p <- p_blank # Assign the created placeholder to 'p'
    }
    
    # Add the ID tag ONLY to the first plot of the row
    if (is_first_col) {
      p <- p +
        labs(tag = id_val) +
        theme(
          # place the tag at x = 0 (left edge), y = 0.5 (vert. mid‐panel)
          plot.tag.position = c(0, 0.5),
          # hjust = 0 makes the *left* of the text sit at x=0,
          # vjust = 0.5 centers it vertically around y=0.5
          plot.tag          = element_text(
            face  = "bold",
            size  = 10,
            hjust = 0.6,
            vjust = 0.5
          ),
          # give yourself a little left margin so it doesn’t bump the axis
          plot.margin       = margin(t = 1, r = 1, b = 1, l = 10)
        )
    }
    
    # Append the generated plot (or placeholder) to the list
    plot_list[[length(plot_list) + 1]] <- p
  }
  is_first_row <- FALSE
}

combined_plot_base <- wrap_plots(plot_list, ncol = length(months_to_plot)) +
  # Collect axis titles to the outer grid edges; keep tags associated with individual plots
  plot_layout(axis_titles = "collect", tag_level = 'keep')

final_plot <- ggdraw(combined_plot_base) +
  # move the x-label even lower (y < 0 pushes it outside the plot area)
  draw_label(
    "Local time (HH:MM)",
    x        = 0.5,
    y        = -0.01,    # <- pull down a bit further
    fontface = "bold",
    size     = 10
  ) +
  # move the y-label even farther left (x < 0 pushes it outside)
  draw_label(
    "Melanopic EDI (lux)",
    x        = -0.01,    # <- pull left a bit more
    y        = 0.5,
    angle    = 90,
    fontface = "bold",
    size     = 10
  ) +
  # ***don't forget the "+" here***
  theme(
    # add some extra room around the whole thing
    plot.margin = margin(t = 5, r = 5, b = 20, l = 20)
  ) +
  plot_annotation(
    title = "Median and Percentiles of Light Exposure per 30 Minutes per Study Month",
    theme = theme(
      plot.title = element_textbox_simple(
        size   = 14, face   = "bold",
        halign = 0.5, margin = margin(b = 15)
      )
    )
  ) &
  theme(
    # and re-apply your tag styling
    plot.tag          = element_text(face = "bold", size = 9, hjust = 0, vjust = 0.5),
    plot.tag.position = c(-0.02, 0.5)
  )

print(final_plot)

ggsave(
  filename = "result_26/Quarter3.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 16,                       # in inches (default)
  height   = 8,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)
