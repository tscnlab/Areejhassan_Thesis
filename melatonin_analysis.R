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
library(ggpubr) 
library(hms)

# Load sample data (provided with the package)
file_cortisol <- "output_data/output_updated_data_v4.xlsx"
data_cortisol <- read_excel(file_cortisol)

present_data <- readRDS("input_data/lightLogR/present_data_2025.rds")
present_data_filtered <- present_data[present_data$reason == "present",]


unique_id <- sort(unique(data_cortisol$ID))
df_correlation <- c()
time_duration <- seq(from = 0, to = 19, length.out = 20)
# Initialize an empty dataframe to store correlations
correlation_df <- data.frame()
temp_df <- data.frame()
df_box_plot <- data.frame()
dlmo_plots <- list()
threshold <- log(250)
light_data_id <- data.frame()
melatonin_threshold <- 3
df_melatonin_data_analysis <- data.frame()

get_mode <- function(x) {
  uniq_x <- unique(x)  # Get unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))]  # Find the value that occurs most
}

calculate_auc <- function(cortisol) {
  auc <- 0
  for (i in 1:(length(cortisol) - 1)) {
    auc <- auc + (0.5 * (cortisol[i] + cortisol[i + 1]) * (30))
  }
  return(auc)
}

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
    
    data_month_id_cortosol <- data_saliva_id %>% filter(ymd(date, tz = "Europe/Berlin") == unique_date + lubridate::days(1))
    

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
      
      dlmo_result <- calculate_dlmo(data = melatonin_complete_cases, threshold = melatonin_threshold, fine_flag = FALSE)
      melatonin_value <- as.numeric(data_month_id_melatonin$melatonin[1:11])
      
      # Calculate cortisol AUC
      cortisol_value <- as.numeric(tail(data_month_id_cortosol$`cortisol (ng/mL)`, 4))
      auc <- NA
      if (length(cortisol_value) > 0){
        auc <- calculate_auc(cortisol_value)
      }
      # Store the metrics and AUC in a temporary dataframe
      new_row <- list(c(
        participant_ID,
        auc,
        as.numeric(dlmo_result$dlmo$coarse$time),
        dlmo_result$dlmo$coarse$fit_melatonin,
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
      dlmo_plots <- append(dlmo_plots, list(rbind, dlmo_result))
      temp_df <- bind_rows(temp_df, data.frame(do.call(rbind, new_row)))
      df_box_plot <- bind_rows(df_box_plot, data.frame(do.call(rbind, list(c(participant_ID, melatonin_value,
                                                                             as.numeric(dlmo_result$dlmo$coarse$time),
                                                                             dlmo_result$dlmo$coarse$fit_melatonin,
                                                                             mean(light_data_month$MEDI),
                                                                             median(light_data_month$MEDI),
                                                                             get_mode(light_data_month$MEDI))))))
    }
  }
}
# colnames(df_box_plot) <- c("ID", "m1" , "m2", "m3", "m4", "AUC", "mean", "median", "mode", 
#                            "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11")

colnames(df_box_plot) <- c("ID","e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "e10", "e11",
                           "melatonin_onset_time","melatonin_onset_value","mean", "median", "mode")

colnames(temp_df) <- c(
  "ID",
  "AUC",
  "melatonin_onset_time","melatonin_onset_value",
  "duration_above_threshold", "frequency", "period_above_threshold",
  "pulse_above_threshold", "pulse_above_threshold_mean_level",
  "pulse_above_threshold_mean_duration", "pulse_above_threshold_total_duration", "mean_time_above_threshold",
  "first_timing_above_threshold", "last_timing_above_threshold", "centroid", "mid_point"
)

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

########################### Correlation Matrix #####################################################
correlation_df <- as.data.frame(lapply(temp_df, type.convert, as.is = TRUE))
correlation_df <- na.omit(correlation_df)

correlation_df_wo_auc <- correlation_df[ , !(names(correlation_df) %in% "AUC") ]
# cor_matrix <- cor(correlation_df_wo_auc[ , -1 ])
# p.mat <- cor.mtest(correlation_df_wo_auc[,-1])

cor_matrix <- cor(correlation_df[ , -1 ])
p.mat <- cor.mtest(correlation_df[,-1])


png(
  filename = "result_26/correlation_matrix_habitual_melatonin_auc.png",
  width    = 10,     # width in inches
  height   = 8,     # height in inches
  units    = "in",
  res      = 300    # resolution in dots per inch
)

alpha <- 0.05

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
  main   = "\nCorrelation Matrix for Habitual Melatonin Onset, AUC and Light Metrics",
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
###############################Box Plots############################################################
temp_df_clean <- temp_df
temp_df_clean$melatonin_onset_time<-hms(as.numeric(temp_df_clean$melatonin_onset_time))
temp_df_clean$melatonin_onset_value<-as.numeric(temp_df_clean$melatonin_onset_value)
fences <- temp_df_clean %>% 
  group_by(ID) %>% 
  summarize(
    lower = boxplot.stats(melatonin_onset_value)$stats[1],
    upper = boxplot.stats(melatonin_onset_value)$stats[5]
  )

# 2. Join them back and filter
temp_df_clean <- temp_df_clean %>% 
  left_join(fences, by = "ID") %>% 
  filter(
    melatonin_onset_value >= lower,
    melatonin_onset_value <= upper
  )
#######################################################################################################
df_box_plot[df_box_plot == "NA"] <- NA
df_box_plot <- na.omit(df_box_plot)
df_box_plot[] <- lapply(df_box_plot, function(x) as.numeric(as.character(x)))
df_box_plot$ID <- as.factor(df_box_plot$ID)

df_box_plot <- subset(df_box_plot, melatonin_onset_time > as.numeric(as_hms("16:00:00")))

ggplot(df_box_plot, 
       aes(x = melatonin_onset_time, 
           y = melatonin_onset_value)) +
  
  # your points
  geom_point(aes(color = ID), size = 2, alpha = 0.8) +
  
  # add linear regression line + 95% CI
  geom_smooth(method = "lm", se = TRUE, color = "blue") +  # :contentReference[oaicite:0]{index=0}
  
  # add correlation coefficient and p-value
  stat_cor(
    aes(label = paste(..r.label.., ..p.label.., sep = "~`,`~")),
    method    = "pearson",
    label.x.npc = "left",    # position at left
    label.y.npc = "top",     # position at top
    output.type = "expression"
  ) +                        # :contentReference[oaicite:1]{index=1}
  
  scale_x_continuous(labels = as_hms(seq(
    as.POSIXct("2025-04-28 17:00", tz="UTC"),
    as.POSIXct("2025-04-29 00:00", tz="UTC"),
    by = "1 hour"
  )),
  breaks = as_hms(seq(
    as.POSIXct("2025-04-28 17:00", tz="UTC"),
    as.POSIXct("2025-04-29 00:00", tz="UTC"),
    by = "1 hour"
  )),
  limits = as_hms(c(
    as.POSIXct("2025-04-28 17:00", tz="UTC"),
    as.POSIXct("2025-04-29 21:00", tz="UTC")
  ))
  ) +                  
  
  labs(
    title = paste0("Onset Value vs Onset Time (N = ", nrow(df_box_plot), ")"),
    x     = "Melatonin Onset Time",
    y     = "Melatonin Onset Value (pg/mL)",
    color = "ID"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )



ggsave(
  filename = "result_26/scatter_plot_onset_time_onset_value.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 6,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)

##########################################################################################
ct <- cor.test(
  x = as.numeric(df_box_plot$melatonin_onset_time),
  y = df_box_plot$melatonin_onset_value,
  method = "pearson"
)

r_val <- signif(ct$estimate, 2)
p_val <- ct$p.value
label <- if(p_val > 0.05) {
  paste0("R = ", r_val, ", p = ", signif(p_val, 2)," (>0.05)")
} else {
  paste0("R = ", r_val, ", p = ", signif(p_val, 2))
}

# 2. plot and annotate
ggplot(df_box_plot, aes(x = melatonin_onset_time, y = melatonin_onset_value)) +
  geom_point(aes(color = ID), size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  
  # add our custom label in the top-left corner
  annotate("text",
           x    = min(df_box_plot$melatonin_onset_time),
           y    = max(df_box_plot$melatonin_onset_value),
           label= label,
           hjust= 0, vjust = 1.2,
           size = 4) +
  
  scale_x_continuous(labels = as_hms) +
  labs(
    title = paste0("Onset Value vs Onset Time (N = ", nrow(df_box_plot), ")"),
    x     = "Melatonin Onset Time",
    y     = "Melatonin Onset Value (pg/mL)",
    color = "ID"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10))
  )
ggsave(
  filename = "figure/scatter_plot_onset_time_onset_value.png",   # or .tiff, .jpg, .pdf, .svg …
  plot     = last_plot(),             # or plot = p
  width    = 8,                       # in inches (default)
  height   = 6,
  units    = "in",                    # "in", "cm", or "mm"
  dpi      = 300,                      # dots per inch; 300+ is print quality
  bg = "white"
)



##############################################################################################
df_long <- df_box_plot[,c("ID", "melatonin_onset_time")] %>%
  pivot_longer(cols = c(melatonin_onset_time), names_to = "variable")
ggplot(df_long, aes(x = variable, y = value, fill = variable)) +
  geom_violin(color = "black",
              fill  = "#d6eaf8",
              size  = 0.8,
              trim  = FALSE,
              adjust = 1.5 ) + 
  geom_boxplot(width     = 0.4,
               position  = position_nudge(x = 0.2),
               outlier.shape = NA,
               color    = "black",
               fill     = "white",
               size     = 0.8) +
  geom_point(aes(color = ID),
             position = position_jitter(width = 0.1),
             alpha    = 0.7,
             size     = 2) +
  scale_y_continuous(
    labels = as_hms,                # <-- convert seconds → "HH:MM:SS"
    name   = "Melatonin Onset Time" # relabel axis
  ) +
  labs(title = "Distribution of Melatonin Onset",
       x     = "Variable") +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    axis.title.x  = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y  = element_text(face = "bold", margin = margin(r = 10))
  )
ggsave("results_15_04/violen_plot_onset_time.png", bg = "white")



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

############################### Light Plots #########################################################
save_plot_for_id <- function(id) {
  plot <- light_data_id %>%
    filter(Id == id) %>%
    gg_day(aes_col = MEDI < 250, size = 0.75) + 
    geom_line(aes(y = PIM), color = "red", alpha = 0.2) +
    theme(legend.position = "bottom")
  
  # Save the plot with the ID in the filename
  ggsave(filename = paste0("results_15_04/melatonin_plot_", id, ".png"), plot = plot, width = 8, height = 16, dpi = 300, bg = "white")
}
unique_ids <- unique(light_data_id$Id)
walk(unique_ids, save_plot_for_id)
###########################################################################################################

ggsave(filename = paste0("results_15_04/melatonin_onset_101_07_05_2024.png"),
       plot = dlmo_plots[[2]], width = 8, height = 8, dpi = 300, bg = "white")

