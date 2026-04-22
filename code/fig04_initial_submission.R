

############################################################################
############################################################################
###                                                                      ###
###  CALCULATE LOST SOC ON CLEARED HILLSLOPES                            ###
###                                                                      ###
############################################################################
############################################################################

# script author: Laura Summerauer

# remove objects in environment
rm(list = ls())

# load required package
library(tidyverse)

# load plotting theme details
source("code/plotting_details.R")



#################################################################
##                    Load and prepare data                    ##
#################################################################

data <- read.csv("data/soildeg_data.csv") 

# calculate TC stocks in tons per hectare
data$TCstocks_tha <- data$BD_gcm3 * data$increment_length_cm *  data$TC_gkg / 10  # in kg/ha

# subset the data 
forest_congo <- data[data$geology == "mafic" & data$set == "TropSOC ref profiles" & data$land_use == "forest",]
slopes_congo <- data[data$set == "slope" & data$geology == "mafic" & data$mid_increment_depth_cm == 5,]

# congo forest and slope data
cod_sl_set <- data %>% 
  filter(sample_id %in% c(forest_congo$sample_id, slopes_congo$sample_id)) 

# prepare hillslope data
cod_slopes_congo <- cod_sl_set %>% 
  filter(sample_id %in% slopes_congo$sample_id) %>% 
  mutate(join_id = 1:nrow(slopes_congo)) %>% 
  select(sample_id, TC_gkg, TCstocks_tha, join_id) %>% 
  rename(slope_TC_gkg = TC_gkg, 
         slope_TCstocks_tha = TCstocks_tha )

# prepare forest data
avg_forest_data <- cod_sl_set %>% 
  filter(set == "TropSOC ref profiles") %>% 
  group_by(set, mid_increment_depth_cm) %>% 
  summarize(mean_forest_TCstocks_tha = mean(TCstocks_tha), sd_forest_TCstocks_tha = sd(TCstocks_tha), 
            mean_forest_TC_gkg = mean(TC_gkg), sd_forest_TC_gkg = sd(TC_gkg)) %>% 
  arrange(mid_increment_depth_cm)


av_forest_data_ext <- do.call("rbind", replicate(length(unique(cod_slopes_congo$sample_id)), avg_forest_data, simplify = FALSE))
av_forest_data_ext$join_id <- rep(1:51, each = 10) 


# merge forest and slope and calculate differences in TC content for each forest soil depth
df_diff_calc <- inner_join(av_forest_data_ext, cod_slopes_congo, by = "join_id") %>% 
  mutate(abs_diff = abs(mean_forest_TC_gkg - slope_TC_gkg), 
         real_diff = mean_forest_TC_gkg - slope_TC_gkg)

# filter for minimum absolute difference and filter for the corresponding depth in the forest profile
filtered_min_depth <- df_diff_calc %>% 
  group_by(sample_id) %>% 
  filter(abs_diff == min(abs_diff)) %>% 
  select(sample_id, mid_increment_depth_cm)



###################################################################################
##  Calculate sum of SOC stocks in forest above the previously calculated depth  ##
###################################################################################


summarized <- list()

for (slope in filtered_min_depth$sample_id){
  
  forest_depth <- filtered_min_depth$mid_increment_depth_cm[filtered_min_depth$sample_id == slope]
  test_diff <- with(df_diff_calc[df_diff_calc$sample_id == slope & df_diff_calc$mid_increment_depth_cm == 5,], mean_forest_TCstocks_tha - slope_TCstocks_tha)
  
  if(test_diff < 0) {
    df_diff_calc_sub <- df_diff_calc[df_diff_calc$sample_id == slope & df_diff_calc$mid_increment_depth_cm == forest_depth,]
    sum_c_stocks <- df_diff_calc_sub$mean_forest_TCstocks_tha - df_diff_calc_sub$slope_TCstocks_tha
    sum_sd_c_stocks <- df_diff_calc_sub$sd_forest_TCstocks_tha
    note <- "difference of stocks on surface"
  } else {
    df_diff_calc_sub <- df_diff_calc[df_diff_calc$sample_id == slope & df_diff_calc$mid_increment_depth_cm <= forest_depth,]
    sum_c_stocks <- sum(df_diff_calc_sub$mean_forest_TCstocks_tha)
    sum_sd_c_stocks <- sum(df_diff_calc_sub$sd_forest_TCstocks_tha)
    note <- "summ forest stocks above"
  }

  summarized[[slope]] <- data.frame(
    "depth" = forest_depth,
    "id" = slope,
    "sum_c_stocks" = sum_c_stocks,
    "sum_sd_c_stocks" = sum_sd_c_stocks,
    "note" = note
    )
  
}


# create a dataframe for subsequent visualization
df_summ <- do.call("rbind", summarized) |> as.data.frame()

# merge with initial data
data_sel <- data[,c("sample_id", "core_id", "land_use", "years_since_deforestation")]
data_sel$land_use <- factor(data_sel$land_use, levels = c("cropland", "abandoned", "eucalyptus"))
data_sel$years_since_deforestation <- factor(data_sel$years_since_deforestation, 
                                           levels = c("2–7", "10–20", "40–60", "> 60"))

sum_def_stocks <- inner_join(df_summ, data_sel, by = c("id" = "sample_id")) %>% 
  mutate(sum_c_stocks = sum_c_stocks)



#################################################################
##                        Visualization                        ##
#################################################################


(p_depth <- ggplot(sum_def_stocks, 
                   mapping = aes(x = years_since_deforestation, 
                                 y = depth, 
                                 fill = years_since_deforestation)) + 
  geom_boxplot(linewidth = 0.3, notch = FALSE) +
    geom_jitter(shape = 21, alpha = 0.5) +
  facet_grid(.~land_use, scales = "free", space = "free_x")+
  scale_fill_manual(values = palette_withoutforest) +
  xlab("")+
  ylab("Depth in forest profile (cm)") +
  theme_ls +
  theme(legend.position = "none",
        axis.text.x = element_blank(),
        strip.text = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))
)


(p_stocks <-  ggplot(sum_def_stocks, aes(x = years_since_deforestation, y = sum_c_stocks, fill = years_since_deforestation)) + 
  geom_boxplot(linewidth = 0.3) +
  geom_jitter(shape = 21, alpha = 0.5) +
  facet_grid(.~land_use, scales = "free", space = "free_x")+
   scale_fill_manual(values = palette_withoutforest) +
  xlab("Years since deforestation")+
  ylab(expression(paste("Lost carbon stocks (t ", ha^{-1}, ")"))) +
  theme_ls +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
  )


# save plot
p_both <- cowplot::plot_grid(p_depth, p_stocks, nrow = 2, align = "v")
p_both
# ggsave(p_both, filename = "out/fig04.png", width = 7, height = 6)









library(boot)

# Function to calculate Bootstrap SE of the Median
get_median_se <- function(x, R = 2000) {
  if(length(x) < 4) return(NA) # Kirchner: uncertainty needs enough data
  b <- boot(x, function(d, i) median(d[i]), R = R)
  return(sd(b$t)) # The SD of the bootstrap distribution is the SE
}

# Calculate for your data table
summary_table <- sum_def_stocks %>%
  group_by(years_since_deforestation, land_use) %>%
  summarise(
    n = n(),
    Median = median(sum_c_stocks),
    SE_Median = get_median_se(sum_c_stocks)
  )


print(summary_table)



summary_results <- sum_def_stocks %>%
  group_by(years_since_deforestation, land_use) %>%
  summarise(
    n = n(),
    Mean = mean(sum_c_stocks, na.rm = TRUE),
    # Standard Deviation / Square Root of n
    SE = sd(sum_c_stocks, na.rm = TRUE) / sqrt(n())
  ) %>%
  # Rounding for a clean report
  mutate(across(c(Mean, SE), \(x) round(x, 2)))

print(summary_results)





library(ggtext)


# 1. Calculate the total range of your data first
depth_range <- diff(range(sum_def_stocks$depth, na.rm = TRUE))
# We want the label to sit roughly 7% of the total plot height above each box
offset.depth <- depth_range * 0.07 


depth.label_stats_fixed_gap <- function(x) {
  n_val <- length(x)
  m_val <- round(mean(x, na.rm = TRUE), 1)
  s_val <- round(sd(x, na.rm = TRUE) / sqrt(n_val), 1)
  
  txt <- if(n_val < 2) {
    sprintf("%.1f<br><i>n</i> = %d", m_val, n_val)
  } else {
    sprintf("%.1f &plusmn; %.1f<br><i>n</i> = %d", m_val, s_val, n_val)
  }
  
  # FIX: Use a constant offset instead of a percentage multiplier
  # This makes the "air" above every box look exactly the same
  return(data.frame(y = max(x, na.rm = TRUE) + offset.depth, label = txt))
}




(p_depth <- ggplot(sum_def_stocks,
                   mapping = aes(x = years_since_deforestation,
                                 y = depth,
                                 fill = years_since_deforestation)) +
    geom_boxplot(linewidth = 0.3, notch = FALSE) +
    geom_jitter(shape = 21, alpha = 0.5) +
    stat_summary(fun.data = depth.label_stats_fixed_gap, 
                 geom = "richtext", 
                 fill = NA,          # Remove background box
                 label.color = NA,   # Remove border
                 size = 3.5, 
                 lineheight = 0.8) + # ADJUST THIS to tighten/loosen the rows
    facet_grid(.~land_use, scales = "free", space = "free_x")+
    scale_fill_manual(values = palette_withoutforest) +
    xlab("")+
    ylab("Depth in forest profile (cm)") +
    theme_ls +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          strip.text = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12))
)






# 1. Calculate the total range of your data first
stock.range <- diff(range(sum_def_stocks$sum_c_stocks, na.rm = TRUE))
# We want the label to sit roughly 7% of the total plot height above each box
stock.offset <- stock.range * 0.07 


stock.label_stats_fixed_gap <- function(x) {
  n_val <- length(x)
  m_val <- round(mean(x, na.rm = TRUE), 1)
  s_val <- round(sd(x, na.rm = TRUE) / sqrt(n_val), 1)
  
  txt <- if(n_val < 2) {
    sprintf("%.1f<br><i>n</i> = %d", m_val, n_val)
  } else {
    sprintf("%.1f &plusmn; %.1f<br><i>n</i> = %d", m_val, s_val, n_val)
  }
  
  # FIX: Use a constant offset instead of a percentage multiplier
  # This makes the "air" above every box look exactly the same
  return(data.frame(y = max(x, na.rm = TRUE) + stock.offset, label = txt))
}


(p_stocks <-  ggplot(sum_def_stocks, 
                     mapping = aes(x = years_since_deforestation, 
                                   y = sum_c_stocks, 
                                   fill = years_since_deforestation)) + 
    geom_boxplot(linewidth = 0.3) +
    geom_jitter(shape = 21, alpha = 0.5) +
    # Use geom_richtext from ggtext for HTML support
    stat_summary(fun.data = stock.label_stats_fixed_gap, 
                 geom = "richtext", 
                 fill = NA,          # Remove background box
                 label.color = NA,   # Remove border
                 size = 3.5, 
                 lineheight = 0.8) + # ADJUST THIS to tighten/loosen the rows
    facet_grid(.~land_use, scales = "free", space = "free_x")+
    scale_fill_manual(values = palette_withoutforest) +
    xlab("Years since deforestation")+
    ylab(expression(paste("Lost carbon stocks (t ", ha^{-1}, ")"))) +
    theme_ls +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
)

p_both <- cowplot::plot_grid(p_depth, p_stocks, nrow = 2, align = "v")
p_both
