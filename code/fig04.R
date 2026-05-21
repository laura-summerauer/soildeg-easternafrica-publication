

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
library(ggtext)

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
cod_sl_set <- data |> 
  filter(sample_id %in% c(forest_congo$sample_id, slopes_congo$sample_id)) 


# prepare hillslope data
cod_slopes_congo <- cod_sl_set |> 
  filter(sample_id %in% slopes_congo$sample_id) |> 
  mutate(join_id = 1:nrow(slopes_congo)) |> 
  select(sample_id, TC_gkg, TCstocks_tha, join_id, land_use, years_since_deforestation) |> 
  rename(slope_TC_gkg = TC_gkg, 
         slope_TCstocks_tha = TCstocks_tha )


# prepare forest data
avg_forest_data <- cod_sl_set |> 
  filter(set == "TropSOC ref profiles") |> 
  group_by(set, mid_increment_depth_cm) |> 
  summarize(mean_forest_TCstocks_tha = mean(TCstocks_tha), 
            sd_forest_TCstocks_tha = sd(TCstocks_tha), 
            low_forest_TC_stocks_tha = mean_forest_TCstocks_tha - sd_forest_TCstocks_tha,
            high_forest_TC_stocks_tha = mean_forest_TCstocks_tha + sd_forest_TCstocks_tha,
            
            mean_forest_TC_gkg = mean(TC_gkg), 
            sd_forest_TC_gkg = sd(TC_gkg),
            high_forest_TC_gkg = mean_forest_TC_gkg + sd_forest_TC_gkg,
            low_forest_TC_gkg = mean_forest_TC_gkg - sd_forest_TC_gkg) |> 
  
  arrange(mid_increment_depth_cm) |> 
  mutate(cumstock_baseline = lag(cumsum(mean_forest_TCstocks_tha), default = 0),
         cumstock_high = lag(cumsum(high_forest_TC_stocks_tha), default = 0),
         cumstock_low = lag(cumsum(low_forest_TC_stocks_tha), default = 0))




get_depth <- function(site_soc, ref_soc, ref_depths) {
  ord <- order(ref_soc)
  approx(x = ref_soc[ord], y = ref_depths[ord], xout = site_soc, rule = 2)$y
}


get_loss <- function(site_soc, ref_soc, ref_cum_stock) {
  ord <- order(ref_soc)
  approx(x = ref_soc[ord], y = ref_cum_stock[ord], xout = site_soc, rule = 2)$y
}


results <- cod_slopes_congo |>
  filter(!years_since_deforestation == "10–20") |> 
  rowwise() |>
  mutate(

    depth_base = get_depth(site_soc = slope_TC_gkg, ref_soc = avg_forest_data$mean_forest_TC_gkg, ref_depths = avg_forest_data$mid_increment_depth_cm),
    loss_base = get_loss(site_soc = slope_TC_gkg, ref_soc = avg_forest_data$mean_forest_TC_gkg, ref_cum_stock = avg_forest_data$cumstock_baseline),
    
    depth_min  = get_depth(site_soc = slope_TC_gkg, ref_depths = avg_forest_data$low_forest_TC_gkg, ref_soc = avg_forest_data$mid_increment_depth_cm),
    loss_min  = get_loss(slope_TC_gkg, avg_forest_data$low_forest_TC_gkg, avg_forest_data$cumstock_low),
    
    depth_max  = get_depth(slope_TC_gkg, avg_forest_data$high_forest_TC_gkg, avg_forest_data$mid_increment_depth_cm),
    loss_max  = get_loss(slope_TC_gkg, avg_forest_data$high_forest_TC_gkg, avg_forest_data$cumstock_high)
  )




depth_grid <- seq(min(avg_forest_data$mid_increment_depth_cm), max(avg_forest_data$mid_increment_depth_cm), length.out = 500)

forest_fit <- data.frame(
  depth = depth_grid,
  soc_pct = approx(avg_forest_data$mid_increment_depth_cm, avg_forest_data$mean_forest_TC_gkg, xout = depth_grid, rule = 2)$y,
  soc_low = approx(avg_forest_data$mid_increment_depth_cm, avg_forest_data$low_forest_TC_gkg, xout = depth_grid, rule = 2)$y,
  soc_high = approx(avg_forest_data$mid_increment_depth_cm, avg_forest_data$high_forest_TC_gkg, xout = depth_grid, rule = 2)$y
)



plot_summary <- results |>
  filter(!years_since_deforestation == "10–20") |> 
  group_by(land_use, years_since_deforestation)  |> 
  summarise(
    n_val = n(),
    mean_soc = mean(slope_TC_gkg),
    mean_depth = mean(depth_base),
    mean_loss = mean(loss_base),
    
    # propagated SE for Depth (vertical)
    se_depth_spatial = ifelse(n() > 1, sd(depth_base) / sqrt(n()), 0),
    se_depth_ref = mean((depth_max - depth_min) / 2) / sqrt(n()),
    total_se_depth = sqrt(se_depth_spatial^2 + se_depth_ref^2),
    
    # propagated SE for Stock loss
    se_loss_spatial = ifelse(n() > 1, sd(loss_base) / sqrt(n()), 0),
    se_loss_ref = mean((loss_max - loss_min) / 2) / sqrt(n()),
    total_se_loss = sqrt(se_loss_spatial^2 + se_loss_ref^2),
    
    # SE for SOC (horizontal)
    total_se_soc = ifelse(n() > 1, sd(slope_TC_gkg) / sqrt(n()), 0),
    
    .groups = 'drop'
  ) 


results$years_since_deforestation <- factor(results$years_since_deforestation, 
                                           levels = c("0", "2–7", "10–20", "40–60", "> 60"))
results$land_use <- factor(results$land_use, levels = c("cropland",  "abandoned", "eucalyptus"))

plot_summary$years_since_deforestation <- factor(plot_summary$years_since_deforestation, 
                                            levels = c("0", "2–7", "10–20", "40–60", "> 60"))
plot_summary$land_use <- factor(plot_summary$land_use, levels = c("cropland",  "abandoned", "eucalyptus"))




calib <- plot_summary |> 
  filter(years_since_deforestation == "40–60") |> 
  filter(land_use == "cropland") |> 
  mutate(t_mid = 50, t_err = 10) |> 
  summarise(
    t_mid = t_mid,
    t_err = t_err,
    r_mean = mean_depth / t_mid,
    r_err  = r_mean * sqrt((total_se_depth / mean_depth)^2 + (t_err / t_mid)^2)
  )


plot_summary_label <- plot_summary |>

  mutate(
    n_label = paste0("<i>n</i> = ", n_val, "<br>"),
    
    stock_label = paste0("Loss: ", round(mean_loss, 1), " ± ", round(total_se_loss, 1),
                        " t C ha<sup>-1</sup>"),
    
    age_fixed = ifelse(years_since_deforestation == "40–60", 
                       paste0("<br>Age: ", calib$t_mid, " \u00b1 ", calib$t_err, " y"), ""),
    
    rate_info = ifelse(years_since_deforestation == "40–60" & land_use == "cropland",
                       paste0("<br>Rate: ", round(calib$r_mean, 2), 
                              " \u00b1 ", round(calib$r_err, 2), " cm yr<sup>-1</sup>"), ""),
    
    age_est_mean = mean_depth / calib$r_mean,
    age_est_err  = age_est_mean * sqrt((total_se_depth / mean_depth)^2 + (calib$r_err / calib$r_mean)^2),
    
    age_info = ifelse(years_since_deforestation == "> 60",
                      paste0("<br>Est. Age: ", round(age_est_mean), 
                             " \u00b1 ", round(age_est_err), " yr"), ""),
    
    final_label = paste0(n_label, stock_label, age_fixed, rate_info, age_info)
  ) 





(p <- ggplot() +
    
  # Forest data
  geom_ribbon(data = forest_fit, aes(x = depth, ymin = soc_low, ymax = soc_high), 
              fill = "#073814", alpha = 0.1) +
  geom_line(data = forest_fit, aes(x = depth, y = soc_pct), 
            color = "#073814", linewidth = 0.5, alpha = 0.3) +
  geom_point(data = avg_forest_data, aes(x = mid_increment_depth_cm, y = mean_forest_TC_gkg), 
             size = 1, fill = "#073814", shape = 24, alpha = 0.3) +
  
  
  # Hillslopes
  geom_point(data = results, aes(x = depth_base, y = slope_TC_gkg, 
                                 fill = years_since_deforestation, shape = land_use), 
             alpha = 0.3, size = 3) +
  
  # error bars and points of mean values
  geom_errorbarh(data = plot_summary, 
                 aes(y = mean_soc, 
                     xmin = mean_depth - total_se_depth, 
                     xmax = mean_depth + total_se_depth, 
                     color = years_since_deforestation),
                 height = 1.2, linewidth = 1) +
  
  geom_errorbar(data = plot_summary, 
                aes(x = mean_depth, 
                    ymin = mean_soc - total_se_soc, 
                    ymax = mean_soc + total_se_soc, 
                    color = years_since_deforestation),
                width = 1.2, linewidth = 1) +

  geom_point(data = plot_summary, aes(x = mean_depth, y = mean_soc, 
                                      fill = years_since_deforestation, shape = land_use), 
             size = 4) +
    
    geom_richtext(data = plot_summary_label,
                  aes(x = ifelse(years_since_deforestation == "> 60" & land_use == "eucalyptus", 
                                 mean_depth + 7, mean_depth),
                      y = ifelse(years_since_deforestation == "2–7" & land_use == "cropland", 
                                 mean_soc - 60, mean_soc),
                      label = final_label,
                      fill = years_since_deforestation),
                  nudge_x = -1,
                  nudge_y = 5,
                  alpha = 1,
                  hjust = 0,
                  size = 3,
                  text.color = "black",
                  label.color = NA,
                  lineheight = 1.1,
                  show.legend = FALSE) +
  
  scale_fill_manual("Years since deforestation: ", 
                    values = palette_withoutforest[c(1,3,4,2)],
                    labels = function(x) gsub(">", "&gt;", x)) +
  scale_colour_manual("Years since deforestation: ",
                      values = palette_withoutforest[c(1,3,4,2)],
                      labels = function(x) gsub(">", "&gt;", x))+
  scale_shape_manual("Land use: ", values = shape_values_withoutforest,
                     labels = c("cropland", "abandoned", "*Eucalyptus*"))+
    guides(fill = guide_legend(override.aes = list(shape = 21)))+
  
  facet_grid(. ~ land_use,
             labeller = as_labeller(
               c("cropland" = "cropland",
                 "abandoned" = "abandoned",
               "eucalyptus" = "*Eucalyptus*"))) +
  
  scale_x_reverse(breaks = seq(0, 100, 10)) + 
  ylab(expression(paste("SOC (g ", kg^{-1}, ")")))+
  xlab("Equivalent depth in forest profile (cm)")+
  coord_flip() +
  theme_ls +
  theme(legend.position = "bottom",
        strip.text = ggtext::element_markdown(size = 12),
        strip.background = element_rect(fill = "transparent", color = NA),
        legend.text = ggtext::element_markdown(size = 10),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12))
)


p

# ggsave(p, filename = "out/fig04_ed.png", width = 9.5, height = 6)
