
############################################################################
############################################################################
###                                                                      ###
###                           PLOT SOC AND FMC                           ###
###                                                                      ###
############################################################################
############################################################################

# script author: Laura Summerauer

# remove objects in environment
rm(list = ls())

# load required packages
library(tidyverse)

# load plotting details
source("plotting_details.R")



#################################################################
##                    Load and prepare data                    ##
#################################################################

all_joined <- read_csv("data/soildeg_data.csv") %>% 
  filter(!is.na(FMC))

all_joined$geology <- factor(all_joined$geology, levels = c("mafic", "felsic"))
all_joined$years_since_deforestation <- factor(all_joined$years_since_deforestation, 
                                               levels = c("0", "2–7", "10–20", "40–60", "> 60")) 
all_joined$land_use <- factor(all_joined$land_use, levels = c("forest", "cropland", "abandoned", "eucalyptus"))



#################################################################
##                        Visualization                        ##
#################################################################

(p_depth_FMC <- ggplot() +
  geom_path(all_joined[all_joined$set == "TropSOC",], mapping = aes(x = FMC, y = mid_increment_depth_cm, group = core_id, colour = years_since_deforestation)) +
  geom_point(all_joined[all_joined$set == "TropSOC",], mapping = aes(x = FMC, y = mid_increment_depth_cm, fill = years_since_deforestation, shape = land_use), size = 3, alpha = 0.8, colour = "black") +
  geom_path(all_joined[!all_joined$set == "TropSOC",], mapping = aes(x = FMC, y = mid_increment_depth_cm, group = core_id, colour = years_since_deforestation)) +
  geom_point(all_joined[!all_joined$set == "TropSOC",], mapping = aes(x = FMC, y = mid_increment_depth_cm, fill = years_since_deforestation, shape = land_use), size = 3, alpha = 0.8, colour = "black") +
  scale_fill_manual(values = palette_all[c(1,5)]) +
  scale_color_manual(values = palette_all[c(1,5)]) +
  scale_shape_manual(values = shape_values) +
  facet_grid(. ~geology) +
  xlab("Fm")+
  ylab("Depth (cm)")+
  scale_y_reverse()+
  theme_ls+
  theme(legend.position = "none"))
 

(p_depth_tc <- ggplot() +
    geom_path(all_joined[all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, group = core_id, colour = years_since_deforestation)) +
    geom_point(all_joined[all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, fill = years_since_deforestation, shape = land_use), size = 3, alpha = 0.8, colour = "black") +
    geom_path(all_joined[!all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, group = core_id, colour = years_since_deforestation)) +
    geom_point(all_joined[!all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, fill = years_since_deforestation, shape = land_use), size = 3, alpha = 0.8, colour = "black") +
    
    scale_fill_manual(values = palette_all[c(1,5)]) +
    scale_color_manual(values = palette_all[c(1,5)]) +
    scale_shape_manual(values = shape_values) +
    facet_grid(. ~geology) +
    # scale_colour_manual("", values = palette_all)+
    scale_x_continuous(limits = c(0, 95), breaks = seq(10, 95, 20), expand = c(0,0))+
    xlab(expression(paste("SOC (g ", kg^{-1}, ")")))+
    ylab("Depth cm")+
    scale_y_reverse()+
    theme_ls+
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text.x = element_blank()))



# arrange both plots in one 
(p_graphs_arr <- cowplot::plot_grid(p_depth_FMC, p_depth_tc,
                                    ncol = 1, align = "v",
                                    labels = c("a", "b"),
                                    label_y = c(0.94, 1),
                                    rel_heights = c(1,1)))

##################################################################
##                          Get legend                          ##
##################################################################

(legend.plot <- ggplot() +
    geom_path(all_joined[all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, group = core_id, colour = years_since_deforestation)) +
    geom_point(all_joined[all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, fill = years_since_deforestation, shape = land_use), size = 3, alpha = 0.8, colour = "black") +
    geom_path(all_joined[!all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, group = core_id, colour = years_since_deforestation)) +
    geom_point(all_joined[!all_joined$set == "TropSOC",], mapping = aes(x = TC_gkg, y = mid_increment_depth_cm, fill = years_since_deforestation, shape = land_use), size = 3, alpha = 0.8, colour = "black") +
    
    scale_fill_manual("Years since deforestation: ", values = palette_all[c(1,5)]) +
    scale_color_manual("Years since deforestation: ", values = palette_all[c(1,5)]) +
    scale_shape_manual("Land cover: ", values = shape_values) +
    guides(fill = guide_legend(override.aes = list(shape = 21), title.hjust = 1))+
    facet_grid(. ~geology) +
    # scale_colour_manual("", values = palette_all)+
    scale_x_continuous(limits = c(0, 95), breaks = seq(10, 95, 20), expand = c(0,0))+
    xlab(expression(paste("SOC g ", kg^{-1})))+
    ylab("Depth (cm)")+
    scale_y_reverse()+
    theme_ls+
    theme(legend.position = "top",
          strip.background = element_blank(),
          strip.text.x = element_blank()))

legend <- cowplot::get_plot_component(legend.plot, pattern = "guide-box-top")

p_legend <- cowplot::plot_grid(legend, NULL, p_graphs_arr, ncol = 1, rel_heights = c(0.09, -0.01, 1))
p_legend


# save plot
ggsave(p_legend, filename = "out/fig03.png", width = 7, height = 8, bg = "white")


