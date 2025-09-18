
###########################################################################
###########################################################################
###                                                                     ###
###       CORRELATIONS OF SOC WITH ECEC, ALPYOX, FEPYOX, AND CLAY       ###
###                                                                     ###
###########################################################################
###########################################################################

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

data <- read_csv("data/soildeg_data.csv") 

# filtered
data_top <- data %>% 
  filter(mid_increment_depth_cm == 5) 

# define variables as factors
data_top$land_use <- factor(data_top$land_use, levels = c("forest", "cropland", "abandoned", "eucalyptus"),
                            labels = c("forest", "cropland", "abandoned", "eucalyptus"))

data_top$years_since_deforestation <- factor(data_top$years_since_deforestation, 
                                         levels = c("0", "2–7", "10–20", "40–60", "> 60"))

# gather data for subsequent calculations and plotting
data_top_gath <- data_top %>%
  gather(property, value, "ECEC_cmolckg", "Al_pyox_perc", "Fe_pyox_perc", "clay_perc") 

data_top_gath$property <- factor(data_top_gath$property, 
                                  levels = c("ECEC_cmolckg", "Al_pyox_perc", "Fe_pyox_perc", "clay_perc"))

stats <- data_top_gath %>% 
  group_by(geology, property) %>%
  summarize(r2  = round(summary(lm(TC_gkg ~ value))$r.squared, digits = 2),
            p = round(summary(lm(TC_gkg ~ value))$coefficients[2,4], digits = 2)) %>% 
  filter(p <= 0.05)



annotation <- mutate(stats,
                     r2 = as.character(as.expression(paste0("italic(R)^2 == ",
                                                            round(r2, 2)))),
                     p = ifelse(p >= 0.01, as.character(as.expression(paste0("italic(p) == ", round(p, 2)))),
                                as.character(as.expression(paste0("italic(p) < 0.01")))
                     ))


(p <- ggplot() +
    geom_smooth(data_top_gath[data_top_gath$property == stats$property & data_top_gath$geology == stats$geology,], mapping = aes(value, TC_gkg), se = TRUE, colour = "gray", alpha = 0.2, method = "lm") +
    geom_point(data_top_gath, mapping = aes(value, TC_gkg, fill = years_since_deforestation, shape = land_use),
               size = 3.5, alpha = 0.7, colour = "black")+
    scale_shape_manual("Land use",  values = shape_values) +
    scale_fill_manual("Years since deforestation", values = palette_all) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(override.aes = list(shape = 21), order = 2))+
    ggplot2::geom_text(inherit.aes = FALSE, data = annotation,
                       ggplot2::aes(x = -Inf, y = Inf, label = r2), size = 4,
                       hjust = -0.1,
                       vjust = 1.2,
                       parse = TRUE) +
    ggplot2::geom_text(inherit.aes = FALSE, data = annotation,
                       ggplot2::aes(x = -Inf, y = Inf, label = p), size = 4,
                       hjust = -0.1,
                       vjust = 2.5,
                       parse = TRUE) +
    facet_grid(factor(geology, levels = c("mafic", "felsic")) ~ property, scales = "free_x", switch = "x",
               labeller = labeller(property =  as_labeller(facet_names_property, label_parsed),
               )) +
    xlab("") +
    ylab(expression(paste("SOC (g ", kg^{-1}, ")")))+
    theme_ls +
    theme(strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10))
)

ggsave(p, filename = "out/fig07.png", height = 5.5, width = 9)

