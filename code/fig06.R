
############################################################################
############################################################################
###                                                                      ###
###  CORRELATIONS ph_ecec, PH, AL3+, SUM OF BASE CATIONS                    ###
###                                                                      ###
############################################################################
############################################################################

# script author: Laura Summerauer

# remove objects in environment
rm(list = ls())

# load required packages
ld_pkgs <- c("tidyverse", "cowplot", "grid", "gridExtra")
lapply(ld_pkgs, library, character.only = TRUE)

# load plotting details
source("plotting_details.R")

#################################################################
##                    Load and prepare data                    ##
#################################################################

data <- read_csv("data/soildeg_data.csv") 

# filter for topsoils
data_rem <- data %>% 
  filter(mid_increment_depth_cm == 5) 

# define land_use and years_since_deforestation as factors

data_rem$land_use <- factor(data_rem$land_use, levels = c("forest", "cropland", "abandoned", "eucalyptus"),
                            labels = c("forest", "cropland", "abandoned", "eucalyptus"))

data_rem$years_since_deforestation <- factor(data_rem$years_since_deforestation, 
                                         levels = c("0", "2–7", "10–20", "40–60", "> 60"))

data_rem$H_cmolckg <- 10^-(data_rem$pH) * 100 
data_rem$Al_av_cmolckg <- data_rem$ECEC_cmolckg - data_rem$sum_bases_cmolckg - data_rem$H_cmolckg
data_rem$Al_av_cmolckg[data_rem$Al_av_cmolckg < 0] <- 0.1 # detection limit

##################################################################
##                         Mafic region                         ##
##################################################################


data_mafic_gathered <- data_rem %>%
  filter(geology == "mafic") %>%
  gather(bases_al, bases_al_value, "sum_bases_cmolckg", "Al_av_cmolckg") %>%
  gather(ph_ecec, ph_ecec_value, "pH", "ECEC_cmolckg")

stats_mafic <- data_mafic_gathered %>% 
  group_by(bases_al, ph_ecec) %>%
  summarize(r2  = summary(lm(ph_ecec_value ~ bases_al_value))$r.squared,
            p = summary(lm(ph_ecec_value ~ bases_al_value))$coefficients[2,4]) %>% 
  filter(p <= 0.05)

fit_mafic <- lm(data = data_rem[data_rem$geology == "mafic" & data_rem$slope_deg < 35,], sum_bases_cmolckg ~ ECEC_cmolckg)
summary(fit_mafic)

annotation_mafic <- mutate(stats_mafic,
                     r2 = as.character(as.expression(paste0("italic(R)^2 == ",
                                                            round(r2, 2)))),
                     p = ifelse(p >= 0.01, as.character(as.expression(paste0("italic(p) == ", round(p, 2)))),
                                as.character(as.expression(paste0("italic(p) < 0.01")))
                     )
)

# draw geom_smooth only for the signifcant correlations!  
data_smooth_mafic <- data_mafic_gathered
data_smooth_mafic$sign_corr <- with(data_mafic_gathered, interaction(bases_al, ph_ecec))
data_smooth_mafic <- data_smooth_mafic[!data_smooth_mafic$sign_corr == "Al_av_cmolckg.ECEC_cmolckg",]
data_smooth_mafic$sign_corr |> unique()

labels_mafic <- data_mafic_gathered %>% 
  select(bases_al, ph_ecec) %>% 
  group_by(bases_al, ph_ecec) %>% slice(1L) %>% ungroup() 

labels_mafic$label <- c("a", "b", "c", "d")



#################################################################
##                        Visualization                        ##
#################################################################


(p_mafic <- ggplot() +
    geom_smooth(data_smooth_mafic,
                mapping = aes(ph_ecec_value, bases_al_value), 
                se = TRUE, colour = "gray", alpha = 0.2, method = "lm") +
    geom_point(data_mafic_gathered, 
               mapping = aes(ph_ecec_value, bases_al_value, fill = years_since_deforestation, shape = land_use), 
               size = 3.5, alpha = 0.7, colour = "black")+
    scale_shape_manual("",  values = shape_values) +
    scale_fill_manual("", values = palette_all) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(override.aes = list(shape = 21), order = 2))+
    ggplot2::geom_text(inherit.aes = FALSE, data = annotation_mafic,
                       ggplot2::aes(x = Inf, y = -Inf, label = r2), size = 4.3,
                       hjust = 1,
                       vjust = -2,
                       parse = TRUE) +
    ggplot2::geom_text(inherit.aes = FALSE, data = annotation_mafic,
                       ggplot2::aes(x = Inf, y = -Inf, label = p), size = 4.3,
                       hjust = 1.1,
                       vjust = -0.8,
                       parse = TRUE) +
    ggplot2::geom_text(inherit.aes = FALSE, data = labels_mafic,
                       ggplot2::aes(x = -Inf, y = Inf, label = label), size = 6,
                       hjust = -0.5,
                       vjust = 1.5,
                       parse = TRUE) +
    facet_grid(bases_al ~ ph_ecec, scales = "free", switch = c("both"),
               labeller = labeller(bases_al =  as_labeller(facet_names_property, label_parsed),
                                   ph_ecec =  as_labeller(facet_names_property, label_parsed))) +
    ggh4x::facetted_pos_scales(
      x = list(
        ph_ecec == "ECEC_cmolckg" ~ scale_x_continuous(position = "bottom", breaks = seq(0, 27, 5), limits = c(0, 27)),
        ph_ecec == "pH" ~ scale_x_continuous(position = "bottom", breaks = seq(2.5, 7.5, 1), limits = c(2.5,7.5))
      ),
      y = list(
        bases_al == "sum_bases_cmolckg" ~ scale_y_continuous(breaks = seq(0, 27, 10), limits = c(-5, 27)),
        bases_al == "Al_av_cmolckg" ~ scale_y_continuous(breaks = seq(0, 8, 2), limits = c(-1.5, 8))
      )
    ) +
    xlab("") + ylab("")+
    ggtitle("mafic")+
    theme_ls +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          strip.text = element_text(size = 12),
          axis.text = element_text(size = 10))
)







#################################################################
##                        Felsic region                        ##
#################################################################



data_fel_gathered <- data_rem %>%
  filter(geology == "felsic") %>%
  gather(bases_al, bases_al_value, "sum_bases_cmolckg", "Al_av_cmolckg") %>%
  gather(ph_ecec, ph_ecec_value, "pH", "ECEC_cmolckg")

data_fel_gathered[data_fel_gathered$bases_al == "Al_av_cmolckg",]$bases_al_value |> range()


stats_felsic <- data_fel_gathered %>% 
  group_by(bases_al, ph_ecec) %>%
  summarize(r2  = summary(lm(ph_ecec_value ~ bases_al_value))$r.squared,
            p = summary(lm(ph_ecec_value ~ bases_al_value))$coefficients[2,4]) %>% 
  filter(p <= 0.05)

fit_felsic <- lm(data = data_rem[data_rem$geology == "felsic",], sum_bases_cmolckg ~ ECEC_cmolckg)
summary(fit_felsic)

annotation_felsic <- mutate(stats_felsic,
                     r2 = as.character(as.expression(paste0("italic(R)^2 == ",
                                                            round(r2, 2)))),
                     p = ifelse(p >= 0.01, as.character(as.expression(paste0("italic(p) == ", round(p, 2)))),
                                as.character(as.expression(paste0("italic(p) < 0.01")))
                     )
)


labels_felsic <- data_fel_gathered %>% 
  select(bases_al, ph_ecec) %>% 
  group_by(bases_al, ph_ecec) %>% slice(1L) %>% ungroup() 

labels_felsic$label <- c("e", "f", "g", "h")

# draw geom_smooth only for the signifcant correlations!  
data_smooth_felsic <- data_fel_gathered
data_smooth_felsic$sign_corr <- with(data_smooth_felsic, interaction(bases_al, ph_ecec))
data_smooth_felsic <- data_smooth_felsic[!data_smooth_felsic$sign_corr %in% c("Al_av_cmolckg.ECEC_cmolckg", "Al_av_cmolckg.pH"),]
data_smooth_felsic$sign_corr |> unique()



#################################################################
##                        Visualization                        ##
#################################################################


(p_felsic <- ggplot() +
    geom_smooth(data_smooth_felsic, 
                mapping = aes(ph_ecec_value, bases_al_value), 
                se = TRUE, colour = "gray", alpha = 0.2, method = "lm") +
    geom_point(data_fel_gathered, 
               mapping = aes(ph_ecec_value, bases_al_value, fill = years_since_deforestation, shape = land_use), 
               size = 3.5, alpha = 0.7, colour = "black")+
    scale_shape_manual("",  values = shape_values[-3]) +
    scale_fill_manual("", values = palette_all) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(override.aes = list(shape = 21), order = 2))+
    ggplot2::geom_text(inherit.aes = FALSE, data = annotation_felsic,
                       ggplot2::aes(x = Inf, y = -Inf, label = r2), size = 4.3,
                       hjust = 1,
                       vjust = -2,
                       parse = TRUE) +
    ggplot2::geom_text(inherit.aes = FALSE, data = annotation_felsic,
                       ggplot2::aes(x = Inf, y = -Inf, label = p), size = 4.3,
                       hjust = 1.1,
                       vjust = -0.8,
                       parse = TRUE) +
    ggplot2::geom_text(inherit.aes = FALSE, data = labels_felsic,
                       ggplot2::aes(x = -Inf, y = Inf, label = label), size = 6,
                       hjust = -0.5,
                       vjust = 1.5,
                       parse = TRUE) +
    facet_grid(bases_al ~ ph_ecec, scales = "free", switch = c("both"),
               labeller = labeller(bases_al =  as_labeller(facet_names_property, label_parsed),
                                   ph_ecec =  as_labeller(facet_names_property, label_parsed))) +
    # scale x and y axis same way as for COD plot
    ggh4x::facetted_pos_scales(
      x = list(
        ph_ecec == "ECEC_cmolckg" ~ scale_x_continuous(position = "bottom", breaks = seq(0, 27, 5), limits = c(0, 27)),
        ph_ecec == "pH" ~ scale_x_continuous(position = "bottom", breaks = seq(2.5, 7.5, 1), limits = c(2.5,7.5))
      ),
      y = list(
        bases_al == "sum_bases_cmolckg" ~ scale_y_continuous(breaks = seq(0, 27, 10), limits = c(-5, 27)),
        bases_al == "Al_av_cmolckg" ~ scale_y_continuous(breaks = seq(0, 8, 2), limits = c(-1.5, 8))
      )
    ) +
    xlab("") + ylab("")+
    ggtitle("felsic")+
    theme_ls +
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          strip.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          strip.text.x = element_text(size = 12),
          axis.text.x = element_text(size = 10)
          )
)




#################################################################
##           Get legend and arrange all plots in one           ##
#################################################################


(legend_plot <- ggplot() +

    geom_point(data_mafic_gathered, 
               mapping = aes(ph_ecec_value, bases_al_value, fill = years_since_deforestation, shape = land_use), 
               size = 3.5, alpha = 0.7, colour = "black")+
    scale_shape_manual("Land use",  values = shape_values) +
    scale_fill_manual("Years since deforestation   ", values = palette_all) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(override.aes = list(shape = 21), order = 2))+

    facet_grid(bases_al ~ ph_ecec, scales = "free", switch = c("both"),
               labeller = labeller(bases_al =  as_labeller(facet_names_property, label_parsed),
                                   ph_ecec =  as_labeller(facet_names_property, label_parsed)))+ 
    theme_ls
)



guidebox <- get_plot_component(legend_plot, pattern = "guide-box-top")
ggdraw(guidebox)

p_arr <- cowplot::plot_grid(p_mafic, NULL, p_felsic, rel_widths = c(1, -0.01, 1), nrow = 1)
p_legend <- cowplot::plot_grid(guidebox, p_arr, ncol = 1, rel_heights = c(0.09, 1))

# export plot as png file
ggsave(p_legend, filename = "out/fig06.png", height = 7, width = 11, bg = "white")


