
############################################################################
############################################################################
###                                                                      ###
###                      CODE TO REPRODUCE FIGURE 5                      ###
###                                                                      ###
############################################################################
############################################################################

# Date: December 2023; updated: July 2025
# Script author: Laura Summerauer

# remove objects in environment
rm(list = ls())

# load required packages
ld_pkgs <- c("tidyverse", "multcomp", "multcompView", "cowplot", "grid", "gridExtra")
lapply(ld_pkgs, library, character.only = TRUE)

# load plotting details
source("plotting_details.R")

##################################################################
##              Read and prepare data for plotting              ##
##################################################################


data <- read_csv("data/soildeg_data.csv") 


# filtered
data_sel <- data %>% 
  filter(mid_increment_depth_cm == 5) %>% 
  filter(!set == "roadcut") 



data_sel$land_use <- factor(data_sel$land_use, levels = c("forest", "cropland", "abandoned", "eucalyptus"),
                            labels = c("forest", "cropland", "abnd.", "eucalyptus"))

data_sel$years_since_deforestation <- factor(data_sel$years_since_deforestation, 
                                             levels = c("0", "2–7", "10–20", "40–60", "> 60"))


data_sel <- data_sel



## plot with resinP, TN, ECEC
data_sel[,c(59, 28, 33)]
data_sel$TN_gkg

data_sel_gathered <- data_sel %>%
  gather(property, value, "ECEC_cmolckg", "TN_gkg", "resinP_mgkg")



data_sel_gathered$property <- factor(data_sel_gathered$property,
                                  levels = c("TN_gkg", "resinP_mgkg", "ECEC_cmolckg"))

data_sel$def_lu <- with(data_sel, interaction(years_since_deforestation, land_use, sep = "."))



##################################################################
##                         Mafic region                         ##
##################################################################

## check whether model assumptions are given 
data_sel_maf <- data_sel %>% filter(geology == "mafic") %>% 
  mutate(resinP_mgkg = ifelse(resinP_mgkg == 0, 0.1, resinP_mgkg))


## unbalanced data! 
# We use contr.sum, otherwise type III sum of squares will be wrong
options(contrasts = c("contr.sum", "contr.poly"))

# Type III (sign. interaction!)
fit.maf.TN <- aov(data = data_sel_maf, log(TN_gkg) ~ def_lu)
fit.maf.resinP <- aov(data = data_sel_maf[!is.na(data_sel_maf$resinP_mgkg),], log(resinP_mgkg) ~ def_lu)
fit.maf.ECEC <- aov(data = data_sel_maf, log(ECEC_cmolckg) ~ def_lu)

# 
# # plot(fit)
qqnorm(resid(fit.maf.TN))
qqnorm(resid(fit.maf.resinP))
qqnorm(resid(fit.maf.ECEC))


##################################################################
##                           TukeyHSD                           ##
##################################################################


# mafic region-------------------------------------------------------------

# get statistical letters (Tukey)
properties <- c("TN_gkg", "resinP_mgkg", "ECEC_cmolckg")

all <- list()

for (property in properties) { ## only TN and ECEC because P has 2 NA values
  
  if (property == "resinP_mgkg"){
    noNA_maf <- data_sel_maf[!is.na(data_sel_maf$resinP_mgkg),]
  }else{
    noNA_maf <- data_sel_maf
  }

  noNA_maf$property_sel <- noNA_maf[[property]]
  
  fit <- aov(data = noNA_maf, log(property_sel) ~ def_lu)
  tukey <- TukeyHSD(fit, conf.level=0.95)
  cld <- multcompLetters4(fit, tukey)
  
  Tk <- noNA_maf %>%  
    group_by(years_since_deforestation, land_use, def_lu) %>%
    summarize(mean=mean(property_sel), quant = quantile(property_sel, probs = 0.75), max = max(property_sel))
  
  cld_def <- as.data.frame.list(cld$def_lu)
  cld_def$def_lu <- rownames(cld_def)
  cld_def <- cld_def[,c("Letters", "def_lu")]
  
  Tk_out <- inner_join(Tk, cld_def) %>%
    mutate(geology = "mafic", 
           property = paste0(property))
  
  all[[property]] <- Tk_out
  
}


stats_merged_out <- do.call("rbind", all)

stats_merged_out$property <- factor(stats_merged_out$property, 
                                    levels = c("TN_gkg", "resinP_mgkg", "ECEC_cmolckg"))





# felsic region -----------------------------------------------------------


## check whether model assumptions are given 
data_sel_fel <- data_sel %>% filter(!geology == "mafic")

## unbalanced data! 
# We use contr.sum, otherwise type III sum of squares will be wrong (technical issue).
options(contrasts = c("contr.sum", "contr.poly"))

# Type III (sign. interaction!)
fit.fel.TN <- aov(data = data_sel_fel, log(TN_gkg) ~ def_lu)
fit.fel.resinP <- aov(data = data_sel_fel[!is.na(data_sel_fel$resinP_mgkg),], log(resinP_mgkg) ~ def_lu)
fit.fel.ECEC <- aov(data = data_sel_fel, log(ECEC_cmolckg) ~ def_lu)

# plot(fit)
qqnorm(resid(fit.maf.TN))
qqnorm(resid(fit.maf.resinP))
qqnorm(resid(fit.maf.ECEC))


# get statistical letters (Tukey)
properties <- c("TN_gkg", "resinP_mgkg", "ECEC_cmolckg")
all <- list()

for (property in properties) {
  
  if (property == "resinP_mgkg"){
    noNA_fel <- data_sel_fel[!is.na(data_sel_fel$resinP_mgkg),]
  }else{
    noNA_fel <- data_sel_fel
  }
  
  noNA_fel$property_sel <- noNA_fel[[property]]
  
  fit <- aov(data = noNA_fel, log(property_sel) ~ def_lu)
  tukey <- TukeyHSD(fit, conf.level=0.95)
  cld <- multcompLetters4(fit, tukey)
  
  Tk <- noNA_fel %>%  
    group_by(years_since_deforestation, land_use, def_lu) %>%
    summarize(mean=mean(property_sel), quant = quantile(property_sel, probs = 0.75), max = max(property_sel))
  
  cld_def <- as.data.frame.list(cld$def_lu)
  cld_def$def_lu <- rownames(cld_def)
  cld_def <- cld_def[,c("Letters", "def_lu")]
  Tk_out <- inner_join(Tk, cld_def) %>%
  mutate(geology = "felsic", 
         property = paste0(property))
  
  all[[property]] <- Tk_out
  
}


stats_merged_out_fel <- do.call("rbind", all)
stats_merged_out_fel$property <- factor(stats_merged_out_fel$property, 
                                    levels = c("TN_gkg", "resinP_mgkg", "ECEC_cmolckg"))



#################################################################
##                        Visualization                        ##
#################################################################

# plot
(p_mafic <- data_sel_gathered %>% 
   filter(geology == "mafic") %>%
   filter(!is.na(value)) %>% # one missing value for resinP
   ggplot() +
   geom_boxplot(mapping = aes(x = years_since_deforestation, y = value, fill = years_since_deforestation), alpha = 0.9, linewidth = 0.2) +
   geom_jitter(mapping = aes(x = years_since_deforestation, y = value, fill = years_since_deforestation), alpha = 0.5, shape = 21) +
   scale_fill_manual("", values = palette_all) +
   geom_text(data = stats_merged_out, aes(x = years_since_deforestation, y = max, label = Letters), 
             size = 3.5, vjust=-1, hjust =0.5) +
   facet_grid(property ~ land_use, 
              scales = "free", space = "free_x",  switch = "y",
              labeller = labeller(property =  as_labeller(facet_names_property, label_parsed))
   ) +
   ggh4x::facetted_pos_scales(
     y = list(
       property == "TN_gkg" ~  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)),
       property == "ECEC_cmolckg" ~ scale_y_continuous(breaks = seq(0, 27, 5), limits = c(0, 27)),
       property == "resinP_mgkg"  ~ scale_y_continuous(breaks = seq(0, 32, 5), limits = c(0, 32))
     )) +
   xlab("") + 
   ylab("")+
   ggtitle("mafic")+
   theme_ls +
   theme(legend.position = "none",
         strip.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         strip.text.x = element_text(size = 12))
)



(p_felsic <- data_sel_gathered %>% 
  filter(geology == "felsic") %>%
  filter(!is.na(value)) %>% # one missing value for resinP
  ggplot() +
  geom_boxplot(mapping = aes(x = years_since_deforestation, y = value, fill = years_since_deforestation), alpha = 0.9, linewidth = 0.2) +
  geom_jitter(mapping = aes(x = years_since_deforestation, y = value, fill = years_since_deforestation), alpha = 0.5, shape = 21) +
  scale_fill_manual("", values = palette_all) +
    geom_text(data = stats_merged_out_fel, aes(x = years_since_deforestation, y = max, label = Letters), 
              size = 3.5, vjust=-1, hjust =0.5) +
  facet_grid(property ~ land_use, 
             scales = "free", space = "free_x",  switch = "y",
             labeller = labeller(property =  as_labeller(facet_names_property, label_parsed))
  ) +
    ggh4x::facetted_pos_scales(
      y = list(
        property == "TN_gkg" ~  scale_y_continuous(breaks = seq(0, 10, 2), limits = c(0, 10)),
        property == "ECEC_cmolckg" ~ scale_y_continuous(breaks = seq(0, 27, 5), limits = c(0, 27)),
        property == "resinP_mgkg"  ~ scale_y_continuous(breaks = seq(0, 32, 5), limits = c(0, 32))
      )) +
  xlab("") + 
  ylab("")+
  ggtitle("felsic")+
  theme_ls +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        strip.background.y  = element_blank(),
        strip.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        strip.text.x = element_text(size = 12)
        )
)




##################################################################
##         Arrange mafic and felis regions in one graph         ##
##################################################################


p_mafic_felsic <- cowplot::plot_grid(p_mafic, NULL, p_felsic, nrow = 1,align = "hv", rel_widths = c(1.2, -0.013, 1) )
x.grob <- textGrob("Years since deforestation")
p_arranged <- grid.arrange(arrangeGrob(p_mafic_felsic, bottom = x.grob))
p_arranged

# save plot
ggsave(p_arranged, filename = "out/fig05.png", height = 6, width = 9)


