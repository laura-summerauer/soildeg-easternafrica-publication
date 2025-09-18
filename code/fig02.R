############################################################################
############################################################################
###                                                                      ###
###                      CODE TO REPRODUCE FIGURE 2                      ###
###                                                                      ###
############################################################################
############################################################################

# remove objects in environment
rm(list = ls())

# load required packages
ld_pkgs <- c("tidyverse", "multcompView")
lapply(ld_pkgs, library, character.only = TRUE)


# load plotting theme detils for ggplot2
source("plotting_details.R")

##################################################################
##                         Prepare data                         ##
##################################################################


data <- read_csv("data/soildeg_data.csv") 

# filtered
data_rem <- data %>% 
  filter(mid_increment_depth_cm == 5) %>% 
  filter(!set == "roadcut")


# define land use as factor
data_rem$land_use <- factor(data_rem$land_use, levels = c("forest", "cropland", "abandoned", "eucalyptus"),
                            labels = c("forest", "cropland", "abnd.", "eucalyptus"))

# define years since deforestation as factor
data_rem$years_since_deforestation <- factor(data_rem$years_since_deforestation, 
                                             levels = c("0", "2–7", "10–20", "40–60", "> 60"))

# define geolog as factor
data_rem$geology <- factor(data_rem$geology, 
                           levels = c("mafic", "felsic"), 
                           labels = c("mafic", "felsic"))


##################################################################
##                           TukeyHSD                           ##
##################################################################


# mafic -------------------------------------------------------------------

data_mafic <- data_rem %>% 
  filter(geology == "mafic") %>% 
  mutate(def_lu = interaction(years_since_deforestation, land_use, sep = "."))


## highly unbalanced data! 
# We use contr.sum, otherwise type III sum of squares will be wrong (technical issue).
options(contrasts = c("contr.sum", "contr.poly"))

# fit
fit.maf <- aov(data = data_mafic, log(TC_gkg) ~ def_lu)
qqnorm(resid(fit.maf))


tukey.m <- TukeyHSD(fit.maf, conf.level=0.95)
cld.m <- multcompLetters4(fit.maf, tukey.m)

### positioning of letters
# table with factors and 3rd quantile
Tk.m <- data_mafic %>%  group_by(years_since_deforestation, land_use, def_lu) %>%
  summarize(mean=mean(TC_gkg), quant = quantile(TC_gkg, probs = 0.75)) 

# extracting the compact letter display and adding to the Tk table
cld_def <- as.data.frame.list(cld.m$def_lu)
cld_def$def_lu <- rownames(cld_def)
cld_def <- cld_def[,c("Letters", "def_lu")]
Tk_maf <- inner_join(Tk.m, cld_def) %>% 
  mutate(geology = "mafic")


# felsic ------------------------------------------------------------------

data_felsic <- data_rem %>% 
  filter(geology == "felsic") %>%
  mutate(def_lu = interaction(years_since_deforestation, land_use, sep = "."))

## fit 
fit.fel <- aov(data = data_felsic, log(TC_gkg) ~ def_lu)
qqnorm(resid(fit.fel))

## tukey
tukey.f <- TukeyHSD(fit.fel, conf.level=0.95)
cld.f <- multcompLetters4(fit.fel, tukey.f)

### positioning of letters
# table with factors and 3rd quantile
Tk.f <- data_felsic %>%  group_by(years_since_deforestation, land_use, def_lu) %>%
  summarize(mean=mean(TC_gkg), quant = quantile(TC_gkg, probs = 0.75)) 

# extracting the compact letter display and adding to the Tk table
cld_felsic <- as.data.frame.list(cld.f$def_lu)
cld_felsic$def_lu <- rownames(cld_felsic)
cld_felsic <- cld_felsic[,c("Letters", "def_lu")]
Tk_fel <- inner_join(Tk.f, cld_felsic) %>% 
  mutate(geology = "felsic")


# merge mafic and felsic data
Tk_merged <- bind_rows(Tk_maf, Tk_fel)



#################################################################
##                        Visualization                        ##
#################################################################


(p_TC <- data_rem %>% 
   ggplot(aes(x = years_since_deforestation, y = TC_gkg)) +
   geom_boxplot(mapping = aes(fill = years_since_deforestation), alpha = 0.9, linewidth = 0.2) +
   geom_jitter(mapping = aes(fill = years_since_deforestation), alpha = 0.5, shape = 21) +
   scale_fill_manual("", values = palette_all) +
   scale_y_continuous(limits = c(0, 112), breaks = seq(0, 112, by = 30))+
   facet_grid(factor(geology, levels = c("mafic", "felsic")) ~ land_use , 
              scales = "free", space = "free_x",  
              labeller = labeller(property =  as_labeller(facet_names_property, label_parsed))
   ) +
   geom_text(data = Tk_merged, aes(x = years_since_deforestation, y = quant, label = Letters), 
             size = 3.5, vjust=-3.8, hjust =0.5) +
   xlab("Years since deforestation") + 
   ylab(expression(paste("SOC (g ", kg^{-1}, ")")))+
   theme_ls +
   theme(legend.position = "none",
         axis.title = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         strip.text.x = element_text(size = 12),
         strip.text.y = element_text(size = 12))
 ) 



ggsave(p_TC, filename = "out/fig02.png", height = 6, width = 7)









