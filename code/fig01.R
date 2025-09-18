
############################################################################
############################################################################
###                                                                      ###
###                   CREATE MAP WITH SAMPLING REGIONS                   ###
###                                                                      ###
############################################################################
############################################################################

# Script author: Laura Summerauer

# remove objects in environment
rm(list = ls())

# load required packages
ld_pkgs <- c("sf", "elevatr", "terra", "tidyverse", "tidyterra", 
              "ggnewscale", "giscoR", "units", "ggblend", "raster", "geodata")

lapply(ld_pkgs, library, character.only = TRUE)


##################################################################
##  Load data containing GPS coordinates of sampling locations  ##
##################################################################
data <- read_csv("data/soildeg_data.csv") 

# filtered
data_filt <- data %>%
  filter(mid_increment_depth_cm == 5) 

# define variables as factors and edit for subsequent plotting
# edit years since deforestation levels
data_filt$years_since_deforestation <- factor(data_filt$years_since_deforestation, 
                                             levels = c("0", "2–7", "10–20", "40–60", "> 60"))
# edit land use levels 
data_filt$land_use <- factor(data_filt$land_use, levels = c("forest", "cropland", "abandoned", "eucalyptus"))

# create a spatial object (for plotting later)
data_sf <- st_as_sf(data_filt, coords = c("long", "lat"))
# set projection
data_sf <- st_set_crs(data_sf, 4269)


##################################################################
##              Load base map (Natural Earth data)              ##
##################################################################
natural_earth <- terra::rast("maps/NaturalEarth/NE1_HR_LC_SR_W_DR.tif")
# crop
ne_local <- crop(natural_earth, c(26, 33, -12, 9))
ne_large <- crop(natural_earth, c(10, 40, -15, 10))

## sampling points
mafic_sf <- data_sf %>% filter(geology == "mafic")
felsic_sf <- data_sf %>% filter(geology == "felsic")


##################################################################
##             Elevation data for two study regions             ##
##################################################################

## main source (date: December 2023): 
# https://dominicroye.github.io/en/2022/hillshade-effects/

## Source: 90m SRTM available in geodata package
elev_mafic <- geodata::elevation_3s(lat = -2.49077, lon = 28.84281, path = "maps/SRTM/")
elev_mafic <- crop(elev_mafic, c(28.5, 29.5, -3, -1.8))
elev_mafic <- raster(elev_mafic)

elev_felsic <- elevation_3s(lat = 0.66174, lon = 30.2748, path = "maps/SRTM/")
elev_felsic <- crop(elev_felsic, c(30, 31, 0, 1.1))
elev_felsic <- raster(elev_felsic)

## create hillshade from downloaded SRTM 90m DEM
# estimate the slope
sl_mafic <- terra::terrain(elev_mafic, "slope", unit = "radians")
sl_felsic <- terrain(elev_felsic, "slope", unit = "radians")

# estimate the aspect or orientation
asp_mafic <- terra::terrain(elev_mafic, "aspect", unit = "radians")
asp_felsic <- terra::terrain(elev_felsic, "aspect", unit = "radians")

# calculate the hillshade effect with 45º of elevation
sl_mafic <- as(sl_mafic, Class = "SpatRaster")
asp_mafic <- as(asp_mafic, Class = "SpatRaster")
hill_mafic <- terra::shade(slope = sl_mafic, aspect = asp_mafic, 
                         angle = 45, 
                         direction = 300,
                         normalize= TRUE)

sl_felsic <- as(sl_felsic, Class = "SpatRaster")
asp_felsic <- as(asp_felsic, Class = "SpatRaster")
hill_felsic <- terra::shade(slope = sl_felsic, aspect = asp_felsic, 
                         angle = 45, 
                         direction = 300,
                         normalize= TRUE)


#################################################################
##           Forest cover data for two study regions           ##
#################################################################

# get land cover map filtered from ESACCI 20m for Africa 2016 
# filtered for forest (value = 1)

# congo
forestc_mafic <- rast("maps/ESACCI-LC-L4-LC10-Map-20m/forest_bukavu.tif")
forestc_mafic <- crop(forestc_mafic, elev_mafic)
forestc_mafic[forestc_mafic < 1] <- NA

# uganda
forestc_felsic <- rast("maps/ESACCI-LC-L4-LC10-Map-20m/forest_forportal.tif")
forestc_felsic <- crop(forestc_felsic, elev_felsic)
forestc_felsic[forestc_felsic < 1] <- NA

#################################################################
##               Lake data for two study regions               ##
#################################################################
lakes <- st_read("maps/hydrology/lakes_central_africa.shp")
lakes <- as(lakes, "Spatial")

# mafic region
lakes_mafic <- terra::crop(x = lakes, y = elev_mafic)
lakes_mafic <- st_as_sf(lakes_mafic)

# felsic region
lakes_felsic <- terra::crop(x = lakes, y = elev_felsic)
lakes_felsic <- st_as_sf(lakes_felsic)


##################################################################
##       Load colour coding as in all the remaining plots       ##
##################################################################
source("plotting_details.R")

#################################################################
##                       Large-scale map                       ##
#################################################################

m_large <- ggplot() +
  # base map (Natural Earth data)
  geom_spatraster_rgb(data = ne_local, alpha = 1) +
  # squares with two study regions
  geom_rect(aes(xmin = 30, xmax = 31, ymin = 0, ymax = 1), color = "black", fill = NA, linewidth = 1)  +
  geom_text(aes(x = 29.0, y = 1.5, label = "felsic"), size = 4, fontface = "bold") +
  geom_rect(aes(xmin = 28.5, xmax = 29.5, ymin = -2, ymax = -3), color = "black", fill = NA, linewidth = 1)  +
  geom_text(aes(x = 27.4, y = -1.9, label = "mafic"), size = 4, fontface = "bold") +
  # remove x and y lab
  xlab("") + ylab("")+
  # scale x and y axis
  scale_x_continuous(expand = c(0, 0), limits = c(26,33), breaks = seq(26, 33, 3)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-12, 7)) +
  # place a scale bar in bottom right corner
  ggspatial::annotation_scale(location = "br", style = "ticks", line_width = 1, width_hint = 0.3)+
  # edit theme
  theme(plot.background = element_blank(),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        axis.title = element_blank(),
        legend.position = "none")

m_large

#################################################################
##                      Two regional maps                      ##
#################################################################

m_mafic <- ggplot() +
  list(
    # hillshade
    geom_spatraster(data = hill_mafic, show.legend = FALSE, alpha = 1),
    scale_fill_gradientn(values = c(0, 0.8, 1, 150, 250), colours = c("black", "grey98", "grey99", "grey100",  "white")),
    new_scale_fill(),
    # forest cover
    geom_spatraster(data = forestc_mafic,
                    show.legend = FALSE, alpha = 0.6),
    scale_fill_gradient(low = "#006400", high = "#006400", na.value = "transparent"),
    new_scale_fill()
  ) %>% 
  blend("multiply") +
  # lakes
  geom_sf(data = lakes_mafic, alpha = 1,
          fill = "#81b6da", colour = "#81b6da")+
  new_scale_fill()+
  # add main city Bukavu for orientation purposes
  geom_point(mapping = aes(x = 28.84281, y = -2.49077), shape = 15, colour = "black", size = 2) +
  geom_text(mapping = aes(x = 28.9, y = -2.53, label = "Bukavu"), size = 3, fontface = "bold")+
  new_scale_fill() +
  # add sampling points (colour by year since deforestation)
  geom_sf(data = mafic_sf, aes(fill = years_since_deforestation, shape = land_use), colour = "black", size = 3, alpha = 0.6) +
  scale_fill_manual("Years since deforestation", values = palette_all) +
  scale_shape_manual(values = shape_values)+
  xlab("") + ylab("")+
  scale_x_continuous(expand = c(0, 0), breaks = seq(28, 29.5, by = 0.5), limits = c(28.5, 29.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-3, -1.8, by = 0.5), limits = c(-3, -2)) +
  geom_label(aes(x = 28.62, y = -2.07, label = "mafic"), fill = "white", alpha = 0.7, size = 4, fontface = "bold", label.size = NA) +
  ggspatial::annotation_scale(location = "br", style = "ticks", line_width = 1, width_hint = 0.2)+
  theme(plot.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        axis.title = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")

m_mafic


m_felsic <- ggplot() +
  list(
    # hillshade
    geom_spatraster(data = hill_felsic, show.legend = FALSE),
    scale_fill_gradientn(values = c(0, 0.8, 1, 150, 250), colours = c("black", "grey98", "grey99", "grey100",  "white")),
    new_scale_fill(),
    # land cover / forest
    geom_spatraster(data = forestc_felsic,
                    show.legend = FALSE, alpha = 0.6),
    scale_fill_gradient(low = "#006400", high = "#006400", na.value = "transparent"),
    new_scale_fill()
  ) %>% 
  blend("multiply") +
  new_scale_fill() +
  # lakes
  geom_sf(data = lakes_felsic,
          fill = alpha("#81B6DA", 1), colour = "#81B6DA")+
  new_scale_fill() +
  # add main city Fort Portal for orientation purposes
  geom_point(mapping = aes(x = 30.2748, y = 0.66174), shape = 15, colour = "black", size = 2) +
  geom_text(mapping = aes(x = 30.35, y = 0.69, label = "Fort Portal"), size = 3, fontface = "bold")+
  # add sampling points (colour by year since deforestation)
  geom_sf(data = felsic_sf, aes(fill = years_since_deforestation, shape = land_use), colour = "black", size = 3, alpha = 0.6) +
  scale_fill_manual("Years since deforestation", values = palette_all) +
  scale_shape_manual(values = shape_values[-3])+
  xlab("") + ylab("")+
  scale_x_continuous(expand = c(0, 0), breaks = seq(30, 31, by = 0.5), limits = c(30, 31)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 1, by = 0.5), limits = c(0, 1)) +
  geom_label(aes(x = 30.12, y = 0.93, label = "felsic"), fill = "white", alpha = 0.7, size = 4, fontface = "bold", label.size = NA) +
  ggspatial::annotation_scale(location = "br", style = "ticks", line_width = 1, width_hint = 0.2)+
  theme(plot.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        axis.title = element_blank(),
        axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")


m_felsic



##################################################################
##                            Legend                            ##
##################################################################


# create a plot to retrieve legend separately
legend.plot <- ggplot() +  
  geom_sf(data = mafic_sf, aes(fill = years_since_deforestation, shape = land_use), colour = "black", size = 3, alpha = 0.7) +
  guides(shape = guide_legend(order = 1),
         fill = guide_legend(override.aes = list(shape = 21), order = 2))+
  scale_fill_manual("Years since deforestation", values = palette_all) +
  scale_shape_manual("Land use", values = shape_values) +
  theme_ls

# get legend
legend <- cowplot::get_plot_component(legend.plot, pattern = "guide-box-top")
cowplot::ggdraw(legend)


##################################################################
##                 Arrange all maps in one plot                 ##
##################################################################

# mafic and felsic study regions
mafic_felsic <- cowplot::plot_grid(m_felsic, m_mafic, ncol = 1)

# arrange all plots (larger overview and two study region details)
p_all <- cowplot::plot_grid(m_large, NULL, mafic_felsic, axis = "bt", nrow = 1, rel_widths = c(1, -0.31, 1))

# add legend on bottom of plot
p_with_legend <- cowplot::plot_grid(p_all, legend, ncol = 1, rel_heights = c(1, 0.09))

# save plot
ggsave("out/fig01.png", p_with_legend, 
       width = 7.5, 
       height = 7, 
       unit = "in",
       device = png, 
       type = "cairo",
       bg = "white")


