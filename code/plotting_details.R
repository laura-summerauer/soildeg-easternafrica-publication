facet_names_property <- c(Al_ar_gkg = "Al[AR]~(g~kg^{-1})",
                          Fe_ar_gkg = "Fe[AR]~(g~kg^{-1})",
                          Al_xrf_perc = "Al[XRF]~('%')",
                          Fe_xrf_perc = "Fe[XRF]~('%')",
                          CIA = "CIA~('%')",
                          ECEC_cmolckg = "ECEC~(cmol[c]~kg^{-1})",
                          Fe_tot_gkg = "Fe[tot]~(g~kg^{-1})",
                          pH = "pH[CaCl[2]]",
                          Si_xrf_perc = "Si[XRF]~('%')",
                          TC_gkg = "SOC~(g~kg^{-1})",
                          TN_gkg = "TN~(g~kg^{-1})",
                          CN_ratio = "C:N",
                          TRB_gkg = "TRB~(g~kg^{-1})",
                          Ca_av_cmolckg = "Ca^{'2+'}~(cmol[c]~kg^{-1})",
                          Mg_av_cmolckg = "Mg^{'2+'}~(cmol[c]~kg^{-1})",
                          sum_bases_cmolckg = "Ca^{'2+'}~Mg^{'2+'}~Na^{'+'}~K^{'+'}~(cmol[c]~kg^{-1})",
                          acidity = "Al^{'3+'}~'&'~H^{'+'}~(cmol[c]~kg^{-1})",
                          Ca_tot_gkg = "Ca[AR]~(g~kg^{-1})",
                          Mg_tot_gkg = "Mg[AR]~(g~kg^{-1})",
                          P_tot_gkg = "P[AR]~(g~kg^{-1})", 
                          K_tot_gkg = "K[AR]~(g~kg^{-1})", 
                          BS = "BS~('%')",
                          ca_sat = "Ca^{'2+'}~saturation~('%')",
                          mg_sat = "Mg^{'2+'}~saturation~('%')", 
                          ca_mg_ratio = "Ca^{'2+'}~':'~Mg^{'2+'}", 
                          Al_av_cmolckg = "Al^{'3+'}~(cmol[c]~kg^{-1})", 
                          al_sat = "Al^{'3+'}~saturation~('%')", 
                          Ti_xrf_perc = "Ti[XRF]~('%')", 
                          clay_perc = "clay~('%')", 
                          sand_perc = "sand~('%')", 
                          silt_perc = "silt~('%')",
                          silt_clay_perc = "silt~'+'~clay~('%')", 
                          Ti_xrf_perc_corrOC = "Ti[XRF]~corrOC~('%')", 
                          Fe_pyox_perc = "Fe[PyOx]~('%')",
                          Fe_pyoxdcb_perc = "Fe[py-ox-dcb]~('%')",
                          Al_pyox_perc = "Al[PyOx]~('%')",
                          resinP_mgkg = "P[resin]~(mg~kg^{-1})"
                          )


theme_ls <- theme_bw()+
    theme(legend.position = "top",
          legend.box="vertical", 
          legend.margin=margin(t = 0, unit = "cm"),
          strip.placement = "outside",
          strip.background = element_blank(),
          panel.background = element_rect(fill='transparent'),
          axis.line = element_line(),
          legend.background = element_rect(fill='transparent')) 


theme_rel_change <- theme_bw()+
  theme(legend.position = "none",
        legend.box="vertical", 
        legend.margin=margin(t = 0, unit = "cm"),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.background = element_rect(fill='transparent'),
        axis.line = element_line(),
        legend.background = element_rect(fill='transparent'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank()
        # axis.ticks.y = element_blank(),
        # axis.line.y = element_blank()) 
  )

theme_rel_change_sub <- theme_bw()+
  theme(legend.position = "none",
        legend.box="vertical", 
        legend.margin=margin(t = 0, unit = "cm"),
        strip.placement = "outside",
        strip.background = element_blank(),
        panel.background = element_rect(fill='transparent'),
        axis.line = element_line(),
        legend.background = element_rect(fill='transparent'),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        # axis.line.y = element_blank(),
        # axis.text.x = element_blank(),
        axis.title.x = element_blank()) 

shape_values <-  c(24, 21, 25, 23)
shape_values_withoutforest <-  shape_values[-1]


# paletteFunc <- colorRampPalette(c('#1A4C39', '#EFB100'));
# palette_all     <- paletteFunc(5)
# palette_withoutforest <- palette_all[-1]

# https://www.schemecolor.com/yellow-to-dark-green.php
palette_all <- rev(c("#FDFF00", "#CCD704",  "#69880C", "#386010", "#073814"))
palette_withoutforest <- palette_all[-1]
palette_aband <- rev(c("#FDFF00", "#CCD704", "#9BAF08", "#69880C", "#386010", "#073814"))


