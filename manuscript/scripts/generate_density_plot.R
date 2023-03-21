lb1 <- unique(data_leaf_par[data_leaf_par$var == par_names[parameter_name1], "lb"])
ub1 <- unique(data_leaf_par[data_leaf_par$var == par_names[parameter_name1], "ub"])
lb2 <- unique(data_leaf_par[data_leaf_par$var == par_names[parameter_name2], "lb"])
ub2 <- unique(data_leaf_par[data_leaf_par$var == par_names[parameter_name2], "ub"] )

data_leaf_par_density <- data.frame(x = data_leaf_par[data_leaf_par$var == par_names[parameter_name1], "value"], 
                                    y = data_leaf_par[data_leaf_par$var == par_names[parameter_name2], "value"] )





plot <- ggplot(data = data_leaf_par_density, aes(x = x, y = y)) +
  geom_density_2d_filled(
    aes(fill = ..level..),
    contour_var = "ndensity",
    alpha = 1,
    colour="white") +
  scale_fill_grey(start = 0.4, end = 0.9) +
  # scale_fill_manual(values = c("#FFEAA1","#FDDA94","#FBC988","#F9B97B","#F7A96E",
  #                              "#F49862","#F28855","#F07848","#EE673C","#EC572F")) +
  geom_segment(aes(x=lb1,xend=ub1,y=lb2,yend=lb2), col = NA, size = 1) +
  geom_segment(aes(x=lb1,xend=ub1,y=ub2,yend=ub2), col = NA, size = 1) +
  geom_segment(aes(x=lb1,xend=lb1,y=lb2,yend=ub2), col = NA, size = 1) +
  geom_segment(aes(x=ub1,xend=ub1,y=lb2,yend=ub2), col = NA, size = 1) +
  scale_y_continuous(limits = c(lb2, ub2), expand = c(0,0), 
                     breaks = c(lb2, breaks2[1], breaks2[2], ub2)) +
  scale_x_continuous(limits = c(lb1, ub1), expand = c(0,0),
                     breaks = c(lb1, breaks1[1], breaks1[2], ub1)) +
  theme_minimal() +
  theme(axis.ticks.y = element_line(colour = "black", size = 0.5),
        axis.ticks.x = element_line(colour = "black", size = 0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text.y = element_text(family = "Linux Libertine G"),
        axis.title.y = element_text(family = "Linux Libertine G"),
        axis.text.x = element_text(family = "Linux Libertine G"),
        axis.title.x = element_text(family = "Linux Libertine G"),
        panel.grid.major = element_line(size = 0),
        legend.title = element_text(family = "Linux Libertine G"),
        legend.text = element_text(family = "Linux Libertine G"),
        legend.title.align = 0.5) +
  theme (legend.key.height = unit(0.2, 'cm')) +
  labs(x = TeX(paste0('$', parameter_name1, '$')), y = TeX(paste0('$', parameter_name2, '$')), fill = "Density") +
  guides(fill = guide_colorsteps(direction = "horizontal", 
                                 barwidth = unit(6, "cm"),
                                 title.position = "top",
                                 frame.colour = "black",
                                 frame.linewidth = 0.5)) +
  theme(legend.position = "bottom")

assign(plot_name, plot)