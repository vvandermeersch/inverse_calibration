
auc_tot_temp <- data.frame(auc_tot = fagus_auc_data_tot$tot, 
                           rep =paste0("subset_", parse_number(fagus_auc_data_tot$subset), "_", fagus_auc_data_tot$rep))
auc_tot_temp  <- auc_tot_temp[order(auc_tot_temp$auc_tot),]
auc_tot_temp <- split(auc_tot_temp, ceiling(seq_along(auc_tot_temp$auc_tot)/10))
for(i in 1:10){
  auc_tot_temp[[i]]['col_group_auc'] <- i
}
auc_tot_temp<- do.call("rbind", auc_tot_temp) %>% dplyr::select(c(rep, col_group_auc))

data_leaf_par <- left_join(data_leaf_par, auc_tot_temp)

data_leaf_par %>%
  ggplot(aes(y = value, x = 1)) +
  geom_violin(fill = 'lightgrey', alpha= 0.2, size = 0.4, colour = 'darkgrey') +
  geom_beeswarm(aes(fill = factor(col_group_auc), col = factor(col_group_auc)), 
                alpha = 0.5, cex = 2.5, size = 2) +
  facet_wrap("var_name", scales="free_y",
             labeller= label_parsed) +
  scale_y_continuous(breaks = breaks_fun1) +
  geom_point(aes(y = forward, x= 0.55), fill = 'darkred', size = 1.5, alpha = 0.9, shape = 23, colour="white", stat = "unique") +
  geom_point(data = data.frame(best = unique(data_leaf_par$best), var_name = unique(data_leaf_par$var_name)),
             aes(y = best, x= c(0.55, 0.65, 0.55, 0.55, 0.55, 0.55)), fill = 'darkblue', size = 1.5, 
             alpha = 0.9, shape = 23, colour="white", inherit.aes = F) +
  scale_fill_manual(values = c("#ef7e56", "#e28143", "#d38531", "#c18820", "#ae8c0f", "#988e00", "#819100", "#67920c", "#49921d", "#16922e")) +
  scale_color_manual(values = c("#ef7e56", "#e28143", "#d38531", "#c18820", "#ae8c0f", "#988e00", "#819100", "#67920c", "#49921d", "#16922e")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Linux Libertine G"),
        axis.title.y = element_text(family = "Linux Libertine G"),
        strip.text.x = element_text(family = "Linux Libertine G", face = "bold", size = 11),
        axis.ticks.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position="none") +
  geom_blank(aes(y = lb)) +
  geom_blank(aes(y = ub)) +
  labs(y = "")



















