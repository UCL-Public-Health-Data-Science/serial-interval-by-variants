
# PLOTS -------------------------------------------------------------------
library(stringr)

certain_transmissions <- certain_transmissions %>% 
  group_by(infector_nVar) %>% 
  mutate(axis_nVar_count = paste0(
    infector_nVar, 
    "\n", 
    "n = ", 
    length(infector_nVar))
    ) # adds counts to axis

# SERIAL INTERVAL BY VARIANTS 95% CI -----------------------------

fig_si_conf <- table_variants %>% 
  ggplot()+
  aes(y= mean, x = paste0(Variant, "\n", "n=", no_of_pairs), color = Variant)+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = lwr.ci, ymax = upr.ci), width = 0.5)+
  theme_minimal()+
  labs(x ="",
       y = "serial interval (days)")



# VIOLIN PLOTS ------------------------------------------------------------~

fig_violin <- certain_transmissions %>% 
  ggplot(aes(x = axis_nVar_count, 
             y = serial_interval, 
             fill = infector_nVar))+ 
  geom_violin(alpha = 0.3)+
  geom_boxplot(width = 0.1, color = "black")+
  theme_minimal()+
  labs(x = "",
       y = "serial interval (days)",
       fill = "Variant")


panel_conf_violin <- ggarrange(fig_si_conf+theme(axis.text.x = element_blank()),
                               fig_violin,
                               labels = c("A", "B"),
                               nrow = 2)

panel_conf_violin



#  HISTOGRAMS ----------------------------------------------------

fig_hist <- certain_transmissions %>%
  filter(infector_nVar != "[0] Unknown") %>% 
  ggplot(aes(x = serial_interval, fill = TRUE, color = "black"))+ 
  geom_bar(bins = 1)+ 
  scale_x_continuous(breaks = seq(0,20, 2))+ # change if max_si is changed
  facet_wrap(~axis_nVar_count)+
  theme_minimal()+
  geom_vline(data = filter(certain_transmissions,
                           str_detect(axis_nVar_count,"Omicron")),
             aes(xintercept = mean(serial_interval)), linetype = "dotted")+
  geom_vline(data =filter(certain_transmissions,
                          str_detect(axis_nVar_count,"Delta")),
             aes(xintercept = mean(serial_interval)), linetype = "dotted")+
  geom_vline(data = filter(certain_transmissions,
                          str_detect(axis_nVar_count,"Alpha")),
             aes(xintercept = mean(serial_interval)), linetype = "dotted")+
  geom_vline(data = filter(certain_transmissions,
                           str_detect(axis_nVar_count,"Wild Type")),
             aes(xintercept = mean(serial_interval)), linetype = "dotted")+
  # geom_vline(data = filter(certain_transmissions,
  #                          str_detect(axis_nVar_count,"Unknown")),
  #            aes(xintercept = mean(serial_interval)), linetype = "dotted")+
  geom_vline(data = filter(certain_transmissions,
                           str_detect(axis_nVar_count,"Omicron")),
             aes(xintercept = median(serial_interval)), linetype = "dashed")+
  geom_vline(data =filter(certain_transmissions,
                          str_detect(axis_nVar_count,"Delta")),
             aes(xintercept = median(serial_interval)), linetype = "dashed")+
  geom_vline(data = filter(certain_transmissions,
                           str_detect(axis_nVar_count,"Alpha")),
             aes(xintercept = median(serial_interval)), linetype = "dashed")+
  geom_vline(data = filter(certain_transmissions,
                           str_detect(axis_nVar_count,"Wild Type")),
             aes(xintercept = median(serial_interval)), linetype = "dashed")+
  # geom_vline(data = filter(certain_transmissions,
  #                          str_detect(axis_nVar_count,"Unknown")),
  #            aes(xintercept = median(serial_interval)), linetype = "dashed")+
  scale_fill_manual(values = "#971A82")+
  scale_color_manual(values = "black")+
  guides(fill = FALSE,
         color= FALSE)+
  labs(x = "serial interval (days)",
       y = "count")





# GAMMA -------------------------------------------------------------------


fig_gamma <- certain_transmissions %>% 
  filter(infector_nVar != "[0] Unknown") %>% 
  ggplot() +
  geom_histogram(
    aes(x = serial_interval, 
        y = ..density..),
    binwidth = 1,
    boundary = 0.5,
    fill = "#971A82",
    color = "black") +
  geom_line(data = all_dist %>% 
              filter(infector_nVar != "[0] Unknown") ,
            aes(x = si, y = d), color = "steelblue",
            size = 1) +
  geom_text(data = all_params %>% filter(infector_nVar != "[0] Unknown") ,
            aes(label = paste0("shape= ",round(shape, digits = 2),"\n",
                               "scale= ", round(scale, digits = 2)), 
                x = 13, y=0.3)) +
facet_wrap(~infector_nVar) +
  theme_minimal() +
  labs(x = "serial interval (days)")

print(fig_gamma)


panel_hist_gamma <- ggarrange(fig_hist,
                               fig_gamma,
                               labels = c("A", "B"),
                               nrow = 1)
panel_hist_gamma


warning("figures.R ran successfully")
