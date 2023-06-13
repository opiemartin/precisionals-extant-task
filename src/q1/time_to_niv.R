#time to NIV



NIV_stats <- ext_main %>% 
  select(id, site, non_invasive_ventilation,age_at_non_invasive_ventilation, 
         age_at_onset) %>% 
  mutate(time_to_niv = age_at_non_invasive_ventilation - age_at_onset) %>% 
  filter(time_to_niv > 0) %>% 
  drop_na() %>% 
  group_by(site) %>% 
  summarise(niv_num = sum(non_invasive_ventilation == TRUE, na.rm =),
            median_time_to_niv = round(median(time_to_niv),1),
            IQR_time_to_niv = round(IQR(time_to_niv),1))
