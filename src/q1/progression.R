##### CHARACTERISING PROGRESSION IN GROUPS #####

source("src/ext/main.r")
library(ggplot2)

## took 20 mins to run this so bear with it or we need to rejig a bit
##source("src/ext/alsfrs.r")


## this source file has the alsfrs.r source file in it and consequently also takes a long time to run
## done run both staging.r and aslfrs.r
#source("src/ext/staging.r")

## get age and date of onset for each record



alsfrsr_rate <- ext_alsfrs %>%
  left_join(select(ext_main, date_of_birth, calculated_age_at_onset, id),  by=c("id"="id")) %>%
  mutate(calculated_age_at_assessment = case_when(is.na(age_at_assessment)~ as.numeric((date_of_assessment-date_of_birth)/365.25),
                                                  TRUE ~ as.numeric(age_at_assessment)))



######## Time to Gastrostomy

## calculate time to gastrostomy from onset
gast_stats <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0) %>%
  group_by(site) %>%
  summarise(number_with_gastrostomy = sum(gastrostomy == TRUE, na.rm=TRUE) ,
            number_with_time_to_gastrostomy = sum(gastrostomy == TRUE & !is.na(time_to_gast), na.rm=TRUE),
            median_time_to_gastrostomy = median(time_to_gast, na.rm=TRUE),
            iqr_time_to_gastrostomy = IQR(time_to_gast, na.rm=TRUE)
              )

gast_hist <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast > 0) %>%
  ggplot(.,) +
  geom_density(aes(x=time_to_gast, fill=site), alpha=0.6)

gast_box <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast > 0) %>%
  ggplot(.,) +
  geom_boxplot(aes(y=time_to_gast, x=site, fill=site), alpha=0.6)

##### Time to Gastrostomy by genetic status

gast_genetic_box <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0 & (c9orf72_tested == TRUE | sod1_tested == TRUE | fus_tested== TRUE | tardbp_tested == TRUE)) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  ggplot(.,) +
  geom_boxplot(aes(x=gene, y=time_to_gast, col=status))

gast_genetic_stats <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0 & (c9orf72_tested == TRUE | sod1_tested == TRUE | fus_tested== TRUE | tardbp_tested == TRUE)) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  group_by(gene, status) %>%
  summarise(n= n(),
            median(time_to_gast),
            IQR(time_to_gast))

gast_genetic_mwu <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0 & (c9orf72_tested == TRUE | sod1_tested == TRUE | fus_tested== TRUE | tardbp_tested == TRUE) & c9orf72_status != "Intermediate") %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  group_by(gene) %>%
  summarise(mwu = wilcox.test(time_to_gast ~status)$p.value)
  
  

  
