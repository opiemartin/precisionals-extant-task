##### CHARACTERISING PROGRESSION IN GROUPS #####

source("src/ext/main.r")
library(ggplot2)
library(ggfortify)
library(survival)

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
  group_by(site) %>%
  summarise(n=n(),
            number_with_gastrostomy = sum(gastrostomy == TRUE, na.rm=TRUE) ,
            percentage_with_gastrostomy = round((number_with_gastrostomy/n)*100,1),
            number_with_time_to_gastrostomy = sum(gastrostomy == TRUE & !is.na(time_to_gast), na.rm=TRUE),
            percentage_with__time_gastrostomy = round((number_with_time_to_gastrostomy/n)*100,1),
            median_time_to_gastrostomy = round(median(time_to_gast, na.rm=TRUE),1),
            iqr_time_to_gastrostomy = round(IQR(time_to_gast, na.rm=TRUE),1)
              )

write.csv(gast_stats, "gast_stats.csv")

gast_hist <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast > 0) %>%
  ggplot(.,) +
  geom_density(aes(x=time_to_gast, fill=site), alpha=0.6)  +
  theme_classic()

gast_box <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast > 0) %>%
  ggplot(.,) +
  geom_boxplot(aes(y=time_to_gast, x=site, fill=site), alpha=0.6)  +
  theme_classic()

##### Time to Gastrostomy by genetic status

gast_genetic_box <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  ggplot(.,) +
  geom_boxplot(aes(x=gene, y=time_to_gast, col=status)) +
  theme_classic()

gast_genetic_stats <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  group_by(gene, status) %>%
  summarise(n= n(),
            median_time_to_gast = round(median(time_to_gast),1),
            IQR_time_to_gast = round(IQR(time_to_gast),1))

write.csv(gast_genetic_stats, "gast_genetic_stats.csv")

gast_genetic_mwu <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  group_by(gene) %>%
  summarise(mwu = wilcox.test(time_to_gast ~status)$p.value)
  
write.csv(gast_genetic_mwu, "gast_genetic_mwu.csv")  

### kaplan meier by genetic status
gast_genetic_km <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  mutate(time_to_gast = age_at_gastrostomy - age_at_onset) %>%
  filter(time_to_gast>0) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, age_at_onset, age_at_gastrostomy, time_to_gast), names_to = c("gene", ".value"), names_sep = "_") %>%
  drop_na() %>%
  group_by(gene) %>%
  summarise(fit = list(survfit(Surv(time_to_gast, gastrostomy) ~ status)),
            log_rank = glance(survdiff(Surv(time_to_gast, gastrostomy) ~ status))$p.value)

c9 <- autoplot(gast_genetic_km$fit[1], main=gast_genetic_km$gene[1]) + stat_pvalue_manual(gast_genetic_km$log_rank[1], label="p") + theme_classic()
fus <- autoplot(gast_genetic_km$fit[2], main=gast_genetic_km$gene[2])  + theme_classic()
sod1 <- autoplot(gast_genetic_km$fit[3], main=gast_genetic_km$gene[3])  + theme_classic()
tardbp <- autoplot(gast_genetic_km$fit[4], main=gast_genetic_km$gene[4])  + theme_classic()

c9 + fus +sod1 + tardbp

write.csv(gast_genetic_km %>% select(gene, log_rank), "gast_genetic_km.csv")

