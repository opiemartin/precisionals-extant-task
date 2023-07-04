##### CHARACTERISING PROGRESSION IN GROUPS #####

source("src/ext/main.r")
source("src/ext/alsfrs.r")

library(ggplot2)
library(ggfortify)
library(survival)
library(grid)
library(gridExtra)
library(gtsummary)
library(forcats)


source("src/ext/staging.r")

################################################
####### extra cleaning for categorisation ######
################################################

ext_main_cat_clean <- ext_main %>%
  mutate(
    diagnosis_clean = case_when(
      diagnosis %in% c(
        "ALS",
        "ALS plus",
        "ALS/FTD",
        "Atypical",
        "Head drop",
        "Flail arm",
        "Flail leg",
        "PBP") | motor_neuron_predominance == "UMN+LMN" ~ "ALS",
      diagnosis %in% c(
        "PLS",
        "PLS/ALS") | motor_neuron_predominance == "UMN" ~ "PLS",
      diagnosis %in% c(
        "PMA") | motor_neuron_predominance == "LMN" ~ "PMA",
      diagnosis %in% c(
        "UMN Predominant ALS",
        "Suspected PLS") ~ "ALS",
      diagnosis %in% c(
        "LMN Predominant ALS") ~ "ALS",
      diagnosis %in% c(
        "MMN",
        "FTD",
        "Unknown (son of ALS patient)") ~ as.character(NA),
      TRUE ~ as.character(NA)
    ),# end case_when
    site_of_onset_clean = case_when(
      site_of_onset %in% c("Bulbar",
                           "Bulbaire",
                           "Bulbar and Cognitive/Behavioural",
                           "Cognitive/Behavioural and Bulbar",
                           "PBP") ~ "Bulbar",
      site_of_onset %in% c("Spinal",
                           "Arms",
                           "Cervical",
                           "Flail-Arm",
                           "Flail-Leg",
                           "Hemiplegic",
                           "Lower limb",
                           "Spinal",
                           "Spinal and Cognitive/Behavioural",
                           "Cognitive/Behavioural and Spinal",
                           "Upper limb",
                           "Neck",
                           "PMA",
                           "PLS",
                           "Trunk",
                           "trunk") | grepl("Membre", site_of_onset, fixed=FALSE) ~ "Spinal",
      site_of_onset %in% c("Respiratory",
                           "Respiratoire",
                           "Thoracic/Respiratory",
                           "Thoracic/respiratory",
                           "respiratory") ~ "Respiratory",
      site_of_onset %in% c("Bulbar and Spinal",
                           "Bulbar and Thoracic/Respiratory",
                           "Generalised",
                           "Generalized",
                           "Mixed Presentation",
                           "Thoracic/Respiratory and Spinal"
                           
      ) ~ "Generalised",
      site_of_onset %in% c("Monomyelic",
                           "Other",
                           "Pseudopolyneuritic",
                           "Weightloss",
                           "unclear weakness",
                           "Cognitive",
                           "Cognitive impairment",
                           "Cognitive/Behavioural",
                           "Cognitive/Behavioural and Spinal",
                           "FTD") ~ as.character(NA),
      TRUE ~ site_of_onset), # end case_when
    site_of_onset_clean = factor(site_of_onset_clean, levels=c("Bulbar","Spinal","Respiratory","Generalised")),
    diagnosis_clean = factor(diagnosis_clean, levels=c("ALS","PLS","PMA"))
  )

###########################################
######## Use of gastrostomy ###############
###########################################

gast_na <- ext_main%>% group_by(gastrostomy, site) %>% tally()

sites_gast <- c("Bellvitge", "Karolinska", "Turin")
  

## calculate time to gastrostomy from onset
gast_by_site <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, vital_status, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  filter(vital_status == "Deceased" & site %in% sites_gast) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, vital_status, age_at_onset, age_at_gastrostomy), names_to = c("gene", ".value"), names_sep = "_") %>%
  filter(!is.na(tested) & tested != FALSE) %>%
  group_by(gene, status, gastrostomy) %>%
  summarise(cnt=n()) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  drop_na() %>%

total_tested <- ext_main %>%
  select(id, site, gastrostomy, age_at_onset, vital_status, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  filter(vital_status == "Deceased" & site %in% sites_gast) %>%
  pivot_longer(cols = -c(id, site, gastrostomy, vital_status, age_at_onset, age_at_gastrostomy), names_to = c("gene", ".value"), names_sep = "_") %>%
  filter(!is.na(tested) & tested != FALSE) %>%
  group_by(gene, status) %>%
  summarise(cnt=n()) 

gast_perc <- left_join(gast_by_site, total_tested, by=c("gene"="gene", "status"="status"), multiple="all") %>%
  mutate(percent = (cnt.x/cnt.y)*100) %>%
  ggplot() +
  geom_col(aes(
    x=interaction(status, gene), 
    fill=gastrostomy,
    y=percent)
    #position = "dodge"
    ) +
  coord_cartesian(ylim = c(0, 100)) +
  annotate("text", c(0, 1.5, 3.5, 5.5, 7.5), y = - 10, label = c("gene","c9orf72", "fus", "sod1", "tardbp")) +
  annotate("text", x = 0:8, y = - 15,label = c("result", rep(c("Negative", "Positive"), 4))) +
  annotate("text", x = 0:8, y = - 20,label = c(gast_counts[1,])) +
  annotate("text", x = 0:8, y = - 25,label = c(gast_counts[2,])) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

g2 <- ggplot_gtable(ggplot_build(gast_perc))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)

gast_counts <- left_join(gast_by_site, total_tested, by=c("gene"="gene", "status"="status"), multiple="all") %>%
  select(gene, status, cnt.x, gastrostomy) %>%
  drop_na() %>%
  pivot_wider(names_from = c(gene, status), values_from = cnt.x)

write.csv(gast_counts, "gast_counts.csv")

####### summary table of overall gastrostomy use #########

main_summary_gast <- ext_main_cat_clean %>%
  filter(vital_status=="Deceased") %>%
  mutate(gastrostomy_cat = case_when(gastrostomy == TRUE ~ "Gastrostomy",
                                     gastrostomy == FALSE ~ "No gastrostomy",
                                     is.na(gastrostomy) ~ as.character(NA)), 
          gastrostomy_cat = fct_explicit_na(factor(gastrostomy_cat))) %>%
  select(gastrostomy_cat,
         calculated_age_at_onset, 
         calculated_age_at_diagnosis, 
         sex, 
         site_of_onset_clean, 
         diagnosis_clean) %>%
  tbl_summary(by="gastrostomy_cat",
              label = list(
                calculated_age_at_onset = "Age at onset", 
                calculated_age_at_diagnosis = "Age at diagnosis",
                sex = "Sex",
                site_of_onset_clean = "Site of onset of motor symptoms",
                diagnosis_clean = "Diagnostic category")) %>%
  add_p() %>%
  as_gt() %>%
  gt::gtsave(filename = "gastrostomy_table.png")

#############################################
############## Use of NIV ###################
#############################################

niv_na <- ext_main%>% group_by(niv, site) %>% tally()

sites_niv <- c("Bellvitge", "Karolinska", "Turin")

## calculate numbers of people with genetic tests in sites with high proportion of yes/no recorded for niv
niv_by_site <- ext_main %>%
  select(id, site, niv, age_at_onset, vital_status, age_at_gastrostomy, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  filter(vital_status == "Deceased" & site %in% sites_niv) %>%
  pivot_longer(cols = -c(id, site, niv, vital_status, age_at_onset, age_at_gastrostomy), names_to = c("gene", ".value"), names_sep = "_") %>%
  filter(!is.na(tested) & tested != FALSE) %>%
  group_by(gene, status, niv) %>%
  summarise(cnt=n()) %>%
  mutate(percent = cnt/sum(cnt)) %>%
  drop_na() %>%
  
  total_tested_niv <- ext_main %>%
  select(id, site, niv, age_at_onset, vital_status, age_at_niv, c9orf72_tested, c9orf72_status, sod1_tested, sod1_status, fus_tested, fus_status, tardbp_status, tardbp_tested) %>%
  filter(vital_status == "Deceased" & site %in% sites_niv) %>%
  pivot_longer(cols = -c(id, site, niv, vital_status, age_at_onset, age_at_niv), names_to = c("gene", ".value"), names_sep = "_") %>%
  filter(!is.na(tested) & tested != FALSE) %>%
  group_by(gene, status) %>%
  summarise(cnt=n()) 

niv_counts <- left_join(niv_by_site, total_tested_niv, by=c("gene"="gene", "status"="status"), multiple="all") %>%
  select(gene, status, cnt.x, niv) %>%
  drop_na() %>%
  pivot_wider(names_from = c(gene, status), values_from = cnt.x)

niv_perc <- left_join(niv_by_site, total_tested_niv, by=c("gene"="gene", "status"="status"), multiple="all") %>%
  mutate(percent = (cnt.x/cnt.y)*100) %>%
  ggplot() +
  geom_col(aes(
    x=interaction(status, gene), 
    fill=niv,
    y=percent)
    #position = "dodge"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  annotate("text", c(0, 1.5, 3.5, 5.5, 7.5), y = - 10, label = c("gene","c9orf72", "fus", "sod1", "tardbp")) +
  annotate("text", x = 0:8, y = - 15,label = c("result", rep(c("Negative", "Positive"), 4))) +
  annotate("text", x = 0:8, y = - 20,label = c(niv_counts[1,])) +
  annotate("text", x = 0:8, y = - 25,label = c(niv_counts[2,])) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

g2 <- ggplot_gtable(ggplot_build(niv_perc))
g2$layout$clip[g2$layout$name == "panel"] <- "off"
grid.draw(g2)


main_summary_niv <- ext_main_cat_clean %>%
  filter(vital_status=="Deceased") %>%
  mutate(niv_cat = case_when(niv == TRUE ~ "NIV",
                             niv == FALSE ~ "No NIV",
                                     is.na(niv) ~ as.character(NA)), 
         niv_cat = fct_explicit_na(factor(niv_cat))) %>%
  select(niv_cat,
         calculated_age_at_onset, 
         calculated_age_at_diagnosis, 
         sex, 
         site_of_onset_clean, 
         diagnosis_clean) %>%
  tbl_summary(by="niv_cat",
              label = list(
                calculated_age_at_onset = "Age at onset", 
                calculated_age_at_diagnosis = "Age at diagnosis",
                sex = "Sex",
                site_of_onset_clean = "Site of onset of motor symptoms",
                diagnosis_clean = "Diagnostic category")) %>%
  add_p() %>%
  as_gt() %>%
  gt::gtsave(filename = "niv_table.png")
 

####################################################
############## Use of ambulation ###################
####################################################

###### selecting people with ALSFRSR scores
alsfrsr_amb <- ext_alsfrs %>% 
  group_by(id) %>% 
  slice(which.max(age_at_assessment)) %>%
  ungroup() %>%
  select(id, age_at_assessment, q8_walking) %>%
  right_join(ext_main_cat_clean) %>%
  filter(vital_status=="Deceased") %>%
  mutate(perc_alsfrsr = (age_at_assessment/calculated_age_at_death)*100,
        amb_cat = case_when(q8_walking < 3 & (perc_alsfrsr > 80 & perc_alsfrsr < 101) ~ "Ambulatory assistance needed",
                            q8_walking > 2 & (perc_alsfrsr > 80 & perc_alsfrsr < 101) ~ "No ambulatory assistance needed",
                            TRUE ~ as.character(NA)),
        amb_cat = fct_explicit_na(factor(amb_cat)))

main_summary_ambulation <- alsfrsr_amb %>%
  select(amb_cat,
         calculated_age_at_onset, 
         calculated_age_at_diagnosis, 
         sex, 
         site_of_onset_clean, 
         diagnosis_clean,
         c9orf72_status,
         sod1_status,
         fus_status,
         tardbp_status) %>%
  tbl_summary(by="amb_cat",
              label = list(
                calculated_age_at_onset = "Age at onset", 
                calculated_age_at_diagnosis = "Age at diagnosis",
                sex = "Sex",
                site_of_onset_clean = "Site of onset of motor symptoms",
                diagnosis_clean = "Diagnostic category")) %>%
  add_p() %>%
  as_gt() %>%
  gt::gtsave(filename = "amb_table.png")


########################################
######### staging ######################
########################################

### average time spent in stages

kings_time_spent <- ext_kings %>% 
  group_by(id, kings) %>% 
  slice(which.max(time_from_baseline)) %>%
  pivot_wider(names_from=kings, values_from=time_from_baseline) %>%
  mutate(k_stage2 = as.numeric(`2`, "weeks"),
         k_stage3 = as.numeric(`3`, "weeks"),
         k_stage4 = as.numeric(`4`, "weeks"),
         k_stage5 = as.numeric(`5`, "weeks")) %>%
  ungroup()

mitos_time_spent <- ext_mitos %>% 
  group_by(id, mitos) %>% 
  slice(which.max(time_from_baseline)) %>%
  pivot_wider(names_from=mitos, values_from=time_from_baseline) %>%
  mutate(m_stage1 = as.numeric(`1`, "weeks"),
       m_stage2 = as.numeric(`2`, "weeks"),
       m_stage3 = as.numeric(`3`, "weeks"),
       m_stage4 = as.numeric(`4`, "weeks"),
       m_stage5 = as.numeric(`5`, "weeks")) %>%
  ungroup()

time_to_stages <- ext_main_cat_clean %>%
  select(id, diagnosis_clean) %>%
  left_join(select(kings_time_spent, k_stage2, k_stage3, k_stage4, k_stage5, id)) %>%
  left_join(select(mitos_time_spent, m_stage1, m_stage2, m_stage3, m_stage4, m_stage5, id)) %>%
  select(-id) %>%
  tbl_summary(by="diagnosis_clean",
              label = list(
                k_stage2 = "Baseline to King's Stage 2", 
                k_stage3 = "Baseline to King's Stage 3",
                k_stage4 = "Baseline to King's Stage 4",
                k_stage5 = "Baseline to King's Stage 5",
                m_stage1 = "Baseline to Mitos Stage 1",
                m_stage2 = "Baseline to Mitos Stage 2", 
                m_stage3 = "Baseline to Mitos Stage 3",
                m_stage4 = "Baseline to Mitos Stage 4",
                m_stage5 = "Baseline to Mitos Stage 5"
                )) %>%
  add_p()
  


  