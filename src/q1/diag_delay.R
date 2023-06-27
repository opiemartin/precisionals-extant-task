#diag delay - known factors

source("src/q1/inital.r")
source("src/q2/source_clean_frs.r")

ext_main <- ext_main %>% 
  mutate(diagdelay = ((calculated_age_at_diagnosis - calculated_age_at_onset)*12)) %>%
  filter(diagdelay > 0) %>%
  mutate(
    GENE = case_when(
      sod1_stat_BOOL == T ~ "SOD1",
      c9orf72_stat_BOOL == T ~ "C9ORF72",
      fus_status_BOOL == T ~ "FUS",
      tardbp_status_BOOL == T ~ "TARDBP"
    )
  )# %>% 
  #Validation step below- This adds no new values
 # mutate(date_diagdelay = as.numeric(difftime(date_of_diagnosis, date_of_onset, 
  #                                            units = "days"))) %>% 
  #mutate(diagdelay = coalesce(diagdelay, date_diagdelay))

ext_main %>% 
  group_by(GENE) %>%
  summarise(median = round(median(diagdelay, na.rm = TRUE)),
            IQR = round(IQR(diagdelay, na.rm = TRUE)),
            N = length(diagdelay))



nortest::ad.test(ext_main$diagdelay)
pairwise.wilcox.test(ext_main$diagdelay, ext_main$GENE,
                     p.adjust.method = "BH")
#not internally different but perhaps different from gene neg 
#compare only those who have been tested and neg to those with pos gene

diag_compare <- ext_main %>% 
  filter(sod1_test_BOOL == T |
           c9orf72_test_BOOL == T |
           fus_tested_BOOL == T |
           tardbp_tested_BOOL == T) %>% 
  mutate(GENE = ifelse(is.na(GENE), "Negative", GENE))

diag_compare$GENE <- factor(diag_compare$GENE, labels = c("C9ORF72", "FUS","SOD1", "TARDBP", "Negative"))
pairwise.wilcox.test(diag_compare$diagdelay, diag_compare$GENE,
                     p.adjust.method = "BH")

ext_main$GENE <- as.factor(ext_main$GENE)
ext_main %>%
  filter(!is.na(GENE)) %>% 
  ggplot(aes(x = GENE, y = diagdelay, fill = GENE)) +
  geom_boxplot()
