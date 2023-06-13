#fast and non-fast progressors within gentic variants
#Ruben suggested mounthly change: 
#> -0.68ALSFRS - SLOW
# -0.68 - -1.26 - AVERAGE
# -1.26 - FAST

source("src/q1/inital.r")
source("source_clean_frs.r")

 D_max <- aggregate(TOTAL ~ ID, data = D, FUN = max)
 D_max <- rename(D_max, MAX_FRS = TOTAL)
 D_min <- aggregate(TOTAL ~ ID, data = D, FUN = min)
 D_min <- rename(D_min, MIN_FRS = TOTAL)
 D_FRS <- merge(D_max, D_min, by = "ID")

#clock starts at first reading fomr time so only max needed
 
D_time <- aggregate(TIME ~ ID, data = D, FUN = max)
D_FRS <- merge(D_FRS, D_time, by = "ID")

D_FRS <- D_FRS %>% 
  mutate(FRS_DIFF = MAX_FRS - MIN_FRS) %>% 
  filter(!is.na(FRS_DIFF), FRS_DIFF != 0) %>% 
  mutate(FRS_CHANGE = FRS_DIFF/TIME)

#gene merge 
gene_merge <- ext_main %>%  
  select(sod1_test_BOOL,
         sod1_stat_BOOL,
         c9orf72_test_BOOL,
         c9orf72_stat_BOOL,
         fus_tested_BOOL,
         fus_status_BOOL,
         tardbp_tested_BOOL,
         tardbp_status_BOOL,
         id)
gene_merge <- rename(gene_merge, ID = id)
D_FRS <- merge(D_FRS, gene_merge, by = "ID")

D_FRS <- D_FRS %>% 
  filter(FRS_CHANGE < Inf) %>% 
  mutate(PROG = case_when(
      FRS_CHANGE > 1.26 ~ "Fast",
      FRS_CHANGE > 0.6 & FRS_CHANGE <= 1.26 ~ "Average",
      FRS_CHANGE <= 0.6 ~ "Slow"
  ))
table(D_FRS$PROG)

C9_progress <- D_FRS %>% 
  select(ID, c9orf72_stat_BOOL, PROG) %>% 
  filter(c9orf72_stat_BOOL == T)
table(C9_progress$PROG)

sod_progress <- D_FRS %>% 
  select(ID, sod1_stat_BOOL, PROG) %>% 
  filter(sod1_stat_BOOL == T)
table(sod_progress$PROG)

fus_progress <- D_FRS %>% 
  select(ID, fus_status_BOOL, PROG) %>% 
  filter(fus_status_BOOL == T)
table(fus_progress$PROG)


tardbp_progress <- D_FRS %>% 
  select(ID, tardbp_status_BOOL, PROG) %>% 
  filter(tardbp_status_BOOL == T)
table(tardbp_progress$PROG)

#sig diff between progression rates - keep continuous - between genes?

D_FRS <- D_FRS %>% 
  mutate(
    GENE = case_when(
      sod1_stat_BOOL == T ~ "SOD1",
      c9orf72_stat_BOOL == T ~ "C9ORF72",
      fus_status_BOOL == T ~ "FUS",
      tardbp_status_BOOL == T ~ "TARDBP"
    )
  )

gene_compare <- D_FRS %>% 
  filter(!is.na(GENE))
gene_compare$GENE <- as.factor(gene_compare$GENE)

gene_compare %>% 
  group_by(GENE) %>%
  summarise(median = round(median(FRS_CHANGE, na.rm = TRUE)),
            IQR = round(IQR(FRS_CHANGE, na.rm = TRUE)),
            N = length(FRS_CHANGE))

gene_compare %>% 
  filter(FRS_CHANGE < 30) %>% 
  ggplot(aes(x = GENE, y = FRS_CHANGE, fill = GENE)) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "green", "yellow"))


library("ggpubr")
gene_compare %>% 
  filter(FRS_CHANGE < 30) %>% 
  ggline( x = "GENE", y = "FRS_CHANGE", 
       add = c("mean_se", "jitter"), 
       #order = c("ctrl", "trt1", "trt2"),
       ylab = "FRS CHANGE", xlab = "GENE")

shapiro.test(gene_compare$FRS_CHANGE)
#no
pairwise.wilcox.test(gene_compare$FRS_CHANGE, gene_compare$GENE,
                     p.adjust.method = "BH")


