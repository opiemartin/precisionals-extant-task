#### FREQUENCY OF KNOWN GENE VARIANTS####

source("src/q1/inital.r")

###tested or not
summary(ext_main$sod1_test_BOOL)
summary(ext_main$c9orf72_test_BOOL)
summary(ext_main$fus_tested_BOOL)
summary(ext_main$tardbp_tested_BOOL)

#Frequency of known gene variants
summary(ext_main$sod1_stat_BOOL)
summary(ext_main$c9orf72_stat_BOOL)
summary(ext_main$fus_status_BOOL)
summary(ext_main$tardbp_status_BOOL)

#create a column where we have a total of out gene positives
ext_main$gene_positive <- ifelse(ext_main$sod1_stat_BOOL | 
                                   ext_main$c9orf72_stat_BOOL | 
                                   ext_main$fus_status_BOOL | 
                                   ext_main$tardbp_status_BOOL, TRUE, FALSE)
summary(ext_main$gene_positive) #1127 with positive genetic mutation

ext_main %>% 
  filter(site != "King's") %>% 
  ggplot() + 
  geom_bar(mapping = aes(x = site, fill = gene_positive)) +
  xlab("PRECISION ALS SITE") +
  ylab("PATIENTS TESTED") +
  scale_fill_discrete(name = "Genetic test result")

#positive as a breakdown
positive_breakdown <- data.frame(
  gene = c("C9ORF72", "FUS", "TARDBP", 'SOD1'),
  positive = c(sum(ext_main$c9orf72_stat_BOOL), 
               sum(ext_main$fus_status_BOOL),
               sum(ext_main$tardbp_status_BOOL),
               sum(ext_main$tardbp_status_BOOL))
)
#Pie chart
ggplot(positive_breakdown, aes(x = "", y = positive, fill = gene)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = positive), position = position_stack(vjust = 0.5)) +
  labs(fill = "Gene")

#Filter by site
ext_main$site <- as.factor(ext_main$site)
summary(ext_main$site)

levels(ext_main$site)
#BUG - I don't know why this doesn't work, will have to do individually
for (x in  c((levels(ext_main$site)))){
  ext_main %>% 
    filter(site == x , (c9orf72_stat_BOOL == TRUE |
                          fus_status_BOOL == TRUE |
                          tardbp_status_BOOL == TRUE|
                          sod1_stat_BOOL == TRUE)) %>% 
    select(c9orf72_test_BOOL, 
           fus_status_BOOL, 
           tardbp_status_BOOL, 
           sod1_stat_BOOL) %>% 
    summary()
}

####Site specific####
#Trinity
ext_main %>% 
  filter(site == 'Trinity', (c9orf72_stat_BOOL == TRUE |
                               fus_status_BOOL == TRUE |
                               tardbp_status_BOOL == TRUE|
                               sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()

#Kings
#Is there no genetic data available?
levels(ext_main$site)
ext_main %>% 
  filter(site == "King's", (c9orf72_stat_BOOL == TRUE |
                              fus_status_BOOL == TRUE |
                              tardbp_status_BOOL == TRUE|
                              sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()

#Bellvitge
ext_main %>% 
  filter(site == "Bellvitge", (c9orf72_stat_BOOL == TRUE |
                                 fus_status_BOOL == TRUE |
                                 tardbp_status_BOOL == TRUE|
                                 sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()

#Karolinska
ext_main %>% 
  filter(site == "Karolinsk", (c9orf72_stat_BOOL == TRUE |
                                 fus_status_BOOL == TRUE |
                                 tardbp_status_BOOL == TRUE|
                                 sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()
#Leuven
ext_main %>% 
  filter(site == "Leuven", (c9orf72_stat_BOOL == TRUE |
                              fus_status_BOOL == TRUE |
                              tardbp_status_BOOL == TRUE|
                              sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()
#Tours
ext_main %>% 
  filter(site == "Tours", (c9orf72_stat_BOOL == TRUE |
                             fus_status_BOOL == TRUE |
                             tardbp_status_BOOL == TRUE|
                             sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()
#Sheffield
ext_main %>% 
  filter(site == "Sheffield", (c9orf72_stat_BOOL == TRUE |
                                 fus_status_BOOL == TRUE |
                                 tardbp_status_BOOL == TRUE|
                                 sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()
#Turin 
ext_main %>% 
  filter(site == "Turin", (c9orf72_stat_BOOL == TRUE |
                             fus_status_BOOL == TRUE |
                             tardbp_status_BOOL == TRUE|
                             sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()
#Utrecht
ext_main %>% 
  filter(site == "Utrecht", (c9orf72_stat_BOOL == TRUE |
                               fus_status_BOOL == TRUE |
                               tardbp_status_BOOL == TRUE|
                               sod1_stat_BOOL == TRUE)) %>% 
  select(c9orf72_stat_BOOL, 
         fus_status_BOOL, 
         tardbp_status_BOOL, 
         sod1_stat_BOOL) %>% 
  summary()

#What is the staging of each of the gene variants? spinal v bulbar in c9 for example

c9_pos <- ext_main %>% 
  filter(c9orf72_stat_BOOL == TRUE)

summary(c9_pos$bulbar_onset)
summary(c9_pos$respiratory_onset)
summary(c9_pos$cognitive_onset)
summary(c9_pos$spinal_onset)

c9site <- data.frame(
  site = c("Bulbar", "Respiratory", "Cognitive", 'Spinal'),
  number = c(sum(c9_pos$bulbar_onset), 
             sum(c9_pos$respiratory_onset),
             sum(c9_pos$cognitive_onset),
             sum(c9_pos$spinal_onset)))

ggplot(c9site, aes(x = "", y = number, fill = site)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Site of onset in C9ORF72",
     title = "C9ORF72")

#sod1
sod1_pos <- ext_main %>% 
  filter(sod1_stat_BOOL == TRUE)

sod1_site <- data.frame(
  site = c("Bulbar", "Respiratory", "Cognitive", 'Spinal'),
  number = c(sum(sod1_pos$bulbar_onset), 
             sum(sod1_pos$respiratory_onset),
             sum(sod1_pos$cognitive_onset),
             sum(sod1_pos$spinal_onset)))

ggplot(sod1_site, aes(x = "", y = number, fill = site)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Site of onset in SOD1",
       title = "SOD1")
#fus

fus_pos <- ext_main %>% 
  filter(fus_status_BOOL == TRUE)

fus_site <- data.frame(
  site = c("Bulbar", "Respiratory", "Cognitive", 'Spinal'),
  number = c(sum(fus_pos$bulbar_onset), 
             sum(fus_pos$respiratory_onset),
             sum(fus_pos$cognitive_onset),
             sum(fus_pos$spinal_onset)))

ggplot(fus_site, aes(x = "", y = number, fill = site)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Site of onset in FUS",
       title = "FUS")

#tardbp

tardbp_pos <- ext_main %>% 
  filter(tardbp_status_BOOL == TRUE)

tardbp_site <- data.frame(
  site = c("Bulbar", "Respiratory", "Cognitive", 'Spinal'),
  number = c(sum(tardbp_pos$bulbar_onset), 
             sum(tardbp_pos$respiratory_onset),
             sum(tardbp_pos$cognitive_onset),
             sum(tardbp_pos$spinal_onset)))

ggplot(tardbp_site, aes(x = "", y = number, fill = site)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(fill = "Site of onset in TARDBP",
       title = "TARDBP")


library(data.table)

#earliest mitos stage
ext_mitos <- ext_alsfrs %>%
  left_join(ext_main, by = "id") %>%
  transmute(
    id = id,
    date_of_assessment = date_of_assessment,
    age_at_assessment = coalesce(
      age_at_assessment, (date_of_assessment - date_of_birth) / dyears(1)
    ),
    mitos = {
      walking_selfcare <- q8_walking <= 1 | q6_dressing_and_hygiene <= 1
      swallowing <- q3_swallowing <= 1
      communication <- q1_speech <= 1 | q4_handwriting <= 1
      breathing <- q10_dyspnea <= 1 | q12_respiratory_insufficiency <= 2
      walking_selfcare + swallowing + communication + breathing
    }
  ) %>%
  drop_na(id, mitos)

ext_mitos <- ext_mitos %>%
  group_by(id) %>%
  mutate(time_since_first = as.numeric(difftime(date_of_assessment, min(date_of_assessment), 
                                                units = "days"))/30)
ext_mitos %>% 
  filter(time_since_first == 0, c9orf72_stat_BOOL == TRUE) %>% 
  ggplot(aes(x = mitos, fill = "red")) +
  geom_bar() +
  labs(title = 'MITOS score at diagnosis in C9ORF72',
       x = "MITOS score",
       y = "")

#add c9 to mitos
ext_mitos <- merge(ext_mitos, ext_main[,c("id", "c9orf72_stat_BOOL")], by="id")


#earliest kings stage
ext_kings <- ext_kings %>%
  group_by(id) %>%
  mutate(time_since_first = as.numeric(difftime(date_of_assessment, min(date_of_assessment), 
                                                units = "days"))/30)  

ext_kings %>% 
  filter(time_since_first == 0) %>% 
  ggplot(aes(x = kings)) +
  geom_histogram()




####END####
