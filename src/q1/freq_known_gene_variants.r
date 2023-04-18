#### FREQUENCY OF KNOWN GENE VARIANTS####

source("src/rmcf/inital.r")

# Frequency of known gene variants
summary(ext_main$sod1_stat_BOOL)
summary(ext_main$c9orf72_stat_BOOL)
summary(ext_main$fus_status_BOOL)
summary(ext_main$tardbp_status_BOOL)

# Filter by site
ext_main$site <- as.factor(ext_main$site)
summary(ext_main$site)

levels(ext_main$site)
# BUG - I don't know why this doesn't work, will have to do individually
for (x in c((levels(ext_main$site)))) {
  ext_main %>%
    filter(site == x, (c9orf72_stat_BOOL == TRUE |
      fus_status_BOOL == TRUE |
      tardbp_status_BOOL == TRUE |
      sod1_stat_BOOL == TRUE)) %>%
    select(
      c9orf72_test_BOOL,
      fus_status_BOOL,
      tardbp_status_BOOL,
      sod1_stat_BOOL
    ) %>%
    summary()
}


# Trinity
ext_main %>%
  filter(site == "Trinity", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()

# Kings
# Is there no genetic data available?
levels(ext_main$site)
ext_main %>%
  filter(site == "King's", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()

# Bellvitge
ext_main %>%
  filter(site == "Bellvitge", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()

# Karolinska
ext_main %>%
  filter(site == "Karolinsk", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()
# Leuven
ext_main %>%
  filter(site == "Leuven", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()
# Tours
ext_main %>%
  filter(site == "Tours", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()
# Sheffield
ext_main %>%
  filter(site == "Sheffield", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()
# Turin
ext_main %>%
  filter(site == "Turin", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()
# Utrecht
ext_main %>%
  filter(site == "Utrecht", (c9orf72_stat_BOOL == TRUE |
    fus_status_BOOL == TRUE |
    tardbp_status_BOOL == TRUE |
    sod1_stat_BOOL == TRUE)) %>%
  select(
    c9orf72_stat_BOOL,
    fus_status_BOOL,
    tardbp_status_BOOL,
    sod1_stat_BOOL
  ) %>%
  summary()
#### END####
