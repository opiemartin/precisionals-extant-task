library("tidyverse")

# Is the age at diagnosis different at each site?
ext_main %>%
  filter(site != "Karolinsk") %>% # Removing for now to investigate why
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
  geom_histogram() +
  facet_wrap(~site)

# Karolinsk is missing this - checked with Éanna, should be removed for age of diag anal

ext_main %>%
  filter(site == "Karolinsk")
select(ext_main$calculated_age_at_diagnosis)
unique(ext_main$site)

# I almost prefer it without the facet wrap actually, clearer image that not that different
ext_main %>%
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
  geom_histogram()

# With c9, is it different?
ext_main %>%
  filter(site != "Karolinsk") %>%
  filter(., c9orf72_stat_BOOL == TRUE) %>%
  ggplot(aes(x = calculated_age_at_diagnosis, fill = site)) +
  geom_histogram() +
  facet_wrap(~site)

# Violins work better
ext_main %>%
  filter(site != "Karolinsk") %>%
  ggplot(aes(x = site, y = calculated_age_at_diagnosis, fill = site)) +
  geom_violin()

# Age of onset may well be good to collect too
ext_main %>%
  filter(date_of_diagnosis) %>%
  ggplot(aes(x = site, y = calculated_age_at_onset, fill = site)) +
  geom_violin()
