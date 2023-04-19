##### CHARACTERISING PROGRESSION IN GROUPS #####

source("src/ext/main.r")

## took 20 mins to run this so bear with it or we need to rejig a bit
##source("src/ext/alsfrs.r")


## this source file has the alsfrs.r source file in it and consequently also takes a long time to run
## done run both staging.r and aslfrs.r
source("src/ext/staging.r")

## get age and date of onset for each record



alsfrsr_rate <- ext_alsfrs %>%
  left_join(select(ext_main, date_of_birth, calculated_age_at_onset, id),  by=c("id"="id")) %>%
  mutate(calculated_age_at_assessment = case_when(is.na(age_at_assessment)~ as.numeric((date_of_assessment-date_of_birth)/365.25),
                                                  TRUE ~ as.numeric(age_at_assessment)))