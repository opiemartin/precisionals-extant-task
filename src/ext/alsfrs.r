library(dplyr)
library(lubridate)
library(tidyr)

source("src/ext/main.r")

ext_alsfrs_clean <- function(data) {
    data %>%
        drop_na(total_score) %>%
        filter(total_score >= 0) %>%
        mutate(total_score = if_else(total_score %>% between(0, 48), total_score, 33)) %>%
        ext_rows_delete(tibble(id = "BEL-0461", date_of_assessment = ymd("2011-01-20")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0559", date_of_assessment = ymd("2011-12-20")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0627", date_of_assessment = ymd("2015-04-30")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0845", date_of_assessment = ymd("2016-10-20")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0999", date_of_assessment = ymd("2021-09-03")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-1010", date_of_assessment = ymd("2010-01-12")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-1160", date_of_assessment = ymd("2019-01-03")), by = c("id", "date_of_assessment")) %>%
        # ext_rows_delete(tibble(id = "BEL-1205", date_of_assessment = ymd("2019-09-20")), by = c("id", "date_of_assessment")) %>%
        # ext_rows_delete(tibble(id = "BEL-1581", date_of_assessment = ymd("2019-08-23")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "IRE-1635", date_of_assessment = ymd("2014-10-09")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "IRE-3419", date_of_assessment = ymd("2020-01-16")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "IRE-3926", date_of_assessment = ymd("2021-02-11")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "ITA-0697", date_of_assessment = ymd("2007-10-30")), by = c("id", "date_of_assessment")) %>%
        filter(!(id == "NLD-0010" & age_at_assessment >= 59.63)) %>%
        ext_rows_update(tibble(id = "NLD-0253", age_at_assessment = 65.56), by = c("id", "age_at_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "BEL-1098" & date_of_assessment == ymd("2018-01-22"), ymd("2019-01-22"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "FRA-0260" & date_of_assessment == ymd("2004-03-15"), ymd("2005-03-15"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "FRA-0964" & date_of_assessment == ymd("2009-10-10"), ymd("2008-10-10"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "IRE-3035" & date_of_assessment == ymd("2014-09-25"), ymd("2015-09-25"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "BEL-0353" & date_of_assessment == ymd("2010-01-21"), ymd("2009-01-21"), date_of_assessment)) %>%
        ext_rows_update(tibble(id = "BEL-0596", date_of_assessment = ymd("2013-06-04"), total_score = 42), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0353", date_of_assessment = ymd("2009-05-14")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0353", date_of_assessment = ymd("2009-08-06")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-0530", date_of_assessment = ymd("2015-06-04")), by = c("id", "date_of_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "BEL-0535" & date_of_assessment == ymd("2013-11-29"), ymd("2012-11-29"), date_of_assessment)) %>%
        ### BEL-1013 this person going up and down
        mutate(date_of_assessment = if_else(id == "BEL-1447" & date_of_assessment == ymd("2021-04-15"), ymd("2020-04-15"), date_of_assessment)) %>%
        ext_rows_delete(tibble(id = "BEL-1013", date_of_assessment = ymd("2015-04-21")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "BEL-1013", date_of_assessment = ymd("2020-10-19")), by = c("id", "date_of_assessment")) %>%
        ext_rows_update(tibble(id = "SHE-0094", date_of_assessment = ymd("2016-10-05"), q12_respiratory_insufficiency = 4), by = c("id", "date_of_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "SHE-0156" & date_of_assessment == ymd("2011-07-11"), ymd("2012-07-11"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SHE-0195" & date_of_assessment == ymd("2009-09-10"), ymd("2008-09-10"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SHE-0208" & date_of_assessment == ymd("2017-02-28"), ymd("2018-02-28"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SHE-0228" & date_of_assessment == ymd("2009-09-14"), ymd("2008-09-14"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SHE-0343" & date_of_assessment == ymd("2014-07-23"), ymd("2015-07-23"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SHE-0508" & date_of_assessment == ymd("2014-10-08"), ymd("2015-10-08"), date_of_assessment)) %>%
        ## Based on date of diagnosis/death from main file
        mutate(date_of_assessment = if_else(id == "SHE-0589" & date_of_assessment == ymd("2013-05-22"), ymd("2012-05-22"), date_of_assessment)) %>%
        ext_rows_delete(tibble(id = "SHE-0867", date_of_assessment = ymd("2017-07-26")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SHE-0871", date_of_assessment = ymd("2011-03-16")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SHE-0871", date_of_assessment = ymd("2011-09-22")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SHE-1220", date_of_assessment = ymd("2019-11-12")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SHE-1313", date_of_assessment = ymd("2018-03-13")), by = c("id", "date_of_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "SHE-1341" & date_of_assessment == ymd("2020-01-22"), ymd("2019-01-22"), date_of_assessment)) %>%
        ext_rows_delete(tibble(id = "SPA-0008", date_of_assessment = ymd("2017-12-01")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0025", date_of_assessment = ymd("2021-11-02")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0046", date_of_assessment = ymd("2017-10-06")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0068", date_of_assessment = ymd("2017-06-09")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0068", date_of_assessment = ymd("2019-03-22")), by = c("id", "date_of_assessment")) %>%
        ## SPA-0070 checked but the numbers are all over the place generally, not sure what to exclude/change
        # . Ruben: seems like two different patients:
        # checkFRS ("SPA-0070")
        # . Exclude totaly
        ext_rows_delete(tibble(id = "SPA-0070"), by = "id") %>%
        ext_rows_delete(tibble(id = "SPA-0083", date_of_assessment = ymd("2018-02-02")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0083", date_of_assessment = ymd("2018-06-08")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0083", date_of_assessment = ymd("2018-10-19")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0097", date_of_assessment = ymd("2020-10-06")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0189", date_of_assessment = ymd("2016-12-16")), by = c("id", "date_of_assessment")) %>%
        ## SPA-0305 person appears to be getting better and then worse throughout
        # checkFRS ("SPA-0305") # two different patients
        ext_rows_delete(tibble(id = "SPA-0305"), by = "id") %>%
        mutate(date_of_assessment = if_else(id == "SPA-0361" & date_of_assessment == ymd("2019-02-19"), ymd("2020-02-19"), date_of_assessment)) %>%
        ## SPA-0391 - 2019-02-15 probably not wrong year, might be genuine increase
        # . Ruben: this is entry mistake
        ext_rows_delete(tibble(id = "SPA-0391", date_of_assessment = ymd("2017-03-24")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0418", date_of_assessment = ymd("2018-04-27")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0466", date_of_assessment = ymd("2022-01-18")), by = c("id", "date_of_assessment")) %>%
        ## SPA-0480 think it is a genuine rapid drop
        # . Ruben: yes but 3rd measure is incorrect
        # checkFRS ("SPA-0488")
        ext_rows_delete(tibble(id = "SPA-0488", date_of_assessment = ymd("2019-02-15")), by = c("id", "date_of_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "SPA-0673" & date_of_assessment == ymd("2020-04-23"), ymd("2019-04-23"), date_of_assessment)) %>%
        ext_rows_delete(tibble(id = "SPA-0698", date_of_assessment = ymd("2020-10-30")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "SPA-0762", date_of_assessment = ymd("2021-06-04")), by = c("id", "date_of_assessment")) %>%
        # checkFRS ("SPA-0762")
        # D[!(D$ID == "SWE-0114" & (D$DATE == "2017-11-06"|
        #                             D$DATE == "2019-06-13"|
        #                             D$DATE == "2020-02-03"|
        #                             D$DATE == "2020-08-13"|
        #                             D$DATE == "2020-11-03"|
        #                             D$DATE == "2021-03-29"|
        #                             D$DATE == "2021-07-01")), ]
        # checkFRS ("SWE-0114") # here only second is mistake
        ext_rows_delete(tibble(id = "SWE-0114", date_of_assessment = ymd("2017-07-01")), by = c("id", "date_of_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "SWE-0243" & date_of_assessment == ymd("2021-12-31"), ymd("2020-12-31"), date_of_assessment)) %>%
        ext_rows_delete(tibble(id = "SWE-0263", date_of_assessment = ymd("2017-12-01")), by = c("id", "date_of_assessment")) %>%
        mutate(date_of_assessment = if_else(id == "SWE-0270" & date_of_assessment == ymd("2019-02-02"), ymd("2020-02-02"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SWE-0283" & date_of_assessment == ymd("2018-09-09"), ymd("2019-09-09"), date_of_assessment)) %>%
        mutate(date_of_assessment = if_else(id == "SWE-0331" & date_of_assessment == ymd("2016-11-07"), ymd("2017-11-07"), date_of_assessment)) %>%
        ext_rows_delete(tibble(id = "SWE-0355", date_of_assessment = ymd("2022-04-20")), by = c("id", "date_of_assessment")) %>%
        # checkFRS ("SWE-0355")
        # . Addition Daphne
        ext_rows_delete(tibble(id = "IRE-2352", date_of_assessment = ymd("2002-09-25")), by = c("id", "date_of_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-0327"), by = "id") %>%
        filter(!(id == "NLD-2208" & age_at_assessment > 36)) %>%
        ext_rows_delete(tibble(id = "NLD-2383"), by = "id") %>%
        ext_rows_delete(tibble(id = "NLD-2403"), by = "id") %>%
        ext_rows_delete(tibble(id = "NLD-2917"), by = "id") %>%
        ext_rows_delete(tibble(id = "NLD-0592", age_at_assessment = 44.11), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-1690", age_at_assessment = 47.81), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-1814", age_at_assessment = 54.91), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-1822", age_at_assessment = 55.07), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-1890", age_at_assessment = 62.67), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2022", age_at_assessment = 57.59), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2097", age_at_assessment = 47.78), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2316", age_at_assessment = 72.45), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2366", age_at_assessment = 69.61), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2473", age_at_assessment = 64.66), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2540", age_at_assessment = 52.85), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2731", age_at_assessment = 76), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2765", age_at_assessment = 74.25), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2786", total_score = 43), by = c("id", "total_score")) %>%
        ext_rows_delete(tibble(id = "NLD-2828", age_at_assessment = 72.23), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2946", age_at_assessment = 72.98), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2682", age_at_assessment = 58.90), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2682", age_at_assessment = 59.31), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2682", age_at_assessment = 59.04), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2798", age_at_assessment = 69.32), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2798", age_at_assessment = 69.3), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2798", age_at_assessment = 69.23), by = c("id", "age_at_assessment")) %>%
        # ext_rows_delete(tibble(id = "NLD-2861", age_at_assessment = 40), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2861", age_at_assessment = 62.69), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2861", age_at_assessment = 62.77), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2864", age_at_assessment = 71.21), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2864", age_at_assessment = 71.3), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2919", age_at_assessment = 55.01), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2919", age_at_assessment = 55.12), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2919", total_score = 41), by = c("id", "total_score")) %>%
        ext_rows_delete(tibble(id = "NLD-2950", age_at_assessment = 58.23), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2950", age_at_assessment = 58.24), by = c("id", "age_at_assessment")) %>%
        ext_rows_delete(tibble(id = "NLD-2964", age_at_assessment = 46.26), by = c("id", "age_at_assessment")) %>%
        mutate(age_at_assessment = if_else(id == "NLD-0979" & age_at_assessment == 74.18, 73.18, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-1073" & age_at_assessment == 77.11, 76.11, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-1556" & age_at_assessment == 59.77, 60.77, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-1999" & age_at_assessment == 72.72, 71.72, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2188" & age_at_assessment == 52.71, 51.71, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2275" & age_at_assessment == 70.34, 71.34, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2349" & age_at_assessment == 77.4, 76.4, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2807" & age_at_assessment == 67.59, 68.59, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2847" & age_at_assessment == 68.59, 67.59, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2919" & age_at_assessment == 54.39, 55.39, age_at_assessment)) %>%
        mutate(age_at_assessment = if_else(id == "NLD-2955" & age_at_assessment == 69, 70, age_at_assessment)) %>%
        ext_rows_update(tibble(id = "NLD-2759", age_at_assessment = 47.29, total_score = 43), by = c("id", "age_at_assessment"))
}

ext_alsfrs_calculate_assessment_times <- function(data) {
    data %>%
        group_by(id) %>%
        mutate(
            time_from_baseline = case_when(
                all(!is.na(date_of_assessment)) ~
                    (date_of_assessment - min(date_of_assessment)) / dmonths(1),
                all(!is.na(age_at_assessment)) ~
                    (age_at_assessment - min(age_at_assessment)) * 12,
                TRUE ~ NA
            )
        ) %>%
        arrange(time_from_baseline, .by_group = TRUE) %>%
        mutate(
            time_from_last_assessment = time_from_baseline - lag(time_from_baseline)
        ) %>%
        ungroup()
}

ext_alsfrs_progression_category <- function(x) {
    case_when(
        x < 0.8 ~ "SP",
        x %>% between(0.8, 1.35) ~ "NP",
        x > 1.35 ~ "FP"
    )
}

ext_alsfrs <- suppressWarnings(
    ext_load_data(
        "P-ALS_Ext_V2_ALSFRS-R.xlsx",
        col_types = c(
            "text", # ID
            "text", # Site
            "date", # Date of Assessment
            "numeric", # Age of Assessment
            rep("numeric", times = 15) # ...
        )
    )
) %>%
    rename_with(~ str_replace(.x, "x(\\d+[abx]?)_", "q\\1_")) %>%
    rename_with(~ str_replace_all(.x, "hygine", "hygiene")) %>%
    rename(age_at_assessment = "age_of_assessment") %>%
    filter(!is.na(date_of_assessment) | !is.na(age_at_assessment)) %>%
    ext_alsfrs_clean() %>%
    ext_alsfrs_calculate_assessment_times()

ext_baseline <- ext_alsfrs %>%
    filter(time_from_baseline == 0) %>%
    group_by(id) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    left_join(ext_main, by = "id") %>%
    transmute(
        id, total_score,
        date_of_baseline = date_of_assessment,
        age_at_baseline = coalesce(
            age_at_assessment,
            (date_of_baseline - date_of_birth) / dyears(1)
        ),
        time_from_onset = coalesce(
            (age_at_assessment - age_at_onset) * 12,
            (date_of_assessment - date_of_onset) / dmonths(1)
        ),
        delta_fs = (48 - total_score) / time_from_onset
    )
