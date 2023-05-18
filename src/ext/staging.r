library(rlang)
library(tibble)
library(stringr)

source("src/ext/main.r")
source("src/ext/alsfrs.r")

ext_staging_deaths <- ext_main %>%
    filter(vital_status == "Deceased") %>%
    left_join(ext_baseline, by = "id") %>%
    transmute(
        id = id, kings = 5, mitos = 5,
        time_from_baseline = coalesce(
            (age_at_death - age_at_baseline) * 12,
            (date_of_death - date_of_baseline) / dmonths(1)
        )
    )

ext_mitos <- ext_alsfrs %>%
    mutate(mitos = {
        walking_selfcare <- q8_walking <= 1 | q6_dressing_and_hygiene <= 1
        swallowing <- q3_swallowing <= 1
        communication <- q1_speech <= 1 | q4_handwriting <= 1
        breathing <- q10_dyspnea <= 1 | q12_respiratory_insufficiency <= 2
        walking_selfcare + swallowing + communication + breathing
    }) %>%
    bind_rows(ext_staging_deaths) %>%
    select(id, time_from_baseline, mitos) %>%
    arrange(id, time_from_baseline) %>%
    drop_na()

ext_kings <- ext_alsfrs %>%
    left_join(ext_main, by = "id") %>%
    mutate(
        has_gastrostomy = case_when(
            gastrostomy == TRUE & date_of_assessment >= date_of_gastrostomy ~ TRUE,
            gastrostomy == TRUE & age_at_assessment >= age_at_gastrostomy ~ TRUE,
            gastrostomy == TRUE & date_of_assessment < date_of_gastrostomy ~ FALSE,
            gastrostomy == TRUE & age_at_assessment < age_at_gastrostomy ~ FALSE,
            gastrostomy == FALSE ~ FALSE,
            !is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ FALSE,
            !is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ TRUE,
        ),
        kings = case_when(
            q10_dyspnea == 0 ~ 4,
            q12_respiratory_insufficiency < 4 ~ 4,
            has_gastrostomy == TRUE ~ 4,
            has_gastrostomy == FALSE ~ {
                bulbar <- any(c(q1_speech, q2_salivation, q3_swallowing) < 4)
                upper <- any(c(
                    q4_handwriting,
                    q5a_cutting_food_without_gastrostomy,
                    q5x_cutting_food_with_gastrostomy_status_unknown
                ) < 4)
                lower <- q8_walking < 4
                bulbar + upper + lower
            }
        ),
    ) %>%
    bind_rows(ext_staging_deaths) %>%
    select(id, time_from_baseline, kings) %>%
    arrange(id, time_from_baseline) %>%
    drop_na()
