library(tibble)

source("src/ext/alsfrs.r")
source("src/ext/main.r")

ext_mitos <- ext_alsfrs %>%
    ext_alsfrs_clean() %>%
    ext_alsfrs_calculate_assessment_times() %>%
    mutate(mitos = {
        walking_selfcare <- q8_walking <= 1 | q6_dressing_and_hygiene <= 1
        swallowing <- q3_swallowing <= 1
        communication <- q1_speech <= 1 | q4_handwriting <= 1
        breathing <- q10_dyspnea <= 1 | q12_respiratory_insufficiency <= 2
        walking_selfcare + swallowing + communication + breathing
    }) %>%
    select(id, time_from_baseline, mitos) %>%
    drop_na()

ext_kings <- ext_alsfrs %>%
    ext_alsfrs_clean() %>%
    ext_alsfrs_calculate_assessment_times() %>%
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
    select(id, time_from_baseline, kings) %>%
    drop_na()
