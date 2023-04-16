library(tibble)

source("src/ext/alsfrs.r")
source("src/ext/main.r")

ext_mitos <- ext_alsfrs %>%
    ext_alsfrs_clean() %>%
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

time_to_mitos_by_age <-
    ext_main %>%
    select(id) %>%
    cross_join(tibble(mitos = 0:4)) %>%
    bind_rows(ext_mitos) %>%
    select(-date_of_assessment) %>%
    slice_min(
        age_at_assessment,
        by = c(id, mitos),
        n = 1,
        with_ties = FALSE
    ) %>%
    group_by(id) %>%
    arrange(mitos, .by_group = TRUE) %>%
    fill(age_at_assessment, .direction = "up") %>%
    ungroup() %>%
    pivot_wider(
        names_from = mitos,
        names_prefix = "age_at_mitos_",
        values_from = age_at_assessment
    )

ext_kings <- ext_alsfrs %>%
    ext_alsfrs_clean() %>%
    left_join(ext_main, by = "id") %>%
    mutate(
        age_at_assessment = coalesce(
            age_at_assessment, (date_of_assessment - date_of_birth) / dyears(1),
        ),
        has_gastrostomy = case_when(
            !is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ FALSE,
            !is.na(q5b_cutting_food_with_gastrostomy) &
                is.na(q5a_cutting_food_without_gastrostomy) &
                is.na(q5x_cutting_food_with_gastrostomy_status_unknown) ~ TRUE,
            gastrostomy == TRUE & date_of_assessment >= date_of_gastrostomy ~ TRUE,
            gastrostomy == TRUE & age_at_assessment >= age_at_gastrostomy ~ TRUE,
            gastrostomy == TRUE & date_of_assessment < date_of_gastrostomy ~ FALSE,
            gastrostomy == TRUE & age_at_assessment < age_at_gastrostomy ~ FALSE,
            gastrostomy == FALSE ~ FALSE,
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
    select(id, date_of_assessment, age_at_assessment, kings) %>%
    drop_na(id, kings)

time_to_kings_by_age <-
    ext_main %>%
    select(id) %>%
    cross_join(tibble(kings = 0:4)) %>%
    bind_rows(ext_kings) %>%
    select(-date_of_assessment) %>%
    slice_min(
        age_at_assessment,
        by = c(id, kings),
        n = 1,
        with_ties = FALSE
    ) %>%
    group_by(id) %>%
    arrange(kings, .by_group = TRUE) %>%
    fill(age_at_assessment, .direction = "up") %>%
    pivot_wider(
        names_from = kings,
        names_prefix = "age_at_kings_",
        values_from = age_at_assessment
    )
