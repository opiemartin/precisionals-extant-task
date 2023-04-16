library(rlang)
library(tibble)
library(stringr)

source("src/ext/alsfrs.r")
source("src/ext/main.r")

ext_calculate_time_to_stage <- function(data, time, stage, values) {
    time_col <- as_label(enquo(time))
    stage_col <- as_label(enquo(stage))
    time_to_stage_col <- str_glue("time_to_{stage_col}_")

    tibble(id = unique(data$id)) %>%
        cross_join(tibble({{ stage }} := values)) %>%
        bind_rows(data %>% select(id, {{ time }}, {{ stage }})) %>%
        slice_min({{ time }}, by = c(id, {{ stage }}), n = 1, with_ties = FALSE) %>%
        group_by(id) %>%
        arrange({{ stage }}, .by_group = TRUE) %>%
        fill({{ time }}, .direction = "up") %>%
        ungroup() %>%
        pivot_wider(
            names_from = stage_col,
            names_prefix = time_to_stage_col,
            values_from = time_col
        )
}

ext_mitos <- ext_alsfrs %>%
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

ext_baseline <- ext_alsfrs %>%
    filter(time_from_baseline == 0) %>%
    group_by(id) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    left_join(ext_main, by = "id") %>%
    transmute(id,
        time_from_diagnosis = coalesce(
            (date_of_assessment - date_of_diagnosis) / dmonths(1),
            (age_at_assessment - age_at_diagnosis) / (365.25 / 12)
        ),
        time_from_onset = coalesce(
            (date_of_assessment - date_of_onset) / dmonths(1),
            (age_at_assessment - age_at_onset) * (365.25 / 12)
        )
    )

ext_time_to_mitos <- ext_mitos %>%
    ext_calculate_time_to_stage(
        time = time_from_baseline,
        stage = mitos, values = 0:4
    )

ext_time_to_kings <- ext_kings %>%
    ext_calculate_time_to_stage(
        time = time_from_baseline,
        stage = kings, values = 0:4
    )
