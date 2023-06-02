library(dplyr)
library(lazyeval)
library(lubridate)
library(rlang)
library(stringr)
library(tidyr)
library(writexl)

source("src/ext/main.r")
source("src/ext/staging.r")

q3_calculate_time_to_stage <- function(data, time, stage, values) {
    stage_col <- as_label(enquo(stage))
    time_to_stage_col <- str_glue("time_from_baseline_to_{stage_col}_")
    tibble(id = unique(data$id)) %>%
        cross_join(tibble({{ stage }} := values)) %>%
        bind_rows(data %>% select(id, {{ time }}, {{ stage }})) %>%
        slice_min({{ time }}, by = c(id, {{ stage }}), n = 1, with_ties = FALSE) %>%
        group_by(id) %>%
        arrange({{ stage }}, .by_group = TRUE) %>%
        fill({{ time }}, .direction = "up") %>%
        ungroup() %>%
        pivot_wider(
            names_from = {{ stage }},
            values_from = {{ time }},
            names_prefix = time_to_stage_col
        )
}

q3_calculate_age_at_origin <- function(data, origin) {
    if (origin == "birth") {
        0
    } else {
        data[[str_glue("age_at_{origin}")]]
    }
}

q3_analyze_time_to_event <- function(data, origin, events, duration_for, censored_for = NULL) {
    result <- tibble()
    for (o in origin) {
        odata <- data %>%
            mutate(
                .date_of_origin = .data[[str_glue("date_of_{o}")]],
                .age_at_origin = q3_calculate_age_at_origin(data, o)
            ) %>%
            filter(!is.na(.date_of_origin) | !is.na(.age_at_origin))
        for (e in names(events)) {
            epochs_to_event <- f_eval(events[[e]], odata)
            duration_key <- if_else(exists(e, where = duration_for), e, ".otherwise")
            epochs_from_origin <- f_eval(duration_for[[duration_key]], odata)
            epochs_to_loss <- epochs_from_origin
            if (!is.null(censored_for) && exists(e, where = censored_for)) {
                epochs_to_loss <- pmin(
                    epochs_to_loss,
                    f_eval(censored_for[[e]], odata),
                    na.rm = TRUE
                )
            }
            duration <- pmin(epochs_from_origin, epochs_to_event, epochs_to_loss, na.rm = TRUE)
            status <- case_when(
                duration == epochs_to_event ~ "event",
                duration == epochs_from_origin ~ "loss",
                duration == epochs_to_loss ~ "censored"
            )
            result <- result %>%
                bind_rows(tibble(
                    id = odata$id,
                    origin = o,
                    event = e,
                    status = status,
                    duration = duration,
                    time_to_event = epochs_to_event,
                    time_to_loss = pmin(epochs_from_origin, epochs_to_loss, na.rm = TRUE)
                ))
        }
    }
    result
}

q3_time_to_mitos <- ext_mitos %>%
    q3_calculate_time_to_stage(
        time = time_from_baseline,
        stage = mitos, values = 0:5
    ) %>%
    left_join(ext_baseline, by = "id") %>%
    mutate(
        across(
            starts_with("time_from_baseline_to_mitos_"),
            ~ date_of_baseline + dmonths(.x),
            .names = "date_of__{.col}"
        ),
        across(
            starts_with("time_from_baseline_to_mitos_"),
            ~ age_at_baseline + .x / 12,
            .names = "age_at__{.col}"
        )
    ) %>%
    rename_with(~ str_replace(.x, "__time_from_baseline_to", ""))

q3_time_to_kings <- ext_kings %>%
    q3_calculate_time_to_stage(
        time = time_from_baseline,
        stage = kings, values = 0:5
    ) %>%
    left_join(ext_baseline, by = "id") %>%
    mutate(
        across(
            starts_with("time_from_baseline_to_kings_"),
            ~ date_of_baseline + dmonths(.x),
            .names = "date_of__{.col}"
        ),
        across(
            starts_with("time_from_baseline_to_kings_"),
            ~ age_at_baseline + .x / 12,
            .names = "age_at__{.col}"
        )
    ) %>%
    rename_with(~ str_replace(.x, "__time_from_baseline_to", ""))

q3_time_to_walking_support <- ext_alsfrs %>%
    filter(q8_walking <= 2) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    right_join(ext_baseline, by = "id") %>%
    transmute(
        id,
        age_at_walking_support = age_at_baseline + time_from_baseline / 12,
        date_of_walking_support = date_of_baseline + dmonths(time_from_baseline)
    )

q3_time_to_respiratory_onset <- ext_alsfrs %>%
    filter(q10_dyspnea <= 3 | q11_orthopnea <= 3) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    right_join(ext_baseline, by = "id") %>%
    transmute(
        id,
        age_at_respiratory_onset = age_at_baseline + time_from_baseline / 12,
        date_of_respiratory_onset = date_of_baseline + dmonths(time_from_baseline)
    )

q3_time_to_niv_by_alsfrs <- ext_alsfrs %>%
    filter(q12_respiratory_insufficiency <= 3) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    right_join(ext_baseline, by = "id") %>%
    transmute(
        id,
        age_at_niv_by_alsfrs = age_at_baseline + time_from_baseline / 12,
        date_of_niv_by_alsfrs = date_of_baseline + dmonths(time_from_baseline)
    )

q3_time_to_niv_23h_by_alsfrs <- ext_alsfrs %>%
    filter(q12_respiratory_insufficiency <= 1) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    right_join(ext_baseline, by = "id") %>%
    transmute(
        id,
        age_at_niv_23h_by_alsfrs = age_at_baseline + time_from_baseline / 12,
        date_of_niv_23h_by_alsfrs = date_of_baseline + dmonths(time_from_baseline)
    )

q3_time_to_imv_by_alsfrs <- ext_alsfrs %>%
    filter(q12_respiratory_insufficiency == 0) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    right_join(ext_baseline, by = "id") %>%
    transmute(
        id,
        age_at_imv_by_alsfrs = age_at_baseline + time_from_baseline / 12,
        date_of_imv_by_alsfrs = date_of_baseline + dmonths(time_from_baseline)
    )

q3_time_to_events <- ext_main %>%
    left_join(q3_time_to_kings, by = "id") %>%
    left_join(q3_time_to_mitos, by = "id") %>%
    left_join(q3_time_to_walking_support, by = "id") %>%
    left_join(q3_time_to_respiratory_onset, by = "id") %>%
    left_join(q3_time_to_niv_by_alsfrs, by = "id") %>%
    left_join(q3_time_to_niv_23h_by_alsfrs, by = "id") %>%
    left_join(q3_time_to_imv_by_alsfrs, by = "id") %>%
    q3_analyze_time_to_event(
        origin = c("birth", "onset", "diagnosis"),
        events = list(
            onset = ~ coalesce(
                (age_at_onset - .age_at_origin) * 12,
                (date_of_onset - .date_of_origin) / dmonths(1)
            ),
            diagnosis = ~ coalesce(
                (age_at_diagnosis - .age_at_origin) * 12,
                (date_of_diagnosis - .date_of_origin) / dmonths(1)
            ),
            respiratory_onset = ~ coalesce(
                (age_at_respiratory_onset - .age_at_origin) * 12,
                (date_of_respiratory_onset - .date_of_origin) / dmonths(1)
            ),
            walking_support = ~ coalesce(
                (age_at_walking_support - .age_at_origin) * 12,
                (date_of_walking_support - .date_of_origin) / dmonths(1)
            ),
            niv = ~ pmin(
                (age_at_niv - .age_at_origin) * 12,
                (age_at_niv_by_alsfrs - .age_at_origin) * 12,
                (age_at_23h_niv - .age_at_origin) * 12,
                (age_at_niv_23h_by_alsfrs - .age_at_origin) * 12,
                (age_at_tracheostomy - .age_at_origin) * 12,
                (age_at_imv_by_alsfrs - .age_at_origin) * 12,
                (date_of_niv - .date_of_origin) / dmonths(1),
                (date_of_niv_by_alsfrs - .date_of_origin) / dmonths(1),
                (date_of_23h_niv - .date_of_origin) / dmonths(1),
                (date_of_niv_23h_by_alsfrs - .date_of_origin) / dmonths(1),
                (date_of_tracheostomy - .date_of_origin) / dmonths(1),
                (date_of_imv_by_alsfrs - .date_of_origin) / dmonths(1),
                na.rm = TRUE
            ),
            niv_23h = ~ pmin(
                (age_at_23h_niv - .age_at_origin) * 12,
                (age_at_niv_23h_by_alsfrs - .age_at_origin) * 12,
                (age_at_tracheostomy - .age_at_origin) * 12,
                (age_at_imv_by_alsfrs - .age_at_origin) * 12,
                (date_of_23h_niv - .date_of_origin) / dmonths(1),
                (date_of_niv_23h_by_alsfrs - .date_of_origin) / dmonths(1),
                (date_of_tracheostomy - .date_of_origin) / dmonths(1),
                (date_of_imv_by_alsfrs - .date_of_origin) / dmonths(1),
                na.rm = TRUE
            ),
            gastrostomy = ~ coalesce(
                (age_at_gastrostomy - .age_at_origin) * 12,
                (date_of_gastrostomy - .date_of_origin) / dmonths(1),
            ),
            tracheostomy = ~ coalesce(
                (age_at_tracheostomy - .age_at_origin) * 12,
                (age_at_imv_by_alsfrs - .age_at_origin) * 12,
                (date_of_tracheostomy - .date_of_origin) / dmonths(1),
                (date_of_imv_by_alsfrs - .date_of_origin) / dmonths(1)
            ),
            death = ~ coalesce(
                (age_at_death - .age_at_origin) * 12,
                (date_of_death - date_of_birth) / dmonths(1)
            ),
            kings_1 = ~ coalesce(
                (age_at_kings_1 - .age_at_origin) * 12,
                (date_of_kings_1 - .date_of_origin) / dmonths(1)
            ),
            kings_2 = ~ coalesce(
                (age_at_kings_2 - .age_at_origin) * 12,
                (date_of_kings_2 - .date_of_origin) / dmonths(1)
            ),
            kings_3 = ~ coalesce(
                (age_at_kings_3 - .age_at_origin) * 12,
                (date_of_kings_3 - .date_of_origin) / dmonths(1)
            ),
            kings_4 = ~ coalesce(
                (age_at_kings_4 - .age_at_origin) * 12,
                (date_of_kings_4 - .date_of_origin) / dmonths(1)
            ),
            mitos_1 = ~ coalesce(
                (age_at_mitos_1 - .age_at_origin) * 12,
                (date_of_mitos_1 - .date_of_origin) / dmonths(1)
            ),
            mitos_2 = ~ coalesce(
                (age_at_mitos_2 - .age_at_origin) * 12,
                (date_of_mitos_2 - .date_of_origin) / dmonths(1)
            ),
            mitos_3 = ~ coalesce(
                (age_at_mitos_3 - .age_at_origin) * 12,
                (date_of_mitos_3 - .date_of_origin) / dmonths(1)
            ),
            mitos_4 = ~ coalesce(
                (age_at_mitos_4 - .age_at_origin) * 12,
                (date_of_mitos_4 - .date_of_origin) / dmonths(1)
            )
        ),
        duration_for = list(
            death = ~ pmin(
                (age_at_transfer - .age_at_origin) * 12,
                if_else(
                    vital_status == "Alive",
                    (date_of_transfer - .date_of_origin) / dmonths(1),
                    NA_real_
                ),
                na.rm = TRUE
            ),
            .otherwise = ~ pmin(
                (age_at_death - .age_at_origin) * 12,
                (age_at_transfer - .age_at_origin) * 12,
                (date_of_death - .date_of_origin) / dmonths(1),
                if_else(
                    vital_status == "Alive",
                    (date_of_transfer - .date_of_origin) / dmonths(1),
                    NA_real_
                ),
                na.rm = TRUE
            )
        ),
        censored_for = list(
            death = ~ pmin(
                (age_at_tracheostomy - .age_at_origin) * 12,
                (age_at_imv_by_alsfrs - .age_at_origin) * 12,
                (date_of_tracheostomy - .date_of_origin) / dmonths(1),
                (date_of_imv_by_alsfrs - .date_of_origin) / dmonths(1),
                na.rm = TRUE
            )
        )
    )

q3_subgroups <- ext_main %>%
    left_join(ext_baseline, by = "id") %>%
    mutate(
        onset_sites = bulbar_onset + spinal_onset +
            cognitive_onset + respiratory_onset,
        altered_genes = (
            (c9orf72_status == "Positive") +
                (sod1_status == "Positive") +
                (fus_status == "Positive") +
                (tardbp_status == "Positive")
        ),
        age_at_onset = cut(calculated_age_at_onset, c(0, 18, 30, 40, 50, 60, 70, 80)),
        causal_gene = case_when(
            altered_genes > 1 ~ "Multiple",
            c9orf72_status == "Positive" ~ "C9orf72",
            sod1_status == "Positive" ~ "SOD1",
            fus_status == "Positive" ~ "FUS",
            tardbp_status == "Positive" ~ "TARDBP",
            TRUE ~ "Unknown"
        ),
        site_of_onset = case_when(
            is.na(site_of_onset) ~ "Unknown",
            onset_sites > 1 ~ "Multiple",
            bulbar_onset ~ "Bulbar",
            spinal_onset ~ "Spinal",
            cognitive_onset ~ "Cognitive",
            respiratory_onset ~ "Respiratory",
            TRUE ~ "Other"
        ),
        across(ends_with("_status"), ~ case_when(
            .x == "Negative" & causal_gene != "Unknown" ~ "Negative (known gene)",
            .x == "Negative" & causal_gene == "Unknown" ~ "Negative (unknown gene)",
            TRUE ~ .x
        )),
        progression_rate = ext_alsfrs_progression_category(delta_fs)
    ) %>%
    select(
        id, site, sex,
        age_at_onset,
        c9orf72_status,
        sod1_status,
        fus_status,
        tardbp_status,
        causal_gene,
        site_of_onset,
        progression_rate
    )

q3_data <- q3_subgroups %>%
    left_join(q3_time_to_events, by = "id") %>%
    filter(
        duration >= 0,
        !(event == "onset" & origin == "diagnosis")
    ) %>%
    arrange(origin, event)

output_dir <- "output/q3"
output_path <- file.path(output_dir, "time-to-event.xlsx")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
q3_data %>% write_xlsx(output_path)
