suppressPackageStartupMessages({
    library(dplyr)
    library(lazyeval)
    library(lubridate)
    library(rlang)
    library(stringr)
    library(tidyr)
    library(writexl)
    library(xfun)
})

message("Loading the dataset...", appendLF = FALSE)
source("src/ext/main.r")
source("src/ext/resp.r")
source("src/ext/staging.r")
message("\rLoading the dataset... done.")

source("src/q3/common.r")

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
            duration_key <- if_else(exists(e, duration_for), e, ".otherwise")
            epochs_from_origin <- f_eval(duration_for[[duration_key]], odata)
            epochs_to_loss <- epochs_from_origin
            if (!is.null(censored_for) && exists(e, censored_for)) {
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

message("Calculating time to King's...", appendLF = FALSE)
q3_time_to_kings <- ext_kings %>%
    q3_calculate_time_to_stage(
        time = time_from_baseline,
        stage = kings, values = 0:5
    ) %>%
    left_join(ext_baseline, by = "id") %>%
    mutate(
        across(
            starts_with("time_from_baseline_to_kings_"),
            ~ date_of_baseline + .x,
            .names = "date_of__{.col}"
        ),
        across(
            starts_with("time_from_baseline_to_kings_"),
            ~ age_at_baseline + .x / dyears(1),
            .names = "age_at__{.col}"
        )
    ) %>%
    rename_with(~ str_replace(.x, "__time_from_baseline_to", ""))
message("\rCalculating time to King's... done.")

message("Calculating time to MiToS...", appendLF = FALSE)
q3_time_to_mitos <- ext_mitos %>%
    q3_calculate_time_to_stage(
        time = time_from_baseline,
        stage = mitos, values = 0:5
    ) %>%
    left_join(ext_baseline, by = "id") %>%
    mutate(
        across(
            starts_with("time_from_baseline_to_mitos_"),
            ~ date_of_baseline + .x,
            .names = "date_of__{.col}"
        ),
        across(
            starts_with("time_from_baseline_to_mitos_"),
            ~ age_at_baseline + .x / dyears(1),
            .names = "age_at__{.col}"
        )
    ) %>%
    rename_with(~ str_replace(.x, "__time_from_baseline_to", ""))
message("\rCalculating time to MiToS... done.")

message("Calculating time to walking support...", appendLF = FALSE)
q3_time_to_walking_support <- ext_alsfrs %>%
    filter(time_from_baseline >= ddays(0), q8_walking <= 2) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    select(
        id,
        age_at_walking_support = "age_at_assessment",
        date_of_walking_support = "date_of_assessment"
    )
message("\rCalculating time to walking support... done.")

message("Calculating time to respiratory onset...", appendLF = FALSE)
q3_time_to_respiratory_onset <- ext_alsfrs %>%
    filter(time_from_baseline >= ddays(0), q10_dyspnea <= 3 | q11_orthopnea <= 3) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    select(
        id,
        age_at_respiratory_onset = "age_at_assessment",
        date_of_respiratory_onset = "date_of_assessment"
    )
message("\rCalculating time to respiratory onset... done.")

q3_vc_assessments <- ext_resp %>%
    left_join(ext_main, by = "id") %>%
    mutate(
        time_from_onset = coalesce(
            date_of_assessment - date_of_onset,
            dyears(age_at_assessment - age_at_onset)
        )
    )

message("Calculating time to vital capacity decline...", appendLF = FALSE)
q3_time_to_vc_decline <- q3_vc_assessments %>%
    filter(fvc_rel < 80 | svc_rel < 80) %>%
    slice_min(time_from_onset, n = 1, with_ties = FALSE, by = "id") %>%
    select(
        id,
        date_of_vc_decline = "date_of_assessment",
        age_at_vc_decline = "age_at_assessment"
    )
message("\rCalculating time to vital capacity decline... done.")

message("Calculating time to NIV by ALSFRS-R...", appendLF = FALSE)
q3_time_to_niv_by_alsfrs <- ext_alsfrs %>%
    filter(time_from_baseline >= ddays(0), q12_respiratory_insufficiency <= 3) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    select(
        id,
        age_at_niv_by_alsfrs = "age_at_assessment",
        date_of_niv_by_alsfrs = "date_of_assessment"
    )
message("\rCalculating time to NIV by ALSFRS-R... done.")

message("Calculating time to NIV >23h by ALSFRS-R...", appendLF = FALSE)
q3_time_to_niv_23h_by_alsfrs <- ext_alsfrs %>%
    filter(time_from_baseline >= ddays(0), q12_respiratory_insufficiency <= 1) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    select(
        id,
        age_at_niv_23h_by_alsfrs = "age_at_assessment",
        date_of_niv_23h_by_alsfrs = "date_of_assessment"
    )
message("\rCalculating time to NIV >23h by ALSFRS-R... done.")

message("Calculating time to IMV by ALSFRS-R...", appendLF = FALSE)
q3_time_to_imv_by_alsfrs <- ext_alsfrs %>%
    filter(time_from_baseline >= ddays(0), q12_respiratory_insufficiency == 0) %>%
    slice_min(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    select(
        id,
        age_at_imv_by_alsfrs = "age_at_assessment",
        date_of_imv_by_alsfrs = "date_of_assessment"
    )
message("\rCalculating time to IMV by ALSFRS-R... done.")

message("Calculating time to last ALSFRS-R assessment...", appendLF = FALSE)
q3_time_to_last_alsfrs_assessment <- ext_alsfrs %>%
    filter(time_from_baseline >= ddays(0)) %>%
    slice_max(time_from_baseline, by = "id", n = 1, with_ties = FALSE) %>%
    select(
        id,
        age_at_last_alsfrs_assessment = "age_at_assessment",
        date_of_last_alsfrs_assessment = "date_of_assessment"
    )
message("\rCalculating time to last ALSFRS-R assessment... done.")

message("Calculating time to last vital capacity assessment...", appendLF = FALSE)
q3_time_to_last_vc_assessment <- q3_vc_assessments %>%
    slice_max(time_from_onset, n = 1, with_ties = FALSE, by = "id") %>%
    select(
        id,
        date_of_last_vc_assessment = "date_of_assessment",
        age_at_last_vc_assessment = "age_at_assessment"
    )
message("\rCalculating time to last vital capacity assessment... done.")

# NOTE: explicit casts are necessary because pmin seems to get confused
#       when mixing NA days (POSIXct) and dyears(x) (lubridate) types.

message("Calculating time to events...", appendLF = FALSE)
q3_time_to_events <- ext_main %>%
    left_join(q3_time_to_kings, by = "id") %>%
    left_join(q3_time_to_mitos, by = "id") %>%
    left_join(q3_time_to_walking_support, by = "id") %>%
    left_join(q3_time_to_respiratory_onset, by = "id") %>%
    left_join(q3_time_to_vc_decline, by = "id") %>%
    left_join(q3_time_to_niv_by_alsfrs, by = "id") %>%
    left_join(q3_time_to_niv_23h_by_alsfrs, by = "id") %>%
    left_join(q3_time_to_imv_by_alsfrs, by = "id") %>%
    left_join(q3_time_to_last_vc_assessment, by = "id") %>%
    left_join(q3_time_to_last_alsfrs_assessment, by = "id") %>%
    q3_analyze_time_to_event(
        origin = c("birth", "onset", "diagnosis"),
        events = list(
            onset = ~ coalesce(
                as.duration(date_of_onset - .date_of_origin),
                dyears(age_at_onset - .age_at_origin)
            ),
            diagnosis = ~ coalesce(
                as.duration(date_of_diagnosis - .date_of_origin),
                dyears(age_at_diagnosis - .age_at_origin)
            ),
            walking_support = ~ coalesce(
                as.duration(date_of_walking_support - .date_of_origin),
                dyears(age_at_walking_support - .age_at_origin)
            ),
            respiratory_onset = ~ coalesce(
                as.duration(date_of_respiratory_onset - .date_of_origin),
                dyears(age_at_respiratory_onset - .age_at_origin)
            ),
            vc_decline = ~ coalesce(
                as.duration(date_of_vc_decline - .date_of_origin),
                dyears(age_at_vc_decline - .age_at_origin)
            ),
            ventilatory_support = ~ pmin(
                as.duration(date_of_niv - .date_of_origin),
                as.duration(date_of_niv_by_alsfrs - .date_of_origin),
                as.duration(date_of_23h_niv - .date_of_origin),
                as.duration(date_of_niv_23h_by_alsfrs - .date_of_origin),
                as.duration(date_of_tracheostomy - .date_of_origin),
                as.duration(date_of_imv_by_alsfrs - .date_of_origin),
                dyears(age_at_niv - .age_at_origin),
                dyears(age_at_niv_by_alsfrs - .age_at_origin),
                dyears(age_at_23h_niv - .age_at_origin),
                dyears(age_at_niv_23h_by_alsfrs - .age_at_origin),
                dyears(age_at_tracheostomy - .age_at_origin),
                dyears(age_at_imv_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            ),
            niv = ~ pmin(
                as.duration(date_of_niv - .date_of_origin),
                as.duration(date_of_niv_by_alsfrs - .date_of_origin),
                as.duration(date_of_23h_niv - .date_of_origin),
                as.duration(date_of_niv_23h_by_alsfrs - .date_of_origin),
                dyears(age_at_niv - .age_at_origin),
                dyears(age_at_niv_by_alsfrs - .age_at_origin),
                dyears(age_at_23h_niv - .age_at_origin),
                dyears(age_at_niv_23h_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            ),
            niv_23h = ~ pmin(
                as.duration(date_of_23h_niv - .date_of_origin),
                as.duration(date_of_niv_23h_by_alsfrs - .date_of_origin),
                dyears(age_at_23h_niv - .age_at_origin),
                dyears(age_at_niv_23h_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            ),
            tracheostomy = ~ pmin(
                as.duration(date_of_tracheostomy - .date_of_origin),
                as.duration(date_of_imv_by_alsfrs - .date_of_origin),
                dyears(age_at_tracheostomy - .age_at_origin),
                dyears(age_at_imv_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            ),
            gastrostomy = ~ coalesce(
                as.duration(date_of_gastrostomy - .date_of_origin),
                dyears(age_at_gastrostomy - .age_at_origin)
            ),
            kings_1 = ~ coalesce(
                as.duration(date_of_kings_1 - .date_of_origin),
                dyears(age_at_kings_1 - .age_at_origin)
            ),
            kings_2 = ~ coalesce(
                as.duration(date_of_kings_2 - .date_of_origin),
                dyears(age_at_kings_2 - .age_at_origin)
            ),
            kings_3 = ~ coalesce(
                as.duration(date_of_kings_3 - .date_of_origin),
                dyears(age_at_kings_3 - .age_at_origin)
            ),
            kings_4 = ~ coalesce(
                as.duration(date_of_kings_4 - .date_of_origin),
                dyears(age_at_kings_4 - .age_at_origin)
            ),
            kings_5 = ~ coalesce(
                as.duration(date_of_kings_5 - .date_of_origin),
                dyears(age_at_kings_5 - .age_at_origin)
            ),
            mitos_1 = ~ coalesce(
                as.duration(date_of_mitos_1 - .date_of_origin),
                dyears(age_at_mitos_1 - .age_at_origin)
            ),
            mitos_2 = ~ coalesce(
                as.duration(date_of_mitos_2 - .date_of_origin),
                dyears(age_at_mitos_2 - .age_at_origin)
            ),
            mitos_3 = ~ coalesce(
                as.duration(date_of_mitos_3 - .date_of_origin),
                dyears(age_at_mitos_3 - .age_at_origin)
            ),
            mitos_4 = ~ coalesce(
                as.duration(date_of_mitos_4 - .date_of_origin),
                dyears(age_at_mitos_4 - .age_at_origin)
            ),
            mitos_5 = ~ coalesce(
                as.duration(date_of_mitos_5 - .date_of_origin),
                dyears(age_at_mitos_5 - .age_at_origin)
            ),
            death = ~ coalesce(
                as.duration(date_of_death - .date_of_origin),
                dyears(age_at_death - .age_at_origin)
            )
        ),
        duration_for = list(
            vc_decline = ~ coalesce(
                as.duration(date_of_last_vc_assessment - .date_of_origin),
                dyears(age_at_last_vc_assessment - .age_at_origin)
            ),
            walking_support = ~ coalesce(
                as.duration(date_of_last_alsfrs_assessment - .date_of_origin),
                dyears(age_at_last_alsfrs_assessment - .age_at_origin)
            ),
            respiratory_onset = ~ coalesce(
                as.duration(date_of_last_alsfrs_assessment - .date_of_origin),
                dyears(age_at_last_alsfrs_assessment - .age_at_origin)
            ),
            death = ~ pmin(
                dyears(age_at_transfer - .age_at_origin),
                if_else(
                    vital_status == "Alive",
                    as.duration(date_of_transfer - .date_of_origin),
                    as.duration(NA)
                ),
                na.rm = TRUE
            ),
            .otherwise = ~ pmin(
                dyears(age_at_death - .age_at_origin),
                dyears(age_at_transfer - .age_at_origin),
                if_else(
                    vital_status == "Alive",
                    as.duration(date_of_transfer - .date_of_origin),
                    as.duration(date_of_death - .date_of_origin)
                ),
                na.rm = TRUE
            )
        ),
        censored_for = list(
            death = ~ pmin(
                as.duration(date_of_tracheostomy - .date_of_origin),
                as.duration(date_of_imv_by_alsfrs - .date_of_origin),
                dyears(age_at_tracheostomy - .age_at_origin),
                dyears(age_at_imv_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            ),
            niv = ~ pmin(
                as.duration(date_of_tracheostomy - .date_of_origin),
                as.duration(date_of_imv_by_alsfrs - .date_of_origin),
                dyears(age_at_tracheostomy - .age_at_origin),
                dyears(age_at_imv_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            ),
            niv_23h = ~ pmin(
                as.duration(date_of_tracheostomy - .date_of_origin),
                as.duration(date_of_imv_by_alsfrs - .date_of_origin),
                dyears(age_at_tracheostomy - .age_at_origin),
                dyears(age_at_imv_by_alsfrs - .age_at_origin),
                na.rm = TRUE
            )
        )
    )
message("\rCalculating time to events... done.")

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
        age_at_onset = cut(calculated_age_at_onset,
            right = FALSE, ordered_result = TRUE,
            breaks = c(0, 19, 30, 40, 50, 60, 70, 80, +Inf),
            labels = c(
                "0-18", "19-29", "30-39", "40-49",
                "50-59", "60-69", "70-79", "80+"
            )
        ),
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
        ))
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
        progression_category
    )

q3_data <- q3_subgroups %>%
    left_join(q3_time_to_events, by = "id") %>%
    filter(
        q3_is_valid_event_from_origin(event, origin),
        site_of_onset != "Cognitive",
        duration >= as.duration(0)
    ) %>%
    arrange(origin, event)

message("Exporting results...", appendLF = FALSE)
output_dir <- "output/q3"
output_path <- file.path(output_dir, "time-to-event")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
q3_data %>% saveRDS(output_path %>% with_ext(".rds"))
message("\rExporting results... done.")
