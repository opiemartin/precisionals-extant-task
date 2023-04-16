library(dplyr)
library(ggplot2)
library(lazyeval)
library(rlang)
library(stringr)
library(survival)
library(survminer)
library(tidyr)

source("src/ext/main.r")
source("src/ext/stage.r")
source("src/ext/alsfrs.r")

ext_fit_survival_curves <- function(data, origin, events, groups = NULL) {
    res <- list()
    for (o in origin) {
        for (e in names(events)) {
            key <- str_glue("{e}_from_{o}_overall")
            age_at_event_col <- str_glue("age_at_{e}")
            age_at_origin_col <- str_glue("age_at_{o}")
            event_data <- data %>% mutate(
                time = .data[[age_at_event_col]] - .data[[age_at_origin_col]],
                event = f_eval(events[[e]], data)
            )
            res[[key]] <- surv_fit(Surv(time, event) ~ 1, data = event_data)

            for (g in groups) {
                key <- str_glue("{e}_from_{o}_by_{g}")
                group_data <- event_data %>% mutate("{g}" := .data[[g]])
                formula <- as.formula(str_glue("Surv(time, event) ~ {g}"))
                res[[key]] <- surv_fit(formula, data = group_data)
            }
        }
    }
    res
}

data <- ext_main %>%
    left_join(time_to_mitos_by_age, by = "id") %>%
    left_join(time_to_kings_by_age, by = "id") %>%
    mutate(
        gene = case_when(
            c9orf72_status == "Positive" ~ "C9orf72",
            sod1_status == "Positive" ~ "SOD1",
            fus_status == "Positive" ~ "FUS"
        ),
        site_of_onset = case_when(
            bulbar_onset == TRUE ~ "Bulbar",
            spinal_onset == TRUE ~ "Spinal",
            cervical_onset == TRUE ~ "Cervical",
            cognitive_onset == TRUE ~ "Cognitive",
            respiratory_onset == TRUE ~ "Respiratory",
        ),
        proximal_or_distal_onset = case_when(
            proximal_onset == TRUE ~ "Proximal",
            distal_onset == TRUE ~ "Distal",
        )
    )

time_to_event <- ext_fit_survival_curves(data,
    origin = c("onset", "diagnosis"),
    groups = c("sex", "gene", "site_of_onset", "proximal_or_distal_onset"),
    events = list(
        "death" = ~ vital_status == "Deceased",
        "niv" = ~ niv == TRUE,
        "gastrostomy" = ~ gastrostomy == TRUE,
        "tracheostomy" = ~ tracheostomy == TRUE,
        "kings_1" = ~ !is.na(age_at_kings_1),
        "kings_2" = ~ !is.na(age_at_kings_2),
        "kings_3" = ~ !is.na(age_at_kings_3),
        "kings_4" = ~ !is.na(age_at_kings_4),
        "mitos_1" = ~ !is.na(age_at_mitos_1),
        "mitos_2" = ~ !is.na(age_at_mitos_2),
        "mitos_3" = ~ !is.na(age_at_mitos_3),
        "mitos_4" = ~ !is.na(age_at_mitos_4)
    )
)

for (key in names(time_to_event)) {
    m <- str_match(key, "^([a-z0-9_]+)_from_([a-z_]+)_(?:overall|by_([a-z_]+))$")
    event <- str_replace_all(m[[2]], "_", " ")
    origin <- str_replace_all(m[[3]], "_", " ")
    group <- str_replace_all(m[[4]], "_", " ")
    if (is.na(group)) {
        ggsurvplot(time_to_event[[key]],
            title = str_glue("Time to {event} from {origin}"),
            xlab = "Time (years)", ylab = "Survival probability",
            legend = "none", surv.scale = "percent"
        )
    } else {
        ggsurvplot(time_to_event[[key]],
            conf.int = TRUE, legend.title = group, surv.scale = "percent",
            title = str_glue("Time to {event} from {origin} by {group}"),
            xlab = "Time (years)", ylab = "Survival probability"
        )
    }
    dir.create("output/q3", showWarnings = FALSE)
    ggsave(str_glue("output/q3/{key}.png"))
}
