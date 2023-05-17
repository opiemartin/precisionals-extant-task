source("src/ext/common.r")

ext_suppl_data_path <- "P-ALS_Ext_V2_Supplementary_Data.xlsx"

ext_suppl_kcl <- ext_load_data(
    ext_suppl_data_path,
    sheet = "KCL Respiratory & Gastrostomy"
) %>%
    mutate(gastrostomyyesno = as.logical(gastrostomyyesno)) %>%
    rename(
        gastrostomy_type = "gastrostomy",
        gastrostomy = "gastrostomyyesno",
        gastrostomy_age = "agegastrostomy",
    )

ext_suppl_ire_gastrostomy <- ext_load_data(
    ext_suppl_data_path,
    sheet = "IRE Gastrostomy"
)

ext_suppl_nld_gastrostomy <- ext_load_data(
    ext_suppl_data_path,
    sheet = "NLD Gastrostomy"
) %>%
    mutate(
        across(c(peg, rig), ext_parse_boolean),
        across(starts_with("age_of_"), as.numeric),
    )

ext_suppl_she_gastrostomy <- ext_load_data(
    ext_suppl_data_path,
    sheet = "SHE Gastrostomy"
) %>%
    mutate(
        gastrostomy = !is.na(gastrostomy),
        gastrostomy_type = gastrostomy
    )

ext_suppl_spa_gastrostomy <- ext_load_data(
    ext_suppl_data_path,
    sheet = "SPA PEG"
)

ext_suppl_gastrostomy <- bind_rows(
    ext_suppl_kcl %>% select(id, gastrostomy, gastrostomy_age),
    ext_suppl_ire_gastrostomy %>% transmute(
        id = id,
        gastrostomy = peg | rig,
        gastrostomy_date = rig_peg_date,
    ),
    ext_suppl_nld_gastrostomy %>% transmute(
        id = id,
        gastrostomy = peg | rig,
        gastrostomy_age = case_when(
            peg == TRUE ~ age_of_peg,
            rig == TRUE ~ age_of_rig,
        ),
    ),
    ext_suppl_she_gastrostomy,
    ext_suppl_spa_gastrostomy %>%
        group_by(id) %>%
        summarize(
            gastrostomy = any(peg_indication),
            gastrostomy_date = min(peg_indication_date)
        ) %>% ungroup()
)
