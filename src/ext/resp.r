suppressPackageStartupMessages({
    library(stringr)
})

source("src/ext/common.r")

ext_resp <- ext_load_data(
    "P-ALS_Ext_V2_Respiratory_Assessments.xlsx",
    col_types = c(
        "text", # ID
        "text", # Site
        "date", # Date of Assessment
        "numeric", # Age of Assessment
        rep("numeric", 32) # ...
    )
) %>%
    mutate(
        fvc_lying_l = fvc_lying_abs / 1000,
        fvc_sitting_l = coalesce(fvc_sitting_l, fvc_sitting_abs / 1000),
    ) %>%
    select(-fvc_lying_abs, -fvc_sitting_abs) %>%
    rename_with(~ str_replace(.x, "^age_of_", "age_at_")) %>%
    rename_with(~ str_replace(.x, "forced_vital_capacity", "fvc")) %>%
    rename_with(~ str_replace(.x, "slow_vital_capacity", "svc")) %>%
    rename_with(~ str_replace(.x, "peak_cough_flow", "pcf")) %>%
    rename_with(~ str_replace(.x, "_l$", "_abs")) %>%
    rename_with(~ str_replace(.x, "_percent_of_predicted$", "_rel")) %>%
    rename_with(~ str_replace(.x, "_perentage$", "_percentage")) %>%
    rename_with(~ str_replace(.x, "_percentage$", "_rel")) %>%
    rename(
        fvc_lying_rel = "fvc_lying",
        mep = "maximal_expiratory_pressur",
        mev = "maximal_expiratory_volume_mev",
        mip = "maximal_inspiratory_pressure",
        snip = "sniff_nasal_inspiratory_pressure_snip",
    ) %>%
    mutate(
        fvc_abs = coalesce(fvc_abs, fvc_sitting_abs),
        fvc_rel = coalesce(fvc_rel, fvc_sitting_rel),
        snip = coalesce(snip, snip_occluded, snip_unoccluded),
    )
