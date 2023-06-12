library(lubridate)

source("src/ext/common.r")

ext_main <- ext_load_data(
    "P-ALS_Ext_V2_Main_Data_File.xlsx",
    col_types = c(
        "text", # ID
        "text", # Site
        "text", # Sex
        "date", # Date of Birth
        "text", # Year/Year and Month of Birth
        "numeric", # Age
        "text", # First Symptom
        "text", # Site of Onset
        "text", # Site of Onset 2
        "text", # Site of Onset 3
        "text", # Site of Onset 4
        "text", # Side of Onset
        "date", # Date of Onset
        "date", # Month of Onset
        "numeric", # Age at Onset
        "numeric", # Calculated Age at Onset
        "text", # Diagnosis
        "text", # Diagnosis 2
        "text", # Diagnosis 3
        "text", # Other Diagnosis
        "text", # Motor Neuron Predominance
        "date", # Date of Diagnosis
        "date", # Month of Diagnosis
        "numeric", # Age at Diagnosis
        "numeric", # Calculated Age at Diagnosis
        "text", # Vital Status
        "date", # Date of Death
        "numeric", # Age at Death
        "numeric", # Calculated Age at Death
        "text", # Tracheostomy
        "date", # Date of Tracheostomy
        "numeric", # Age at Tracheostomy
        "text", # >23h NIV
        "date", # Date of 23h NIV
        "numeric", # Age at >23 h NIV
        "date", # Date of Last Follow Up
        "numeric", # Age at Last Follow-up (if alive)
        "date", # Date of Transfer
        "numeric", # Age at Transfer (if alive)
        "text", # Non-invasive Ventilation
        "date", # Date of Non-invasive Ventialtion
        "numeric", # Age at Non-invasive Ventilation
        "text", # Gastrostomy
        "date", # Date of Gastrostomy
        "numeric", # Age at Gastrostomy
        "text", # C9orf72 Tested
        "text", # C9orf72 Status
        "text", # Commercial Result
        "text", # SOD1 Tested
        "text", # SOD1 Status
        "text", # FUS Tested
        "text", # FUS Status
        "text", # TARDBP Tested
        "text", # TARDBP Status
        "text", # Riluzole Use
        "date", # Riluzole Start Date
        "numeric", # Riluzole Start Age
        "text", # Rilzole Stopped
        "date", # Riluzole Stop Date
        "text", # Edaravone Use
        "text", # Edaravone Stopped
        "date", # Edaravone Stop Date
        "text" # Current Working Status
    )
) %>%
    rename_with(~ str_replace(.x, "non_invasive_venti(la|al)tion$", "niv")) %>%
    rename_with(~ str_replace(.x, "rilzole", "riluzole")) %>%
    rename_with(~ str_replace(.x, "_if_alive$", "")) %>%
    rename(age_at_23h_niv = "age_at_gt_23h_niv") %>%
    rows_update(tibble(id = "FRA-0560", date_of_niv = dmy("15/10/2016")), by = "id") %>%
    mutate(
        across(ends_with("_tested"), ext_parse_boolean),
        across(c(gastrostomy, niv, tracheostomy), ext_parse_boolean),
        sex = case_match(
            sex,
            c("Man", "Male") ~ "Male",
            c("Woman", "Female") ~ "Female",
        ),
        c9orf72_status = case_when(
            c9orf72_status == "Intermediate" ~ "Negative",
            TRUE ~ c9orf72_status
        ),
        sod1_status = case_when(
            sod1_status == "Unknown effect" ~ NA_character_,
            TRUE ~ sod1_status
        ),
        date_of_birth = coalesce(
            date_of_birth,
            make_date(str_extract(year_year_and_month_of_birth, "\\d{4}"), 1, 1),
            make_date(
                str_extract(year_year_and_month_of_birth, "(\\d{4})-\\d{1,2}", group = 1),
                str_extract(year_year_and_month_of_birth, "\\d{4}-(\\d{1,2})", group = 1),
                1
            )
        ),
        across(starts_with("date_of_") & -date_of_birth,
            ~ (.x - date_of_birth) / dyears(1),
            .names = "calculated_age_from_{.col}"
        ),
        calculated_age_from_date_of_transfer = if_else(
            vital_status == "Alive", calculated_age_from_date_of_transfer, NA_real_
        ),
        # BUG: for some reason this doesn't work...
        #
        # across(starts_with("age_at_"), \(x) {
        #    event <- str_extract(cur_column(), "age_at_([a-z0-9_]+)", group = 1)
        #    calculated_age_col <- str_glue("calculated_age_from_date_of_{event}")
        #    coalesce(x, .data[[calculated_age_col]])
        # }, .names = "coalesced_{.col}"),
        age_at_onset = coalesce(calculated_age_at_onset, age_at_onset, calculated_age_from_date_of_onset),
        age_at_death = coalesce(calculated_age_at_death, age_at_death, calculated_age_from_date_of_death),
        age_at_diagnosis = coalesce(calculated_age_at_diagnosis, age_at_diagnosis, calculated_age_from_date_of_diagnosis),
        age_at_gastrostomy = coalesce(age_at_gastrostomy, calculated_age_from_date_of_gastrostomy),
        age_at_23h_niv = coalesce(age_at_23h_niv, calculated_age_from_date_of_23h_niv),
        age_at_niv = coalesce(age_at_niv, calculated_age_from_date_of_niv),
        age_at_tracheostomy = coalesce(age_at_tracheostomy, calculated_age_from_date_of_tracheostomy),
        age_at_transfer = if_else(vital_status == "Alive", coalesce(
            age_at_transfer, calculated_age_from_date_of_transfer
        ), NA_real_),
        age_at_last_follow_up = coalesce(
            age_at_last_follow_up,
            calculated_age_from_date_of_last_follow_up
        ),
        bulbar_onset = site_of_onset %in% c(
            "Bulbar", "Bulbaire", "Bulbar and Spinal",
            "Bulbar and Spinal",
            "Bulbar and Cognitive/Behavioural",
            "Bulbar and Thoracic/Respiratory",
            "Cognitive/Behavioural and Bulbar",
            "PBP"
        ),
        spinal_onset = site_of_onset %in% c(
            "Arms", "Cervical",
            "Spinal", "Bulbar and Spinal",
            "Spinal and Cognitive/Behavioural",
            "Cognitive/Behavioural and Spinal",
            "Thoracic/Respiratory and Spinal",
            "Membre supérieur distal Bilat",
            "Membre inférieur distal D",
            "Membre supérieur distal G",
            "Membre inférieur proximal D",
            "Membre inférieur proximal Bilat",
            "Membre inférieur distal Bilat",
            "Membre inférieur distal G",
            "Membre supérieur distal D",
            "Membre inférieur proximal G",
            "Membre supérieur proximal G",
            "Membre supérieur proximal Bilat",
            "Membre supérieur proximal D",
            "Neck", "Upper limb", "Lower limb",
            "Flail-Leg", "Flail-Arm", "Hemiplegic"
        ),
        respiratory_onset = site_of_onset %in% c(
            "Bulbar and Thoracic/Respiratory",
            "Respiratory",
            "Respiratoire",
            "Thoracic/respiratory",
            "Thoracic/Respiratory",
            "Thoracic/Respiratory and Spinal"
        ),
        cognitive_onset = site_of_onset %in% c(
            "Cognitive",
            "Cognitive/Behavioural",
            "Cognitive/Behavioural and Bulbar",
            "Cognitive/Behavioural and Spinal",
            "Cognitive impairment",
            "FTD"
        ),
        side_of_onset = case_when(
            side_of_onset == "Right" ~ "R",
            side_of_onset == "Left" ~ "L",
            side_of_onset == "Both sides" ~ "B",
            str_ends(site_of_onset, " D") ~ "R",
            str_ends(site_of_onset, " G") ~ "L",
            str_ends(site_of_onset, " Bilat") ~ "B"
        )
    )
