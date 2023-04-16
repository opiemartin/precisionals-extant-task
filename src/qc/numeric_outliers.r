library(tidyr)
library(writexl)

source("src/ext/main.r")
source("src/ext/resp.r")

ext_main_values <- ext_main %>%
    pivot_longer(
        where(is.numeric),
        names_to = "param_name",
        values_to = "param_value"
    ) %>%
    drop_na(id, site, param_value) %>%
    group_by(param_name) %>%
    mutate(z_score = scale(param_value)) %>%
    select(id, site, param_name, param_value, z_score)

ext_main_outliers <- ext_main_values %>%
    filter(abs(z_score) >= 3) %>%
    arrange(desc(abs(z_score)))

ext_resp_values <- ext_resp %>%
    pivot_longer(
        where(is.numeric),
        names_to = "param_name",
        values_to = "param_value"
    ) %>%
    drop_na(id, site, param_value) %>%
    group_by(param_name) %>%
    mutate(z_score = scale(param_value)) %>%
    select(
        site, id, date_of_assessment,
        param_name, param_value, z_score
    )

ext_resp_outliers <- ext_resp_values %>%
    filter(abs(z_score) >= 3) %>%
    arrange(desc(abs(z_score)))

output_dir <- "output/qc"
output_path <- file.path(output_dir, "numeric-outliers.xlsx")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write_xlsx(list(
    "P-ALS_Ext_Main_Data_File" = ext_main_outliers,
    "P-ALS_Ext_Respiratory_Assesments" = ext_resp_outliers
), output_path)
