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
)
