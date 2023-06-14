#### Precision ALS ####
# . Question 2: ALSFRS-R decline over time
# . Analysis script

# . Source file to data cleaning
source("src/q2/source_clean_frs.r")

# . libs
library(optimx)
library(lme4)

# . Baseline data
B <- D[D$TIME == 0, ]
head (B)

barplot (table (B$SITE), las = 2, ylim = c (0, 3500),
         ylab = "Number of patients")
boxplot (B$TOTAL ~ B$SITE, ylim = c (0, 48),
         xlab = "", las = 2, ylab = "ALSFRS-R total score")
abline (h = median (B$TOTAL), lty = 3)
boxplot (B$MAXOBS ~ B$SITE, ylim = c (0, 30),
         xlab = "", las = 2, ylab = "Number of observations per patient")
boxplot (B$MAXTIME ~ B$SITE,
         xlab = "", las = 2, ylab = "Observation time")

#. Assessing time period data
# table (is.na (B$DATE), B$SITE)
# boxplot (B$YEAR ~ B$SITE,
#          xlab = "", las = 2, ylab = "Observation time")
# tapply (B$YEAR, B$SITE, range)
# barplot (table (B$SITE, B$YEAR), col = 1:9)

# plot (NULL, ylim = c (1,9), xlim = c (1990, 2023), yaxt = "n", ylab = "", xlab = "Year")
# segments (x0 = tapply (B$YEAR, B$SITE, min),
#           x1 = tapply (B$YEAR, B$SITE, max),
#           y0 = 1:9, y1 = 1:9)
# points (tapply (B$YEAR, B$SITE, min), 1:9)
# points (tapply (B$YEAR, B$SITE, max), 1:9)
# text (1990, 1:9, adj = 0, names (tapply (B$YEAR, B$SITE, min)))

#. Assessing follow-up time:
fu <- sapply (0:36, function (t.ii){prop.table (table (B$MAXTIME > t.ii, B$SITE), 2)[2, ]})
plot (NULL, ylim = c (0,1), xlim = c (0, 36),
      ylab = "Proportion of patients with FU data", xlab = "Follow-up (months)")
for (ii in 1:nrow (fu)){
  lines (0:36, fu[ii, ], type = "b", col = gg.cols (9)[ii], lty = 1)
}
legend ("topright", legend = rownames (fu), col = gg.cols (9), pch = 15, bty = "n")
#. Clearly King's & Utrecht are outliers with FU data, as expected...

#. Bias in FU data:
boxplot (B$TOTAL ~ I (B$MAXTIME > 0) + B$SITE, las = 2, col = gg.cols (2))
drop1 (lm (B$TOTAL ~ I (B$MAXTIME > 0) * B$SITE), test = "F")
summary (lm (B$TOTAL ~ I (B$MAXTIME > 0) * B$SITE))
summary (lm (B$TOTAL ~ I (B$MAXTIME > 0) + B$SITE))
#. Clear selection bias of patients w/ FU (4.55 points on average in FRS)
#. Selection bias is bit different per site, but pattern in same direction

#### 5. Modelling ####

#. From the above, data after month 24 is sparse
#. Utrecht & king's to few patients with FU to match random effects?
D <- D[(D$TIME < 25.5), ]
ggplot (D, aes (TIME, TOTAL, by = ID)) + geom_line (alpha = 0.1) + facet_wrap (~SITE, ncol = 4) +
  ylab ("ALSFRS-R total score") + xlab ("Time since baseline (months)")

#. Test run per site
sts <- unique (D$SITE)
m <- lapply (sts, function (site.ii){
  lmer (TOTAL ~ TIME + (TIME|ID), data = D[D$SITE == site.ii, ], REML = F)
})
#. Errors in basic modelling

#. MINIMAL DATA REQUIRED: DF = 3 per individual: intercept, slope, co-var intercept - slope
(3 * table (B$SITE)) > table (D$SITE) #. King's & Utrecht indeed to few data for modelling...

#. Remove sites:
D <- D[(D$SITE %in% c ("King's", "Utrecht") == F), ]

#. Re-run test
sts <- unique (D$SITE)
m <- lapply (sts, function (site.ii){
  lmer (TOTAL ~ TIME + (TIME|ID), data = D[D$SITE == site.ii, ], REML = F,
        control = lmerControl(
          optimizer = "optimx", 
          optCtrl = list (method = "nlminb")
        ))
})
#. Addressed model convergence issues by changing optimizer

output <- do.call ("rbind", lapply (m, function (model.ii){
  
  data.frame (intercept = summary (model.ii)$coefficients["(Intercept)", "Estimate"],
              slope = summary (model.ii)$coefficients["TIME", "Estimate"],
              sigma = sigma (model.ii),
              re.int = as.data.frame (VarCorr(model.ii))$sdcor[1],
              re.slp = as.data.frame (VarCorr(model.ii))$sdcor[2])
  
}))
cbind ("Site" = sts, round (output, 2))
#. Consistent with data: more baseline variability per site
#. There may be quality differences between sites (reflected in sigma, i.e. within-patient var)
#. Degree of population heterogeneity is different between sites (re.slp)
#. Slopes in general are different accross sites

## Data remains limited for advanced individual patient modelling:
#. Available degrees of freedom:
table (D$SITE) / table (D[D$TIME == 0, ]$SITE)

## Model to test differences per site ## 
m000 <- lmer (TOTAL ~ TIME + (1|ID), data = D, REML = F, 
              control = lmerControl(
                optimizer = "optimx", 
                optCtrl = list (method = "nlminb")
              ))
#. Allow for slope variability between pts
m00 <- lmer (TOTAL ~ TIME + (TIME|ID), data = D, REML = F, 
             control = lmerControl(
               optimizer = "optimx", 
               optCtrl = list (method = "nlminb")
             ))
#. Allow for baseline variability per site
m0 <- lmer (TOTAL ~ TIME + (TIME|ID) + (1|SITE), data = D, REML = F, 
            control = lmerControl(
              optimizer = "optimx", 
              optCtrl = list (method = "nlminb")
            ))
#. Allow for variability in progression rate
m <- lmer (TOTAL ~ TIME + (TIME|ID) + (TIME|SITE), data = D, REML = F, 
           control = lmerControl(
             optimizer = "optimx", 
             optCtrl = list (method = "nlminb")
           ))
anova (m000, m00, m0, m)

#. Variability between patients
#. Variability between sites in baseline value
#. Variability between sites in progression rates

summary (m)
#. Average progression of 0.98 pts / mo

#. Site var:
coef (m)$SITE
plot (coef (m)$SITE[,1],
      coef (m)$SITE[,2],
      cex = 20*table (D$SITE)/nrow (D),
      ylab = "Population monthly progression rate",
      xlab = "Population baseline total sore",
      ylim = c (-1.25, -0.75), xlim = c (34, 41),
      col = adjustcolor (gg.cols (7), .25), pch = 16)
points (coef (m)$SITE[,1],
        coef (m)$SITE[,2],
        cex = 20*table (D$SITE)/nrow (D), col = gg.cols (7))
legend ("topright", bty = "n", legend = rownames (coef (m)$SITE), pch = 15, col = gg.cols (7))

abline (v = summary (m)$coefficients[1,1],
        h = summary (m)$coefficients[2,1], lty = 3)

#. Example spline model
t <- 0:6
plot (t, 40 + (-1*t) + (0.5)*(t>6)*(t - 6),
      type = "l", ylim = c (24, 48), xlim = c (0,12),
      ylab = "ALSFRS-R total score",
      xlab = "Time since baseline (months)")
t <- 6:12
lines (t, 40 + (-1*t) + (0.75)*(t>6)*(t - 6), lty = 3)

#. Applied:
D$TIME6 <- pmax (0, D$TIME - 6)
D$TIME12 <- pmax (0, D$TIME - 12)
D$TIME18 <- pmax (0, D$TIME - 18)

m <- lmer (TOTAL ~ TIME + (TIME|ID) + (TIME|SITE), data = D, REML = F, 
           control = lmerControl(
             optimizer = "optimx", 
             optCtrl = list (method = "nlminb")
           ))
m.spline <- lmer (TOTAL ~ TIME + TIME6 + TIME12 + TIME18 + (TIME|ID) + (TIME|SITE), data = D, REML = F, 
                  control = lmerControl(
                    optimizer = "optimx", 
                    optCtrl = list (method = "nlminb")
                  ))
anova (m, m.spline)

time <- seq (0, 24, by = 0.1)
cfs <- summary (m.spline)$coefficients[, "Estimate"]
plot (time, cfs[1] + time*cfs["TIME"] + 
        pmax (0, time-6)*cfs["TIME6"] + 
        pmax (0, time-12)*cfs["TIME12"] +
        pmax (0, time-18)*cfs["TIME18"], type = "l",
      ylim = c (0, 48), 
      ylab = "ALSFRS-R total score",
      xlab = "Time since baseline (months)",
      xaxt = "n")
axis (1, seq (0,24, by = 6))
lines (time, summary (m)$coefficients[1, "Estimate"] + 
         time*summary (m)$coefficients[2, "Estimate"],
       lty = 3, col = 2)
abline (v = seq (6,18, by = 6), col = "grey70")
text (seq (3, 21, by = 6), 42,
      round (cumsum (summary (m.spline)$coefficients[2:5, "Estimate"]), 2),
      font = 2)

#. Variability between patients:

hist (coef (m.spline)$ID[, "TIME"], xlab = "Slope - points per month", main = "")
plot (quantile (coef (m.spline)$ID[, "TIME"], seq (0.2, 0.8, by = 0.1)))

plot (sort (coef (m.spline)$ID[, "TIME"]), 
      (1:length (coef (m.spline)$ID[, "TIME"])) / (length (coef (m.spline)$ID[, "TIME"]) + 1),
      type = "step", xlim = c (-4, 0.25), ylab = "Percentile", xlab = "Slope - points per month")
abline (h = seq (0.25, 0.75, by =0.25), lty = 3)
rect (xleft = quantile (coef (m.spline)$ID[, "TIME"], 0.25),
      xright = quantile (coef (m.spline)$ID[, "TIME"], 0.75), 
      ybottom = -100, ytop = 1e4, border = NA, col = adjustcolor(1, .1))

text (quantile (coef (m.spline)$ID[, "TIME"], 0.25), 0.05, round (quantile (coef (m.spline)$ID[, "TIME"], 0.25), 2), adj = 1, font = 2)
text (quantile (coef (m.spline)$ID[, "TIME"], 0.75), 0.05, round (quantile (coef (m.spline)$ID[, "TIME"], 0.75), 2), adj = 0, font = 2)        

####

### Delta ALSFRSR
source("src/ext/main.R")

B_age_onset <- B %>% 
  left_join(select(ext_main, id, age_at_onset, date_of_onset), by=c("ID"="id")) %>%
  mutate(
    DATE = as.Date(DATE),
    date_of_onset = as.Date(date_of_onset),
    time_to_alsfrsr = case_when(!is.na(DATE) & !is.na(date_of_onset) ~ (as.numeric(DATE-date_of_onset)/365.25)*12,
                                !is.na(AGE) & !is.na(age_at_onset) ~ (AGE-age_at_onset)*12 
    ),
    delta_alsfrsr = case_when(time_to_alsfrsr > 0 & TOTAL < 48 ~ (48-TOTAL)/time_to_alsfrsr,
                              TRUE ~ as.numeric(NA))) %>%
  select(ID, SITE, DATE, AGE, TOTAL, date_of_onset, age_at_onset, time_to_alsfrsr, delta_alsfrsr)%>%
  filter(!is.na(time_to_alsfrsr)) %>%
  #group_by(SITE) %>%
  summarise(median_delta = median(delta_alsfrsr, na.rm=T),
            lower_quartile = quantile(delta_alsfrsr, 0.25, na.rm=T),
            upper_quartile = quantile(delta_alsfrsr, 0.75, na.rm=T))


##Data description table ####



alsfrsr_ids <- unique(D$ID)

ext_main_dx_clean <- ext_main %>%
  mutate(diagnosis_ALS = diagnosis %in% c(
    "ALS",
    "ALS plus",
    "ALS/FTD",
    "Atypical",
    "Head drop",
    "PBP"),
    diagnosis_PLS = diagnosis %in% c(
      "PLS",
      "PLS/ALS"
    ),
    diagnosis_PMA = diagnosis %in% c(
      "PMA"
    ),
    diagnosis_UMNpred = diagnosis %in% c(
      "UMN Predominant ALS",
      "Suspected PLS"
    ),
    diagnosis_LMNpred = diagnosis %in% c(
      "LMN Predominant ALS",
      "Flail arm",
      "Flail leg"
    ), 
    diagnosis_not_ALS = diagnosis %in% c(
      "MMN",
      "FTD",
      "Unknown (son of ALS patient)"
    ),
    diagnosis_na = is.na(diagnosis)
  ) 


main_summary_alsfrsr <- ext_main_dx_clean %>%
  mutate(ALSFRSR = case_when(id %in% alsfrsr_ids ~ 1,
                             TRUE ~ 0 )) %>%
  filter(ALSFRSR == 1) %>%
  summarise(n = n(),
            mean_age_onset = mean(as.numeric(calculated_age_at_onset), na.rm=T),
            mean_age_diagnosis = mean(as.numeric(calculated_age_at_diagnosis), na.rm=T),
            female_percent = round((sum(sex=="Female", na.rm=T)/n())*100,1),
            male_percent = round((sum(sex=="Male", na.rm=T)/n())*100,1),
            bulbar_onset_percent = round((sum(bulbar_onset==TRUE)/n())*100,1),
            spinal_onset_percent = round((sum(spinal_onset==TRUE)/n())*100,1),
            respiratory_onset_percent = round((sum(respiratory_onset==TRUE)/n())*100,1),
            cognitive_onset_percent = round((sum(cognitive_onset==TRUE)/n())*100,1),
            na_onset_percent = round((sum(is.na(site_of_onset))/n())*100,1),
            diagnosis_ALS_percent = round((sum(diagnosis_ALS==TRUE)/n())*100,1),
            diagnosis_PLS_percent = round((sum(diagnosis_PLS==TRUE)/n())*100,1),
            diagnosis_PMA_percent = round((sum(diagnosis_PMA==TRUE)/n())*100,1),
            diagnosis_na_percent = round((sum(diagnosis_na==TRUE)/n())*100,1),
            mean_disease_duration = mean(age_at_death-age_at_onset, na.rm = T),
            median_disease_duration = median(age_at_death-age_at_onset, na.rm = T))

main_summary <- ext_main_dx_clean %>%
  summarise(n = n(),
            mean_age_onset = mean(as.numeric(calculated_age_at_onset), na.rm=T),
            mean_age_diagnosis = mean(as.numeric(calculated_age_at_diagnosis), na.rm=T),
            female_percent = round((sum(sex=="Female", na.rm=T)/n())*100,1),
            male_percent = round((sum(sex=="Male", na.rm=T)/n())*100,1),
            bulbar_onset_percent = round((sum(bulbar_onset==TRUE)/n())*100,1),
            spinal_onset_percent = round((sum(spinal_onset==TRUE)/n())*100,1),
            respiratory_onset_percent = round((sum(respiratory_onset==TRUE)/n())*100,1),
            cognitive_onset_percent = round((sum(cognitive_onset==TRUE)/n())*100,1),
            na_onset_percent = round((sum(is.na(site_of_onset))/n())*100,1),
            diagnosis_ALS_percent = round((sum(diagnosis_ALS==TRUE)/n())*100,1),
            diagnosis_PLS_percent = round((sum(diagnosis_PLS==TRUE)/n())*100,1),
            diagnosis_PMA_percent = round((sum(diagnosis_PMA==TRUE)/n())*100,1),
            diagnosis_na_percent = round((sum(diagnosis_na==TRUE)/n())*100,1),
            mean_disease_duration = mean(age_at_death-age_at_onset, na.rm = T),
            median_disease_duration = median(age_at_death-age_at_onset, na.rm = T))

compare <- main_summary %>%
  bind_rows(main_summary_alsfrsr) %>%
  mutate(header = c("main", "alsfrsr")) %>%
  pivot_longer(-header) %>%
  pivot_wider(name, names_from="header", values_from = "value") %>%
  mutate_if(is.numeric, round, 1) 