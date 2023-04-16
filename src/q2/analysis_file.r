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
head(B)

barplot(table(B$SITE),
  las = 2, ylim = c(0, 3500),
  ylab = "Number of patients"
)
boxplot(B$TOTAL ~ B$SITE,
  ylim = c(0, 48),
  xlab = "", las = 2, ylab = "ALSFRS-R total score"
)
boxplot(B$MAXOBS ~ B$SITE,
  ylim = c(0, 30),
  xlab = "", las = 2, ylab = "Number of observations"
)
boxplot(B$MAXTIME ~ B$SITE,
  xlab = "", las = 2, ylab = "Observation time"
)

# . Assessing time period data
table(is.na(B$DATE), B$SITE)
boxplot(B$YEAR ~ B$SITE,
  xlab = "", las = 2, ylab = "Observation time"
)
tapply(B$YEAR, B$SITE, range)
barplot(table(B$SITE, B$YEAR), col = 1:9)

plot(NULL, ylim = c(1, 9), xlim = c(1990, 2023), yaxt = "n", ylab = "", xlab = "Year")
segments(
  x0 = tapply(B$YEAR, B$SITE, min),
  x1 = tapply(B$YEAR, B$SITE, max),
  y0 = 1:9, y1 = 1:9
)
points(tapply(B$YEAR, B$SITE, min), 1:9)
points(tapply(B$YEAR, B$SITE, max), 1:9)
text(1990, 1:9, adj = 0, names(tapply(B$YEAR, B$SITE, min)))

# . Assessing follow-up time:
fu <- sapply(0:36, function(t.ii) {
  prop.table(table(B$MAXTIME > t.ii, B$SITE), 2)[2, ]
})
plot(NULL,
  ylim = c(0, 1), xlim = c(0, 36),
  ylab = "Proportion of patients with FU data", xlab = "Follow-up (months)"
)
for (ii in 1:nrow(fu)) {
  lines(0:36, fu[ii, ], type = "b", col = gg.cols(9)[ii], lty = 1)
}
legend("topright", legend = rownames(fu), col = gg.cols(9), pch = 15, bty = "n")
# . Clearly King's & Utrecht are outliers with FU data, as expected...

# . Bias in FU data:
boxplot(B$TOTAL ~ I(B$MAXTIME > 0) + B$SITE, las = 2, col = gg.cols(2))
drop1(lm(B$TOTAL ~ I(B$MAXTIME > 0) * B$SITE), test = "F")
summary(lm(B$TOTAL ~ I(B$MAXTIME > 0) * B$SITE))
summary(lm(B$TOTAL ~ I(B$MAXTIME > 0) + B$SITE))
# . Clear selection bias of patients w/ FU (4.55 points on average in FRS)
# . Selection bias is bit different per site, but pattern in same direction

#### 5. Modelling ####

# . From the above, data after month 24 is sparse
# . Utrecht & king's to few patients with FU to match random effects?
D <- D[(D$TIME < 25.5), ]
ggplot(D, aes(TIME, TOTAL, by = ID)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~SITE)

# . Test run per site
sts <- unique(D$SITE)
m <- lapply(sts, function(site.ii) {
  lmer(TOTAL ~ TIME + (TIME | ID), data = D[D$SITE == site.ii, ], REML = F)
})
# . Errors in basic modelling

# . MINIMAL DATA REQUIRED: DF = 3 per individual: intercept, slope, co-var intercept - slope
(3 * table(B$SITE)) > table(D$SITE) # . King's & Utrecht indeed to few data for modelling...

# . Remove sites:
D <- D[(D$SITE %in% c("King's", "Utrecht") == F), ]

# . Re-run test
sts <- unique(D$SITE)
m <- lapply(sts, function(site.ii) {
  lmer(TOTAL ~ TIME + (TIME | ID),
    data = D[D$SITE == site.ii, ], REML = F,
    control = lmerControl(
      optimizer = "optimx",
      optCtrl = list(method = "nlminb")
    )
  )
})
# . Addressed model convergence issues by changing optimizer

output <- do.call("rbind", lapply(m, function(model.ii) {
  data.frame(
    intercept = summary(model.ii)$coefficients["(Intercept)", "Estimate"],
    slope = summary(model.ii)$coefficients["TIME", "Estimate"],
    sigma = sigma(model.ii),
    re.int = as.data.frame(VarCorr(model.ii))$sdcor[1],
    re.slp = as.data.frame(VarCorr(model.ii))$sdcor[2]
  )
}))
cbind(sts, output)
# . Consistent with data: more baseline variability per site
# . There may be quality differences between sites (reflected in sigma, i.e. within-patient var)
# . Degree of population heterogeneity is different between sites (re.slp)
# . Slopes in general are different accross sites

## Data remains limited for advanced individual patient modelling:
# . Available degrees of freedom:
table(D$SITE) / table(D[D$TIME == 0, ]$SITE)

## Model to test differences per site ##
m000 <- lmer(TOTAL ~ TIME + (1 | ID),
  data = D, REML = F,
  control = lmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "nlminb")
  )
)
# . Allow for slope variability between pts
m00 <- lmer(TOTAL ~ TIME + (TIME | ID),
  data = D, REML = F,
  control = lmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "nlminb")
  )
)
# . Allow for baseline variability per site
m0 <- lmer(TOTAL ~ TIME + (TIME | ID) + (1 | SITE),
  data = D, REML = F,
  control = lmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "nlminb")
  )
)
# . Allow for variability in progression rate
m <- lmer(TOTAL ~ TIME + (TIME | ID) + (TIME | SITE),
  data = D, REML = F,
  control = lmerControl(
    optimizer = "optimx",
    optCtrl = list(method = "nlminb")
  )
)
anova(m000, m00, m0, m)

# . Variability between patients
# . Variability between sites in baseline value
# . Variability between sites in progression rates

summary(m)
# . Average progression of 0.991 pts / mo

# . Site var:
coef(m)$SITE
plot(coef(m)$SITE[, 1],
  coef(m)$SITE[, 2],
  cex = 20 * table(D$SITE) / nrow(D),
  ylab = "Population monthly progression rate",
  xlab = "Population baseline total sore",
  ylim = c(-1.25, -0.75), xlim = c(34, 41),
  col = adjustcolor(gg.cols(7), .25), pch = 16
)
points(coef(m)$SITE[, 1],
  coef(m)$SITE[, 2],
  cex = 20 * table(D$SITE) / nrow(D), col = gg.cols(7)
)
legend("topright", bty = "n", legend = rownames(coef(m)$SITE), pch = 15, col = gg.cols(7))

abline(
  v = summary(m)$coefficients[1, 1],
  h = summary(m)$coefficients[2, 1], lty = 3
)
