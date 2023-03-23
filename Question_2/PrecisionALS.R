#### Precision ALS ####
source ("https://raw.githubusercontent.com/rpavaneijk/Basics/master/Source_Basics.R")
D <- read.xlsx('/Users/daphneweemering/surfdrive/data/precision/P-ALS_Ext_ALSFRS-R.xlsx', 
               detectDates = F)

#. Rename cols
D <- rn (D, 
    old = c ("1).Speech", "2).Salivation", 
             "3).Swallowing", 
             "4).Handwriting", "5a).Cutting.Food.without.Gastrostomy",
             "5b).Cutting.Food.with.Gastrostomy",
             "5x).Cutting.Food.with.-.Gastrostomy.Status.Unknown",
             "6).Dressing.and.Hygine",
             "7).Turning.in.Bed", "8).Walking", "9).Climbing.Stairs",
             "10).Dyspnea", "11).Orthopnea", "12).Respiratory.Insufficiency",
             "Total.Score"),
    new = c ("I1", "I2", "I3", "I4", "I5A", "I5B", "I5X", "I6", "I7", "I8", "I9", "I10", "I11", "I12", "TOTAL"))

#. Recreate date
D$DATE <- D$Date.of.Assessment
D$DATE[D$DATE == "Missing"] <- NA
D$DATE[D$DATE == "N/A"] <- NA
D$DATE <- substr (D$DATE, 1, 5)
D$DATE <- as.Date (as.numeric (D$DATE), origin = "1899-12-30")

#. Recreate time
D$AGE <- as.numeric (D$Age.of.Assessment)
D <- D[!(is.na (D$DATE) & is.na (D$AGE)), ] # 36415 -> 36025
D <- D[order (D$ID), ]

D$TIME <- unlist (by (D, D$ID, function (d){
  
  # Select or Age or Date to calculate time difference since first measurement
  if (any (is.na (d$DATE))){ 
    if (all (is.na (d$DATE))) {
      (d$AGE - min (d$AGE)) * 12
    } else {
      rep (NA, nrow (d))
    }
  } else {
    as.numeric (d$DATE - min (d$DATE)) / (365.25/12)
  }
}))
D[is.na (D$TIME), ] # correct

#. Clean total score
D$TOTAL[D$TOTAL == "N/A" | D$TOTAL == "Missing"] <- NA
D$TOTAL <- as.numeric (D$TOTAL)
D[!is.na (D$TOTAL) & D$TOTAL > 48, ]$TOTAL <- 33
D <- D[!is.na (D$TOTAL), ] # 36025 -> 35908

#. Manual adjustments based on below data: 
D <- D[!(D$ID == "BEL-0461" & D$DATE == "2011-01-20"), ]
D <- D[!(D$ID == "BEL-0559" & D$DATE == "2011-12-20"), ]
D <- D[!(D$ID == "BEL-0627" & D$DATE == "2015-04-30"), ]
D <- D[!(D$ID == "BEL-0845" & D$DATE == "2016-10-20"), ]
D <- D[!(D$ID == "BEL-0999" & D$DATE == "2021-09-03"), ]
D <- D[!(D$ID == "BEL-1010" & D$DATE == "2010-01-12"), ]
D <- D[!(D$ID == "BEL-1160" & D$DATE == "2019-01-03"), ]
D <- D[!(D$ID == "BEL-1205" & D$DATE == "2019-09-20"), ]
D <- D[!(D$ID == "BEL-1581" & D$DATE == "2019-08-23"), ]
D <- D[!(D$ID == "IRE-1635" & D$DATE == "2014-10-09"), ]
D <- D[!(D$ID == "IRE-3419" & D$DATE == "2020-01-16"), ]
D <- D[!(D$ID == "IRE-3926" & D$DATE == "2021-02-11"), ]
D <- D[!(D$ID == "ITA-0697" & D$DATE == "2007-10-30"), ]

D <- D[!(D$ID == "KCL-0523" & D$TOTAL == 0), ]
D <- D[!(D$ID == "KCL-4320" & D$TOTAL == 0), ]
D <- D[!(D$ID == "KCL-4491" & D$TOTAL == 0), ]
D <- D[!(D$ID == "NLD-0010" & D$TIME > 100), ]
D <- D[!(D$ID == "FRA-0048" & D$TOTAL == 0), ]
D <- D[!(D$ID == "NLD-0087" & D$TIME == 0), ]
D <- D[!(D$ID == "NLD-0253" & D$AGE == 65.56), ]

D[(D$ID == "BEL-1098" & D$DATE == "2018-01-22"), ]$DATE <- "2019-01-22" 
D[(D$ID == "FRA-0260" & D$DATE == "2004-03-15"), ]$DATE <- "2005-03-15"
D[(D$ID == "FRA-0964" & D$DATE == "2009-10-10"), ]$DATE <- "2008-10-10"
D[(D$ID == "IRE-3035" & D$DATE == "2014-09-25"), ]$DATE <- "2015-09-25"
D[(D$ID == "BEL-0353" & D$DATE == "2010-01-21"), ]$DATE <- "2009-01-21"
D[D$ID == "BEL-0596" & D$TIME == 0, ]$TOTAL <- 42


### Sarah data edits
## BEL-0353 final 3 items on respiratory ??
D[(D$ID == "BEL-0353" & D$DATE == "2009-05-14"), ]

## BEL-0350 final 3 items on respiratory - could be zeros not 4s?
D[(D$ID == "BEL-0350" & D$DATE == "2016-02-25"), ] # cannot find this patient in the data

D[(D$ID == "BEL-0535" & D$DATE == "2013-11-29"), ]$DATE <- "2012-11-29"

### Not sure? this person going up and down
D[(D$ID == "BEL-1013" & D$DATE == "2015-04-21"), ]$DATE 
  
#D[(D$ID == "BEL-1098" & D$DATE == "2018-01-22"), ]$DATE <- "2019-01-22"
D[(D$ID == "BEL-1447" & D$DATE == "2021-04-15"), ]$DATE <- "2020-04-15"


D[(D$ID == "SHE-0094" & D$DATE == "2016-10-05"), ]$I12 <- 4
D[(D$ID == "SHE-0156" & D$DATE == "2011-07-11"), ]$DATE <- "2012-07-11"
D[(D$ID == "SHE-0195" & D$DATE == "2009-09-10"), ]$DATE <- "2008-09-10"
D[(D$ID == "SHE-0208" & D$DATE == "2017-02-28"), ]$DATE <- "2018-02-28"
D[(D$ID == "SHE-0228" & D$DATE == "2009-09-14"), ]$DATE <- "2008-09-14"
D[(D$ID == "SHE-0343" & D$DATE == "2014-07-23"), ]$DATE <- "2015-07-23"
D[(D$ID == "SHE-0508" & D$DATE == "2014-10-08"), ]$DATE <- "2015-10-08"

## year is likely wrong but don't know in which direction should change
## need year diagnosis/death/onset
D[(D$ID == "SHE-0589" & D$DATE == ""), ]

## possibly just missing values for respiratory assessment
D[(D$ID == "SHE-0867"), ] 

#. Visual check data
ggplot (D[D$TIME < 13.5, ], aes (TIME, TOTAL, by = ID)) + geom_line (alpha = .1)

#. Calculate difference between observations
D <- D[order (D$ID, D$TIME), ]
D$DIFF <- unlist (by (D, D$ID, function (d){
  c (0, diff (d$TOTAL))
}))

D$DIFF.PREV <- unlist (by (D, D$ID, function (d){
  c (0, d[-nrow (d), ]$TOTAL)
}))

D$ALL0 <- unlist (by (D, D$ID, function (d){
  rep (all (d$TOTAL == 0), nrow (d))
}))

D <- D[!(D$DIFF > 10 & D$DIFF.PREV == 0), ]
D <- D[!(D$TOTAL == 0 & D$DIFF < -5), ]
D <- D[!(D$TIME == 0 & D$TOTAL == 0), ]
D <- D[!D$ALL0, ]
D <- D[!(D$TOTAL == 0 & D$Site == "Bellvitge"), ]

#. Redo time:
D$TIME <- unlist (by (D, D$ID, function (d){
  
  # Select or Age or Date to calculate time difference since first measurement
  if (any (is.na (d$DATE))){ 
    if (all (is.na (d$DATE))) {
      (d$AGE - min (d$AGE)) * 12
    } else {
      rep (NA, nrow (d))
    }
  } else {
    as.numeric (d$DATE - min (d$DATE)) / (365.25/12)
  }
}))

#. Probably mistake in year
D[D$DIFF > 5 & D$TIME > 10 & D$TIME < 14, ]$TIME <- abs (D[D$DIFF > 5 & D$TIME > 10 & D$TIME < 14, ]$TIME - 12)
D <- D[!D$TIME > 120, ] 

#. Cleaned data:
ggplot (D[D$TIME < 13.5, ], aes (TIME, TOTAL, by = ID)) + geom_line (alpha = .1)

d <- D[D$DIFF > 9, ]
table (d$Site)

write.xlsx (d, file = "~/SurfDrive/data.xlsx")

D[D$ID == "BEL-0353", ]


## Addition Daphne
# where D$DIFF > 9
D[(D$ID == 'NLD-0592' & D$AGE == '44.11'), ] <- NA
D[D$ID == 'NLD-0327', ] <- NA 
D$AGE <- ifelse(D$ID == 'NLD-0979' & D$AGE == '73.54', '74.18', 
                ifelse(D$ID == 'NLD-0979' & D$AGE == '74.18', '73.54', D$AGE))
D$AGE <- ifelse(D$ID == 'NLD-1073' & D$AGE == '76.47', '77.11', 
                ifelse(D$ID == 'NLD-1073' & D$AGE == '77.11', '76.47', D$AGE))
D$AGE <- ifelse(D$ID == 'NLD-1556' & D$AGE == '59.77', '60.25', 
                ifelse(D$ID == 'NLD-1556' & D$AGE == '60.25', '59.77', D$AGE))
D[(D$ID == 'NLD-1814' & D$AGE == '54.91'), ] <- NA 
D$AGE <- ifelse(D$ID == 'NLD-1822' & D$AGE == '55.07', '55.54',
                ifelse(D$ID == 'NLD-1822' & D$AGE == '55.54', '55.07', D$AGE))
D[(D$ID == 'NLD-1890' & D$AGE == '62.67'), ] <- NA 
D[(D$ID == 'NLD-2022' & D$AGE == '57.59'), ] <- NA
D[(D$ID == 'NLD-2097' & D$AGE == '47.78'), ] <- NA
D[(D$ID == 'NLD-2188' & D$AGE == '52.71'), ]$AGE <- '51.71'
D$AGE <- ifelse(D$ID == 'NLD-2275' & D$AGE == '70.34', '70.58', 
                ifelse(D$ID == 'NLD-2275' & D$AGE == '70.58', '70.34', D$AGE))
D[(D$ID == 'NLD-2316' & D$AGE == '72.45'), ] <- NA
D[(D$ID == 'NLD-2349' & D$AGE == '77.4'), ]$AGE <- '76.4'
D[(D$ID == 'NLD-2366' & D$AGE == '69.61'), ] <- NA
D$AGE <- ifelse(D$ID == 'NLD-2383' & D$AGE == '63.49', '64.52',
                ifelse(D$ID == 'NLD-2383' & D$AGE == '64.52', '63.49', D$AGE))



m <- lmer (TOTAL ~ TIME + (TIME|ID), data = D[D$TIME < 13.5, ])
summary (m)

hist (D$TIME, breaks = 100)

