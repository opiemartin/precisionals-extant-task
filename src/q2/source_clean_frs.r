#### Precision ALS ####
# . Question 2: ALSFRS-R decline over time
# . Source file to clean ALSFRS-R data

# . Source functions:
# source ("https://raw.githubusercontent.com/rpavaneijk/Basics/master/Source_Basics.r")
source("src/q2/source_basics.r") # source local copy to keep track of changes

# . Libraries
library(dplyr)
library(openxlsx)

#### 0. Functions ####
checkFRS <- function (ID){
  with (D[D$ID == ID, ],
        plot (TIME, TOTAL, type = "b"))
}

#### 1. Data ####
D <- read.xlsx ("data/P-ALS_Ext_V2_ALSFRS-R.xlsx", detectDates = T)

#### 2. Cleaning ####
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

#. Recreate date [Updated May 2023]
D$DATE <- D$Date.of.Assessment
D$DATE[D$DATE == "Missing"] <- NA
D$DATE[D$DATE == "N/A"] <- NA
D$DATE <- as.Date (D$DATE)
#D$DATE <- substr (D$DATE, 1, 5)
#D$DATE <- suppressWarnings (as.Date (as.numeric (D$DATE), origin = "1899-12-30"))

#. Recreate time
D$AGE <- suppressWarnings (as.numeric (D$Age.of.Assessment))
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
# D[is.na (D$TIME), ] # correct

#. Clean total score
D$TOTAL[D$TOTAL == "N/A" | D$TOTAL == "Missing"] <- NA
D$TOTAL <- as.numeric (D$TOTAL)
#D[!is.na (D$TOTAL) & D$TOTAL > 48, ]$TOTAL <- 33
D <- D[!is.na (D$TOTAL), ] # 36025 -> 35908
D <- D[!D$TOTAL == 0, ] # this leads to many errors; maybe 1 or 2 true but not worth keeping
# 35908 -> 35839
D <- D[order (D$ID, D$TIME), ]

#### 3. Manual cleaning ####

#. Manual adjustments based on below data: 
# D <- D[!(D$ID == "BEL-0461" & D$DATE == "2011-01-20"), ]
# D <- D[!(D$ID == "BEL-0559" & D$DATE == "2011-12-20"), ]
# D <- D[!(D$ID == "BEL-0627" & D$DATE == "2015-04-30"), ]
# D <- D[!(D$ID == "BEL-0845" & D$DATE == "2016-10-20"), ]
# D <- D[!(D$ID == "BEL-0999" & D$DATE == "2021-09-03"), ]
# D <- D[!(D$ID == "BEL-1010" & D$DATE == "2010-01-12"), ]
# D <- D[!(D$ID == "BEL-1160" & D$DATE == "2019-01-03"), ]
# D <- D[!(D$ID == "BEL-1205" & D$DATE == "2019-09-20"), ]
# D <- D[!(D$ID == "BEL-1581" & D$DATE == "2019-08-23"), ]
D <- D[!(D$ID == "IRE-1635" & D$DATE == "2014-10-09"), ]
D <- D[!(D$ID == "IRE-3419" & D$DATE == "2020-01-16"), ]
D <- D[!(D$ID == "IRE-3926" & D$DATE == "2021-02-11"), ]
D <- D[!(D$ID == "ITA-0697" & D$DATE == "2007-10-30"), ]

# D <- D[!(D$ID == "KCL-0523" & D$TOTAL == 0), ]
# D <- D[!(D$ID == "KCL-4320" & D$TOTAL == 0), ]
# D <- D[!(D$ID == "KCL-4491" & D$TOTAL == 0), ]
D <- D[!(D$ID == "NLD-0010" & D$TIME > 100), ]
# D <- D[!(D$ID == "FRA-0048" & D$TOTAL == 0), ]
# D <- D[!(D$ID == "NLD-0087" & D$TIME == 0), ]
D <- D[!(D$ID == "NLD-0253" & D$AGE == 65.56), ]

# D[(D$ID == "BEL-1098" & D$DATE == "2018-01-22"), ]$DATE <- "2019-01-22"
D[(D$ID == "FRA-0260" & D$DATE == "2004-03-15"), ]$DATE <- "2005-03-15"
#D[(D$ID == "FRA-0964" & D$DATE == "2009-10-10"), ]$DATE <- "2008-10-10"
D[(D$ID == "IRE-3035" & D$DATE == "2014-09-25"), ]$DATE <- "2015-09-25"
#D[(D$ID == "BEL-0353" & D$DATE == "2010-01-21"), ]$DATE <- "2009-01-21"
#D[D$ID == "BEL-0596" & D$TIME == 0, ]$TOTAL <- 42

# D <- D[!(D$ID == "BEL-0353" & D$DATE == "2009-05-14"), ]
# D <- D[!(D$ID == "BEL-0353" & D$DATE == "2009-08-06"), ]
# D <- D[!(D$ID == "BEL-0530" & D$DATE == "2015-06-04"), ]
# D[(D$ID == "BEL-0535" & D$DATE == "2013-11-29"), ]$DATE <- "2012-11-29"

### BEL-1013 this person going up and down
# D[(D$ID == "BEL-1447" & D$DATE == "2021-04-15"), ]$DATE <- "2020-04-15"
# D <- D[!(D$ID == "BEL-1013" & D$DATE == "2015-04-21"), ]
# D <- D[!(D$ID == "BEL-1013" & D$DATE == "2020-10-19"), ]
D[(D$ID == "SHE-0094" & D$DATE == "2016-10-05"), ]$I12 <- 4
D[(D$ID == "SHE-0156" & D$DATE == "2011-07-11"), ]$DATE <- "2012-07-11"
D[(D$ID == "SHE-0195" & D$DATE == "2009-09-10"), ]$DATE <- "2008-09-10"
D[(D$ID == "SHE-0208" & D$DATE == "2017-02-28"), ]$DATE <- "2018-02-28"
D[(D$ID == "SHE-0228" & D$DATE == "2009-09-14"), ]$DATE <- "2008-09-14"
D[(D$ID == "SHE-0343" & D$DATE == "2014-07-23"), ]$DATE <- "2015-07-23"
D[(D$ID == "SHE-0508" & D$DATE == "2014-10-08"), ]$DATE <- "2015-10-08"

## Based on date of diagnosis/death from main file
D[(D$ID == "SHE-0589" & D$DATE == "2013-05-22"), ]$DATE <- "2012-05-22"

D <- D[!(D$ID == "SHE-0867" & D$DATE == "2017-07-26"), ]
D <- D[!(D$ID == "SHE-0871" & D$DATE == "2011-03-16"), ]
D <- D[!(D$ID == "SHE-0871" & D$DATE == "2011-09-22"), ]
D <- D[!(D$ID == "SHE-1220" & D$DATE == "2019-11-12"), ]
D <- D[!(D$ID == "SHE-1313" & D$DATE == "2018-03-13"), ]
D[(D$ID == "SHE-1341" & D$DATE == "2020-01-22"), ]$DATE <- "2019-01-22"
D <- D[!(D$ID == "SPA-0008" & D$DATE == "2017-12-01"), ]
D <- D[!(D$ID == "SPA-0025" & D$DATE == "2021-11-02"), ]
D <- D[!(D$ID == "SPA-0046" & D$DATE == "2017-10-06"), ]
D <- D[!(D$ID == "SPA-0068" & D$DATE == "2017-06-09"), ]
D <- D[!(D$ID == "SPA-0068" & D$DATE == "2019-03-22"), ]

## SPA-0070 checked but the numbers are all over the place generally, not sure what to exclude/change
#. Ruben: seems like two different patients:
# checkFRS ("SPA-0070")

#. Exclude totaly
D <- D[!(D$ID == "SPA-0070"), ]
D <- D[!(D$ID == "SPA-0083" & (D$DATE == "2018-02-02" | D$DATE =="2018-06-08" | D$DATE == "2018-10-19")), ]
D <- D[!(D$ID == "SPA-0097" & D$DATE == "2020-10-06"), ]
D <- D[!(D$ID == "SPA-0189" & D$DATE == "2016-12-16"), ]

## SPA-0305 person appears to be getting better and then worse throughout
# checkFRS ("SPA-0305") # two different patients
D <- D[!D$ID == "SPA-0305", ]
D[(D$ID == "SPA-0361" & D$DATE == "2019-02-19"), ]$DATE <- "2020-02-19"

## SPA-0391 - 2019-02-15 probably not wrong year, might be genuine increase
#. Ruben: this is entry mistake
D <- D[!(D$ID == "SPA-0391" & D$DATE == "2017-03-24"), ]
D <- D[!(D$ID == "SPA-0418" & D$DATE == "2018-04-27"), ]
D <- D[!(D$ID == "SPA-0466" & D$DATE == "2022-01-18"), ]

## SPA-0480 think it is a genuine rapid drop
#. Ruben: yes but 3rd measure is incorrect
# checkFRS ("SPA-0488")
D <- D[!(D$ID == "SPA-0488" & D$DATE == "2019-02-15"), ]
#D[(D$ID == "SPA-0673" & D$DATE == "2020-04-23"), ]$DATE <- "2019-04-23" 
D <- D[!(D$ID == "SPA-0698" & D$DATE == "2020-10-30"), ]
D <- D[!(D$ID == "SPA-0762" & D$DATE == "2021-06-04"), ]
# checkFRS ("SPA-0762")

# D[!(D$ID == "SWE-0114" & (D$DATE == "2017-11-06"| 
#                             D$DATE == "2019-06-13"| 
#                             D$DATE == "2020-02-03"| 
#                             D$DATE == "2020-08-13"| 
#                             D$DATE == "2020-11-03"| 
#                             D$DATE == "2021-03-29"| 
#                             D$DATE == "2021-07-01")), ]
# checkFRS ("SWE-0114") # here only second is mistake
D <- D[!(D$ID == "SWE-0114" & D$DATE == "2017-07-01"), ]

D[(D$ID == "SWE-0243" & D$DATE == "2021-12-31"), ]$DATE <- "2020-12-31" 
D <- D[!(D$ID == "SWE-0263" & D$DATE == "2017-12-01"), ]
D[(D$ID == "SWE-0270" & D$DATE == "2019-02-02"), ]$DATE <- "2020-02-02"
D[(D$ID == "SWE-0283" & D$DATE == "2018-09-09"), ]$DATE <- "2019-09-09"
D[(D$ID == "SWE-0331" & D$DATE == "2016-11-07"), ]$DATE <- "2017-11-07"
D <- D[!(D$ID == "SWE-0355" & D$DATE == "2022-04-20"), ]
# checkFRS ("SWE-0355") 

#. Addition Daphne
D <- D[!(D$ID == 'IRE-2352' & D$DATE == '2002-09-25'), ]
D <- D[!D$ID == 'NLD-0327', ]
D <- D[!(D$ID == 'NLD-2208' & D$AGE > 36),]
D <- D[!D$ID == 'NLD-2383', ]
D <- D[!D$ID == 'NLD-2403', ]
D <- D[!D$ID == 'NLD-2917', ]
D <- D[!(D$ID == 'NLD-0592' & D$AGE == '44.11'), ]
D <- D[!(D$ID == 'NLD-1690' & D$AGE == '47.81'), ] 
D <- D[!(D$ID == 'NLD-1814' & D$AGE == '54.91'), ] 
D <- D[!(D$ID == 'NLD-1822' & D$AGE == '55.07'), ]
D <- D[!(D$ID == 'NLD-1890' & D$AGE == '62.67'), ] 
D <- D[!(D$ID == 'NLD-2022' & D$AGE == '57.59'), ] 
D <- D[!(D$ID == 'NLD-2097' & D$AGE == '47.78'), ] 
D <- D[!(D$ID == 'NLD-2316' & D$AGE == '72.45'), ] 
D <- D[!(D$ID == 'NLD-2366' & D$AGE == '69.61'), ] 
D <- D[!(D$ID == 'NLD-2473' & D$AGE == '64.66'), ]
D <- D[!(D$ID == 'NLD-2540' & D$AGE == '52.85'), ]
D <- D[!(D$ID == 'NLD-2731' & D$AGE == '76'), ]
D <- D[!(D$ID == 'NLD-2765' & D$AGE == '74.25'), ] 
D <- D[!(D$ID == 'NLD-2786' & D$TOTAL == '43'), ] 
D <- D[!(D$ID == 'NLD-2828' & D$AGE == '72.23'), ]
D <- D[!(D$ID == 'NLD-2946' & D$AGE == '72.98'), ]
D <- D[!(D$ID == 'NLD-2682' & (D$AGE == '58.90' |  D$AGE == '59.31' | D$AGE == '59.04')), ] 
D <- D[!(D$ID == 'NLD-2798' & (D$AGE == '69.32' | D$AGE == '69.3' | D$AGE == '69.23')), ]
D <- D[!(D$ID == 'NLD-2861' & (D$AGE == '40' | D$AGE == '62.69' | D$AGE == '62.77')), ]
D <- D[!(D$ID == 'NLD-2864' & (D$AGE == '71.21' | D$AGE == '71.3')), ]
D <- D[!(D$ID == 'NLD-2919' & (D$AGE == '55.01' | D$AGE == '55.12' | D$TOTAL == '41')), ] 
D <- D[!(D$ID == 'NLD-2950' & (D$AGE == '58.23' | D$AGE == '58.24')), ]
D <- D[!(D$ID == 'NLD-2964' & D$AGE == '46.26'), ]
D[(D$ID == 'NLD-0979' & D$AGE == '74.18'), ]$AGE <- 73.18
D[(D$ID == 'NLD-1073' & D$AGE == '77.11'), ]$AGE <- 76.11
D[(D$ID == 'NLD-1556' & D$AGE == '59.77'), ]$AGE <- 60.77
D[(D$ID == 'NLD-1999' & D$AGE == '72.72'), ]$AGE <- 71.72
D[(D$ID == 'NLD-2188' & D$AGE == '52.71'), ]$AGE <- 51.71
D[(D$ID == 'NLD-2275' & D$AGE == '70.34'), ]$AGE <- 71.34
D[(D$ID == 'NLD-2349' & D$AGE == '77.4'), ]$AGE <- 76.4
D[(D$ID == 'NLD-2807' & D$AGE == '67.59'), ]$AGE <- 68.59
D[(D$ID == 'NLD-2847' & D$AGE == '68.59'), ]$AGE <- 67.59
D[(D$ID == 'NLD-2919' & D$AGE == '54.39'),]$AGE <- 55.39
D[(D$ID == 'NLD-2955' & D$AGE == '69'), ]$AGE <- 70
D[(D$ID == 'NLD-2759' & D$AGE == '47.29'), ]$TOTAL <- 43

#. Recalculate time:
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
D <- D[order (D$ID, D$TIME), ]

#. Visual check data
# ggplot (D[D$TIME < 13.5, ], aes (TIME, TOTAL, by = ID)) + geom_line (alpha = .1)

#. Calculate difference between observations
D$DIFF <- unlist (by (D, D$ID, function (d){
  c (0, diff (d$TOTAL))
}))

D$DIFF.PREV <- unlist (by (D, D$ID, function (d){
  c (0, d[-nrow (d), ]$TOTAL)
}))

D$ALL0 <- unlist (by (D, D$ID, function (d){
  rep (all (d$TOTAL == 0), nrow (d))
}))

#. These have been addressed now
# D <- D[!(D$DIFF > 10 & D$DIFF.PREV == 0), ]
# D <- D[!(D$TOTAL == 0 & D$DIFF < -5), ]
# D <- D[!(D$TIME == 0 & D$TOTAL == 0), ]
# D <- D[!D$ALL0, ]
# D <- D[!(D$TOTAL == 0 & D$Site == "Bellvitge"), ]

#. Probably mistake in year
D[D$DIFF > 5 & D$TIME > 10 & D$TIME < 14, ]$TIME <- abs (D[D$DIFF > 5 & D$TIME > 10 & D$TIME < 14, ]$TIME - 12)
D <- D[!D$TIME > 120, ]
# D <- D[!D$DIFF >= 9, ] # Option to reduce noise [e.g. exclude 9 or 8 differences]

#. Cleaned data:
# ggplot (D[D$TIME < 13.5, ], aes (TIME, TOTAL, by = ID)) + geom_line (alpha = .1)

#### 4. Data variables ####
D$MAXOBS <- unlist (by (D, D$ID, function (d){
  rep (nrow (d), nrow (d))
}))

D$MAXTIME <- unlist (by (D, D$ID, function (d){
  rep (max (d$TIME), nrow (d))
}))

#. Further refinements
D <- rn (D, old = "Site", new = "SITE")
D$YEAR <- as.numeric (format (D$DATE, "%Y"))

its <- c ("I1", "I2", "I3", "I4", "I5A", "I5B", "I5X", "I6", "I7", "I8", "I9", "I10", "I11", "I12")
D[, its] <- suppressWarnings (D[, its] %>% mutate_if (is.character, as.numeric))
D$I5 <- ifelse (is.na (D$I5A), D$I5B, D$I5A)

its <- c ("I1", "I2", "I3", "I4", "I6", "I7", "I8", "I9", "I10", "I11", "I12")
D$I5 <- ifelse (is.na (D$I5) & not.na (rowSums (D[,its])), D$TOTAL - rowSums (D[,its]), D$I5)
D$I5X <- ifelse (is.na (D$I5A) & is.na (D$I5B), NA, as.numeric (not.na (D$I5A)))
its <- c ("I1", "I2", "I3", "I4", "I5", "I5X", "I6", "I7", "I8", "I9", "I10", "I11", "I12")

D <- D[, c ("ID", "SITE", "DATE", "TIME", its, "TOTAL", "YEAR", "AGE", "MAXOBS", "MAXTIME")]
rm (its)