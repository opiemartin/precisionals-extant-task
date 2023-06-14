#### Basic functions ####
## Written by van Eijk RP
## This file contains basic R-functions

### Whole number
is.wholenumber <- function (x, tol = .Machine$double.eps^0.5) {
  # Determines whether a number is rounded or not
  #
  # Args:
  #  x = Numeric vector
  #  tol = cut-off
  #
  # Function:
  if (class (x) != "numeric"){
    print ("Variable is not numeric")
  } else {
    abs(x - round(x)) < tol
  }
}

### Not na
not.na <- function (x){is.na (x) == F}

### inverse logit
ilogit <- function (x) {exp (x)/(1 + exp (x))}

### 4. Rename column names
rn <- function (D, old, new){names (D)[names (D) %in% old] <- new; return (D)}

### Simulation CI 
SimCI <- function (n, p, a = 0.05){
  se <- sqrt (p * (1-p)) / sqrt (n)
  cbind (P = p,
         LB = round (p + (qnorm (a/2) * se), 5),
         UB = round (p + (qnorm (1-(a/2)) * se), 5))
}

## Power2N
pwr2n <- function (pwr, n = 300, target.pwr = 0.8, target.alpha = 0.05){
  (n * (qnorm (1 - target.alpha/2) + qnorm (target.pwr))^2) /
    ((qnorm (1 - target.alpha/2) + qnorm (pwr))^2)
}

### Colour ramplette
cols <- colorRampPalette (c ("coral1", "darkolivegreen4"))

### Default colours ggplot
gg.cols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

### Unlink
unLink <- function (v, sep = ";"){as.numeric (unlist (str_split (v, sep)))}

### Rubins
Rubin <- function (coef, var, alpha = 0.05){
  
  # Formula's based on Rubin's 1981 == method rubin1987 in mice (!):
  # http://fmwww.bc.edu/RePEc/bocode/c/carlin.pdf
  
  # Variances
  m <- length (coef)
  Bmi <- mean (coef) # pooled estimate
  W <- mean (var) # variance
  B <- (1/(m - 1)) * sum ((coef - Bmi)^2) # between imputation variance
  Vmi <- W + ((1+ (1/m)) * B) # total variance
  
  # Statistic:
  t <- Vmi^(-0.5) * Bmi
  df <- (m - 1)*(1 + (W/((1+(1/m))* B)))^2
  
  # Put results in dataframe & return:
  RESULT <- data.frame (EST = Bmi,
                        SE = sqrt (Vmi),
                        EST.lo = Bmi + (sqrt (Vmi) * qt (1-alpha/2, df = df)),
                        EST.up = Bmi - (sqrt (Vmi) * qt (1-alpha/2, df = df)),
                        t = t,
                        df = df,
                        p = 2*pt (t, df = df, lower.tail = if (t < 0) {T} else {F}))
  
  return (RESULT)
}

# Polygon step for KM confidence intervals
polygon.step <- function(x, y1, y2, border=FALSE, ...) {
  nx <- length(x)
  ny <- length(y1)
  if (length(y2)!=ny) stop("y1 and y2 must be the same length")
  if (nx != (ny+1)) stop("x must be one longer than y")
  xx <- c(x[1], rep(x[-c(1,nx)], rep(2,nx-2)), x[nx])
  xxx <- c(xx, rev(xx))
  yy1 <- rep(y1, rep(2,ny))
  yy2 <- rep(y2, rep(2,ny))
  yyy <- c(yy1, rev(yy2))
  polygon(xxx, yyy, border=border, ...)
}

# Calculate ENCALS LP
MyLP <- function (AGE, DISDUR, DXDELAY, SLOPE, VC, ONSET, EE, FTD, C9){
  
  AGE_ONSET <- AGE - (DISDUR/12)
  tAGE <- (AGE_ONSET/100)^-2
  tDXDELAY <- ((DXDELAY/10)^-.5) + log (DXDELAY/10)
  tSLOPE <- ((-SLOPE+0.1)^-.5)
  tFVC <- ((VC/100)^-1) + ((VC/100)^-.5)
  (-1.83665700485301*tSLOPE) +
    (-2.37341404599814*tDXDELAY) +
    (-0.266972062796289*tAGE) +
    (0.476853332498141*tFVC) +
    (0.268874087566745*(ONSET)) +
    (0.232783007891563*(EE)) +
    (0.38831322640700*(FTD)) +
    (0.256078493763062*(C9))
}

# Calculate Kings Stage
K <- function (data){
  ifelse ((data$I10 == 0 | data$I12 < 4) | (is.na (data$I5B) == F),
          4, ifelse (((rowSums (data.matrix (data[, c ("I1", "I2", "I3")])) < 12) +
                        (rowSums (data.matrix (data[, c ("I4", "I5")])) < 8) + (as.numeric (data$I8) < 4)) == 0,
                     1, (rowSums (data.matrix (data[, c ("I1", "I2", "I3")])) < 12) +
                       (rowSums (data.matrix (data[, c ("I4", "I5")])) < 8) + (as.numeric (data$I8) < 4)))
}

## Calculate average sample size of imputed lists
l.nrow <- function (list){mean (sapply (list, function (d){nrow (d)}))}

## Number at risk for time-to-event outcome
At.Risk <- function (data,
                     grp.var = "QTL",
                     time = c (0,3,6,9,12,15),
                     clean = F,
                     stime.var = "STIME",
                     status.var = "STATUS"){
  
  ## Factorize
  data$fSTATUS <- factor (data[, status.var])
  data[, grp.var] <- factor (data[, grp.var])
  
  ## Deaths:
  evts <- sapply (time, function (t.ii){table (data[data[, stime.var] > t.ii, "fSTATUS"],
                                               data[data[, stime.var] > t.ii, grp.var])[2, ]})
  mu <- table (data[data[, status.var] == 1, grp.var])
  evts <- -sweep (evts, 1, t (mu))
  
  ## Censored:
  cens <- sapply (time, function (t.ii){table (data[data[, stime.var] > t.ii, "fSTATUS"],
                                               data[data[, stime.var] > t.ii, grp.var])[1, ]})
  mu <- table (data[data[, status.var] == 0, grp.var])
  cens <- -sweep (cens, 1, t (mu))
  
  ## At risk:
  at.risk <- -sweep ((evts + cens), 1, table (data[, grp.var]))
  
  if (isTRUE(clean)){
    t (sapply (1:nrow (at.risk), function (row.ii){
      paste0 (at.risk[row.ii, ], " (", cens[row.ii, ], ")")
    }))
  } else {
    at.risk
  }
  
}

## Round with fixed decimals
my.round <- function (x, digits){
  x <- round (x, digits)
  sprintf (paste0 ("%.", digits, "f"), x)
}

## Source_file
source_file <- function (file){source (paste0 ("https://raw.githubusercontent.com/rpavaneijk/", file))}
