## Parms:
# date = date at screening
# birth = date of birth
# onset = date of symptom onset
# dx = date of diagnosis
# total = ALSFRS-R total score at screening
# vc = %predicted slow/forced vital capacity at screening (GLI-2021 reference)
# onset = bulbar site of symptom onset (T/F)
# ftd = presence of ALS-FTD at screening (T/F)
# ee = definite el escorial at screening (T/F)

#### SOM comment - just wondering what you do for C9 status? The TRICALS beta profile seems to have slightly different betas
#### than the one I use, is this a version of the model without the C9 status?

#RMCF Comment - I see what you mean, the Lancet paper includes the C9 status and this model 
#doesn't, this is the code we use for selection in clinical trials currently, maybe they dropped
#c9 status recently - either way I would prefer not to use this model because of the breathing 
#being so unstandardised in deciding between fast/slow progression - Thank you for adding your code! 


## Create data from input parameters:
D <- data.frame (AGE_ONSET = as.numeric (as.Date (onset) - as.Date (birth))/365.25,
                 DISDUR = (as.numeric (as.Date (date) - as.Date (onset))/365.25)*12,
                 DXDELAY = (as.numeric (as.Date (dx) - as.Date (onset))/365.25)*12,
                 TOTAL = as.numeric (total),
                 FVC = as.numeric (vc), 
                 ONSET = as.numeric (onset),
                 FTD = as.numeric (ftd),
                 EE = as.numeric (ee))

## Estimate the ALSFRS-R progression rate at screening
D$SLOPE <- (D$TOTAL - 48) / D$DISDUR

## Estimate the non-linear transformations
D$tAGE <- (D$AGE_ONSET/100)^-2
D$tDXDELAY <- ((D$DXDELAY/10)^-.5) + log (D$DXDELAY/10)
D$tSLOPE <- ((-D$SLOPE+0.1)^-.5)
D$tFVC <- ((D$FVC/100)^-1) + ((D$FVC/100)^-.5)

## Calculate the TRICALS Risk Profile
D$LP <- (-1.839*D$tSLOPE) + (-2.376*D$tDXDELAY) + (-0.264*D$tAGE) + (0.474*D$tFVC) +
  (0.271*D$ONSET) + (0.238*D$EE) + (0.415*D$FTD) 

## Eligibility (e.g. range -6.00 [lb] to -2.00[ub]):
if (D$LP >= lb & D$LP <= ub){"ELIGIBLE"}else{"NOT ELIGIBLE"}

##### SOM comment: Some of this code duplicates yours but it extends it with a function to predict survival time
##### I don't mind how we put it together but just adding for now
##### If we want to go down this route I also have code to impute some variables (eg C9 status) and then combine
##### survival probabilities using multiple imputation rules (Rubin's rules)

#Your code looks nicer!

# FUNCTION SURVIVAL TIME BASED ON ENCALS MODEL

encals_beta_function <- function(age, als_frs, diag_delay, el_escorial, onset, ftd, fvc, c9orf72) {
  
  
  ### model covariates
  b1_coef <- -1.837
  b2_coef <- -2.373
  b3_coef <- -0.267
  b4_coef <-  0.477
  b5_coef <-  0.269
  b6_coef <-  0.233
  b7_coef <-  0.388
  b8_coef <-  0.256
  
  ### transformations
  ## ALSFRSR SLOPE - if als-frs isn't already rate of decline
  #alsfrsr_slope <- (48-als_frs)/diag_delay
  
  b1_trans <- (als_frs+0.1)^-0.5
  b2_trans <- ((diag_delay/10)^-0.5)+log(diag_delay/10)
  b3_trans <- (age/100)^-2
  b4_trans <- ((fvc/100)^-1)+((fvc/100)^-0.5)
  
  ##### get total value of beta
  b1 <- b1_coef*b1_trans
  b2 <- b2_coef*b2_trans
  b3 <- b3_coef*b3_trans
  b4 <- b4_coef*b4_trans
  b5 <- b5_coef*onset
  b6 <- b6_coef*el_escorial
  b7 <- b7_coef*ftd
  b8 <- b8_coef*c9orf72
  
  total_b = b1+b2+b3+b4+b5+b6+b7+b8
  
  return (total_b)
  
}


encals_survival_function <- function(encals_beta){
  
  if(is.na(encals_beta)){
    encals_survival =NA
  }  else {
    ### spline model
    ## spline coefficients
    y0 <- -6.409
    y1 <-  2.643
    y2 <- -0.546
    y3 <-  0.585
    
    ## knot positions
    kmin <- 0.081
    k1   <- 3.108
    k2   <- 3.638
    kmax <- 5.917
    
    ## lambdas
    lambda_1 <- 0.481323
    lambda_2 <- 0.390507
    
    time <- 1
    survival_probability <- 1
    survival_probabilities <- data.frame(time=numeric(),surv_prob=numeric())
    
    
    while (survival_probability > 0.2) {
      
      log_time <- log(time)
      y1z1 <- y1*log_time
      y2z2 <- y2*((max((log_time-k1)^3,0))-lambda_1*(max((log_time-kmin)^3,0))-(1-lambda_1)*(max((log_time-kmax),0)))
      y3z3 <- y3*(max((log_time-k2)^3,0)-lambda_2*max((log_time-kmin)^3,0)-(1-lambda_2)*max((log_time-kmax),0))
      sx <- y0 + y1z1 + y2z2 + y3z3
      
      cumulative_log_odds <- exp(sx + encals_beta)
      
      survival_probability <- (1+cumulative_log_odds)^-1
      
      temp<-data.frame(time=numeric(),surv_prob=numeric())
      temp[1,1]<-time
      temp[1,2]<-survival_probability
      
      survival_probabilities<-rbind(temp,survival_probabilities)
      
      time <-  time+1
    } ## end while loop
    
    ### pick closest to randomly generated survival probability
    ### can set generated prob as median if preferable
    generated_prob <- rtruncnorm(n=1, a=0, b=1, mean=0.5, sd=0.1)
    median_survival <- survival_probabilities %>% 
      arrange(abs(surv_prob - generated_prob)) %>%
      slice(1)
    
    encals_survival <- median_survival$time
    return(encals_survival)
  }## end if encals survival is na loop
  
}
