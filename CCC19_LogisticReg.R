
library(Hmisc)
library(rms)
library(MatchIt)
library(dplyr)
library(ipw)
library(survey)
library(tableone)

source("CCC19_CombineReport.R")


load("../Data/CCC19_CancerDiscover_data.RData")
summary(ccc19_lc)


## Set Negative control as the reference group
ccc19_lc$treatment_hcq <- factor(ccc19_lc$treatment_hcq,
                                 levels=c("Negative control", "Positive control", "HCQ alone", "HCQ+anything"))
ccc19_lc$treatment_rem <- factor(ccc19_lc$treatment_rem,
                                 levels=c("Negative control", "Positive control", "Rem alone", "Rem+anything"))
ccc19_lc$treatment_cs <- factor(ccc19_lc$treatment_cs,
                                levels=c("Negative control", "Positive control", "Steroids alone", "Steroids+anything"))


## Descriptive statistics, according to treatment_hcq
output0 <- summaryM(dead30 + age_cph + sex + race + region + smoking2 +
                                 obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                                 cancer_status_v2 + severity_of_covid_19_v2 + ac_apa
                                 ~ treatment_hcq,
                                 #~ 1,
                                 data = ccc19_lc, test = F, overall = F, na.include=T)
#sink(paste0("Table1.txt"))
#  print(output0, long=TRUE, what = "%")
#sink()




## Impute missing values
set.seed(42)
Imp.data0 <- aregImpute(~ dead30 + age_cph + sex + race + region + smoking2 +
                          obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                          cancer_status_v2 + severity_of_covid_19_v2 + ac_apa +
                          treatment_hcq + treatment_rem + treatment_cs,
                          data = ccc19_lc, n.impute = 10)

### Before balancing covariate distributions
fmi <- fit.mult.impute(dead30 ~ treatment_hcq + age_cph + sex + race + region + smoking2 +
                         obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                         cancer_status_v2 + severity_of_covid_19_v2 + 
                         ac_apa, fitter = lrm, xtrans = Imp.data0, data = ccc19_lc)
round(coefs.to.OR(fmi)$ORCI, 3)





## A function to form completed data by combining imputed data and non-missing data
fill_data <- function(impute = Imp.data0 , data = ccc19_lc, im) {
  cbind.data.frame(impute.transcan(x = impute,
                                   imputation = im,
                                   data = data,
                                   list.out = TRUE,
                                   pr = FALSE))
}
## Number of imputations
Imputations <- Imp.data0$n.impute


## PS matching + logistic regression analysis
models.PSM.hcq <- lapply(1:Imputations, function(im1) {
  #im1 <- 5
  full_dat1 <- fill_data(im = im1) # form complete data
  
  full_dat1$age_cph <- full_dat1$age_cph/10 # one unit, per 10 years

  ## Create a dichotomous treatment variable for matching
  full_dat1$hcq.2 <- factor(full_dat1$treatment_hcq,
                                levels=c("Negative control", "Positive control", "HCQ alone", "HCQ+anything"),
                                labels = c("0", "0", "1", "1"))
  

  ## Start matching
  set.seed(42)
  formula.match <- hcq.2 ~ age_cph + sex + race + region + smoking2 +
                            obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                            cancer_status_v2 + severity_of_covid_19_v2 + ac_apa
  match.it <- do.call(matchit, list(formula = formula.match, data = full_dat1, method = "nearest",
                                    ratio=2, caliper = 0.3, replace = F))   
  # match.it$nn
  # plot(match.it, type = "jitter", interactive = F)
  # plot(match.it, type = "histogram", interactive = F)
  
  ## Data after matching
  dta_m <- match.data(match.it)
  # round(summary(match.it, standardize = T)$sum.matched,3)
  # with(dta_m, tapply(distance , list(treatment_hcq), summary))
  # PS.4density(dta_m, treatment = "treatment_hcq", ylim = c(0,6)) # after matching
  # full_dat1$distance <- fitted(fit0 <- glm(formula = formula.match, data = full_dat1, family = "binomial"))
  # PS.4density(full_dat1, treatment = "treatment_hcq", main = "Before matching") # before matching

  ## Create Table 1 after PSM
  #output1 <- summaryM(age_cph + sex + race + region + smoking2 +
  #                      obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
  #                      cancer_status_v2 + severity_of_covid_19_v2 + ac_apa
  #                      ~ treatment_hcq,
  #                      data = dta_m, overall = F)
  #sink(paste0("Table1_afterPSM.txt"))
  #  print(output1, long=TRUE, what = "%")
  #sink()

  # A formula for logistic regression analysis
  formula1 <- dead30 ~ treatment_hcq + age_cph + sex + race + region + smoking2 +
    obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
    cancer_status_v2 + severity_of_covid_19_v2 + 
    ac_apa


  fit1 <- glm(formula = formula1, data = dta_m, family = "binomial", weights = dta_m$weights)
  fit1 # sandwich::vcovCL(fit1, cluster = ~ subclass) # robust variance

})


RRR <- CombineImputation(models.PSM.hcq, vcov.=vcovCL, cluster = ~ subclass)

round(coefs.to.OR(RRR$fmi)$ORCI,3)







## IPTW (Inverse Probability Treatment Weighting) + logistic regression analysis
models.IPTW.hcq <- lapply(1:Imputations, function(im1) {
  #im1 <- 5
  full_dat1 <- fill_data(im = im1) # form complete data
  
  full_dat1$age_cph <- full_dat1$age_cph/10 # one unit, per 10 years
  
  
  temp <- ipwpoint(exposure = treatment_hcq,
                   family = "multinomial",
                   numerator = ~ 1, ## must exist
                   denominator = ~ age_cph + sex + race + region + smoking2 +
                     obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                     cancer_status_v2 + severity_of_covid_19_v2 + ac_apa,
                   data = full_dat1)
  full_dat1$ipws <- temp$ipw.weights
  
  # hist(full_dat1$weights)
  Qs <- quantile(full_dat1$ipws, probs = c(0.01, 0.99))
  full_dat1$ipws <- ifelse(full_dat1$ipws > Qs[2], Qs[2], full_dat1$ipws)
  full_dat1$ipws <- ifelse(full_dat1$ipws < Qs[1], Qs[1], full_dat1$ipws)
  
  #weighteddata <- svydesign(ids = ~ 1, weights = ~ ipws, data = full_dat1)
  #weightedtable <- svyCreateTableOne(vars = colnames(full_dat1)[-c(1, 17:20)], 
  #                                   strata = "treatment_hcq", data = weighteddata, test=FALSE)
  #sink(paste0("Table1_postIPTW_one.txt"))
  #print(weightedtable, smd=TRUE)
  #sink()

  
  # A formula for logistic regression analysis
  formula1 <- dead30 ~ treatment_hcq + age_cph + sex + race + region + smoking2 +
    obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
    cancer_status_v2 + severity_of_covid_19_v2 + 
    ac_apa
  
  fit.bin <- svyglm(formula1, 
                    design = svydesign(~ 1, weights = ~ ipws, data = full_dat1), family = "binomial")
  fit.bin
})

RRR <- CombineImputation(models.IPTW.hcq)

round(coefs.to.OR(RRR$fmi)$ORCI,3)


