library(glmnet)


#### elastic-net
elastic.PSM.rem <- lapply(1:Imputations, function(im1) {
  #im1 <- 1
  full_dat1 <- fill_data(im = im1) # complete data

  ## Create a dichotomous treatment variable
  full_dat1$rem.2 <- factor(full_dat1$treatment_rem,
                            levels=c("Negative control", "Positive control", "REM alone", "REM+anything"),
                            labels = c("0", "0", "1", "1"))

  ## Start matching
  set.seed(42)
  formula.match <- rem.2 ~ age_cph + sex + race + region + smoking2 +
    obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
    cancer_status_v2 + severity_of_covid_19_v2 + ac_apa
  match.it <- do.call(matchit, list(formula = formula.match, data = full_dat1, method = "nearest",
                                    ratio=2, caliper = 0.3, replace = F))

  ## Data after matching
  dta_m <- match.data(match.it)

  ## Elastic net variable selection for binary outcome
  variablenames <- names(dta_m)[-c(1,18:23)]
  formula.x <- formula(paste("~", paste(variablenames, collapse=" + ")))
  X <- model.matrix(formula.x, dta_m)[,-1]
  y <- dta_m$dead30

  ## Using cross validation folds to select lambda.
  cv <- cv.glmnet(x=X, y=y, family = "binomial", weights = dta_m$weights, alpha = 0.5) ## alpha = 1, LASSO; = 0, ridge

  #coefs <- coef(cv, s=cv$lambda.min)
  coefs <- coef(cv, s=cv$lambda.1se)
  fre.variables <- names(coefs[which(coefs[,1]!=0),1])
  fre.variables

  #coef(cv, s=cv$lambda[1:5])

})
V1 <- table(unlist(elastic.PSM.rem))
names(V1[V1>=9])[-1] # ignore intercept

#[1] "age_cph"                       "ecogcat22+"                    "severity_of_covid_19_v2Severe" "treatment_remREM+anything"






elastic.IPTW.rem <- lapply(1:Imputations, function(im1) {
  #im1 <- 1
  full_dat1 <- fill_data(im = im1) # complete data

  full_dat1$age_cph <- full_dat1$age_cph/10 # one unit, per 10 years

  temp <- ipwpoint(exposure = treatment_rem,
                   family = "multinomial",
                   numerator = ~ 1, ## must exist
                   denominator = ~ age_cph + sex + race + region + smoking2 +
                     obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                     cancer_status_v2 + severity_of_covid_19_v2 + ac_apa,
                   data = full_dat1)

  full_dat1$ipws <- temp$ipw.weights
  Qs <- quantile(full_dat1$ipws, probs = c(0.01, 0.99))
  full_dat1$ipws <- ifelse(full_dat1$ipws > Qs[2], Qs[2], full_dat1$ipws)
  full_dat1$ipws <- ifelse(full_dat1$ipws < Qs[1], Qs[1], full_dat1$ipws)


  ## Elastic net variable selection for binary outcome
  variablenames <- names(full_dat1)[-c(1,18:20)]
  formula.x <- formula(paste("~", paste(variablenames, collapse=" + ")))
  X <- model.matrix(formula.x, full_dat1)[,-1]
  y <- full_dat1$dead30

  ## Using cross validation folds to select lambda.
  cv <- cv.glmnet(x=X, y=y, family = "binomial", weights = full_dat1$ipws, alpha = 0.5) ## alpha = 1, LASSO; = 0, ridge

  coefs <- coef(cv, s=cv$lambda.min)
  # coefs <- coef(cv, s=cv$lambda.1se)
  fre.variables <- names(coefs[which(coefs[,1]!=0),1])
  fre.variables

  #coef(cv, s=cv$lambda[1:5])

})
V1 <- table(unlist(elastic.IPTW.rem))
names(V1[V1>=9])[-1] # ignore intercept

#[1] "age_cph"                             "cancer_status_v2Active, progressing" "cancer_status_v2Unknown"
#[4] "ecogcat22+"                          "htn1"                                "renal1"
#[7] "severity_of_covid_19_v2Moderate"     "severity_of_covid_19_v2Severe"       "sexMale"
#[10] "treatment_remREM+anything"
