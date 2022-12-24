


require("sandwich")

## vcov. = c("vcov", "vcovHC", "vcovCL")
CombineImputation <- function(models.fit, vcov.=vcov, ...) {
  
  ## Combine the results obtatined from every imputation
  Imputations <- length(models.fit)
  match.size <- sapply(models.fit, function(x) length(x$fitted.values))
  weight.impute <- match.size/sum(match.size)
  
  coef.all <- sapply(models.fit, function(x) x$coefficients)
  coef.bar <- rowSums(t(t(coef.all) * weight.impute)) # weighted average of coefficient estimates
  
  V.between <- cov.wt(t(coef.all), wt = weight.impute)$cov # weighted covariance of coefficient estimates between imputations
  V.bar <- matrix(rowSums(sapply(1:length(weight.impute), function(k) {
    vcov.(models.fit[[k]],...) * weight.impute[k]
  })), length(coef.bar), length(coef.bar)) # weighted average of covariance of coefficient estimates
  V.impute <- V.bar + (1+1/Imputations)*V.between # overall covariance of coefficient estimates
  
  S.E <- sqrt(diag(V.impute))
  Wald_Z <- coef.bar/S.E
  Pvalue <- 2*(1 - pnorm(abs(Wald_Z)))
  
  results <- round(cbind(Coef=coef.bar, S.E=S.E, Wald_Z=Wald_Z, Pvalue=Pvalue), 5)
  
  list(results = results, fmi=list(coefficients = coef.bar, var = V.impute))
  #RRR$results
}


## A function to transform coefficients in logistic regression model into AOR
coefs.to.OR <- function(fmi) {
  pvalue <- 2 * (1 - pnorm(abs(fmi$coefficients)/sqrt(diag(fmi$var))))
  SE <- sqrt(diag(fmi$var))
  ORSE <- SE * exp(fmi$coefficients)
  tt <- rbind(fmi$coefficients - 1.96 * sqrt(diag(fmi$var)),
              fmi$coefficients,
              fmi$coefficients + 1.96 * sqrt(diag(fmi$var)))
  CI <- t( rbind(tt, pvalue, SE) )
  ORCI <- t( rbind(exp(tt), pvalue, ORSE) )
  colnames(ORCI) <- colnames(CI) <- c("CI95L", "OR", "CI95U", "Pvalue", "OR.SE")
  list(CI = round(CI[,c(2,1,3,4,5)], 5), ORCI = round(ORCI[,c(2,1,3,4,5)], 5))
}



PS.4density <- function(dta_m, treatment, main = "After matching", ylim = NULL) {
  
  level.names <- levels(dta_m[,treatment])
  
  par(mfrow = c(1,1), mar = c(5,5,3,3))
  
  plot(density( dta_m$distance[which(dta_m[,treatment]==level.names[1])] ), 
       main=main, xlab="Propensity score", ylim=ylim, 
       lwd=2, cex.lab=1.5, cex.axis=1.5)
  for(i in 2:4) {
    lines(density( dta_m$distance[which(dta_m[,treatment]==level.names[i])] ), col = i, lwd=2)
  }
  
  legend("topright", level.names,
         col = c(1:4), lty = c(1), bty="n", lwd=1.5, cex = 1.5)
  
}



specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


