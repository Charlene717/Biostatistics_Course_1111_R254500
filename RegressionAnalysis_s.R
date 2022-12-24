

#### Please replace ../Data/SimData2.csv by your path.
#### Data loadings
SimData2 <- read.csv('../Data/SimData2.csv', header=T, stringsAsFactors = T)
str(SimData2)
head(SimData2, 10)


#### Descriptive Statistics: Hmisc::summaryM
library(Hmisc)
#output 1
summaryM(y + x1 + x2 + x3 + x4 ~ 1, data = SimData2, test = F, overall = F, na.include=T)
#output 2
summaryM(y + x2 + x3 + x4 ~ x1, data = SimData2, test = F, overall = F, na.include=T)


#### SimData1
SimData1 <- read.csv('../Data/SimData1.csv', header=T)
str(SimData1)
head(SimData1,10)

#### Model fitting: continuous outcome
fit1 <- lm(y ~ x1 + x2 + x3, data=SimData1)
summary(fit1)

#### Model diagnostics
plot(fit1, which=1)
plot(fit1, which=2)

#### SimData2
SimData2 <- read.csv('../Data/SimData2.csv', header=T, stringsAsFactors = T)
fit2 <- lm(y ~ x1 + x2 + x3, data=SimData2)
plot(fit2, which=1)
plot(fit2, which=2)




#### Binary outcome
BinOutcome <- read.csv('../Data/BinOutcome.csv', header=T, stringsAsFactors = T)
str(BinOutcome); head(BinOutcome, 10)

BinOutcome$diabetes <- factor(BinOutcome$diabetes)

fit3 <- glm(diabetes ~ age + glucose + obesity + insulin, data = BinOutcome, family = "binomial")
summary(fit3)


#### Confidence Interval for OR
coeftable <- coef(summary(fit3))
CI_OR <- exp(cbind(Est=coeftable[,"Estimate"],
                         CI.L=coeftable[,"Estimate"] - 1.96 * coeftable[,"Std. Error"],
                         CI.U=coeftable[,"Estimate"] + 1.96 * coeftable[,"Std. Error"]))
round(CI_OR, 3)

# or use R command
CI_OR2 <- exp(cbind(OR=coef(fit3), confint(fit3)))
round(CI_OR2 , 3)

# Model diagnostics: goodness of fit [Hosmer-Lemeshow tests]
library(generalhoslem)
logitgof(BinOutcome$diabetes[-c(fit3$na.action)], fitted(fit3))
#same as logitgof(fit3$y, fitted(fit3))

#library(PredictABEL)
#dataPreImp3 <- BinOutcome
#dataPreImp3$diabetes.num <- as.numeric(dataPreImp3$diabetes) - 1
#pcb <- plotCalibration(data=dataPreImp3[-c(fit3$na.action),], cOutcome=11, predRisk=predRisk(fit3),
#                       groups=10, rangeaxis=c(0,0.2)) ## 10 groups by predRisk

            
#### Ordinal outcome
OrdOutcome <- read.csv('../Data/OrdOutcome.csv', header=T, stringsAsFactors = T)
str(OrdOutcome)

levels(OrdOutcome$apply)  # "somewhat likely"   "unlikely"   "very likely“
OrdOutcome$apply <- ordered(OrdOutcome$apply, levels = c("unlikely", "somewhat likely", "very likely"))
library(MASS)
fit4 <- polr(apply ~ pared + public + gpa, data = OrdOutcome, Hess=T)
summary(fit4)

#library(rms)
#dd <- datadist(OrdOutcome)
#options(datadist='dd')
#lrm(apply ~ pared + public + gpa, data = OrdOutcome)

#### Confidence Interval for OR
coeftable <- coef(summary(fit4))
pvalue <- 2 * pnorm(abs(coeftable[, "t value"]), lower.tail = FALSE)
cbind(coeftable, "p_value" = pvalue)

CI_OR <- exp(cbind(Est=coeftable[,"Value"],
                   CI.L=coeftable[,"Value"] - 1.96 * coeftable[,"Std. Error"],
                   CI.U=coeftable[,"Value"] + 1.96 * coeftable[,"Std. Error"]))
round(CI_OR, 3)

# or use R command
CI_OR2 <- exp(cbind(OR=coef(fit4), confint(fit4)))
round(CI_OR2 , 3)

# Model diagnostics: goodness of fit [Hosmer-Lemeshow tests]
library(generalhoslem)
logitgof(OrdOutcome$apply, fitted(fit4), ord = TRUE)
#logitgof(OrdOutcome$apply[-c(fit4$na.action)], fitted(fit4), ord = TRUE) if NA exists





#### Survival outcome
SurOutcome <- read.csv('../Data/SurOutcome.csv', header=T, stringsAsFactors = T)
str(SurOutcome)
head(SurOutcome, 10)

SurOutcome$sex <- factor(SurOutcome$sex)
SurOutcome$ph.ecog <- factor(SurOutcome$ph.ecog)
str(SurOutcome)


#### Cox proportional hazards model
library(survival)
fit5 <- coxph(Surv(time, status) ~ age + sex + ph.ecog + ph.karno, data =  SurOutcome)
summary(fit5)


#### K-M curves
KM_fit <- survfit(Surv(time, status) ~ sex, data = SurOutcome)
plot(KM_fit, 
     col = c("red","blue"),  # color
     conf.int = F,                  # confidence interval
     mark.time = T,             # mark censoring
     main="",                      # title of figure
     ylab = "Survival probability", 
     xlab = "Days", 
     xlim = c(0,1000),        # range of x-axis
     ylim = c(0,1),              # range of y-axis
     cex.lab = 1.5,             # font size on lab
     cex.axis = 1.5,            # font size on axis
     lwd = 2)                      # width of line
abline(h=0, col="gray")   # horizontal line
abline(v=0, col="gray")   # vertical line
legend("topright", 
       legend = c("Sex = 1", "Sex = 2"), 
       col = c("red","blue"),  # color of line
       cex = 1.5,                      # font size 
       lwd = 2)                        # width of line


# logrank test
survdiff(Surv(time, status) ~ sex, data = SurOutcome, rho=0)


#### Missing Values
BinOutcome <- read.csv('../Data/BinOutcome.csv', header=T, stringsAsFactors = T)
head(BinOutcome, 20)


####
BinOutcome <- read.csv('../Data/BinOutcome.csv', header=T, stringsAsFactors = T)

variablenames <- colnames(BinOutcome)
formula1 <- formula(paste("~", paste(variablenames, collapse=" + ")  ))
# ~pregnant + glucose + pressure + triceps + insulin + mass + pedigree + ...

set.seed(2020)
Imp.data1.all <- aregImpute(formula1, data = BinOutcome, n.impute = 5)    # 5 complete datasets

im1 <- 1    # show the first complete dataset
completedata <- cbind.data.frame(impute.transcan(x = Imp.data1.all, imputation = im1,
                                                 data = BinOutcome, list.out = TRUE, pr = FALSE))


#### Before and after imputation
head(BinOutcome, 10)

head(completedata, 10)


#### Model fitting before and after imputation
fit3 <- glm(diabetes ~ age + glucose + obesity + insulin + 
              pedigree + triceps + pregnant, data = BinOutcome, family = "binomial")
summary(fit3)

fit3_impute1 <- glm(diabetes ~ age + glucose + obesity + insulin + 
                      pedigree + triceps + pregnant, data = completedata, family = "binomial")
summary(fit3_impute1)


#### Model fitting with multiple imputation
library(rms)  # rms::lrm
fit3_allimpute <- fit.mult.impute(diabetes ~ age + glucose + obesity + insulin + 
                                    pedigree +  triceps + pregnant, 
                                  fitter = lrm, xtrans = Imp.data1.all, data = BinOutcome)
fit3_allimpute


#### Binary outcome: Complete Data is Necessary
BinOutcome <- read.csv('../Data/BinOutcome.csv', header=T, stringsAsFactors = T)
variablenames <- colnames(BinOutcome)
formula1 <- formula(paste("~", paste(variablenames, collapse=" + ")  ))

library(Hmisc)
set.seed(2020)
Imp.data1.all <- aregImpute(formula1, data = BinOutcome, n.impute = 5)
im1 <- 1
completedata <- cbind.data.frame(impute.transcan(x = Imp.data1.all, imputation = im1,
                                                 data = BinOutcome, list.out = TRUE, pr = FALSE))


#### R code for glmnet on binary outcome
library(glmnet)  # Note: complete data is necessary
X <- model.matrix(~pregnant+glucose+pressure+triceps+insulin+mass+pedigree+age+obesity, completedata)[,-1]
y <- completedata$diabetes

fit.lasso <- glmnet(X, y, alpha = 1, family="binomial")   # LASSO
coef(fit.lasso)
plot(fit.lasso, xvar="lambda")


#### R code for lambda choice
set.seed(2020)
model <- cv.glmnet(X, y, alpha = 1, family="binomial", type.measure = "deviance") #auc #class
model
plot(model) # CV plot for lambda

# min: value of lambda that gives minimum cvm
# 1se: largest value of lambda such that error is within 1 standard error of the minimum

####
coef(fit.lasso , s = model$lambda.1se)
coef(fit.lasso , s = min(model$lambda.min))





#### Survival outcome: Complete Data is Necessary
SurOutcome <- read.csv('../Data/SurOutcome.csv', header=T, stringsAsFactors = T)
str(SurOutcome)
head(SurOutcome)


SurOutcome$sex <- factor(SurOutcome$sex)
SurOutcome$ph.ecog <- factor(SurOutcome$ph.ecog)


SurOutcome$ph.ecog[SurOutcome$ph.ecog==3] <- 2
SurOutcome$ph.ecog <- droplevels(SurOutcome$ph.ecog)

variablenames <- colnames(SurOutcome)
formula2 <- formula(paste("~", paste(variablenames, collapse=" + ")  ))
set.seed(2021)
Imp.data1.all2 <- aregImpute(formula2, data = SurOutcome, n.impute = 5)
im1 <- 1
completedata2 <- cbind.data.frame(impute.transcan(x = Imp.data1.all2, imputation = im1,
                                                  data = SurOutcome, list.out = TRUE, pr = FALSE))


#### R code for glmnet on survival outcome
library(glmnet)  # Note: complete data is necessary
X <- model.matrix(~inst+age+sex+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, completedata2)[,-1]
y <- cbind(time= completedata2$time, status=completedata2$status)  
# y must be a matrix and y’s names must be time and status

fit.lasso2 <- glmnet(X, y, alpha = 1, family="cox")  # LASSO
coef(fit.lasso2)
plot(fit.lasso2, xvar="lambda")

#### R code for lambda choice
set.seed(2022)
model.cox <- cv.glmnet(X, y, alpha = 1, family="cox")
model.cox
plot(model.cox) # CV plot for lambda

####
coef(fit.lasso2, s = model.cox$lambda.1se)
coef(fit.lasso2, s = min(model.cox$lambda.min))


