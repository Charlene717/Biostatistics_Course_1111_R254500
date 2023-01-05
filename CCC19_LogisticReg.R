##### Presetting ######
rm(list = ls()) # Clean variable
memory.limit(150000)
# options(stringsAsFactors = FALSE)
# Sys.setlocale(category = "LC_ALL", locale = "UTF-8")

##### Load Packages #####
source("FUN_Package_InstLoad.R")
FUN_Basic.set <- c("tidyverse","Hmisc","rms","MatchIt","dplyr","survey","tableone",
                   "rvest")

FUN_Package_InstLoad(Basic.set = FUN_Basic.set, BiocManager.set = FUN_BiocManager.set)
rm(FUN_Basic.set)


# require(devtools)
# install_version("ipw", version = "1.0-11", repos = "http://cran.us.r-project.org")
library(ipw)

##### Function setting #####
## Prepossession
source("CCC19_CombineReport.R")
source("FUN_Beautify_ggplot.R")


#### Current path and new folder setting* ####
Save.Path = paste0(getwd(),"/",Sys.Date(),"_","CCC19Results")

## Create new folder
if (!dir.exists(Save.Path)){dir.create(Save.Path)}


##### Load datasets  #####
load("D:/Dropbox/##_GitHub/##_Charlene/Biostatistics_Course_1111_R254500/Data/CCC19_CancerDiscover_data.RData")
summary(ccc19_lc)


##### Data preprocessing  #####
## Set Negative control as the reference group
ccc19_lc$treatment_hcq <- factor(ccc19_lc$treatment_hcq, # Hydroxychloroquine (HCQ)
                                 levels=c("Negative control", "Positive control", "HCQ alone", "HCQ+anything"))
ccc19_lc$treatment_rem <- factor(ccc19_lc$treatment_rem, # Remdesivir (Rem)
                                 levels=c("Negative control", "Positive control", "Rem alone", "Rem+anything"))
ccc19_lc$treatment_cs <- factor(ccc19_lc$treatment_cs, # Corticosteroid (CS)
                                levels=c("Negative control", "Positive control", "Steroids alone", "Steroids+anything"))


##### Descriptive statistics #####
## Descriptive statistics, according to treatment_rem
output0 <- summaryM(dead30 + age_cph + sex + race + region + smoking2 +
                    obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                    cancer_status_v2 + severity_of_covid_19_v2 + ac_apa
                    ~ treatment_rem,
                    #~ 1,
                    data = ccc19_lc, test = F, overall = F, na.include=T)

## Export txt
sink(paste0(Save.Path,"/CCC19_Descr_Stats_Table1.txt"))
 print(output0, long=TRUE, what = "%")
sink()

## Plot
plot(output0)
plot(output0[["results"]][[".ALL."]][["stats"]][["age_cph"]]) # plot(output0$results$.ALL.$stats$age_cph)
plot(output0[["results"]][[".ALL."]][["stats"]][["sex"]])

#### COnvert  summaryM result to dataframe ####
## Ref: https://stackoverflow.com/questions/32400916/convert-html-tables-to-r-data-frame
library(rvest)
output0.df <- as.data.frame(read_html(html(output0)) %>% html_table(fill=TRUE))

# ## ggPlot
# ## Ref: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
#
# sex.df <- output0[["results"]][[".ALL."]][["stats"]][["sex"]] %>% as.data.frame()
# colnames(sex.df) <- c("Sex","Group","Number")
# sex.df$Group <- gsub("Positive", "Pos", sex.df$Group)
# sex.df$Group <- gsub("Negative", "Neg", sex.df$Group)
#
# ## Ref: https://cloud.tencent.com/developer/ask/sof/103120
# ## Add percent
# sex.df <- left_join(sex.df,sex.df %>% group_by(Group) %>% summarise(sum(Number)))
# sex.df$Percent <-  sprintf("%0.2f", sex.df$Number/sex.df$`sum(Number)`*100 %>% as.numeric())
#
# ## print sex
# library(ggplot2)
# Plt.SexDS <- ggplot(data=sex.df, aes(x = Group, y = Number, fill = sex.df[,1])) +
#                     geom_bar(stat="identity", position=position_dodge(),alpha=.7)
# Plt.SexDS
# Plt.SexDS %>% FUN_Beautify_ggplot +
#               scale_fill_manual("Sex",values=c("#bf54a3","#5b46a3","#41548a","#45856c","#747575")) +
#               geom_text(aes(label=Number),position = position_dodge(0.95), vjust=-0.6, size=3.5,angle =45, colour="#c70e67") +
#               geom_text(aes(label=paste0(Percent,"%")),position = position_dodge(0.9), vjust=0.5, size=2,colour = "#2a2e2d",angle =45) -> Plt.SexDS_B
# Plt.SexDS_B
#
#
# ## Export pdf
# pdf(file = paste0(Save.Path,"/CCC19_Descr_Stats_Barplot_sex.pdf"),width = 7, height = 7 )
#   Plt.SexDS_B
# dev.off()
#

## loop for ggPlot
pdf(file = paste0(Save.Path,"/CCC19_Descr_Stats_Barplot.pdf"),width = 7, height = 7 )

  Var.set <- output0[["results"]][[".ALL."]][["stats"]] %>% names()
  Var.set <- Var.set[-2]
  for (i in 1:length(Var.set)) {


  ## ggPlot
  ## Ref: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization

  sex.df <- output0[["results"]][[".ALL."]][["stats"]][[Var.set[i]]] %>% as.data.frame()
  colnames(sex.df) <- c(Var.set[i],"Group","Number")
  sex.df$Group <- gsub("Positive", "Pos", sex.df$Group)
  sex.df$Group <- gsub("Negative", "Neg", sex.df$Group)

  ## Ref: https://cloud.tencent.com/developer/ask/sof/103120
  ## Add percent
  sex.df <- left_join(sex.df,sex.df %>% group_by(Group) %>% summarise(sum(Number)))
  sex.df$Percent <-  sprintf("%0.2f", sex.df$Number/sex.df$`sum(Number)`*100 %>% as.numeric())

  ## print sex
  library(ggplot2)
  Plt.SexDS <- ggplot(data=sex.df, aes(x = Group, y = Number, fill = sex.df[,1])) +
    geom_bar(stat="identity", position=position_dodge(),alpha=.7)
  Plt.SexDS
  Plt.SexDS %>% FUN_Beautify_ggplot +
    scale_fill_manual(Var.set[i],values=c("#bf54a3","#5b46a3","#41548a","#45856c","#747575")) +
    geom_text(aes(label=Number),position = position_dodge(0.95), vjust=-0.6, size=3.5,angle =45, colour="#c70e67") +
    geom_text(aes(label=paste0(Percent,"%")),position = position_dodge(0.9), vjust=0.5, size=2,colour = "#2a2e2d",angle =45) -> Plt.SexDS_B
  Plt.SexDS_B %>% print()
  }
dev.off()
# ## Export pdf
# pdf(file = paste0(Save.Path,"/CCC19_Descr_Stats_Barplot_",Var.set[i],".pdf"),width = 7, height = 7 )
# Plt.SexDS_B
# dev.off()






##### Impute missing values #####
set.seed(42)
Imp.data0 <- aregImpute(~ dead30 + age_cph + sex + race + region + smoking2 +
                          obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
                          cancer_status_v2 + severity_of_covid_19_v2 + ac_apa +
                          treatment_hcq + treatment_rem + treatment_cs,
                          data = ccc19_lc, n.impute = 10)

### Before balancing covariate distributions
fmi <- fit.mult.impute(dead30 ~ treatment_rem + age_cph + sex + race + region + smoking2 +
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
  full_dat1$hcq.2 <- factor(full_dat1$treatment_rem,
                                levels=c("Negative control", "Positive control", "Rem alone", "Rem+anything"),
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
  # with(dta_m, tapply(distance , list(treatment_rem), summary))
  # PS.4density(dta_m, treatment = "treatment_rem", ylim = c(0,6)) # after matching
  # full_dat1$distance <- fitted(fit0 <- glm(formula = formula.match, data = full_dat1, family = "binomial"))
  # PS.4density(full_dat1, treatment = "treatment_rem", main = "Before matching") # before matching

  ## Create Table 1 after PSM
  #output1 <- summaryM(age_cph + sex + race + region + smoking2 +
  #                    obesitylevel + dm2 + pulm + card + renal + htn + ecogcat2 +
  #                    cancer_status_v2 + severity_of_covid_19_v2 + ac_apa
  #                    ~ treatment_rem,
  #                    data = dta_m, overall = F)
  #sink(paste0("Table1_afterPSM.txt"))
  #  print(output1, long=TRUE, what = "%")
  #sink()

  # A formula for logistic regression analysis
  formula1 <- dead30 ~ treatment_rem + age_cph + sex + race + region + smoking2 +
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



