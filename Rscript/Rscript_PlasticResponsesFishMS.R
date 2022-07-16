#########################################################################################################################################
# This is the main R script to run the various analyses performed in                                                                    # 
# title: "'Adaptive plastic responses to metal contamination in a multistress context: a field experiment in fish'"                     #
# author:  "Quentin PETITJEAN[q.petitjean1@gmail.com], Pascal LAFFAILLE, Annie PERRAULT, Myriam COUSSEAU, Séverine JEAN, Lisa JACQUIN"  #                                                                                        #
# date: "25/06/2022"                                                                                                                    #
#########################################################################################################################################


#' # Load libraries 
## ----libraries, eval = FALSE-------------------------------------------------------------------------------------------
 packageList <- c("lme4", # To build linear mixed effect model with binomial family
                  "car", # To perform logit and power transformations and obtain p-value from linear mixed effect model
                  "MuMIn", # To perform automated model selection based on AIC and compute conditional and marginal R square for mixed effect model
                  "bimixt", # To transform data using bimixt::boxcox transformation (power transformation)
                  "dplyr", # To manage the database (e.g., left_join, count)
                  "tidyr", # To manage the database (e.g., separate)
                  "performance", # check model performances
                  "kableExtra") # To draw table
 for(i in packageList){
   if(require(i, character.only = TRUE)){
   install.packages(i)
 }
   }
 for(i in packageList){
 library(i, character.only = TRUE)
 }
 
#' # Load Some custom functions (making tables and drawing plots)
## ----VariousFunc, eval = FALSE-----------------------------------------------------------------------------------------
# load some custom functions used to draw the plots included within the MS
source("https://raw.githubusercontent.com/qpetitjean/Adaptive-plastic-responses-of-fish-in-a-multistress-context/main/Rscript/SourcePlots_PlasticResponsesFishMS.R")
 
# load some custom functions used to draw the statistic tables included within the MS
source("https://raw.githubusercontent.com/qpetitjean/Adaptive-plastic-responses-of-fish-in-a-multistress-context/main/Rscript/SourceTables_PlasticResponsesFishMS.R")
 

#' 
#' # Load the dataset
## ----dataset-----------------------------------------------------------------------------------------------------------
dat1 <- read.csv2("https://raw.githubusercontent.com/qpetitjean/Adaptive-plastic-responses-of-fish-in-a-multistress-context/main/Dataset/Dataset_Petitjeanetal-Plastic_responses_of_fish_in_multistress_context.csv", dec = ".", sep = ";")

# specify that the cage Id is a factor instead of numeric to use it as random intercept in mixed effect models
dat1[["CageSiteID"]] <- as.factor(dat1[["CageSiteID"]])

# Add merged treatments variables
dat1$CxOxI = paste(dat1$TransplantContam, dat1$OriginSite, dat1$Injection, sep = "_")
dat1$CxO2xI = paste(dat1$TransplantContam,
                    dat1$OriginContam,
                    dat1$Injection,
                    sep = "_")
dat1$CxO = paste(dat1$TransplantContam, dat1$OriginSite, sep = "_")
dat1$CxO2 = paste(dat1$TransplantContam, dat1$OriginContam, sep = "_")
dat1$CxI = paste(dat1$TransplantContam, dat1$Injection, sep = "_")
dat1$O2xI = paste(dat1$OriginContam, dat1$Injection, sep = "_")
head(dat1,5)


#' # Check sample size per treatments 
#' ## Including escaped fish
## ----sampleSize1-------------------------------------------------------------------------------------------------------
tab1 <- data.frame(table(dat1$OriginSite, dat1$CagingSite, dat1$OriginContam, dat1$TransplantContam, dat1$Injection))
names(tab1) <- c("Origin site",	"Transplant site",	"Contamination level in origin site", 	"Contamination level in transplant site",	"Immune challenge", "Sample size")
tab1 <- tab1[which(tab1[["Sample size"]]>0),]
tab1 <- tab1[order(tab1[["Origin site"]], tab1[["Transplant site"]]),]
rownames(tab1) <- NULL
kableExtra::kable_classic(kableExtra::kbl(tab1), bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F,  html_font = "arial", font_size = 10)
# total sample size: 
sum(tab1[["Sample size"]])

#' ## Excluding escaped fish
## ----sampleSize2-------------------------------------------------------------------------------------------------------
dat1bis = dat1[dat1$Escaped == "No", ]
tab2 <- data.frame(table(dat1bis$OriginSite, dat1bis$CagingSite, dat1bis$OriginContam, dat1bis$TransplantContam, dat1bis$Injection))
names(tab2) <- c("Origin site",	"Transplant site",	"Contamination level in origin site", 	"Contamination level in transplant site",	"Immune challenge", "Sample size")
tab2 <- tab2[which(tab2[["Sample size"]]>0),]
tab2 <- tab2[order(tab2[["Origin site"]], tab2[["Transplant site"]]),]
rownames(tab2) <- NULL
kableExtra::kable_classic(kableExtra::kbl(tab2), bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F,  html_font = "arial",  font_size = 10)
# total sample size: 
sum(tab2[["Sample size"]])

#' 
#' # Test some basics 
#'  here we will test the design of the experiment.\
#'  A quick reminder:\
#' 
#'  - Treatments (fixed effects):\
#'     - TransplantContam (transplant treatment either High contamination-HC or Low contamination-LC)\
#'     - OriginContam (Origin of the population either High contamination-HC or Low contamination-LC)\
#'     - Injection (control saline: PBS, antigen mixture: AMIX)\
#' 
#'  - Covariates :\
#'     - SizeEnd (Fish size at the end of the experiment)\
#'     - Sex (Fish sex determined by visual inspection at the end of the experiment)\
#' 
#'  - Blocks (random effects):\
#'     - CagingSite (The name of the study site where fish are caged, either ARIMAS, AUSCOR, CELFIG or RIOU)\
#'     - CageSiteID (The ID number ranging from 1 to 6 corresponding to the number of the cage within each study site)\
#'     - OriginSite (The name of the site where each population were sampled = population ID)\
#' 
#' ## Test Distribution of fish sex among cages\
#' sex determination was performed at the end of the experiment: visual inspection of the gonads
#' 
## ----SexTabs-----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$Sex) == F, ]
sexTabCage = table(dat2$CageID, dat2$Sex)
sexTabOrigin = table(dat2$OriginContam, dat2$Sex)
sexTabTransplant = table(dat2$TransplantContam, dat2$Sex)
sexTabInj = table(dat2$Injection, dat2$Sex)
sexTabTriple = table(dat2$CxO2xI, dat2$Sex)

#' ### Visualize the data
## ----SexViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(3,2))
plot(sexTabCage, main = "Cage number", ylab = "sex")
plot(sexTabOrigin, main = "Origin site", ylab = "sex")
plot(sexTabTransplant, main = "Transplant site", ylab = "sex")
plot(sexTabInj, main = "Injection", ylab = "sex")
plot(sexTabTriple, main = "All_treatments", ylab = "sex")

#' ### Test sex distribution among cages using Chi square test
## ----SexChiCages-------------------------------------------------------------------------------------------------------
chi = chisq.test(sexTabCage)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi

#' While some cages contains more female than male, the global pattern is not significant\
#' since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :\
## ----SexfisherCages----------------------------------------------------------------------------------------------------
fisher.test(sexTabCage, simulate.p.value = TRUE)

#' Fisher test is non-significant, which confirm the result of the Chisquare-test
#' 
#' ### What about sex distribution among treatments: origin ?
## ----SexChiOrigin------------------------------------------------------------------------------------------------------
chi = chisq.test(sexTabOrigin)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi

#' As observed on the plot and as confirmed by chisquare test,\
#' distribution of male and female is balanced among populations origin
#' 
#' ### What about sex distribution among treatments: TransplantContam ?
## ----sexTabTransplant--------------------------------------------------------------------------------------------------
chi = chisq.test(sexTabTransplant)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi

#' As observed on the plot and as confirmed by chisquare test,\
#' distribution of male and female is balanced between TransplantContam treatments
#' 
#' ### What about sex distribution among treatments: Injection ?
## ----sexTabInj---------------------------------------------------------------------------------------------------------
chi = chisq.test(sexTabInj)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi

#' As observed on the plot and as confirmed by chisquare test,\
#' distribution of male and female is balanced between Injection treatments
#' 
#' ### What about sex distribution among treatments: triple combination of treatments ?
## ----sexTabTriple------------------------------------------------------------------------------------------------------
chi = chisq.test(sexTabTriple)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi

#' As observed on the plot and as confirmed by chisquare test,\
#' distribution of male and female is more or less balanced among treatments
#' 
#' ## Test distribution of fish weight among treatments (at the beginning of the experiment, using WeightStart)
#' Visualize the data
## ----fishWeightViz-----------------------------------------------------------------------------------------------------
dat2 <- dat1[is.na(dat1$WeightStart) == F, ]
par(mfrow = c(2, 2))
hist(log(dat2$WeightStart), 
     main = "WeightStart distribution (log-transformed)")
boxplot(log(WeightStart) ~ OriginContam,
        data = dat2,
        main = "WeightStart vs. \nContam. level at origin site (log-transformed)")
boxplot(log(WeightStart) ~ TransplantContam,
        data = dat2,
        main = "WeightStart vs. \nContam. level at transplant site (log-transformed)")
boxplot(log(WeightStart) ~ Injection, 
        data = dat2, 
        main = "WeightStart vs. \nimmune challenge (log-transformed)")

#' Create the full model
## ----fishWeightModelFull-----------------------------------------------------------------------------------------------
mod1 <- lme4::lmer(
  log(WeightStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2
)
performance::check_model(mod1)
car::Anova(mod1, type = "3")
summary(mod1)

#' now, refine the model using AIC
## ----fishWeightModelRefined--------------------------------------------------------------------------------------------
mod1 =lme4::lmer(
  log(WeightStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
  REML = FALSE,
  na.action = "na.fail"
)
MuMIn::dredge(mod1, rank = "AIC")

#' The best model is the null model meaning that fish mass were not significantly different among treatments\
#' run the model with simple treatments effect to obtain NS p-value (reported in material & methods)
## ----fishWeightModelfinal----------------------------------------------------------------------------------------------
mod2 = lme4::lmer(
  log(WeightStart) ~ TransplantContam + OriginContam + Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
)
car::Anova(mod2, type = "3")
summary(mod2)

#' Compute mean and Sd of fish Weight (reported in material & methods)
## ----fishWeightMeanSd--------------------------------------------------------------------------------------------------
mean(dat2$WeightStart)
sd(dat2$WeightStart)

#' 
#' ## Test Distribution of fish size among treatments (at the beginning of the experiment, using SizeStart)
#' Visualize the data
## ----fishSizeViz-------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$SizeStart) == F, ]
par(mfrow = c(2, 2))
hist(log(dat2$SizeStart), 
     main = "SizeStart distribution (log-transformed)")
boxplot(log(SizeStart) ~ OriginContam,
        data = dat2,
        main = "SizeStart vs. \nContam. level at origin site (log-transformed)")
boxplot(log(SizeStart) ~ TransplantContam,
        data = dat2,
        main = "SizeStart vs. \nContam. level at transplant site (log-transformed)")
boxplot(log(SizeStart) ~ Injection, 
        data = dat2, 
        main = "SizeStart vs. \nimmune challenge (log-transformed)")

#' Create the full model
## ----fishSizeModelFull-------------------------------------------------------------------------------------------------
mod1 <- lme4::lmer(
  log(SizeStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2
)
performance::check_model(mod1)
car::Anova(mod1, type = "3")
summary(mod1)

#' now, refine the model using AIC
## ----fishSizeModelRefined----------------------------------------------------------------------------------------------
mod1 =lme4::lmer(
  log(SizeStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
  REML = FALSE,
  na.action = "na.fail"
)
MuMIn::dredge(mod1, rank = "AIC")

#' The best model is the null model meaning that fish mass were not significantly different among treatments\
#' run the model with simple treatments effect to obtain NS p-value (reported in material & methods)
## ----fishSizeModelfinal------------------------------------------------------------------------------------------------
mod2 = lme4::lmer(
  log(SizeStart) ~ TransplantContam + OriginContam + Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
)
car::Anova(mod2, type = "3")
summary(mod2)

#' Compute mean and Sd of fish size (reported in material & methods)
## ----fishSizeMeanSd----------------------------------------------------------------------------------------------------
mean(dat2$SizeStart)
sd(dat2$SizeStart)

#' ##  Test for Crowding
#' ### Compute mean and Sd of fish density per liter (reported in material & methods)
#' 
#' - At the start of the experiment
## ----CrowdStart--------------------------------------------------------------------------------------------------------
Crowd = aggregate(dat1$WeightStart, list(dat1$CageID), sum)
Crowd$dens = Crowd$x / 130 # 130 is the volume of the cages in liters
mean(Crowd$dens) # expressed in grams of fish per liter of water within the cage
sd(Crowd$dens)

#' - At the end of the experiment
## ----CrowdEnd----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$Death) == F, ]
dat2 = dat2[-c(which(dat2$Death == 1)), ]
Crowd2 = aggregate(dat2$WeightEnd, list(dat2$CageID), function(x)
  sum(x, na.rm = T))
Crowd2$dens = Crowd2$x / 130 # 130 is the volume of the cages in liters
mean(Crowd2$dens)
sd(Crowd2$dens)

#' ### Test for the difference between start and end of the experiment
## ----CrowdTest---------------------------------------------------------------------------------------------------------
CrowdTest = cbind(Crowd$dens, Crowd2$dens)
chi = chisq.test(CrowdTest)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
# Here the test confirmed that density of fish is not significantly different among cages
# but since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :
fisher.test(round(CrowdTest, 1)*10, simulate.p.value = TRUE) # Round values to the first decimal and multiply by 10 to have integer only (necessary for fisher-test)


#' Fisher test is non-significant, confirming the result of the Chisquare-test: The density of fish is not significantly different among cages between the start and end of the experiment, hence, While there was some death in several cages, the global pattern is not significant
#' 
#' ## Check Toxic Unit (TU) consistency among studies and between the start and end of the experiment
#' Retrieve the TU reported in PETITJEAN et al. 2020 a,b (based on water agency database)
## ----Tu2020------------------------------------------------------------------------------------------------------------
Tu2020 = c(-0.9,-0.9,-0.3, 1.3)

#' 
#' Retrieve the global TU reported in the present study\
#' based on water samples collected in this study, both at the start and end of the experiment
## ----TUALL-------------------------------------------------------------------------------------------------------------

TuStart = c(-0.4,-0.5,-0.1, 1.1)
TuEnd = c(-0.3,-0.7, 0.3, 0.9)
TuGlobal = c(-0.4,-0.6, 0.2, 1.0)
Tutab = cbind(Tu2020, TuGlobal)
Tutab2 = cbind(TuStart, TuEnd)

#' 
#' ### Visualize
## ----TUConsistency-----------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2))
plot(Tutab,
     xlab = "TU2020",
     ylab = "TUGlobal",
     main = "Global TU vs. previous studies")
plot(Tutab2,
     xlab = "TuStart",
     ylab = "TuEnd",
     main = "TU measured at the end of this study \n vs. \n TU measured at the start of this study")

#' 
#' ### Test the difference among global and TU reported in this study and reported in PETITJEAN et al. 2020 a,b
## ----TUStats-----------------------------------------------------------------------------------------------------------
chi = chisq.test(Tutab + 1) # add 1 to each values to ensure TU is positive (necessary for chi-test)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
# Here the test confirmed that Toxic units are not significantly different among studies
# but since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :
fisher.test((Tutab + 1)*10, simulate.p.value = TRUE) # add 1 to each values to ensure TU is positive and multiply by 10 to have integer only (necessary for fisher-test)


#' Fisher test is non-significant, confirming the result of the Chisquare-test: The toxic units are not significantly different among studies
#' 
#' ### Test the difference in Toxic Unit between the start and the end of the experiment
## ----TUstats2----------------------------------------------------------------------------------------------------------
chi = chisq.test(Tutab2 + 1) # add 1 to each values to ensure TU is positive (necessary for chi-test)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
# Here the test confirmed that Toxic units are not significantly different between the start and the end of the experiment
# but since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :
fisher.test((Tutab2 + 1)*10, simulate.p.value = TRUE) # add 1 to each values to ensure TU is positive and multiply by 10 to have integer only (necessary for fisher-test)

#' Fisher test is non-significant, confirming the result of the Chisquare-test: the toxic units are not significantly different between the start and the end of the experiment
#' 
#' # Data analyses
#' 
#'  A quick reminder:\
#' 
#'  - Treatments (fixed effects):\
#'     - TransplantContam (transplant treatment either High contamination-HC or Low contamination-LC)\
#'     - OriginContam (Origin of the population either High contamination-HC or Low contamination-LC)\
#'     - Injection (control saline: PBS, antigen mixture: AMIX)\
#' 
#'  - Covariates :\
#'     - SizeEnd (Fish size at the end of the experiment)\
#'     - Sex (Fish sex determined by visual inspection at the end of the experiment)\
#' 
#'  - Blocks (random effects):\
#'     - CagingSite (The name of the study site where fish are caged, either ARIMAS, AUSCOR, CELFIG or RIOU)\
#'     - CageSiteID (The ID number ranging from 1 to 6 corresponding to the number of the cage within each study site)\
#'     - OriginSite (The name of the site where each population were sampled = population ID)\
#' 
#' Here we used The identity of the cage nested within the study site as random effects\
#' to consider possible shared conditions within the cage and the study site.\
#' 
#' Best models were selected by backward selection procedure, eliminating non-significant interactions and variables (i.e., p-value > 0.05). 
#' 
#' When interactions were found significant, we analyzed differences between groups using pairwise t-test with false discovery rate adjustment (Benjamini and Hochberg, 1995) or two-sample fisher's exact test for count data (i.e., survival) (Agresti, 2007), respectively.
#' 
#' In addition, when interactions between the level of contamination in the transplant site and the origin of the population (i.e., HC or LC) were significant, we tested whether slopes were parallel among replicates populations by comparing models including the level of contamination in transplant site (i.e., HC or LC) and the identity of the origin of the population (i.e., ARIMAS, AUSCOR or CELFIG, RIOU) with models including the interaction between the level of contamination in transplant site and the identity of the origin of the population according to (Jacquin et al., 2016).
#' 
#' NB: The results retrieved from the following refined models are reported in Table 3 of the manuscript. The table 3 can also be found in the section "Summary of the best models (Table 3 from the manuscript)"
#' 
#' ## Survival
## ----initSurvival------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$Death) == F, ]
par(mfrow = c(1,2))
plot(dat2$Death)
hist(dat2$Death)

#' The data are binomial, so use glmer function from lme4 package to construct the full model:
#' ### Use GLMER with family binomial
## ----GLMERSurvival, message=FALSE, warning=FALSE-----------------------------------------------------------------------
modfull <-
  lme4::glmer(
    Death ~ (TransplantContam + OriginContam + Injection) ^ 3 + (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    family = binomial(link = "logit"),
    na.action = na.fail
  )
performance::check_model(modfull)
car::Anova(modfull, type = "3")
summary(modfull)
MuMIn::r.squaredGLMM(modfull)

#' The model returns a singular fit, the random structure seems too complex for the data since "OriginSite" have a variance of zero. To solve this, we can use two alternative methods:
#' 
#' ### Use classic glm with family binomial (remove random effects)
## ----GlmSurvivalFull---------------------------------------------------------------------------------------------------
modfull <-
  glm(
    Death ~ (TransplantContam + OriginContam + Injection) ^ 3,
    data = dat2,
    family = "binomial"
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refine the model
## ----GlmSurvivalRefined1-----------------------------------------------------------------------------------------------
mod2 <-
  glm(
    Death ~ (TransplantContam + OriginContam + Injection) ^ 2,
    data = dat2,
    family = "binomial"
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refine the model
## ----GlmSurvivalRefined2-----------------------------------------------------------------------------------------------
mod3 <-
  glm(
    Death ~ (TransplantContam + OriginContam) ^ 2 + Injection,
    data = dat2,
    family = "binomial"
  )
car::Anova(mod3, type = "3")
summary(mod3)

#' #### Refine the model
## ----GlmSurvivalRefined3-----------------------------------------------------------------------------------------------
mod4 <-
  glm(Death ~ (TransplantContam + OriginContam) ^ 2,
      data = dat2,
      family = "binomial")
performance::check_model(mod4)
car::Anova(mod4, type = "3")
summary(mod4)
modSurvival <- mod4

#' Here the interaction between fish origin and the transplant site is significant. 
#' Alternatively, we can compute a survival rate per cage and build model with survival transformed to logit
#' 
#' ### Build model with survival transformed to logit
#' NB: here we used the CagingSite as random effect only since survival is computed for each cage so that there is no need to add the cageSiteID.
#' 
#' #### Create a new dataset containing survival rate within each cage
## ----SurvivalGLMERLOGIT------------------------------------------------------------------------------------------------
dat2$CageIDInj <- paste(dat2$CageID, dat2$Injection, sep = "_")
n = as.data.frame(dplyr::count(dat2, CageIDInj))
names(n)[1] <- "CageIDInj"
names(n)[2] <- "n"
alive <-
  as.data.frame(dplyr::count(dat2[dat2$Death == "0", ], CageIDInj))
dead <-
  as.data.frame(dplyr::count(dat2[dat2$Death == "1", ], CageIDInj))
DeathSum <- dplyr::left_join(alive, dead, by = "CageIDInj")
DeathSum$n.y[is.na(DeathSum$n.y)] <- 0
DeathSum <- dplyr::left_join(DeathSum, n, by = "CageIDInj")
names(DeathSum)[2] <- "alive"
names(DeathSum)[3] <- "dead"
DeathSum$survival <- DeathSum$alive / DeathSum$n
DeathSum <-  dplyr::left_join(DeathSum,
                              dat2[match(DeathSum$CageIDInj,  dat2$CageIDInj), c(
                                "CageIDInj",
                                "CageID",
                                "OriginSite",
                                "OriginContam",
                                "SitesPair",
                                "CageSiteID",
                                "CagingSite",
                                "TransplantContam",
                                "Injection",
                                "CxOxI",
                                "CxO2xI",
                                "CxO",
                                "O2xI"
                              )], by = "CageIDInj")


#' 
#' #### Build the full model
## ----SurvivalLogitFull-------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam + Injection) ^ 3 +
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )

car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refine the model
## ----SurvivalLogitRefined1---------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam + Injection) ^ 2+
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
car::Anova(mod2, type="3")
summary(mod2)

#' #### Refine the model
## ----SurvivalLogitRefined2---------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam) ^ 2 + Injection +
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)

#' #### Refine the model
## ----SurvivalLogitRefined3---------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam) ^ 2 +
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
performance::check_model(mod4)
car::Anova(mod4, type = "3")
summary(mod4)


#' 
#' Although, this last method also result in singular fit ("CagingSite" and "OriginSite" random effects variance is estimated as zero), here the interaction between fish origin and the transplant site is significant.
#' Both simplified methods give similar results, hence the results of the more straightforward method (glm) are reported in the statistic table (table 3).
#' Also, because an interaction is significant, perform a posthoc test.
#' 
#' ### Posthoc test (test for equality of proportions)
## ----SurvivalPosthoc---------------------------------------------------------------------------------------------------
# create two matrix containing fish survival rate according to their origin and transplant sites
DeathSum$CxO2 = paste(DeathSum$TransplantContam, DeathSum$OriginContam, sep =
                        "_")
a = aggregate(DeathSum$alive, list(DeathSum$CxO2), sum)
b = aggregate(DeathSum$dead, list(DeathSum$CxO2), sum)

# create the matrix for Low contamination origin
OriginLC = matrix(
  c(b[b$Group.1 == "LC_LC", 2], a[a$Group.1 == "LC_LC", 2], b[b$Group.1 == "HC_LC", 2], a[a$Group.1 == "HC_LC", 2]),
  nrow = 2,
  ncol = 2,
  dimnames = list(c("Dead", "Alive"), c("LC", "HC"))
)
OriginLC
# create the matrix for High contamination origin
OriginHC = matrix(
  c(b[b$Group.1 == "LC_HC", 2], a[a$Group.1 == "LC_HC", 2], b[b$Group.1 == "HC_HC", 2], a[a$Group.1 == "HC_HC", 2]),
  nrow = 2,
  ncol = 2,
  dimnames = list(c("Dead", "Alive"), c("LC", "HC"))
)
OriginHC

# run test for equality of proportions on each matrix
fisher.test(OriginLC, conf.level = 0.95)
fisher.test(OriginHC, conf.level = 0.95)

#' Here fish from Low Contamination, display a significant decrease in survival when exposed to High Contamination site.
#' On the contrary, fish from High contamination site do not display a significant change in survival according to transplant site.
#' 
#' Also,because interaction between the contamination in origin and transplant site of fish is significant
#' We test for parallel response between replicate populations.
#' 
#' ### Test for parallel responses between replicate populations (from the same origin)
#' #### For Populations from High contamination sites (AUSCOR and RIOU)
## ----survivalParallelHC------------------------------------------------------------------------------------------------
dat3 <- dat2[which(dat2$OriginSite %in% c("AUSCOR", "RIOU")),]
## build the null model
mod.null <-
  glm(Death ~ TransplantContam + OriginSite,
      data = dat3)
## build the model with the site of origin in interaction with the level of contamination in transplant sites
mod.par <-
  glm(Death ~ TransplantContam * OriginSite,
      data = dat3)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)


#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.
#' 
#' #### For Populations from Low contamination sites (ARIMAS and CELFIG)
## ----survivalParallelLC------------------------------------------------------------------------------------------------
dat4 <- dat2[which(dat2$OriginSite %in% c("ARIMAS", "CELFIG")),]
## build the null model
mod.null <-
  glm(Death ~ TransplantContam + OriginSite,
      data = dat4)
## build the model with the site of origin in interaction with the level of contamination in transplant sites
mod.par <-
  glm(Death ~ TransplantContam * OriginSite,
      data = dat4)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)


#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of LC populations are parallel.
#' 
#' ### Visualize 
#' The plot reported below correspond to the figure 2A from the manuscript
## ----survivalViz, fig.dim = c(8, 6)------------------------------------------------------------------------------------
par(mfrow=c(1,1))
Survivplot()

#' 
#' ## Metal bioaccumulation
#' ### Create a global index of metal accumulation: Zn, Cd, Cu
## ----BioaccIndex-------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$MuscleCd) == F, ]
dat2$Cdscaled = scale(log(dat2$MuscleCd), center = T, scale = T)
dat2$Cuscaled = scale(log(dat2$MuscleCu), center = T, scale = T)
dat2$Znscaled = scale(log(dat2$MuscleZn), center = T, scale = T)
dat2$Bioacc = dat2$Cdscaled + dat2$Cuscaled + dat2$Znscaled
hist(dat2$Bioacc)

#' 
#' ### Visualize
## ----BioaccViz---------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(Bioacc ~ OriginContam, data = dat2)
boxplot(Bioacc ~ TransplantContam, data = dat2)
boxplot(Bioacc ~ Injection, data = dat2)
boxplot(Bioacc ~ TransplantContam * Injection, data = dat2)
boxplot(Bioacc ~ TransplantContam * OriginContam, data = dat2)
boxplot(Bioacc ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----BioaccFull--------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----BioaccRefined1----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd+ 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----BioaccRefined2----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)

#' ### Refining the model
## ----BioaccRefined3----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)
modBioacc <- mod4
RmodBioacc <- MuMIn::r.squaredGLMM(modBioacc)

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----BioaccSex---------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
#' The mod4 is thus the best model.
#' 
#' ### Posthoc test (pairwise t-test)
#' The interactions between fish origin and the transplant as well as between transplant and injection treatments are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions
#' 
## ----BioaccPosthoc1----------------------------------------------------------------------------------------------------
pairwise.t.test(dat2$Bioacc, dat2$CxI, p.adjust.method = "fdr")

#' Here, fish from low contamination site and injected with a saline control solution, has bioaccumulated significantly less metals than the fish exposed to other treatment, even compared to LC exposed in Low contamination site and injected to the antigen mixture, suggesting that the antigen mixture could affect bioaccumulation in fish muscle.
#' 
## ----BioaccPosthoc2----------------------------------------------------------------------------------------------------
pairwise.t.test(dat2$Bioacc, dat2$CxO2, p.adjust.method = "fdr")

#' Here fish from Low Contamination, display a significant increase in the amount of metal bioaccumulated in their muscle when exposed to High Contamination site compared to when they are exposed in low contamination site. On the contrary, the metal levels is constant in HC fish, whatever the site of transplantation. 
#' 
#' Also,because interaction between the contamination in origin and transplant site of fish is significant
#' We test for parallel response between replicate populations.
#' 
#' ### Test for parallel responses between replicate populations (from the same origin)
#' #### For Populations from High contamination sites (AUSCOR and RIOU)
## ----BioaccParallelHC--------------------------------------------------------------------------------------------------
dat3 <- dat2[which(dat2$OriginSite %in% c("AUSCOR", "RIOU")),]
## build the null model
mod.null <-
  glm(Bioacc ~ TransplantContam + OriginSite,
      data = dat3)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(Bioacc ~ TransplantContam * OriginSite,
      data = dat3)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)


#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.
#' 
#' #### For Populations from Low contamination sites (ARIMAS and CELFIG)
## ----BioaccParallelLC--------------------------------------------------------------------------------------------------
# We test for parallel response between replicate LC populations (ARIMAS and CELFIG)
dat4 <- dat2[which(dat2$OriginSite %in% c("ARIMAS", "CELFIG")),]
## build the null model
mod.null <-
  glm(Bioacc ~ TransplantContam + OriginSite,
      data = dat4)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(Bioacc ~ TransplantContam * OriginSite,
      data = dat4)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)

#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of LC populations are parallel.
#' 
#' ### Visualize 
#' The plot reported below correspond to the figure 3 and the figure 2B from the manuscript 
## ----BioaccFinalViz, fig.dim = c(8, 6)---------------------------------------------------------------------------------
BioaccPlot3()
BioaccPlot1()

#' 
#' 
#' ## Oxidative Stress Index
#' ### Create a global index of oxidative stress (Costantini and Dell’Omo, 2006)
#' 
## ----ratOXIIndex-------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$mMHCLO) == F, ]
dat2 = dat2[is.na(dat2$mMH2O2) == F, ]
dat2$ratOXI = dat2$mMH2O2 * 1000 / dat2$mMHCLO
hist(log(dat2$ratOXI + 1))

#' 
#' ### Visualize
## ----ratOXIViz---------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(log(dat2$ratOXI + 1) ~ OriginContam, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ Injection, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam * Injection, data = dat2, cex.axis = 0.8)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam * OriginContam * Injection, data = dat2, cex.axis = 0.8, las = 2)

#' 
#' ### Build the full model
## ----ratOXIFull--------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)


#' ### Refining the model
## ----ratOXIRefined1----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----ratOXIRefined2----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----ratOXIRefined3----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    log(ratOXI + 1) ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' ### Refining the model
## ----ratOXIRefined4----------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    log(ratOXI + 1) ~ SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)
modRatOx <- mod5
RmodRatOx <- MuMIn::r.squaredGLMM(modRatOx)

#' 
#' The treatments has no effect on oxidative stress index but the oxidative stress index is lower in bigger fish 
#' Represent the relation between fish size and oxidative stress index
## ----ratOXISize--------------------------------------------------------------------------------------------------------
plot(log(ratOXI + 1) ~ SizeEnd, data = dat2)
abline(b = -0.10109, a = 3.53866, col = "red")

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----ratOXISex---------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
mod5 <-
 lme4::lmer(
    log(ratOXI + 1) ~ SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)


#' The sex effect is non-significant.
#' The mod5 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' ## Oxidative damage
#' ### Check the data
## ----OxDamIndex--------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$mMH2O2) == F, ]
hist(log(dat2$mMH2O2 + 1))


#' 
#' ### Visualize
## ----OxDamViz----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(log(dat2$mMH2O2 + 1) ~ OriginContam, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ Injection, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam * Injection, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----OxDamFull---------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----OxDamRefined1-----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----OxDamRefined2-----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ (TransplantContam + Injection) ^ 2 + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----OxDamRefined3-----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ TransplantContam + Injection + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' ### Refining the model
## ----OxDamRefined4-----------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)
modOXDam <- mod5
RmodOXDam <- MuMIn::r.squaredGLMM(modOXDam)

#' The treatments has no effect on oxidative damage but the oxidative damage are lower in bigger fish 
#' Represent the relation between fish size and oxidative damage
## ----OxDamSize---------------------------------------------------------------------------------------------------------
plot(log(mMH2O2 + 1) ~ SizeEnd, data = dat2)
abline(b = -0.08377, a = 2.06737, col = "red")

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----OxDamSex----------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod5 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' ## Antiox. Capacity
#' ### Check the data
## ----antiOxIndex-------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$mMHCLO) == F, ]
# transform variable to approx. normality
dat2$mMHCLO.p <-
  bimixt::boxcox(dat2$mMHCLO, car::powerTransform(dat2$mMHCLO)$lambda)
plot(dat2$mMHCLO.p, dat2$mMHCLO)
hist(dat2$mMHCLO.p)


#' 
#' ### Visualize
## ----antiOxViz---------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(mMHCLO.p ~ OriginContam, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam, data = dat2)
boxplot(mMHCLO.p ~ Injection, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam * Injection, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----antiOxFull--------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    mMHCLO.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----antiOxRefined1----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    mMHCLO.p~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----antiOxRefined2----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    mMHCLO.p ~ (TransplantContam + OriginContam ) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----antiOxRefined3----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    mMHCLO.p ~ TransplantContam + Injection + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' ### Refining the model
## ----antiOxRefined4----------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    mMHCLO.p ~ OriginContam +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)
modantiOx <- mod5
RmodantiOx <- MuMIn::r.squaredGLMM(modantiOx)

#' 
#' The origin of the fish affects the non-enzymatic antioxidant capacity (measured in plasma), 
#' With HC fish having higher non-enzymatic antioxidant capacity than fish from LC sites (see boxplot and table below).
## ----antiOxOrigin------------------------------------------------------------------------------------------------------
boxplot(mMHCLO.p ~ OriginContam, data = dat2)
# compute mean and Se for LC and HC groups (on raw antioxidant capacity data - not transformed)
meanVal <-
  data.frame(
    mean = c(mean(dat2$mMHCLO[which(dat2$OriginContam == "LC")]),
             mean(dat2$mMHCLO[which(dat2$OriginContam == "HC")])),
    Se = c(
      sd(dat2$mMHCLO[which(dat2$OriginContam == "LC")]) / sqrt(length(dat2$mMHCLO[which(dat2$OriginContam == "LC")])),
      sd(dat2$mMHCLO[which(dat2$OriginContam == "HC")]) / sqrt(length(dat2$mMHCLO[which(dat2$OriginContam == "HC")]))
    ),
    row.names = c("LC", "HC")
  )
kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----antiOxSex---------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    mMHCLO.p ~ OriginContam + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod5 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' ## local inflammatory Response
#' ### Check the data
## ----IRIndex-----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$IR) == F, ]
hist(dat2$IR)

#' 
#' ### Visualize
## ----IRViz-------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(IR ~ OriginContam, data = dat2)
boxplot(IR ~ TransplantContam, data = dat2)
boxplot(IR ~ Injection, data = dat2)
boxplot(IR ~ TransplantContam * Injection, data = dat2)
boxplot(IR ~ TransplantContam * OriginContam, data = dat2)
boxplot(IR ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----IRFull------------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    IR ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----IRRefined1--------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    IR~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----IRRefined2--------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    IR ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----IRRefined3--------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    IR ~ TransplantContam + Injection + OriginContam +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' ### Refining the model
## ----IRRefined4--------------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    IR ~ OriginContam + Injection + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)


#' ### Refining the model
## ----IRRefined5--------------------------------------------------------------------------------------------------------
mod6 <-
 lme4::lmer(
    IR ~  Injection + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod6)
car::Anova(mod6, type = "3")
summary(mod6)
modIR <- mod6
RmodIR <- MuMIn::r.squaredGLMM(modIR)

#' The immune challenge led to a significant skin swelling, and hence triggered the local immune response.
#' 
## ----IRInjection-------------------------------------------------------------------------------------------------------
boxplot(IR ~ Injection, data = dat2)
# compute mean and Se for LC and HC groups (on raw antioxidant capacity data - not transformed)
meanVal <-
  data.frame(
    mean = c(mean(dat2$IR[which(dat2$Injection == "PBS")]),
             mean(dat2$IR[which(dat2$Injection == "AMIX")])),
    Se = c(
      sd(dat2$IR[which(dat2$Injection == "PBS")]) / sqrt(length(dat2$IR[which(dat2$Injection == "PBS")])),
      sd(dat2$IR[which(dat2$Injection == "AMIX")]) / sqrt(length(dat2$IR[which(dat2$Injection == "AMIX")]))
    ),
    row.names = c("PBS", "AMIX")
  )
kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----IRSex-------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    IR ~ Injection + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod6 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' 
#' ## NL inflammatory Response
#' ### compute NL ratio
## ----NLRatio.pIndex----------------------------------------------------------------------------------------------------
# compute NL ratio
dat2 = dat1[is.na(dat1$Lymphocytes) == F, ]
dat2 = dat2[is.na(dat2$Neutrophils) == F, ]
dat2$NLRatio = dat2$Neutrophils / dat2$Lymphocytes
# transform variable to approx. normality
dat2$NLRatio.p <-
  bimixt::boxcox(dat2$NLRatio + 1, car::powerTransform(dat2$NLRatio + 1)$lambda)

#' 
#' ### Visualize
## ----NLRatio.pViz------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(NLRatio.p ~ OriginContam, data = dat2)
boxplot(NLRatio.p ~ TransplantContam, data = dat2)
boxplot(NLRatio.p ~ Injection, data = dat2)
boxplot(NLRatio.p ~ TransplantContam * Injection, data = dat2)
boxplot(NLRatio.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(NLRatio.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----NLRatio.pFull-----------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----NLRatio.pRefined1-------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----NLRatio.pRefined2-------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)

#' ### Refining the model
## ----NLRatio.pRefined3-------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2 + Injection + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)

#' ### Refining the model
## ----NLRatio.pRefined4-------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2  + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod5, type = "3")
summary(mod5)
modNLRatio.p <- mod5
RmodNLRatio.p  <- MuMIn::r.squaredGLMM(modNLRatio.p )

#' Here, the interactions between fish origin and the transplant site are significant. Also, the bigger the fish, the higher the NL ratio and hence the immune inflammatory response.
## ----NLRatio.pSize-----------------------------------------------------------------------------------------------------
# check the relationships between th NlRatio and fish size
plot(NLRatio.p ~  SizeEnd, data = dat2)
abline(a = 0.007545 , b =  0.003648 , col = "red")

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----NLRatio.pSex------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2  + SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
#' The mod5 is thus the best model.
#' 
#' ### Posthoc test (pairwise t-test)
#' The interactions between fish origin and the transplant site are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions.
#' 
## ----NLRatio.pPosthoc--------------------------------------------------------------------------------------------------
pairwise.t.test(dat2$NLRatio.p, dat2$CxO2, p.adjust.method = "fdr")

#' Here fish from LC sites, have a marginally lower NL ratio than fish from HC site when transplanted in HC site. However, the posthoc are mostly non-significant.
#' 
#' Also,because interaction between the contamination in origin and transplant site of fish is significant
#' We test for parallel response between replicate populations.
#' 
#' ### Test for parallel responses between replicate populations (from the same origin)
#' #### For Populations from High contamination sites (AUSCOR and RIOU)
## ----NLRatio.pParallelHC-----------------------------------------------------------------------------------------------
dat3 <- dat2[which(dat2$OriginSite %in% c("AUSCOR", "RIOU")),]
## build the null model
mod.null <-
  glm(NLRatio.p ~ TransplantContam + OriginSite,
      data = dat3)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(NLRatio.p ~ TransplantContam * OriginSite,
      data = dat3)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)


#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.
#' 
#' #### For Populations from Low contamination sites (ARIMAS and CELFIG)
## ----NLRatio.pParallelLC-----------------------------------------------------------------------------------------------
# We test for parallel response between replicate LC populations (ARIMAS and CELFIG)
dat4 <- dat2[which(dat2$OriginSite %in% c("ARIMAS", "CELFIG")),]
## build the null model
mod.null <-
  glm(NLRatio.p ~ TransplantContam + OriginSite,
      data = dat4)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(NLRatio.p ~ TransplantContam * OriginSite,
      data = dat4)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)

#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of LC populations are parallel.
#' 
#' ### Visualize 
#' The plot reported below correspond to the figure 2C from the manuscript
## ----NLRatio.pFinalViz, fig.dim = c(8, 6)------------------------------------------------------------------------------
NLratioPlot1()

#' 
#' ## Available energy - Index (sum of lipids, proteins and carbohydrates)
#' ### Check the data
## ----AvailableEnerJIndex-----------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
hist(log(dat2$AvailableEnerJ + 1))

#' 
#' ### Visualize
## ----AvailableEnerJViz-------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(log(dat2$AvailableEnerJ + 1) ~ OriginContam, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ Injection, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam * Injection, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----AvailableEnerJFull------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----AvailableEnerJRefined1--------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----AvailableEnerJRefined2--------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam) ^ 2 + Injection + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)

#' ### Refining the model
## ----AvailableEnerJRefined3--------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam) ^ 2 + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)
modAvailableEnerJ <- mod4
RmodAvailableEnerJ  <- MuMIn::r.squaredGLMM(modAvailableEnerJ)

#' Here, the interactions between fish origin and the transplant site are significant. 
#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----AvailableEnerJSex-------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam) ^ 2  + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
#' The mod5 is thus the best model.
#' 
#' ### Posthoc test (pairwise t-test)
#' The interactions between fish origin and the transplant site are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions.
#' 
## ----AvailableEnerJPosthoc---------------------------------------------------------------------------------------------
pairwise.t.test(log(dat2$AvailableEnerJ + 1), dat2$CxO2, p.adjust.method = "fdr")

#' Here the available energy increases in LC fish when transplanted in HC sites. Similarly, the available energy increases in HC fish when transplanted in LC sites, suggesting that local condition within each study site could have driven the plastic responses rather than the level of contamination. 
#' 
#' Also,because interaction between the contamination in origin and transplant site of fish is significant
#' We test for parallel response between replicate populations.
#' 
#' ### Test for parallel responses between replicate populations (from the same origin)
#' #### For Populations from High contamination sites (AUSCOR and RIOU)
## ----AvailableEnerJParallelHC------------------------------------------------------------------------------------------
dat3 <- dat2[which(dat2$OriginSite %in% c("AUSCOR", "RIOU")),]
## build the null model
mod.null <-
  glm(log(dat3$AvailableEnerJ + 1) ~ TransplantContam + OriginSite,
      data = dat3)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(log(dat3$AvailableEnerJ + 1) ~ TransplantContam * OriginSite,
      data = dat3)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)


#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.
#' 
#' #### For Populations from Low contamination sites (ARIMAS and CELFIG)
## ----AvailableEnerJParallelLC------------------------------------------------------------------------------------------
# We test for parallel response between replicate LC populations (ARIMAS and CELFIG)
dat4 <- dat2[which(dat2$OriginSite %in% c("ARIMAS", "CELFIG")),]
## build the null model
mod.null <-
  glm(log(dat4$AvailableEnerJ + 1) ~ TransplantContam + OriginSite,
      data = dat4)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(log(dat4$AvailableEnerJ + 1) ~ TransplantContam * OriginSite,
      data = dat4)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)

#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are significantly different, we hence reject the null hypothesis. here, the slopes of LC populations are not parallel.
#' 
#' ### Visualize 
## ----AvailableEnerJFinalViz, fig.dim = c(8, 6)-------------------------------------------------------------------------
AvailEnerPlot1()

#' 
#' ## Available energy - Lipids
#' ### Check the data
## ----LipidsIndex-------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
# transform variable to approx. normality
dat2$MuscleLipid.p <-
  bimixt::boxcox(dat2$MuscleLipid, car::powerTransform(dat2$MuscleLipid)$lambda)

#' 
#' ### Visualize
## ----LipidsViz---------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(MuscleLipid.p  ~ OriginContam, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam, data = dat2)
boxplot(MuscleLipid.p  ~ Injection, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam * Injection, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam * OriginContam, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----LipidsFull--------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----LipidsRefined1----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
     MuscleLipid.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----LipidsRefined2----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----LipidsRefined3----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam) ^ 2 +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)
modLipids <- mod4
RmodLipids  <- MuMIn::r.squaredGLMM(modLipids)

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----LipidsSex---------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam) ^ 2  + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod4 is thus the best model.
#' Here, the interactions between fish origin and the transplant site are significant, confirming the pattern observed for the integrative measure of available energy previously observed.
#' 
#' ## Available energy - Proteins
#' ### Check the data
## ----ProteinsIndex-----------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
hist(dat2$MuscleProtein)

#' 
#' ### Visualize
## ----ProteinsViz-------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(MuscleProtein  ~ OriginContam, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam, data = dat2)
boxplot(MuscleProtein  ~ Injection, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam * Injection, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam * OriginContam, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----ProteinsFull------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    MuscleProtein ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----ProteinsRefined1--------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
     MuscleProtein ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----ProteinsRefined2--------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    MuscleProtein ~ (Injection + OriginContam) ^ 2 +  TransplantContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----ProteinsRefined3--------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    MuscleProtein ~ (Injection + OriginContam) ^ 2 +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)
modProtein <- mod4
RmodProtein  <- MuMIn::r.squaredGLMM(modProtein)

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----ProteinsSex-------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    MuscleProtein ~ (Injection + OriginContam) ^ 2   + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod4 is thus the best model.
#' Here, the interactions between fish origin and the immune challenge are significant, with LC fish site having a decreasing level of proteins in their muscle when immune challenged. On the contrary HC fish have a constant level of proteins, whatever the injection (control saline solution or antigen mixture).
## ----ProteinsPosthoc---------------------------------------------------------------------------------------------------
pairwise.t.test(dat2$MuscleProtein, dat2$O2xI, p.adjust.method = "fdr")

#' 
#' ## Available energy - Carbohydrates
#' ### Check the data
## ----CarbohIndex-------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
hist(log(dat2$MuscleCarbohydrate))

#' 
#' ### Visualize
## ----CarbohViz---------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(log(dat2$MuscleCarbohydrate)  ~ OriginContam, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ Injection, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam * Injection, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----CarbohFull--------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----CarbohRefined1----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
     log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----CarbohRefined2----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----CarbohRefined3----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam) ^ 2 +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)
modCarboh <- mod4
RmodCarboh  <- MuMIn::r.squaredGLMM(modCarboh)

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----CarbohSex---------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam) ^ 2  + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod4 is thus the best model.
#' Here, the interactions between fish origin and the transplant site are significant, confirming the pattern observed for the integrative measure of available energy previously observed.
#' 
#' 
#' ## Daily mass changes
#' ### Check the data
## ----DMCIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DailyMassChange) == F, ]
# transform variable to approx. normality
dat2$DailyMassChange.p <-
  bimixt::boxcox(dat2$DailyMassChange + 3,
         car::powerTransform(dat2$DailyMassChange + 3)$lambda)
hist(dat2$DailyMassChange.p)

#' 
#' ### Visualize
## ----DMCViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DailyMassChange.p ~ OriginContam, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam, data = dat2)
boxplot(DailyMassChange.p ~ Injection, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam * Injection, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----DMCFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----DMCRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----DMCRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2 + Injection + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)

#' ### Refining the model
## ----DMCRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2  + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' ### Refining the model
## ----DMCRefined4-------------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2 + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod5, type = "3")
summary(mod5)
modDMC <- mod5
RmodDMC  <- MuMIn::r.squaredGLMM(modDMC)

#' Here, the interactions between fish origin and the transplant site are significant. 
#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----DMCSex------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2  + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
#' The mod5 is thus the best model.
#' 
#' ### Posthoc test (pairwise t-test)
#' The interactions between fish origin and the transplant site are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions.
#' 
## ----DMCPosthoc--------------------------------------------------------------------------------------------------------
pairwise.t.test(dat2$DailyMassChange.p, dat2$CxO2, p.adjust.method = "fdr")

#' Here the posthoc are non significant, except between the HC and LC fish when exposed to LC sites, likely because responses were very different depending on the population considered (see non-parallel plastic responses below). 
#' Also,because interaction between the contamination in origin and transplant site of fish is significant
#' We test for parallel response between replicate populations.
#' 
#' ### Test for parallel responses between replicate populations (from the same origin)
#' #### For Populations from High contamination sites (AUSCOR and RIOU)
## ----DMCParallelHC-----------------------------------------------------------------------------------------------------
dat3 <- dat2[which(dat2$OriginSite %in% c("AUSCOR", "RIOU")),]
## build the null model
mod.null <-
  glm(DailyMassChange.p ~ TransplantContam + OriginSite,
      data = dat3)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(DailyMassChange.p ~ TransplantContam * OriginSite,
      data = dat3)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)


#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are significantly different, we hence reject the null hypothesis, here the slopes of HC populations are not parallel.
#' 
#' #### For Populations from Low contamination sites (ARIMAS and CELFIG)
## ----DMCParallelLC-----------------------------------------------------------------------------------------------------
# We test for parallel response between replicate LC populations (ARIMAS and CELFIG)
dat4 <- dat2[which(dat2$OriginSite %in% c("ARIMAS", "CELFIG")),]
## build the null model
mod.null <-
  glm(DailyMassChange.p ~ TransplantContam + OriginSite,
      data = dat4)
## build the model with the site of origin in interaction with the level of contam in transplant sites
mod.par <-
  glm(DailyMassChange.p ~ TransplantContam * OriginSite,
      data = dat4)

anova(mod.null, mod.par, test = "LRT")
coef(mod.par)
summary(mod.par)

#' The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are significantly different, we hence reject the null hypothesis, here the slopes of LC populations are not parallel.
#' 
#' ### Visualize 
## ----DMCFinalViz, fig.dim = c(8, 6)------------------------------------------------------------------------------------
DMCPlot1()

#' 
#' ## HSI
#' ### Check the data
## ----HSIIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$HSI) == F, ]
# transform variable to approx. normality
dat2$HSI.p <- bimixt::boxcox(dat2$HSI, car::powerTransform(dat2$HSI)$lambda)
hist(dat2$HSI.p)

#' 
#' ### Visualize
## ----HSIViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(HSI.p ~ OriginContam, data = dat2)
boxplot(HSI.p ~ TransplantContam, data = dat2)
boxplot(HSI.p ~ Injection, data = dat2)
boxplot(HSI.p ~ TransplantContam * Injection, data = dat2)
boxplot(HSI.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(HSI.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----HSIFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    HSI.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----HSIRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    HSI.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----HSIRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    HSI.p ~ (TransplantContam + OriginContam) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----HSIRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    HSI.p ~ TransplantContam + Injection + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' ### Refining the model
## ----HSIRefined4-------------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    HSI.p ~ SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)
modHSI <- mod5
RmodHSI  <- MuMIn::r.squaredGLMM(modHSI)

#' The treatments has no effect on HSI but the HSI is higher in bigger fish 
#' Represent the relation between fish size and HSI
## ----HSISize-----------------------------------------------------------------------------------------------------------
plot(HSI.p ~ SizeEnd, data = dat2)
abline(b = -0.04355, a = 0.10156, col = "red")

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' ### Test for the sex effect
## ----HSISex------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    HSI.p ~ SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is non-significant.
#' The mod5 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' ## GSI
#' ### Check the data
## ----GSIIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$GSI) == F, ]
dat2 = dat2[is.na(dat2$Sex) == F, ]
# transform variable to approx. normality
dat2$GSI.p <- bimixt::boxcox(dat2$GSI, car::powerTransform(dat2$GSI)$lambda)
hist(dat2$GSI.p)

#' 
#' ### Visualize
## ----GSIViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(GSI.p ~ OriginContam, data = dat2)
boxplot(GSI.p ~ TransplantContam, data = dat2)
boxplot(GSI.p ~ Injection, data = dat2)
boxplot(GSI.p ~ TransplantContam * Injection, data = dat2)
boxplot(GSI.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(GSI.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' ### Build the full model
## ----GSIFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    GSI.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' ### Refining the model
## ----GSIRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    GSI.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' ### Refining the model
## ----GSIRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    GSI.p ~ TransplantContam + OriginContam + Injection +  SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' ### Refining the model
## ----GSIRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    GSI.p ~ Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod4)
car::Anova(mod4, type = "3")
summary(mod4)
modGSI <- mod4
RmodGSI <- MuMIn::r.squaredGLMM(modGSI)

#' 
#' The treatments has no effect on GSI but the GSI is higher in females.
#' Represent the relation between fish sex and GSI
## ----GSISize-----------------------------------------------------------------------------------------------------------
boxplot(GSI.p ~ Sex, data = dat2)

#' 
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' ## Summary of the best models for traits excepted gene expression (Table 3 from the manuscript)
#' 
#' The following table report the results of the best models tested above, this table correspond to the Table 3  in the manuscript. Excepted for the survival (GLM), the table report the marginal and conditional Rsquared (R2m and R2c respectively).
## ----summaryStats, echo=FALSE------------------------------------------------------------------------------------------
TraitsTable()


#' 
#' ## Genes expression
#' 
#' The summary of the results of the best models selected below is reported in Table 4 from the manuscript and is also available in the section "Summary of the best models for gene expression (Table 4 from the manuscript)"
#' 
#' ### Mtl
#' #### Check the data
## ----MtlIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DeltaCtMtl) == F, ]
# transform variable to approx. normality
dat2$DeltaCtMtl.p <-
  bimixt::boxcox(dat2$DeltaCtMtl, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtMtl.p)

#' 
#' #### Visualize
## ----MtlViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DeltaCtMtl.p ~ OriginContam, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtMtl.p ~ Injection, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----MtlFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DeltaCtMtl.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----MtlRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DeltaCtMtl.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----MtlRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DeltaCtMtl.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----MtlRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DeltaCtMtl.p ~  OriginContam +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )

car::Anova(mod4, type = "3")
summary(mod4)


#' Mtl expression is significantly different according to fish origin. More specifically, HC fish have higher Mtl expression than LC fish.
#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----MtlSex------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtMtl.p ~ OriginContam + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
car::Anova(modSex, type = "3")
summary(modSex)
modMtl <- modSex
RmodMtl <- MuMIn::r.squaredGLMM(modMtl)

#' The sex effect is significant.
#' The modSex is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
## ----MtlOrigin---------------------------------------------------------------------------------------------------------
boxplot(DeltaCtMtl.p ~ OriginContam, data = dat2)
boxplot(DeltaCtMtl.p ~ Sex, data = dat2)
# compute mean and Se for LC and HC groups
meanVal <-
  data.frame(
    mean = c(mean(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "LC")]),
             mean(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "HC")])),
    Se = c(
      sd(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "LC")]) / sqrt(length(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "LC")])),
      sd(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "HC")]) / sqrt(length(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "HC")]))
    ),
    row.names = c("LC", "HC"),
    n = c(length(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "LC")]),
    length(dat2$DeltaCtMtl.p[which(dat2$OriginContam == "HC")]))
  )

kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' ### Cat
#' #### Check the data
## ----CatIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DeltaCtCat) == F, ]
# transform variable to approx. normality
dat2$DeltaCtCat.p <-
  bimixt::boxcox(dat2$DeltaCtCat, car::powerTransform(dat2$DeltaCtCat)$lambda)
hist(dat2$DeltaCtCat.p)

#' 
#' #### Visualize
## ----CatViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DeltaCtCat.p ~ OriginContam, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtCat.p ~ Injection, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----CatFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DeltaCtCat.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----CatRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DeltaCtCat.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----CatRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DeltaCtCat.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----CatRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DeltaCtCat.p ~  Injection + SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' Cat expression is significantly different according to immune challenge. More specifically, HC fish have higher Cat expression than LC fish. Also, bigger fish have higher expression of Cat.
#' 
## ----CatSize-----------------------------------------------------------------------------------------------------------
plot(DeltaCtCat.p ~ SizeEnd, data = dat2)
abline(a = -3.16488, b = 0.04676 , col = "red")

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----CatSex------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtCat.p ~ Injection + SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
car::Anova(modSex, type = "3")
summary(modSex)
modCat<- modSex
RmodCat <- MuMIn::r.squaredGLMM(modCat)

#' The sex effect is significant.
#' The modSex is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
## ----CatInjection------------------------------------------------------------------------------------------------------
boxplot(DeltaCtCat.p ~ OriginContam, data = dat2)
boxplot(DeltaCtCat.p ~ Sex, data = dat2)
# compute mean and Se for LC and HC groups
meanVal <-
  data.frame(
    mean = c(mean(dat2$DeltaCtCat.p[which(dat2$Injection == "PBS")]),
             mean(dat2$DeltaCtCat.p[which(dat2$Injection == "AMIX")])),
    Se = c(
      sd(dat2$DeltaCtCat.p[which(dat2$Injection == "PBS")]) / sqrt(length(dat2$DeltaCtCat.p[which(dat2$Injection == "PBS")])),
      sd(dat2$DeltaCtCat.p[which(dat2$Injection == "AMIX")]) / sqrt(length(dat2$DeltaCtCat.p[which(dat2$Injection == "AMIX")]))
    ),
    row.names = c("PBS", "AMIX"),
    n = c(length(dat2$DeltaCtCat.p[which(dat2$Injection == "PBS")]),
    length(dat2$DeltaCtCat.p[which(dat2$Injection == "AMIX")]))
  )

kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' ### GPX
#' #### Check the data
## ----GpxIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DeltaCtGpx) == F, ]
# transform variable to approx. normality
dat2$DeltaCtGpx.p <-
  bimixt::boxcox(dat2$DeltaCtGpx, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtGpx.p)

#' 
#' #### Visualize
## ----GpxViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DeltaCtGpx.p ~ OriginContam, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtGpx.p ~ Injection, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----GpxFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DeltaCtGpx.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----GpxRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DeltaCtGpx.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----GpxRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DeltaCtGpx.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----GpxRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DeltaCtGpx.p ~  OriginContam +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' Gpx expression is significantly different according to fish origin. More specifically, HC fish have higher Gpx expression than LC fish.
#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----GpxSex------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtGpx.p ~ OriginContam + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
car::Anova(modSex, type = "3")
summary(modSex)
modGpx <- modSex
RmodGpx <- MuMIn::r.squaredGLMM(modGpx)

#' The sex effect is significant.
#' The modSex is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
## ----GpxOrigin---------------------------------------------------------------------------------------------------------
boxplot(DeltaCtGpx.p ~ OriginContam, data = dat2)
boxplot(DeltaCtGpx.p ~ Sex, data = dat2)
# compute mean and Se for LC and HC groups
meanVal <-
  data.frame(
    mean = c(mean(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "LC")]),
             mean(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "HC")])),
    Se = c(
      sd(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "LC")]) / sqrt(length(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "LC")])),
      sd(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "HC")]) / sqrt(length(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "HC")]))
    ),
    row.names = c("LC", "HC"),
    n = c(length(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "LC")]),
    length(dat2$DeltaCtGpx.p[which(dat2$OriginContam == "HC")]))
  )

kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' ### Casp3
#' #### Check the data
## ----Casp3Index--------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DeltaCtCasp3) == F, ]
# transform variable to approx. normality
dat2$DeltaCtCasp3.p <-
  bimixt::boxcox(dat2$DeltaCtCasp3, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtCasp3.p)

#' 
#' #### Visualize
## ----Casp3Viz----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DeltaCtCasp3.p ~ OriginContam, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtCasp3.p ~ Injection, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----Casp3Full---------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DeltaCtCasp3.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----Casp3Refined1-----------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DeltaCtCasp3.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----Casp3Refined2-----------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DeltaCtCasp3.p ~ TransplantContam + (OriginContam + Injection)^2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----Casp3Refined3-----------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DeltaCtCasp3.p ~  TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' #### Refining the model
## ----Casp3Refined4-----------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    DeltaCtCasp3.p ~  OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)
modCasp3 <- mod5
RmodCasp3 <- MuMIn::r.squaredGLMM(modCasp3)

#' 
#' Casp3 expression is significantly different according to fish origin. More specifically, HC fish have higher Casp3 expression than LC fish. Also, the bigger the fish, the higher the level of Casp3 expression. 
#' 
## ----Casp3Size---------------------------------------------------------------------------------------------------------
plot(DeltaCtCasp3.p ~ SizeEnd, data = dat2)
abline(a = -5.19004, b = 0.21743, col = "red")

#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----Casp3Sex----------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtCasp3.p ~ OriginContam + SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is not significant.
#' The mod5 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
## ----Casp3Origin-------------------------------------------------------------------------------------------------------
boxplot(DeltaCtCasp3.p ~ OriginContam, data = dat2)
# compute mean and Se for LC and HC groups
meanVal <-
  data.frame(
    mean = c(mean(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "LC")]),
             mean(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "HC")])),
    Se = c(
      sd(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "LC")]) / sqrt(length(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "LC")])),
      sd(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "HC")]) / sqrt(length(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "HC")]))
    ),
    row.names = c("LC", "HC"),
    n = c(length(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "LC")]),
    length(dat2$DeltaCtCasp3.p[which(dat2$OriginContam == "HC")]))
  )

kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' ### Pcx
#' #### Check the data
## ----PcxIndex----------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DeltaCtPcx) == F, ]
# transform variable to approx. normality
dat2$DeltaCtPcx.p <-
  bimixt::boxcox(dat2$DeltaCtPcx, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtPcx.p)

#' 
#' #### Visualize
## ----PcxViz------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DeltaCtPcx.p ~ OriginContam, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtPcx.p ~ Injection, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----PcxFull-----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DeltaCtPcx.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----PcxRefined1-------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DeltaCtPcx.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----PcxRefined2-------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DeltaCtPcx.p ~ TransplantContam + (OriginContam + Injection)^2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----PcxRefined3-------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DeltaCtPcx.p ~  TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' #### Refining the model
## ----PcxRefined4-------------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    DeltaCtPcx.p ~  Injection +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )

car::Anova(mod5, type = "3")
summary(mod5)


#' 
#' Pcx expression is significantly different according to the immune challenge. More specifically, immune challenged fish injected with the antigen mixture (AMIX) have a lower Pcx gene expression compare to the fish injected with the control saline solution (PBS). 
#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----PcxSex------------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtPcx.p ~ Injection + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
car::Anova(modSex, type = "3")
summary(modSex)
modPcx <- modSex
RmodPcx <- MuMIn::r.squaredGLMM(modPcx)

#' The sex effect is significant.
#' The modSex is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
## ----PcxOrigin---------------------------------------------------------------------------------------------------------
boxplot(DeltaCtPcx.p ~ Injection, data = dat2)
# compute mean and Se for LC and HC groups
meanVal <-
  data.frame(
    mean = c(mean(dat2$DeltaCtPcx.p[which(dat2$Injection == "PBS")]),
             mean(dat2$DeltaCtPcx.p[which(dat2$Injection == "AMIX")])),
    Se = c(
      sd(dat2$DeltaCtPcx.p[which(dat2$Injection == "PBS")]) / sqrt(length(dat2$DeltaCtPcx.p[which(dat2$Injection == "PBS")])),
      sd(dat2$DeltaCtPcx.p[which(dat2$Injection == "AMIX")]) / sqrt(length(dat2$DeltaCtPcx.p[which(dat2$Injection == "AMIX")]))
    ),
    row.names = c("PBS", "AMIX"),
    n = c(length(dat2$DeltaCtPcx.p[which(dat2$Injection == "PBS")]),
    length(dat2$DeltaCtPcx.p[which(dat2$Injection == "AMIX")]))
  )

kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' ### Pygl
#' #### Check the data
## ----PyglIndex---------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$DeltaCtPygl) == F, ]
# transform variable to approx. normality
dat2$DeltaCtPygl.p <-
  bimixt::boxcox(dat2$DeltaCtPygl, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtPygl.p)

#' 
#' #### Visualize
## ----PyglViz-----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(DeltaCtPygl.p ~ OriginContam, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtPygl.p ~ Injection, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----PyglFull----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    DeltaCtPygl.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----PyglRefined1------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    DeltaCtPygl.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----PyglRefined2------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    DeltaCtPygl.p ~ (TransplantContam + OriginContam)^2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----PyglRefined3------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    DeltaCtPygl.p ~  TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' #### Refining the model
## ----PyglRefined4------------------------------------------------------------------------------------------------------
mod5 <-
 lme4::lmer(
    DeltaCtPygl.p ~  Injection +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
car::Anova(mod5, type = "3")
summary(mod5)
modPygl <- mod5
RmodPygl <- MuMIn::r.squaredGLMM(modPygl)

#' 
#' Pygl expression is significantly different according to the immune challenge. More specifically, immune challenged fish injected with the antigen mixture (AMIX) have a lower Pygl gene expression compare to the fish injected with the control saline solution (PBS). 
#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----PyglSex-----------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtPygl.p ~ Injection + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )

car::Anova(modSex, type = "3")
summary(modSex)


#' The sex effect is not significant.
#' The mod5 is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
## ----PyglOrigin--------------------------------------------------------------------------------------------------------
boxplot(DeltaCtPygl.p ~ Injection, data = dat2)
# compute mean and Se for LC and HC groups
meanVal <-
  data.frame(
    mean = c(mean(dat2$DeltaCtPygl.p[which(dat2$Injection == "PBS")]),
             mean(dat2$DeltaCtPygl.p[which(dat2$Injection == "AMIX")])),
    Se = c(
      sd(dat2$DeltaCtPygl.p[which(dat2$Injection == "PBS")]) / sqrt(length(dat2$DeltaCtPygl.p[which(dat2$Injection == "PBS")])),
      sd(dat2$DeltaCtPygl.p[which(dat2$Injection == "AMIX")]) / sqrt(length(dat2$DeltaCtPygl.p[which(dat2$Injection == "AMIX")]))
    ),
    row.names = c("PBS", "AMIX"),
    n = c(length(dat2$DeltaCtPygl.p[which(dat2$Injection == "PBS")]),
    length(dat2$DeltaCtPygl.p[which(dat2$Injection == "AMIX")]))
  )

kableExtra::kable_classic(
  kableExtra::kbl(meanVal),
  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
  full_width = F,
  html_font = "arial",
  font_size = 10
)

#' 
#' ### CqActb (β-actin)
#' #### Check the data
## ----ActbIndex---------------------------------------------------------------------------------------------------------
dat2 = dat1[is.na(dat1$CqActb) == F, ]
# transform variable to approx. normality
dat2$CqActb.p <-
  bimixt::boxcox(dat2$CqActb, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$CqActb.p)

#' 
#' #### Visualize
## ----ActbViz-----------------------------------------------------------------------------------------------------------
par(mfrow=c(2,3))
boxplot(CqActb.p ~ OriginContam, data = dat2)
boxplot(CqActb.p ~ TransplantContam, data = dat2)
boxplot(CqActb.p ~ Injection, data = dat2)
boxplot(CqActb.p ~ TransplantContam * Injection, data = dat2)
boxplot(CqActb.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(CqActb.p ~ TransplantContam * OriginContam * Injection, data = dat2)

#' 
#' #### Build the full model
## ----ActbFull----------------------------------------------------------------------------------------------------------
modfull <-
 lme4::lmer(
    CqActb.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
summary(modfull)

#' #### Refining the model
## ----ActbRefined1------------------------------------------------------------------------------------------------------
mod2 <-
 lme4::lmer(
    CqActb.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
summary(mod2)

#' #### Refining the model
## ----ActbRefined2------------------------------------------------------------------------------------------------------
mod3 <-
 lme4::lmer(
    CqActb.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
summary(mod3)


#' #### Refining the model
## ----ActbRefined3------------------------------------------------------------------------------------------------------
mod4 <-
 lme4::lmer(
    CqActb.p ~  TransplantContam + SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
summary(mod4)


#' 
#' Now that we have identified the best model, add sex as covariate to check whether results are sensitive
#' NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model
#' 
#' #### Test for the sex effect
## ----ActbSex-----------------------------------------------------------------------------------------------------------
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    CqActb.p ~  TransplantContam + SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
car::Anova(modSex, type = "3")
summary(modSex)
modActb <- modSex
RmodActb <- MuMIn::r.squaredGLMM(modActb)

#' The sex effect is  significant.
#' The modSex is thus the best model.
#' Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct
#' 
#' 
#' ## Summary of the best models for gene expression (Table 4 from the manuscript)
#' 
#' The following table report the results of the best models tested above, this table correspond to the Table 4 in the manuscript. The table report the marginal and conditional Rsquared (R2m and R2c respectively).
## ----summaryStatsGeneExpression,  echo=FALSE---------------------------------------------------------------------------
GenesExpTable()


#' 
#' # R session informations
#' 
## ----sessionInfos------------------------------------------------------------------------------------------------------
sessionInfo()

#' 
#' # References
#' 
#' Costantini, D., Dell’Omo, G., 2006. Effects of T-cell-mediated immune response on avian oxidative stress. Comp. Biochem. Physiol. A. Mol. Integr. Physiol. 145, 137–142. https://doi.org/10.1016/j.cbpa.2006.06.002.
#' 
#' Jacquin, L., Reader, S.M., Boniface, A., Mateluna, J., Patalas, I., Pérez-Jvostov, F., Hendry, A.P., 2016. Parallel and nonparallel behavioural evolution in response to parasitism and predation in Trinidadian guppies. J. Evol. Biol. 29, 1406–1422. https://doi.org/10.1111/jeb.12880.
#' 
#' Petitjean, Q., Jacquin, L., Riem, L., Pitout, M., Perrault, A., Cousseau, M., Laffaille, P., Jean, S., 2020a. Intraspecific variability of responses to combined metal contamination and immune challenge among wild fish populations. Environ. Pollut. 116042. https://doi.org/10.1016/j.envpol.2020.116042.
#' 
#' Petitjean, Q., Jean, S., Côte, J., Larcher, T., Angelier, F., Ribout, C., Perrault, A., Laffaille, P., Jacquin, L., 2020b. Direct and indirect effects of multiple environmental stressors on fish health in human-altered rivers. Sci. Total Environ. 742, 140657. https://doi.org/10.1016/j.scitotenv.2020.140657.
