---
title: "'Adaptive plastic responses to metal contamination in a multistress context: a field experiment in fish'" 
author: "Quentin PETITJEAN[q.petitjean1@gmail.com], Pascal LAFFAILLE, Annie PERRAULT, Myriam COUSSEAU, Séverine JEAN, Lisa JACQUIN"
date: "25/06/2022"
output: 
  html_document:
    keep_md: true
    toc: TRUE
    toc_depth: 3
    toc_float: TRUE
    number_sections: TRUE
    use_fontawesome: TRUE
---



# Load libraries 

```r
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
```

# Load Some custom functions (making tables and drawing plots)

```r
# load some custom functions used to draw the plots included within the MS
source("https://raw.githubusercontent.com/qpetitjean/Adaptive-plastic-responses-of-fish-in-a-multistress-context/main/Rscript/SourcePlots_PlasticResponsesFishMS.R")

# load some custom functions used to draw the statistic tables included within the MS
source("https://raw.githubusercontent.com/qpetitjean/Adaptive-plastic-responses-of-fish-in-a-multistress-context/main/Rscript/SourceTables_PlasticResponsesFishMS.R")
```

# Load the dataset

```r
dat1 <- read.csv2("https://raw.githubusercontent.com/qpetitjean/Adaptive-plastic-responses-of-fish-in-a-multistress-context/main/Dataset/Dataset_Petitjeanetal-Adaptive_plastic_responses_of_fish_in_multistress_context.csv", dec = ".", sep = ";")

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
```

```
##     ColorMark IndID Escaped OriginSite CagingSite OriginContam     SitesPair
## 1    UNMARKED     1      No     AUSCOR     ARIMAS           HC AUSCOR_ARIMAS
## 2 WHITE_GREEN     2      No     AUSCOR     ARIMAS           HC AUSCOR_ARIMAS
## 3 PINK_ORANGE     3      No     AUSCOR     ARIMAS           HC AUSCOR_ARIMAS
## 4    PINK_RED     4      No     AUSCOR     ARIMAS           HC AUSCOR_ARIMAS
## 5  PINK_GREEN     5      No     AUSCOR     ARIMAS           HC AUSCOR_ARIMAS
##   CageID CageSiteID TransplantContam Transplant Injection  Treatment
## 1      6          6               LC      HC_LC      AMIX HC_LC_AMIX
## 2      6          6               LC      HC_LC      AMIX HC_LC_AMIX
## 3      6          6               LC      HC_LC       PBS  HC_LC_PBS
## 4      6          6               LC      HC_LC      AMIX HC_LC_AMIX
## 5      6          6               LC      HC_LC       PBS  HC_LC_PBS
##   WeightStart SizeStart Weight7D Size7D Weight9D Size9D WeightEnd SizeEnd
## 1         4.9       7.6      4.7    7.5      4.6    7.5       4.5     7.5
## 2         6.0       7.8      5.9    7.6      5.5    8.0       5.6     7.9
## 3         7.6       8.9      7.3    8.7      7.2    8.6       7.1     8.7
## 4         4.3       7.0      3.9    6.9      3.7    7.0       3.7     6.8
## 5         5.7       8.0      5.5    7.5      5.4    7.5       5.2     7.7
##   DailyMassChange Death DeathDate   MarkDate InjectionDate EuthanasiaDate
## 1      -0.5830904     0      <NA> 10/09/2018    17/09/2018     24/09/2018
## 2      -0.4761905     0      <NA> 10/09/2018    17/09/2018     24/09/2018
## 3      -0.4699248     0      <NA> 10/09/2018    17/09/2018     24/09/2018
## 4      -0.9966777     0      <NA> 10/09/2018    17/09/2018     24/09/2018
## 5      -0.6265664     0      <NA> 10/09/2018    17/09/2018     24/09/2018
##   DaysAlive InjectionTime Pcaud1D7 Pcaud2D7 Pcaud3D7 MeanPcaudD7  PcaudDate
## 1        14      09:45:00    2.012    2.014    2.018    2.014667 19/09/2018
## 2        14      09:45:00    2.022    2.012    2.006    2.013333 19/09/2018
## 3        14      09:45:00    2.210    2.198    2.184    2.197333 19/09/2018
## 4        14      09:45:00    2.020    2.002    2.000    2.007333 19/09/2018
## 5        14      09:45:00    2.022    2.028    2.016    2.022000 19/09/2018
##   PcaudTime Pcaud1D9 Pcaud2D9 Pcaud3D9 MeanPcaudD9          IR   IRTime
## 1  12:30:00    2.286    2.266    2.282    2.278000  13.0708140 47.88542
## 2  12:30:00    2.422    2.412    2.404    2.412667  19.8344371 47.88542
## 3  12:30:00    2.710    2.724    2.728    2.720667  23.8167476 47.88542
## 4  12:30:00    2.020    2.012    2.036    2.022667   0.7638658 47.88542
## 5  12:30:00    1.630    1.654    1.626    1.636667 -19.0570392 47.88542
##   GonadsWeight Sex LiverWeight       GSI       HSI Lymphocytes Monocytes
## 1        0.079   F       0.051 1.7555556 1.1333333          NA        NA
## 2        0.074   F       0.056 1.3214286 1.0000000          91         8
## 3        0.174   F       0.029 2.4507042 0.4084507          93         3
## 4        0.052   F       0.022 1.4054054 0.5945946          87        13
## 5        0.012   M       0.030 0.2307692 0.5769231          NA        NA
##   Neutrophils   mMH2O2 mMHCLO MuscleCarbohydrate MuscleProtein MuscleLipid
## 1          NA       NA     NA           37.34316      759.5371    1901.281
## 2           1       NA     NA           40.93106     1547.8848    5235.155
## 3           4 4.500544     NA           44.72313     1265.5788    6348.720
## 4           0       NA     NA           19.59270     1287.4869    3870.738
## 5          NA       NA     NA           29.53739     1024.9727    1745.356
##   AvailableEnerJ   CqActb DeltaCtCat DeltaCtCasp3 DeltaCtGpx DeltaCtMtl
## 1       2698.161 24.41974 0.07138744  0.005951188  0.3118747  0.6621643
## 2       6823.971 26.45246 0.03845321  0.026903646  0.6009854  9.4992353
## 3       7659.022 25.32390 0.03655256  0.006485931  0.6950857 14.5623734
## 4       5177.818 25.37811 0.03377133  0.006698988  0.2555632  0.7285995
## 5       2799.866 24.00830 0.04684323  0.007794697  0.3911237  2.4443257
##    DeltaCtPcx DeltaCtPygl  MuscleAl   MuscleCr    MuscleCo  MuscleNi  MuscleCu
## 1 0.037076525  0.10068714 26.497849 0.35363824 0.022987203 0.2452521 10.917660
## 2 0.006979170  0.03752741  1.313570 0.12796698 0.011615140 1.6162667  4.794625
## 3 0.017903927  0.06595523  3.016400 0.11952862 0.011496152 0.0322936  5.239017
## 4 0.007300098  0.03637586  3.657936 0.05860145 0.009343916 0.3097805  1.422597
## 5 0.020840611  0.06521035 11.235868 0.29147406 0.017189684 0.2715557  3.836860
##    MuscleZn  MuscleAs    MuscleCd          CxOxI     CxO2xI       CxO  CxO2
## 1 212.00439 0.5486982 0.013927432 LC_AUSCOR_AMIX LC_HC_AMIX LC_AUSCOR LC_HC
## 2  78.75929 0.6634702 0.009095777 LC_AUSCOR_AMIX LC_HC_AMIX LC_AUSCOR LC_HC
## 3  84.98429 0.3719471 0.003173364  LC_AUSCOR_PBS  LC_HC_PBS LC_AUSCOR LC_HC
## 4 108.06563 0.4981519 0.001854870 LC_AUSCOR_AMIX LC_HC_AMIX LC_AUSCOR LC_HC
## 5 143.04142 0.4685862 0.004087401  LC_AUSCOR_PBS  LC_HC_PBS LC_AUSCOR LC_HC
##       CxI    O2xI
## 1 LC_AMIX HC_AMIX
## 2 LC_AMIX HC_AMIX
## 3  LC_PBS  HC_PBS
## 4 LC_AMIX HC_AMIX
## 5  LC_PBS  HC_PBS
```
# Check sample size per treatments 
## Including escaped fish

```r
tab1 <- data.frame(table(dat1$OriginSite, dat1$CagingSite, dat1$OriginContam, dat1$TransplantContam, dat1$Injection))
names(tab1) <- c("Origin site",	"Transplant site",	"Contamination level in origin site", 	"Contamination level in transplant site",	"Immune challenge", "Sample size")
tab1 <- tab1[which(tab1[["Sample size"]]>0),]
tab1 <- tab1[order(tab1[["Origin site"]], tab1[["Transplant site"]]),]
rownames(tab1) <- NULL
kableExtra::kable_classic(kableExtra::kbl(tab1), bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F,  html_font = "arial", font_size = 10)
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Origin site </th>
   <th style="text-align:left;"> Transplant site </th>
   <th style="text-align:left;"> Contamination level in origin site </th>
   <th style="text-align:left;"> Contamination level in transplant site </th>
   <th style="text-align:left;"> Immune challenge </th>
   <th style="text-align:right;"> Sample size </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 21 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
</tbody>
</table>

```r
# total sample size: 
sum(tab1[["Sample size"]])
```

```
## [1] 274
```
## Excluding escaped fish

```r
dat1bis = dat1[dat1$Escaped == "No", ]
tab2 <- data.frame(table(dat1bis$OriginSite, dat1bis$CagingSite, dat1bis$OriginContam, dat1bis$TransplantContam, dat1bis$Injection))
names(tab2) <- c("Origin site",	"Transplant site",	"Contamination level in origin site", 	"Contamination level in transplant site",	"Immune challenge", "Sample size")
tab2 <- tab2[which(tab2[["Sample size"]]>0),]
tab2 <- tab2[order(tab2[["Origin site"]], tab2[["Transplant site"]]),]
rownames(tab2) <- NULL
kableExtra::kable_classic(kableExtra::kbl(tab2), bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F,  html_font = "arial",  font_size = 10)
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Origin site </th>
   <th style="text-align:left;"> Transplant site </th>
   <th style="text-align:left;"> Contamination level in origin site </th>
   <th style="text-align:left;"> Contamination level in transplant site </th>
   <th style="text-align:left;"> Immune challenge </th>
   <th style="text-align:right;"> Sample size </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 20 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> ARIMAS </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> AUSCOR </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 14 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> CELFIG </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 18 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> RIOU </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 16 </td>
  </tr>
</tbody>
</table>

```r
# total sample size: 
sum(tab2[["Sample size"]])
```

```
## [1] 268
```

# Test some basics 
 here we will test the design of the experiment.\
 A quick reminder:\

 - Treatments (fixed effects):\
    - TransplantContam (transplant treatment either High contamination-HC or Low contamination-LC)\
    - OriginContam (Origin of the population either High contamination-HC or Low contamination-LC)\
    - Injection (control saline: PBS, antigen mixture: AMIX)\

 - Covariates :\
    - SizeEnd (Fish size at the end of the experiment)\
    - Sex (Fish sex determined by visual inspection at the end of the experiment)\

 - Blocks (random effects):\
    - CagingSite (The name of the study site where fish are caged, either ARIMAS, AUSCOR, CELFIG or RIOU)\
    - CageSiteID (The ID number ranging from 1 to 6 corresponding to the number of the cage within each study site)\
    - OriginSite (The name of the site where each population were sampled = population ID)\

## Test Distribution of fish sex among cages\
sex determination was performed at the end of the experiment: visual inspection of the gonads


```r
dat2 = dat1[is.na(dat1$Sex) == F, ]
sexTabCage = table(dat2$CageID, dat2$Sex)
sexTabOrigin = table(dat2$OriginContam, dat2$Sex)
sexTabTransplant = table(dat2$TransplantContam, dat2$Sex)
sexTabInj = table(dat2$Injection, dat2$Sex)
sexTabTriple = table(dat2$CxO2xI, dat2$Sex)
```
### Visualize the data

```r
par(mfrow=c(3,2))
plot(sexTabCage, main = "Cage number", ylab = "sex")
plot(sexTabOrigin, main = "Origin site", ylab = "sex")
plot(sexTabTransplant, main = "Transplant site", ylab = "sex")
plot(sexTabInj, main = "Injection", ylab = "sex")
plot(sexTabTriple, main = "All_treatments", ylab = "sex")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/SexViz-1.png)<!-- -->
### Test sex distribution among cages using Chi square test

```r
chi = chisq.test(sexTabCage)
```

```
## Warning in chisq.test(sexTabCage): Chi-squared approximation may be incorrect
```

```r
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  sexTabCage
## X-squared = 31.578, df = 23, p-value = 0.1093
```
While some cages contains more female than male, the global pattern is not significant\
since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :\

```r
fisher.test(sexTabCage, simulate.p.value = TRUE)
```

```
## 
## 	Fisher's Exact Test for Count Data with simulated p-value (based on
## 	2000 replicates)
## 
## data:  sexTabCage
## p-value = 0.07696
## alternative hypothesis: two.sided
```
Fisher test is non-significant, which confirm the result of the Chisquare-test

### What about sex distribution among treatments: origin ?

```r
chi = chisq.test(sexTabOrigin)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  sexTabOrigin
## X-squared = 0.11037, df = 1, p-value = 0.7397
```
As observed on the plot and as confirmed by chisquare test,\
distribution of male and female is balanced among populations origin

### What about sex distribution among treatments: TransplantContam ?

```r
chi = chisq.test(sexTabTransplant)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  sexTabTransplant
## X-squared = 0.11037, df = 1, p-value = 0.7397
```
As observed on the plot and as confirmed by chisquare test,\
distribution of male and female is balanced between TransplantContam treatments

### What about sex distribution among treatments: Injection ?

```r
chi = chisq.test(sexTabInj)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  sexTabInj
## X-squared = 1.3894, df = 1, p-value = 0.2385
```
As observed on the plot and as confirmed by chisquare test,\
distribution of male and female is balanced between Injection treatments

### What about sex distribution among treatments: triple combination of treatments ?

```r
chi = chisq.test(sexTabTriple)
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  sexTabTriple
## X-squared = 7.7457, df = 7, p-value = 0.3556
```
As observed on the plot and as confirmed by chisquare test,\
distribution of male and female is more or less balanced among treatments

## Test distribution of fish weight among treatments (at the beginning of the experiment, using WeightStart)
Visualize the data

```r
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
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/fishWeightViz-1.png)<!-- -->
Create the full model

```r
mod1 <- lme4::lmer(
  log(WeightStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2
)
performance::check_model(mod1)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/fishWeightModelFull-1.png)<!-- -->

```r
car::Anova(mod1, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(WeightStart)
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             49.1325  1  2.392e-12 ***
## TransplantContam                         0.7673  1     0.3810    
## OriginContam                             0.0335  1     0.8548    
## Injection                                0.2716  1     0.6023    
## TransplantContam:OriginContam            0.0575  1     0.8104    
## TransplantContam:Injection               0.0075  1     0.9308    
## OriginContam:Injection                   0.0177  1     0.8941    
## TransplantContam:OriginContam:Injection  0.0003  1     0.9871    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(WeightStart) ~ TransplantContam * OriginContam * Injection +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 233.5
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.79246 -0.64175 -0.02739  0.62911  2.92879 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.005362 0.07323 
##  CagingSite            (Intercept) 0.009039 0.09507 
##  OriginSite            (Intercept) 0.238754 0.48863 
##  Residual                          0.117980 0.34348 
## Number of obs: 274, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     2.510397   0.358144   7.009
## TransplantContamLC                             -0.116490   0.132982  -0.876
## OriginContamLC                                 -0.090998   0.497147  -0.183
## InjectionPBS                                    0.043809   0.084066   0.521
## TransplantContamLC:OriginContamLC               0.031222   0.130154   0.240
## TransplantContamLC:InjectionPBS                -0.010287   0.118470  -0.087
## OriginContamLC:InjectionPBS                    -0.015510   0.116522  -0.133
## TransplantContamLC:OriginContamLC:InjectionPBS -0.002688   0.166541  -0.016
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.187                                              
## OrignCntmLC -0.695  0.066                                       
## InjectinPBS -0.116  0.312  0.083                                
## TrnCLC:OCLC  0.094 -0.500 -0.130 -0.319                         
## TrnCLC:IPBS  0.082 -0.436 -0.059 -0.710  0.445                  
## OrgCLC:IPBS  0.083 -0.225 -0.114 -0.722  0.436      0.512       
## TCLC:OCLC:I -0.058  0.310  0.080  0.505 -0.617     -0.712 -0.700
```
now, refine the model using AIC

```r
mod1 =lme4::lmer(
  log(WeightStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
  REML = FALSE,
  na.action = "na.fail"
)
MuMIn::dredge(mod1, rank = "AIC")
```

```
## Global model call: lme4::lmer(formula = log(WeightStart) ~ TransplantContam * OriginContam * 
##     Injection + (1 | CagingSite/CageSiteID) + (1 | OriginSite), 
##     data = dat2, REML = FALSE, na.action = "na.fail")
## ---
## Model selection table 
##     (Int) Inj OrC TrC Inj:OrC Inj:TrC OrC:TrC Inj:OrC:TrC df   logLik   AIC
## 1   2.429                                                  5 -108.035 226.1
## 5   2.483           +                                      6 -107.337 226.7
## 2   2.414   +                                              6 -107.764 227.5
## 3   2.471       +                                          6 -108.007 228.0
## 6   2.468   +       +                                      7 -107.065 228.1
## 7   2.524       +   +                                      7 -107.308 228.6
## 4   2.456   +   +                                          7 -107.736 229.5
## 8   2.509   +   +   +                                      8 -107.037 230.1
## 22  2.465   +       +               +                      8 -107.056 230.1
## 39  2.532       +   +                       +              8 -107.264 230.5
## 12  2.452   +   +           +                              8 -107.715 231.4
## 40  2.517   +   +   +                       +              9 -106.992 232.0
## 16  2.505   +   +   +       +                              9 -107.015 232.0
## 24  2.507   +   +   +               +                      9 -107.027 232.1
## 48  2.513   +   +   +       +               +             10 -106.970 233.9
## 56  2.514   +   +   +               +       +             10 -106.983 234.0
## 32  2.502   +   +   +       +       +                     10 -107.004 234.0
## 64  2.510   +   +   +       +       +       +             11 -106.961 235.9
## 128 2.510   +   +   +       +       +       +           + 12 -106.961 237.9
##     delta weight
## 1    0.00  0.245
## 5    0.60  0.181
## 2    1.46  0.118
## 3    1.94  0.093
## 6    2.06  0.087
## 7    2.55  0.069
## 4    3.40  0.045
## 8    4.00  0.033
## 22   4.04  0.032
## 39   4.46  0.026
## 12   5.36  0.017
## 40   5.91  0.013
## 16   5.96  0.012
## 24   5.98  0.012
## 48   7.87  0.005
## 56   7.90  0.005
## 32   7.94  0.005
## 64   9.85  0.002
## 128 11.85  0.001
## Models ranked by AIC(x) 
## Random terms (all models): 
## '1 | CagingSite/CageSiteID', '1 | OriginSite'
```
The best model is the null model meaning that fish mass were not significantly different among treatments\
run the model with simple treatments effect to obtain NS p-value (reported in material & methods)

```r
mod2 = lme4::lmer(
  log(WeightStart) ~ TransplantContam + OriginContam + Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
)
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(WeightStart)
##                    Chisq Df Pr(>Chisq)    
## (Intercept)      49.8378  1   1.67e-12 ***
## TransplantContam  0.9723  1     0.3241    
## OriginContam      0.0290  1     0.8649    
## Injection         0.5419  1     0.4617    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(WeightStart) ~ TransplantContam + OriginContam + Injection +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 222.9
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.77716 -0.63120 -0.03151  0.63460  2.92805 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.004712 0.06864 
##  CagingSite            (Intercept) 0.009156 0.09569 
##  OriginSite            (Intercept) 0.238929 0.48880 
##  Residual                          0.116574 0.34143 
## Number of obs: 274, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)         2.50934    0.35545   7.060
## TransplantContamLC -0.10643    0.10793  -0.986
## OriginContamLC     -0.08362    0.49135  -0.170
## InjectionPBS        0.03044    0.04135   0.736
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.152              
## OrignCntmLC -0.691  0.001       
## InjectinPBS -0.057  0.004  0.001
```
Compute mean and Sd of fish Weight (reported in material & methods)

```r
mean(dat2$WeightStart)
```

```
## [1] 12.93686
```

```r
sd(dat2$WeightStart)
```

```
## [1] 6.933831
```

## Test Distribution of fish size among treatments (at the beginning of the experiment, using SizeStart)
Visualize the data

```r
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
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/fishSizeViz-1.png)<!-- -->
Create the full model

```r
mod1 <- lme4::lmer(
  log(SizeStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2
)
performance::check_model(mod1)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/fishSizeModelFull-1.png)<!-- -->

```r
car::Anova(mod1, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(SizeStart)
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             509.4453  1     <2e-16 ***
## TransplantContam                          0.8929  1     0.3447    
## OriginContam                              0.0191  1     0.8900    
## Injection                                 0.1480  1     0.7005    
## TransplantContam:OriginContam             0.0666  1     0.7964    
## TransplantContam:Injection                0.0073  1     0.9318    
## OriginContam:Injection                    0.1812  1     0.6703    
## TransplantContam:OriginContam:Injection   0.0679  1     0.7945    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod1)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(SizeStart) ~ TransplantContam * OriginContam * Injection +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -336.1
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -3.01022 -0.72980 -0.01529  0.62898  2.69373 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0002450 0.01565 
##  CagingSite            (Intercept) 0.0007335 0.02708 
##  OriginSite            (Intercept) 0.0192863 0.13888 
##  Residual                          0.0141330 0.11888 
## Number of obs: 274, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     2.309201   0.102309  22.571
## TransplantContamLC                             -0.038222   0.040448  -0.945
## OriginContamLC                                 -0.019636   0.141983  -0.138
## InjectionPBS                                    0.011188   0.029083   0.385
## TransplantContamLC:OriginContamLC               0.010833   0.041983   0.258
## TransplantContamLC:InjectionPBS                -0.003507   0.040977  -0.086
## OriginContamLC:InjectionPBS                    -0.017157   0.040302  -0.426
## TransplantContamLC:OriginContamLC:InjectionPBS  0.015005   0.057604   0.260
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.199                                              
## OrignCntmLC -0.695  0.080                                       
## InjectinPBS -0.140  0.354  0.101                                
## TrnCLC:OCLC  0.107 -0.532 -0.146 -0.342                         
## TrnCLC:IPBS  0.099 -0.496 -0.072 -0.710  0.478                  
## OrgCLC:IPBS  0.101 -0.256 -0.138 -0.722  0.468      0.512       
## TCLC:OCLC:I -0.071  0.353  0.097  0.505 -0.662     -0.712 -0.700
```
now, refine the model using AIC

```r
mod1 =lme4::lmer(
  log(SizeStart) ~ TransplantContam * OriginContam * Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
  REML = FALSE,
  na.action = "na.fail"
)
MuMIn::dredge(mod1, rank = "AIC")
```

```
## Global model call: lme4::lmer(formula = log(SizeStart) ~ TransplantContam * OriginContam * 
##     Injection + (1 | CagingSite/CageSiteID) + (1 | OriginSite), 
##     data = dat2, REML = FALSE, na.action = "na.fail")
## ---
## Model selection table 
##     (Int) Inj OrC TrC Inj:OrC Inj:TrC OrC:TrC Inj:OrC:TrC df  logLik    AIC
## 1   2.285                                                  5 185.981 -362.0
## 5   2.301           +                                      6 186.677 -361.4
## 2   2.283   +                                              6 186.029 -360.1
## 3   2.295       +                                          6 185.999 -360.0
## 6   2.299   +       +                                      7 186.724 -359.4
## 7   2.310       +   +                                      7 186.695 -359.4
## 4   2.292   +   +                                          7 186.047 -358.1
## 39  2.315       +   +                       +              8 186.863 -357.7
## 8   2.308   +   +   +                                      8 186.742 -357.5
## 22  2.300   +       +               +                      8 186.736 -357.5
## 12  2.290   +   +           +                              8 186.110 -356.2
## 40  2.312   +   +   +                       +              9 186.911 -355.8
## 16  2.305   +   +   +       +                              9 186.808 -355.6
## 24  2.309   +   +   +               +                      9 186.754 -355.5
## 48  2.310   +   +   +       +               +             10 186.976 -354.0
## 56  2.314   +   +   +               +       +             10 186.924 -353.8
## 32  2.306   +   +   +       +       +                     10 186.817 -353.6
## 64  2.311   +   +   +       +       +       +             11 186.986 -352.0
## 128 2.309   +   +   +       +       +       +           + 12 187.024 -350.0
##     delta weight
## 1    0.00  0.265
## 5    0.61  0.195
## 2    1.90  0.102
## 3    1.96  0.099
## 6    2.51  0.075
## 7    2.57  0.073
## 4    3.87  0.038
## 39   4.24  0.032
## 8    4.48  0.028
## 22   4.49  0.028
## 12   5.74  0.015
## 40   6.14  0.012
## 16   6.35  0.011
## 24   6.46  0.010
## 48   8.01  0.005
## 56   8.12  0.005
## 32   8.33  0.004
## 64   9.99  0.002
## 128 11.91  0.001
## Models ranked by AIC(x) 
## Random terms (all models): 
## '1 | CagingSite/CageSiteID', '1 | OriginSite'
```
The best model is the null model meaning that fish mass were not significantly different among treatments\
run the model with simple treatments effect to obtain NS p-value (reported in material & methods)

```r
mod2 = lme4::lmer(
  log(SizeStart) ~ TransplantContam + OriginContam + Injection +
  (1 | CagingSite / CageSiteID) + (1 | OriginSite),
  data = dat2, 
)
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(SizeStart)
##                     Chisq Df Pr(>Chisq)    
## (Intercept)      519.3942  1     <2e-16 ***
## TransplantContam   0.9642  1     0.3261    
## OriginContam       0.0186  1     0.8916    
## Injection          0.0953  1     0.7575    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(SizeStart) ~ TransplantContam + OriginContam + Injection +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -355
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -3.00146 -0.72982 -0.02693  0.63527  2.70416 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0002086 0.01444 
##  CagingSite            (Intercept) 0.0007474 0.02734 
##  OriginSite            (Intercept) 0.0193006 0.13893 
##  Residual                          0.0139709 0.11820 
## Number of obs: 274, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                     Estimate Std. Error t value
## (Intercept)         2.307936   0.101269  22.790
## TransplantContamLC -0.030846   0.031412  -0.982
## OriginContamLC     -0.019052   0.139785  -0.136
## InjectionPBS        0.004417   0.014307   0.309
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.156              
## OrignCntmLC -0.691  0.001       
## InjectinPBS -0.070  0.004  0.001
```
Compute mean and Sd of fish size (reported in material & methods)

```r
mean(dat2$SizeStart)
```

```
## [1] 9.962044
```

```r
sd(dat2$SizeStart)
```

```
## [1] 1.589071
```
##  Test for Crowding
### Compute mean and Sd of fish density per liter (reported in material & methods)

- At the start of the experiment

```r
Crowd = aggregate(dat1$WeightStart, list(dat1$CageID), sum)
Crowd$dens = Crowd$x / 130 # 130 is the volume of the cages in liters
mean(Crowd$dens) # expressed in grams of fish per liter of water within the cage
```

```
## [1] 1.136122
```

```r
sd(Crowd$dens)
```

```
## [1] 0.4599962
```
- At the end of the experiment

```r
dat2 = dat1[is.na(dat1$Death) == F, ]
dat2 = dat2[-c(which(dat2$Death == 1)), ]
Crowd2 = aggregate(dat2$WeightEnd, list(dat2$CageID), function(x)
  sum(x, na.rm = T))
Crowd2$dens = Crowd2$x / 130 # 130 is the volume of the cages in liters
mean(Crowd2$dens)
```

```
## [1] 0.9001603
```

```r
sd(Crowd2$dens)
```

```
## [1] 0.3783853
```
### Test for the difference between start and end of the experiment

```r
CrowdTest = cbind(Crowd$dens, Crowd2$dens)
chi = chisq.test(CrowdTest)
```

```
## Warning in chisq.test(CrowdTest): Chi-squared approximation may be incorrect
```

```r
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  CrowdTest
## X-squared = 0.47726, df = 23, p-value = 1
```

```r
# Here the test confirmed that density of fish is not significantly different among cages
# but since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :
fisher.test(round(CrowdTest, 1)*10, simulate.p.value = TRUE) # Round values to the first decimal and multiply by 10 to have integer only (necessary for fisher-test)
```

```
## 
## 	Fisher's Exact Test for Count Data with simulated p-value (based on
## 	2000 replicates)
## 
## data:  round(CrowdTest, 1) * 10
## p-value = 1
## alternative hypothesis: two.sided
```
Fisher test is non-significant, confirming the result of the Chisquare-test: The density of fish is not significantly different among cages between the start and end of the experiment, hence, While there was some death in several cages, the global pattern is not significant

## Check Toxic Unit (TU) consistency among studies and between the start and end of the experiment
Retrieve the TU reported in PETITJEAN et al. 2020 a,b (based on water agency database)

```r
Tu2020 = c(-0.9,-0.9,-0.3, 1.3)
```

Retrieve the global TU reported in the present study\
based on water samples collected in this study, both at the start and end of the experiment

```r
TuStart = c(-0.4,-0.5,-0.1, 1.1)
TuEnd = c(-0.3,-0.7, 0.3, 0.9)
TuGlobal = c(-0.4,-0.6, 0.2, 1.0)
Tutab = cbind(Tu2020, TuGlobal)
Tutab2 = cbind(TuStart, TuEnd)
```

### Visualize

```r
par(mfrow = c(1, 2))
plot(Tutab,
     xlab = "TU2020",
     ylab = "TUGlobal",
     main = "Global TU vs. previous studies")
plot(Tutab2,
     xlab = "TuStart",
     ylab = "TuEnd",
     main = "TU measured at the end of this study \n vs. \n TU measured at the start of this study")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/TUConsistency-1.png)<!-- -->

### Test the difference among global and TU reported in this study and reported in PETITJEAN et al. 2020 a,b

```r
chi = chisq.test(Tutab + 1) # add 1 to each values to ensure TU is positive (necessary for chi-test)
```

```
## Warning in chisq.test(Tutab + 1): Chi-squared approximation may be incorrect
```

```r
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  Tutab + 1
## X-squared = 0.56483, df = 3, p-value = 0.9044
```

```r
# Here the test confirmed that Toxic units are not significantly different among studies
# but since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :
fisher.test((Tutab + 1)*10, simulate.p.value = TRUE) # add 1 to each values to ensure TU is positive and multiply by 10 to have integer only (necessary for fisher-test)
```

```
## 
## 	Fisher's Exact Test for Count Data with simulated p-value (based on
## 	2000 replicates)
## 
## data:  (Tutab + 1) * 10
## p-value = 0.1489
## alternative hypothesis: two.sided
```
Fisher test is non-significant, confirming the result of the Chisquare-test: The toxic units are not significantly different among studies

### Test the difference in Toxic Unit between the start and the end of the experiment

```r
chi = chisq.test(Tutab2 + 1) # add 1 to each values to ensure TU is positive (necessary for chi-test)
```

```
## Warning in chisq.test(Tutab2 + 1): Chi-squared approximation may be incorrect
```

```r
contrib <- 100 * chi$residuals ^ 2 / chi$statistic
chi
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  Tutab2 + 1
## X-squared = 0.13923, df = 3, p-value = 0.9867
```

```r
# Here the test confirmed that Toxic units are not significantly different between the start and the end of the experiment
# but since Chi-squared approximation may be incorrect, run fisher exact test to confirm this result :
fisher.test((Tutab2 + 1)*10, simulate.p.value = TRUE) # add 1 to each values to ensure TU is positive and multiply by 10 to have integer only (necessary for fisher-test)
```

```
## 
## 	Fisher's Exact Test for Count Data with simulated p-value (based on
## 	2000 replicates)
## 
## data:  (Tutab2 + 1) * 10
## p-value = 0.7096
## alternative hypothesis: two.sided
```
Fisher test is non-significant, confirming the result of the Chisquare-test: the toxic units are not significantly different between the start and the end of the experiment

# Data analyses

 A quick reminder:\

 - Treatments (fixed effects):\
    - TransplantContam (transplant treatment either High contamination-HC or Low contamination-LC)\
    - OriginContam (Origin of the population either High contamination-HC or Low contamination-LC)\
    - Injection (control saline: PBS, antigen mixture: AMIX)\

 - Covariates :\
    - SizeEnd (Fish size at the end of the experiment)\
    - Sex (Fish sex determined by visual inspection at the end of the experiment)\

 - Blocks (random effects):\
    - CagingSite (The name of the study site where fish are caged, either ARIMAS, AUSCOR, CELFIG or RIOU)\
    - CageSiteID (The ID number ranging from 1 to 6 corresponding to the number of the cage within each study site)\
    - OriginSite (The name of the site where each population were sampled = population ID)\

Here we used The identity of the cage nested within the study site as random effects\
to consider possible shared conditions within the cage and the study site.\

Best models were selected by backward selection procedure, eliminating non-significant interactions and variables (i.e., p-value > 0.05). 

When interactions were found significant, we analyzed differences between groups using pairwise t-test with false discovery rate adjustment (Benjamini and Hochberg, 1995) or two-sample fisher's exact test for count data (i.e., survival) (Agresti, 2007), respectively.

In addition, when interactions between the level of contamination in the transplant site and the origin of the population (i.e., HC or LC) were significant, we tested whether slopes were parallel among replicates populations by comparing models including the level of contamination in transplant site (i.e., HC or LC) and the identity of the origin of the population (i.e., ARIMAS, AUSCOR or CELFIG, RIOU) with models including the interaction between the level of contamination in transplant site and the identity of the origin of the population according to (Jacquin et al., 2016).

NB: The results retrieved from the following refined models are reported in Table 3 of the manuscript. The table 3 can also be found in the section "Summary of the best models (Table 3 from the manuscript)"

## Survival

```r
dat2 = dat1[is.na(dat1$Death) == F, ]
par(mfrow = c(1,2))
plot(dat2$Death)
hist(dat2$Death)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/initSurvival-1.png)<!-- -->
The data are binomial, so use glmer function from lme4 package to construct the full model:
### Use GLMER with family binomial

```r
modfull <-
  lme4::glmer(
    Death ~ (TransplantContam + OriginContam + Injection) ^ 3 + (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    family = binomial(link = "logit"),
    na.action = na.fail
  )
performance::check_model(modfull)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GLMERSurvival-1.png)<!-- -->

```r
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: Death
##                                          Chisq Df Pr(>Chisq)
## (Intercept)                             0.0083  1     0.9273
## TransplantContam                        0.0062  1     0.9373
## OriginContam                            0.0072  1     0.9326
## Injection                               0.0000  1     0.9997
## TransplantContam:OriginContam           0.0077  1     0.9303
## TransplantContam:Injection              0.0023  1     0.9621
## OriginContam:Injection                  0.0000  1     0.9989
## TransplantContam:OriginContam:Injection 0.0024  1     0.9612
```

```r
summary(modfull)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Death ~ (TransplantContam + OriginContam + Injection)^3 + (1 |  
##     CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
##      AIC      BIC   logLik deviance df.resid 
##    136.3    175.8    -57.2    114.3      257 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -0.7322 -0.2658 -0.1625  0.0000  5.7648 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 2.912e-01 5.396e-01
##  CagingSite            (Intercept) 1.310e-10 1.144e-05
##  OriginSite            (Intercept) 0.000e+00 0.000e+00
## Number of obs: 268, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error z value
## (Intercept)                                    -21.4406   235.0225  -0.091
## TransplantContamLC                              18.4876   235.0224   0.079
## OriginContamLC                                  19.8733   235.0222   0.085
## InjectionPBS                                    -0.1350   406.2111   0.000
## TransplantContamLC:OriginContamLC              -20.5680   235.0231  -0.088
## TransplantContamLC:InjectionPBS                -18.6017   391.4285  -0.048
## OriginContamLC:InjectionPBS                      0.5503   406.2112   0.001
## TransplantContamLC:OriginContamLC:InjectionPBS  19.0396   391.4280   0.049
##                                                Pr(>|z|)
## (Intercept)                                       0.927
## TransplantContamLC                                0.937
## OriginContamLC                                    0.933
## InjectionPBS                                      1.000
## TransplantContamLC:OriginContamLC                 0.930
## TransplantContamLC:InjectionPBS                   0.962
## OriginContamLC:InjectionPBS                       0.999
## TransplantContamLC:OriginContamLC:InjectionPBS    0.961
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -1.000                                              
## OrignCntmLC -1.000  1.000                                       
## InjectinPBS -0.020  0.020  0.020                                
## TrnCLC:OCLC  1.000 -1.000 -1.000 -0.020                         
## TrnCLC:IPBS -0.019  0.019  0.019 -0.148 -0.019                  
## OrgCLC:IPBS  0.020 -0.020 -0.020 -1.000  0.020      0.148       
## TCLC:OCLC:I  0.019 -0.019 -0.019  0.148  0.019     -1.000 -0.148
## optimizer (Nelder_Mead) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
MuMIn::r.squaredGLMM(modfull)
```

```
##                   R2m       R2c
## theoretical 0.9595305 0.9628213
## delta       0.8477609 0.8506684
```
The model returns a singular fit, the random structure seems too complex for the data since "OriginSite" have a variance of zero. To solve this, we can use two alternative methods:

### Use classic glm with family binomial (remove random effects)

```r
modfull <-
  glm(
    Death ~ (TransplantContam + OriginContam + Injection) ^ 3,
    data = dat2,
    family = "binomial"
  )
car::Anova(modfull, type = "3")
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Analysis of Deviance Table (Type III tests)
## 
## Response: Death
##                                         LR Chisq Df Pr(>Chisq)   
## TransplantContam                          2.7138  1   0.099486 . 
## OriginContam                              9.6183  1   0.001927 **
## Injection                                 0.0000  1   1.000000   
## TransplantContam:OriginContam             6.2985  1   0.012084 * 
## TransplantContam:Injection                0.0000  1   1.000000   
## OriginContam:Injection                    0.0000  1   0.999960   
## TransplantContam:OriginContam:Injection   0.0000  1   1.000000   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## 
## Call:
## glm(formula = Death ~ (TransplantContam + OriginContam + Injection)^3, 
##     family = "binomial", data = dat2)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.77104  -0.36522  -0.24078  -0.00008   2.66659  
## 
## Coefficients:
##                                                  Estimate Std. Error z value
## (Intercept)                                    -1.957e+01  1.872e+03  -0.010
## TransplantContamLC                              1.676e+01  1.872e+03   0.009
## OriginContamLC                                  1.811e+01  1.872e+03   0.010
## InjectionPBS                                   -9.667e-09  2.713e+03   0.000
## TransplantContamLC:OriginContamLC              -1.883e+01  1.872e+03  -0.010
## TransplantContamLC:InjectionPBS                -1.676e+01  3.313e+03  -0.005
## OriginContamLC:InjectionPBS                     3.944e-01  2.713e+03   0.000
## TransplantContamLC:OriginContamLC:InjectionPBS  1.722e+01  3.313e+03   0.005
##                                                Pr(>|z|)
## (Intercept)                                       0.992
## TransplantContamLC                                0.993
## OriginContamLC                                    0.992
## InjectionPBS                                      1.000
## TransplantContamLC:OriginContamLC                 0.992
## TransplantContamLC:InjectionPBS                   0.996
## OriginContamLC:InjectionPBS                       1.000
## TransplantContamLC:OriginContamLC:InjectionPBS    0.996
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 147.26  on 267  degrees of freedom
## Residual deviance: 115.04  on 260  degrees of freedom
## AIC: 131.04
## 
## Number of Fisher Scoring iterations: 18
```
#### Refine the model

```r
mod2 <-
  glm(
    Death ~ (TransplantContam + OriginContam + Injection) ^ 2,
    data = dat2,
    family = "binomial"
  )
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
car::Anova(mod2, type = "3")
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Analysis of Deviance Table (Type III tests)
## 
## Response: Death
##                               LR Chisq Df Pr(>Chisq)   
## TransplantContam                2.7138  1   0.099486 . 
## OriginContam                    9.6183  1   0.001927 **
## Injection                       2.2948  1   0.129805   
## TransplantContam:OriginContam   6.2985  1   0.012084 * 
## TransplantContam:Injection      0.1140  1   0.735627   
## OriginContam:Injection          3.0363  1   0.081423 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## 
## Call:
## glm(formula = Death ~ (TransplantContam + OriginContam + Injection)^2, 
##     family = "binomial", data = dat2)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.77104  -0.36522  -0.24078  -0.00007   2.66659  
## 
## Coefficients:
##                                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)                        -19.6981  1999.7748  -0.010    0.992
## TransplantContamLC                  16.8947  1999.7749   0.008    0.993
## OriginContamLC                      18.2428  1999.7748   0.009    0.993
## InjectionPBS                       -17.3332  2011.2531  -0.009    0.993
## TransplantContamLC:OriginContamLC  -18.9658  1999.7752  -0.009    0.992
## TransplantContamLC:InjectionPBS      0.4578     1.3746   0.333    0.739
## OriginContamLC:InjectionPBS         17.7276  2011.2531   0.009    0.993
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 147.26  on 267  degrees of freedom
## Residual deviance: 115.04  on 261  degrees of freedom
## AIC: 129.04
## 
## Number of Fisher Scoring iterations: 19
```
#### Refine the model

```r
mod3 <-
  glm(
    Death ~ (TransplantContam + OriginContam) ^ 2 + Injection,
    data = dat2,
    family = "binomial"
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III tests)
## 
## Response: Death
##                               LR Chisq Df Pr(>Chisq)    
## TransplantContam                2.6805  1    0.10159    
## OriginContam                   21.9792  1  2.756e-06 ***
## Injection                       0.1708  1    0.67940    
## TransplantContam:OriginContam   6.5855  1    0.01028 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## 
## Call:
## glm(formula = Death ~ (TransplantContam + OriginContam)^2 + Injection, 
##     family = "binomial", data = dat2)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.74070  -0.32045  -0.25853  -0.00008   2.68644  
## 
## Coefficients:
##                                    Estimate Std. Error z value Pr(>|z|)
## (Intercept)                        -19.6644  1353.9148  -0.015    0.988
## TransplantContamLC                  16.0834  1353.9150   0.012    0.991
## OriginContamLC                      18.3121  1353.9148   0.014    0.989
## InjectionPBS                         0.1991     0.4821   0.413    0.680
## TransplantContamLC:OriginContamLC  -17.8737  1353.9152  -0.013    0.989
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 147.26  on 267  degrees of freedom
## Residual deviance: 118.50  on 263  degrees of freedom
## AIC: 128.5
## 
## Number of Fisher Scoring iterations: 18
```
#### Refine the model

```r
mod4 <-
  glm(Death ~ (TransplantContam + OriginContam) ^ 2,
      data = dat2,
      family = "binomial")
performance::check_model(mod4)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GlmSurvivalRefined3-1.png)<!-- -->

```r
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III tests)
## 
## Response: Death
##                               LR Chisq Df Pr(>Chisq)    
## TransplantContam                2.6807  1    0.10157    
## OriginContam                   21.9923  1  2.738e-06 ***
## TransplantContam:OriginContam   6.5915  1    0.01025 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## 
## Call:
## glm(formula = Death ~ (TransplantContam + OriginContam)^2, family = "binomial", 
##     data = dat2)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.70896  -0.30502  -0.24619  -0.00008   2.65011  
## 
## Coefficients:
##                                   Estimate Std. Error z value Pr(>|z|)
## (Intercept)                         -19.57    1354.88  -0.014    0.988
## TransplantContamLC                   16.08    1354.88   0.012    0.991
## OriginContamLC                       18.31    1354.88   0.014    0.989
## TransplantContamLC:OriginContamLC   -17.88    1354.88  -0.013    0.989
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 147.26  on 267  degrees of freedom
## Residual deviance: 118.67  on 264  degrees of freedom
## AIC: 126.67
## 
## Number of Fisher Scoring iterations: 18
```

```r
modSurvival <- mod4
```
Here the interaction between fish origin and the transplant site is significant. 
Alternatively, we can compute a survival rate per cage and build model with survival transformed to logit

### Build model with survival transformed to logit
NB: here we used the CagingSite as random effect only since survival is computed for each cage so that there is no need to add the cageSiteID.

#### Create a new dataset containing survival rate within each cage

```r
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
```

#### Build the full model

```r
modfull <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam + Injection) ^ 3 +
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
```

```
## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)
```

```r
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: car::logit(survival)
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             78.0809  1  < 2.2e-16 ***
## TransplantContam                         0.6331  1   0.426220    
## OriginContam                             8.6911  1   0.003198 ** 
## Injection                                0.0000  1   1.000000    
## TransplantContam:OriginContam            4.8891  1   0.027026 *  
## TransplantContam:Injection               0.3166  1   0.573688    
## OriginContam:Injection                   0.1793  1   0.671964    
## TransplantContam:OriginContam:Injection  0.1893  1   0.663535    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## car::logit(survival) ~ (TransplantContam + OriginContam + Injection)^3 +  
##     (1 | CagingSite) + (1 | OriginSite)
##    Data: DeathSum
## 
## REML criterion at convergence: 129.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.2969 -0.1086  0.0000  0.4594  2.0478 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  CagingSite (Intercept) 0.000    0.000   
##  OriginSite (Intercept) 0.000    0.000   
##  Residual               1.031    1.016   
## Number of obs: 48, groups:  CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                  Estimate Std. Error t value
## (Intercept)                                     3.664e+00  4.146e-01   8.836
## TransplantContamLC                             -4.665e-01  5.863e-01  -0.796
## OriginContamLC                                 -1.729e+00  5.863e-01  -2.948
## InjectionPBS                                   -1.602e-15  5.863e-01   0.000
## TransplantContamLC:OriginContamLC               1.833e+00  8.292e-01   2.211
## TransplantContamLC:InjectionPBS                 4.665e-01  8.292e-01   0.563
## OriginContamLC:InjectionPBS                    -3.511e-01  8.292e-01  -0.423
## TransplantContamLC:OriginContamLC:InjectionPBS -5.102e-01  1.173e+00  -0.435
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.707                                              
## OrignCntmLC -0.707  0.500                                       
## InjectinPBS -0.707  0.500  0.500                                
## TrnCLC:OCLC  0.500 -0.707 -0.707 -0.354                         
## TrnCLC:IPBS  0.500 -0.707 -0.354 -0.707  0.500                  
## OrgCLC:IPBS  0.500 -0.354 -0.707 -0.707  0.500      0.500       
## TCLC:OCLC:I -0.354  0.500  0.500  0.500 -0.707     -0.707 -0.707
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refine the model

```r
mod2 <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam + Injection) ^ 2+
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
```

```
## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)
```

```r
car::Anova(mod2, type="3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: car::logit(survival)
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   87.8938  1  < 2.2e-16 ***
## TransplantContam               0.4547  1   0.500121    
## OriginContam                  10.1418  1   0.001449 ** 
## Injection                      0.0644  1   0.799735    
## TransplantContam:OriginContam  7.3930  1   0.006548 ** 
## TransplantContam:Injection     0.1327  1   0.715665    
## OriginContam:Injection         1.0905  1   0.296361    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## car::logit(survival) ~ (TransplantContam + OriginContam + Injection)^2 +  
##     (1 | CagingSite) + (1 | OriginSite)
##    Data: DeathSum
## 
## REML criterion at convergence: 131.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.38339 -0.17309  0.06342  0.42306  2.13179 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  CagingSite (Intercept) 0.000    0.000   
##  OriginSite (Intercept) 0.000    0.000   
##  Residual               1.011    1.005   
## Number of obs: 48, groups:  CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                         3.5998     0.3840   9.375
## TransplantContamLC                 -0.3390     0.5027  -0.674
## OriginContamLC                     -1.6010     0.5027  -3.185
## InjectionPBS                        0.1275     0.5027   0.254
## TransplantContamLC:OriginContamLC   1.5784     0.5805   2.719
## TransplantContamLC:InjectionPBS     0.2115     0.5805   0.364
## OriginContamLC:InjectionPBS        -0.6062     0.5805  -1.044
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS TCLC:O TCLC:I
## TrnsplntCLC -0.655                                   
## OrignCntmLC -0.655  0.333                            
## InjectinPBS -0.655  0.333  0.333                     
## TrnCLC:OCLC  0.378 -0.577 -0.577  0.000              
## TrnCLC:IPBS  0.378 -0.577  0.000 -0.577  0.000       
## OrgCLC:IPBS  0.378  0.000 -0.577 -0.577  0.000  0.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refine the model

```r
mod3 <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam) ^ 2 + Injection +
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
```

```
## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)
```

```r
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: car::logit(survival)
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   132.2805  1  < 2.2e-16 ***
## TransplantContam                0.3289  1   0.566321    
## OriginContam                   21.9139  1  2.852e-06 ***
## Injection                       0.0590  1   0.808152    
## TransplantContam:OriginContam   7.5290  1   0.006071 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: car::logit(survival) ~ (TransplantContam + OriginContam)^2 +  
##     Injection + (1 | CagingSite) + (1 | OriginSite)
##    Data: DeathSum
## 
## REML criterion at convergence: 134.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6104 -0.1017  0.1171  0.5260  1.9462 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  CagingSite (Intercept) 0.0000   0.0000  
##  OriginSite (Intercept) 0.0000   0.0000  
##  Residual               0.9927   0.9963  
## Number of obs: 48, groups:  CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        3.69848    0.32157  11.501
## TransplantContamLC                -0.23327    0.40676  -0.573
## OriginContamLC                    -1.90412    0.40676  -4.681
## InjectionPBS                      -0.06984    0.28762  -0.243
## TransplantContamLC:OriginContamLC  1.57841    0.57524   2.744
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.632                     
## OrignCntmLC -0.632  0.500              
## InjectinPBS -0.447  0.000  0.000       
## TrnCLC:OCLC  0.447 -0.707 -0.707  0.000
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refine the model

```r
mod4 <-
 lme4::lmer(
    car::logit(survival) ~ (TransplantContam + OriginContam) ^ 2 +
   (1 | CagingSite) + (1 | OriginSite),
    data = DeathSum,
    na.action = na.exclude
  )
```

```
## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)

## Warning in car::logit(survival): proportions remapped to (0.025, 0.975)
```

```r
performance::check_model(mod4)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/SurvivalLogitRefined3-1.png)<!-- -->

```r
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: car::logit(survival)
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   165.7888  1  < 2.2e-16 ***
## TransplantContam                0.3361  1   0.562109    
## OriginContam                   22.3928  1  2.222e-06 ***
## TransplantContam:OriginContam   7.6935  1   0.005542 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: car::logit(survival) ~ (TransplantContam + OriginContam)^2 +  
##     (1 | CagingSite) + (1 | OriginSite)
##    Data: DeathSum
## 
## REML criterion at convergence: 133.5
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.60333 -0.06735  0.11833  0.56713  1.93187 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  CagingSite (Intercept) 0.0000   0.0000  
##  OriginSite (Intercept) 0.0000   0.0000  
##  Residual               0.9715   0.9856  
## Number of obs: 48, groups:  CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                         3.6636     0.2845  12.876
## TransplantContamLC                 -0.2333     0.4024  -0.580
## OriginContamLC                     -1.9041     0.4024  -4.732
## TransplantContamLC:OriginContamLC   1.5784     0.5691   2.774
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.707              
## OrignCntmLC -0.707  0.500       
## TrnCLC:OCLC  0.500 -0.707 -0.707
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

Although, this last method also result in singular fit ("CagingSite" and "OriginSite" random effects variance is estimated as zero), here the interaction between fish origin and the transplant site is significant.
Both simplified methods give similar results, hence the results of the more straightforward method (glm) are reported in the statistic table (table 3).
Also, because an interaction is significant, perform a posthoc test.

### Posthoc test (test for equality of proportions)

```r
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
```

```
##       LC HC
## Dead   3 16
## Alive 63 56
```

```r
# create the matrix for High contamination origin
OriginHC = matrix(
  c(b[b$Group.1 == "LC_HC", 2], a[a$Group.1 == "LC_HC", 2], b[b$Group.1 == "HC_HC", 2], a[a$Group.1 == "HC_HC", 2]),
  nrow = 2,
  ncol = 2,
  dimnames = list(c("Dead", "Alive"), c("LC", "HC"))
)
OriginHC
```

```
##       LC HC
## Dead   2  0
## Alive 65 63
```

```r
# run test for equality of proportions on each matrix
fisher.test(OriginLC, conf.level = 0.95)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  OriginLC
## p-value = 0.002728
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.02990582 0.63312320
## sample estimates:
## odds ratio 
##  0.1685805
```

```r
fisher.test(OriginHC, conf.level = 0.95)
```

```
## 
## 	Fisher's Exact Test for Count Data
## 
## data:  OriginHC
## p-value = 0.4966
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.1769481       Inf
## sample estimates:
## odds ratio 
##        Inf
```
Here fish from Low Contamination, display a significant decrease in survival when exposed to High Contamination site.
On the contrary, fish from High contamination site do not display a significant change in survival according to transplant site.

Also,because interaction between the contamination in origin and transplant site of fish is significant
We test for parallel response between replicate populations.

### Test for parallel responses between replicate populations (from the same origin)
#### For Populations from High contamination sites (AUSCOR and RIOU)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: Death ~ TransplantContam + OriginSite
## Model 2: Death ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       127     1.9121                     
## 2       126     1.8857  1 0.026394   0.1842
```

```r
coef(mod.par)
```

```
##                       (Intercept)                TransplantContamLC 
##                      5.355515e-17                      5.714286e-02 
##                    OriginSiteRIOU TransplantContamLC:OriginSiteRIOU 
##                     -4.878627e-18                     -5.714286e-02
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = Death ~ TransplantContam * OriginSite, data = dat3)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.05714  -0.04286   0.00000   0.00000   0.94286  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                        5.356e-17  2.272e-02   0.000   1.0000  
## TransplantContamLC                 5.714e-02  3.072e-02   1.860   0.0652 .
## OriginSiteRIOU                    -4.879e-18  3.092e-02   0.000   1.0000  
## TransplantContamLC:OriginSiteRIOU -5.714e-02  4.303e-02  -1.328   0.1866  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.01496599)
## 
##     Null deviance: 1.9692  on 129  degrees of freedom
## Residual deviance: 1.8857  on 126  degrees of freedom
## AIC: -171.4
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.

#### For Populations from Low contamination sites (ARIMAS and CELFIG)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: Death ~ TransplantContam + OriginSite
## Model 2: Death ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       135     15.308                     
## 2       134     15.276  1 0.032262   0.5947
```

```r
coef(mod.par)
```

```
##                         (Intercept)                  TransplantContamLC 
##                          0.23684211                         -0.20653907 
##                    OriginSiteCELFIG TransplantContamLC:OriginSiteCELFIG 
##                         -0.03095975                          0.06126278
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = Death ~ TransplantContam * OriginSite, data = dat4)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.23684  -0.20588  -0.06061  -0.03030   0.96970  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          0.23684    0.05477   4.324 2.97e-05 ***
## TransplantContamLC                  -0.20654    0.08034  -2.571   0.0112 *  
## OriginSiteCELFIG                    -0.03096    0.07970  -0.388   0.6983    
## TransplantContamLC:OriginSiteCELFIG  0.06126    0.11516   0.532   0.5956    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.113998)
## 
##     Null deviance: 16.384  on 137  degrees of freedom
## Residual deviance: 15.276  on 134  degrees of freedom
## AIC: 97.891
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of LC populations are parallel.

### Visualize 
The plot reported below correspond to the figure 2A from the manuscript

```r
par(mfrow=c(1,1))
Survivplot()
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/survivalViz-1.png)<!-- -->

## Metal bioaccumulation
### Create a global index of metal accumulation: Zn, Cd, Cu

```r
dat2 = dat1[is.na(dat1$MuscleCd) == F, ]
dat2$Cdscaled = scale(log(dat2$MuscleCd), center = T, scale = T)
dat2$Cuscaled = scale(log(dat2$MuscleCu), center = T, scale = T)
dat2$Znscaled = scale(log(dat2$MuscleZn), center = T, scale = T)
dat2$Bioacc = dat2$Cdscaled + dat2$Cuscaled + dat2$Znscaled
hist(dat2$Bioacc)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/BioaccIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(Bioacc ~ OriginContam, data = dat2)
boxplot(Bioacc ~ TransplantContam, data = dat2)
boxplot(Bioacc ~ Injection, data = dat2)
boxplot(Bioacc ~ TransplantContam * Injection, data = dat2)
boxplot(Bioacc ~ TransplantContam * OriginContam, data = dat2)
boxplot(Bioacc ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/BioaccViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: Bioacc
##                                          Chisq Df Pr(>Chisq)
## (Intercept)                             0.2687  1     0.6042
## TransplantContam                        0.0320  1     0.8580
## OriginContam                            0.7147  1     0.3979
## Injection                               0.0171  1     0.8961
## SizeEnd                                 0.8024  1     0.3704
## TransplantContam:OriginContam           1.5238  1     0.2170
## TransplantContam:Injection              1.1853  1     0.2763
## OriginContam:Injection                  0.1581  1     0.6909
## TransplantContam:OriginContam:Injection 0.4158  1     0.5191
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Bioacc ~ (TransplantContam + OriginContam + Injection)^3 + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 998.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5929 -0.6166  0.0019  0.6315  3.6126 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.06108  0.2471  
##  CagingSite            (Intercept) 1.62483  1.2747  
##  OriginSite            (Intercept) 0.00000  0.0000  
##  Residual                          3.17253  1.7812  
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    -0.64621    1.24654  -0.518
## TransplantContamLC                              0.24267    1.35588   0.179
## OriginContamLC                                  0.39650    0.46902   0.845
## InjectionPBS                                    0.05873    0.44959   0.131
## SizeEnd                                         0.07050    0.07871   0.896
## TransplantContamLC:OriginContamLC              -0.80902    0.65538  -1.234
## TransplantContamLC:InjectionPBS                -0.68677    0.63080  -1.089
## OriginContamLC:InjectionPBS                     0.25975    0.65322   0.398
## TransplantContamLC:OriginContamLC:InjectionPBS -0.58575    0.90841  -0.645
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.554                                                     
## OrignCntmLC -0.212  0.169                                              
## InjectinPBS -0.180  0.158  0.457                                       
## SizeEnd     -0.639  0.017  0.047  0.013                                
## TrnCLC:OCLC  0.123 -0.240 -0.714 -0.326  0.013                         
## TrnCLC:IPBS  0.117 -0.225 -0.325 -0.712  0.008  0.466                  
## OrgCLC:IPBS  0.125 -0.109 -0.648 -0.688 -0.011  0.463      0.490       
## TCLC:OCLC:I -0.081  0.156  0.466  0.495 -0.006 -0.652     -0.694 -0.719
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd+ 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: Bioacc
##                                Chisq Df Pr(>Chisq)  
## (Intercept)                   0.3280  1    0.56683  
## TransplantContam              0.0802  1    0.77698  
## OriginContam                  1.6806  1    0.19484  
## Injection                     0.2684  1    0.60444  
## SizeEnd                       0.7970  1    0.37199  
## TransplantContam:OriginContam 4.7839  1    0.02873 *
## TransplantContam:Injection    4.5674  1    0.03259 *
## OriginContam:Injection        0.0090  1    0.92439  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Bioacc ~ (TransplantContam + OriginContam + Injection)^2 + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1000.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6406 -0.6315 -0.0083  0.6140  3.6553 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 6.007e-02 2.451e-01
##  CagingSite            (Intercept) 1.622e+00 1.274e+00
##  OriginSite            (Intercept) 1.265e-09 3.557e-05
##  Residual                          3.165e+00 1.779e+00
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -0.71077    1.24102  -0.573
## TransplantContamLC                 0.37903    1.33812   0.283
## OriginContamLC                     0.53707    0.41428   1.296
## InjectionPBS                       0.20217    0.39028   0.518
## SizeEnd                            0.07015    0.07858   0.893
## TransplantContamLC:OriginContamLC -1.08431    0.49575  -2.187
## TransplantContamLC:InjectionPBS   -0.96917    0.45349  -2.137
## OriginContamLC:InjectionPBS       -0.04305    0.45353  -0.095
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.550                                          
## OrignCntmLC -0.198  0.110                                   
## InjectinPBS -0.162  0.094  0.295                            
## SizeEnd     -0.641  0.018  0.056  0.019                     
## TrnCLC:OCLC  0.092 -0.184 -0.611 -0.006  0.011              
## TrnCLC:IPBS  0.085 -0.164 -0.003 -0.590  0.005  0.024       
## OrgCLC:IPBS  0.097  0.005 -0.510 -0.551 -0.023 -0.010 -0.018
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: Bioacc
##                                Chisq Df Pr(>Chisq)  
## (Intercept)                   0.3213  1    0.57080  
## TransplantContam              0.0806  1    0.77648  
## OriginContam                  2.1094  1    0.14639  
## Injection                     0.3124  1    0.57618  
## SizeEnd                       0.7964  1    0.37217  
## TransplantContam:OriginContam 4.7930  1    0.02858 *
## TransplantContam:Injection    4.5959  1    0.03205 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Bioacc ~ (TransplantContam + OriginContam)^2 + (TransplantContam +  
##     Injection)^2 + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1000.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6527 -0.6343 -0.0135  0.6174  3.6569 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.06142  0.2478  
##  CagingSite            (Intercept) 1.62265  1.2738  
##  OriginSite            (Intercept) 0.00000  0.0000  
##  Residual                          3.15118  1.7752  
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -0.69980    1.23449  -0.567
## TransplantContamLC                 0.37990    1.33810   0.284
## OriginContamLC                     0.51742    0.35625   1.452
## InjectionPBS                       0.18171    0.32508   0.559
## SizeEnd                            0.07001    0.07845   0.892
## TransplantContamLC:OriginContamLC -1.08527    0.49572  -2.189
## TransplantContamLC:InjectionPBS   -0.96988    0.45241  -2.144
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O
## TrnsplntCLC -0.554                                   
## OrignCntmLC -0.173  0.130                            
## InjectinPBS -0.130  0.116  0.019                     
## SizeEnd     -0.642  0.018  0.051  0.007              
## TrnCLC:OCLC  0.094 -0.184 -0.716 -0.014  0.011       
## TrnCLC:IPBS  0.087 -0.163 -0.013 -0.718  0.005  0.024
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: Bioacc
##                                Chisq Df Pr(>Chisq)  
## (Intercept)                   0.0001  1    0.99345  
## TransplantContam              0.0699  1    0.79149  
## OriginContam                  2.0077  1    0.15650  
## Injection                     0.3065  1    0.57987  
## TransplantContam:OriginContam 4.9131  1    0.02665 *
## TransplantContam:Injection    4.6157  1    0.03168 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Bioacc ~ (TransplantContam + OriginContam)^2 + (TransplantContam +  
##     Injection)^2 + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 998
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.7307 -0.6338  0.0011  0.6105  3.6689 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.05418  0.2328  
##  CagingSite            (Intercept) 1.66493  1.2903  
##  OriginSite            (Intercept) 0.00000  0.0000  
##  Residual                          3.15254  1.7755  
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.007861   0.957003   0.008
## TransplantContamLC                 0.357625   1.352719   0.264
## OriginContamLC                     0.499324   0.352393   1.417
## InjectionPBS                       0.179984   0.325128   0.554
## TransplantContamLC:OriginContamLC -1.087981   0.490846  -2.217
## TransplantContamLC:InjectionPBS   -0.972115   0.452478  -2.148
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS TCLC:O
## TrnsplntCLC -0.707                            
## OrignCntmLC -0.180  0.127                     
## InjectinPBS -0.162  0.114  0.019              
## TrnCLC:OCLC  0.129 -0.180 -0.718 -0.014       
## TrnCLC:IPBS  0.116 -0.162 -0.014 -0.719  0.024
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modBioacc <- mod4
RmodBioacc <- MuMIn::r.squaredGLMM(modBioacc)
```

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    Bioacc ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: Bioacc
##                                Chisq Df Pr(>Chisq)  
## (Intercept)                   0.0049  1    0.94411  
## TransplantContam              0.0543  1    0.81582  
## OriginContam                  1.9330  1    0.16443  
## Injection                     0.2676  1    0.60495  
## Sex                           0.9997  1    0.31737  
## TransplantContam:OriginContam 4.3526  1    0.03695 *
## TransplantContam:Injection    4.2714  1    0.03876 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: Bioacc ~ (TransplantContam + OriginContam)^2 + (TransplantContam +  
##     Injection)^2 + Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 994.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8057 -0.6443  0.0424  0.5911  3.5562 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.08238  0.287   
##  CagingSite            (Intercept) 1.61475  1.271   
##  OriginSite            (Intercept) 0.00000  0.000   
##  Residual                          3.14100  1.772   
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        -0.0666     0.9500  -0.070
## TransplantContamLC                  0.3116     1.3380   0.233
## OriginContamLC                      0.5101     0.3669   1.390
## InjectionPBS                        0.1686     0.3260   0.517
## SexM                                0.2379     0.2379   1.000
## TransplantContamLC:OriginContamLC  -1.0691     0.5124  -2.086
## TransplantContamLC:InjectionPBS    -0.9358     0.4528  -2.067
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SexM   TCLC:O
## TrnsplntCLC -0.702                                   
## OrignCntmLC -0.181  0.133                            
## InjectinPBS -0.166  0.115  0.009                     
## SexM        -0.088 -0.029 -0.053  0.037              
## TrnCLC:OCLC  0.124 -0.190 -0.719 -0.004  0.099       
## TrnCLC:IPBS  0.115 -0.164 -0.010 -0.718  0.030  0.022
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
The mod4 is thus the best model.

### Posthoc test (pairwise t-test)
The interactions between fish origin and the transplant as well as between transplant and injection treatments are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions


```r
pairwise.t.test(dat2$Bioacc, dat2$CxI, p.adjust.method = "fdr")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  dat2$Bioacc and dat2$CxI 
## 
##         HC_AMIX HC_PBS LC_AMIX
## HC_PBS  0.749   -      -      
## LC_AMIX 0.640   0.530  -      
## LC_PBS  0.012   0.011  0.040  
## 
## P value adjustment method: fdr
```
Here, fish from low contamination site and injected with a saline control solution, has bioaccumulated significantly less metals than the fish exposed to other treatment, even compared to LC exposed in Low contamination site and injected to the antigen mixture, suggesting that the antigen mixture could affect bioaccumulation in fish muscle.


```r
pairwise.t.test(dat2$Bioacc, dat2$CxO2, p.adjust.method = "fdr")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  dat2$Bioacc and dat2$CxO2 
## 
##       HC_HC HC_LC LC_HC
## HC_LC 0.32  -     -    
## LC_HC 0.57  0.18  -    
## LC_LC 0.11  0.01  0.18 
## 
## P value adjustment method: fdr
```
Here fish from Low Contamination, display a significant increase in the amount of metal bioaccumulated in their muscle when exposed to High Contamination site compared to when they are exposed in low contamination site. On the contrary, the metal levels is constant in HC fish, whatever the site of transplantation. 

Also,because interaction between the contamination in origin and transplant site of fish is significant
We test for parallel response between replicate populations.

### Test for parallel responses between replicate populations (from the same origin)
#### For Populations from High contamination sites (AUSCOR and RIOU)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: Bioacc ~ TransplantContam + OriginSite
## Model 2: Bioacc ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1       125     405.31                       
## 2       124     393.62  1   11.688  0.05501 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
coef(mod.par)
```

```
##                       (Intercept)                TransplantContamLC 
##                        -0.5641966                        -0.7376128 
##                    OriginSiteRIOU TransplantContamLC:OriginSiteRIOU 
##                         1.3456317                         1.2108581
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = Bioacc ~ TransplantContam * OriginSite, data = dat3)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.9865  -0.8885   0.0204   0.9680   6.3792  
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)   
## (Intercept)                        -0.5642     0.3308  -1.705  0.09064 . 
## TransplantContamLC                 -0.7376     0.4535  -1.627  0.10638   
## OriginSiteRIOU                      1.3456     0.4504   2.988  0.00339 **
## TransplantContamLC:OriginSiteRIOU   1.2109     0.6310   1.919  0.05731 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 3.174371)
## 
##     Null deviance: 529.49  on 127  degrees of freedom
## Residual deviance: 393.62  on 124  degrees of freedom
## AIC: 517.04
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.

#### For Populations from Low contamination sites (ARIMAS and CELFIG)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: Bioacc ~ TransplantContam + OriginSite
## Model 2: Bioacc ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       116     382.81                     
## 2       115     382.09  1  0.72023   0.6415
```

```r
coef(mod.par)
```

```
##                         (Intercept)                  TransplantContamLC 
##                          -0.3383157                          -1.0450200 
##                    OriginSiteCELFIG TransplantContamLC:OriginSiteCELFIG 
##                           1.8961427                          -0.3118520
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = Bioacc ~ TransplantContam * OriginSite, data = dat4)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -5.1336  -1.1292  -0.1314   1.1556   6.0411  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          -0.3383     0.3385  -1.000 0.319645    
## TransplantContamLC                   -1.0450     0.4673  -2.236 0.027271 *  
## OriginSiteCELFIG                      1.8961     0.4875   3.890 0.000168 ***
## TransplantContamLC:OriginSiteCELFIG  -0.3119     0.6698  -0.466 0.642389    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 3.322503)
## 
##     Null deviance: 513.14  on 118  degrees of freedom
## Residual deviance: 382.09  on 115  degrees of freedom
## AIC: 486.52
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of LC populations are parallel.

### Visualize 
The plot reported below correspond to the figure 3 and the figure 2B from the manuscript 

```r
BioaccPlot3()
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/BioaccFinalViz-1.png)<!-- -->

```r
BioaccPlot1()
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/BioaccFinalViz-2.png)<!-- -->


## Oxidative Stress Index
### Create a global index of oxidative stress (Costantini and Dell’Omo, 2006)


```r
dat2 = dat1[is.na(dat1$mMHCLO) == F, ]
dat2 = dat2[is.na(dat2$mMH2O2) == F, ]
dat2$ratOXI = dat2$mMH2O2 * 1000 / dat2$mMHCLO
hist(log(dat2$ratOXI + 1))
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ratOXIIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(log(dat2$ratOXI + 1) ~ OriginContam, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ Injection, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam * Injection, data = dat2, cex.axis = 0.8)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$ratOXI + 1) ~ TransplantContam * OriginContam * Injection, data = dat2, cex.axis = 0.8, las = 2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ratOXIViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(ratOXI + 1)
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             69.2314  1  < 2.2e-16 ***
## TransplantContam                         0.3238  1   0.569314    
## OriginContam                             0.0007  1   0.979569    
## Injection                                0.0786  1   0.779203    
## SizeEnd                                  8.7427  1   0.003108 ** 
## TransplantContam:OriginContam            0.0505  1   0.822210    
## TransplantContam:Injection               1.3823  1   0.239711    
## OriginContam:Injection                   0.0913  1   0.762495    
## TransplantContam:OriginContam:Injection  0.0696  1   0.791853    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 275.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3106 -0.5139  0.0337  0.6605  1.9304 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.02445  0.1564  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.07611  0.2759  
##  Residual                          0.25584  0.5058  
## Number of obs: 166, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     3.453200   0.415021   8.321
## TransplantContamLC                              0.101436   0.178251   0.569
## OriginContamLC                                 -0.008508   0.332223  -0.026
## InjectionPBS                                   -0.042766   0.152541  -0.280
## SizeEnd                                        -0.100495   0.033988  -2.957
## TransplantContamLC:OriginContamLC              -0.057986   0.258055  -0.225
## TransplantContamLC:InjectionPBS                 0.249744   0.212420   1.176
## OriginContamLC:InjectionPBS                     0.068667   0.227218   0.302
## TransplantContamLC:OriginContamLC:InjectionPBS -0.084638   0.320713  -0.264
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.205                                                     
## OrignCntmLC -0.414  0.278                                              
## InjectinPBS -0.162  0.457  0.244                                       
## SizeEnd     -0.826 -0.022  0.021 -0.042                                
## TrnCLC:OCLC  0.136 -0.691 -0.397 -0.316  0.023                         
## TrnCLC:IPBS  0.080 -0.620 -0.174 -0.719  0.073  0.429                  
## OrgCLC:IPBS  0.124 -0.306 -0.339 -0.671  0.010  0.436      0.482       
## TCLC:OCLC:I -0.066  0.410  0.240  0.476 -0.032 -0.602     -0.661 -0.709
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(ratOXI + 1)
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   69.5593  1    < 2e-16 ***
## TransplantContam               0.5469  1    0.45958    
## OriginContam                   0.0015  1    0.96941    
## Injection                      0.0323  1    0.85744    
## SizeEnd                        8.8570  1    0.00292 ** 
## TransplantContam:OriginContam  0.2313  1    0.63054    
## TransplantContam:Injection     1.8062  1    0.17896    
## OriginContam:Injection         0.0270  1    0.86949    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 274.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3431 -0.5207  0.0314  0.6552  1.9356 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 2.488e-02 1.577e-01
##  CagingSite            (Intercept) 2.116e-10 1.455e-05
##  OriginSite            (Intercept) 7.611e-02 2.759e-01
##  Residual                          2.540e-01 5.040e-01
## Number of obs: 166, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        3.44615    0.41320   8.340
## TransplantContamLC                 0.12028    0.16264   0.740
## OriginContamLC                     0.01237    0.32251   0.038
## InjectionPBS                      -0.02402    0.13371  -0.180
## SizeEnd                           -0.10078    0.03386  -2.976
## TransplantContamLC:OriginContamLC -0.09921    0.20628  -0.481
## TransplantContamLC:InjectionPBS    0.21346    0.15883   1.344
## OriginContamLC:InjectionPBS        0.02625    0.15977   0.164
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.196                                          
## OrignCntmLC -0.412  0.204                                   
## InjectinPBS -0.148  0.325  0.152                            
## SizeEnd     -0.830 -0.010  0.030 -0.031                     
## TrnCLC:OCLC  0.121 -0.610 -0.326 -0.042  0.004              
## TrnCLC:IPBS  0.048 -0.508 -0.020 -0.614  0.069  0.051       
## OrgCLC:IPBS  0.109 -0.025 -0.245 -0.537 -0.018  0.016  0.025
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(ratOXI + 1)
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   69.5593  1    < 2e-16 ***
## TransplantContam               0.5469  1    0.45958    
## OriginContam                   0.0015  1    0.96941    
## Injection                      0.0323  1    0.85744    
## SizeEnd                        8.8570  1    0.00292 ** 
## TransplantContam:OriginContam  0.2313  1    0.63054    
## TransplantContam:Injection     1.8062  1    0.17896    
## OriginContam:Injection         0.0270  1    0.86949    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(ratOXI + 1) ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 274.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3431 -0.5207  0.0314  0.6552  1.9356 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 2.488e-02 1.577e-01
##  CagingSite            (Intercept) 2.116e-10 1.455e-05
##  OriginSite            (Intercept) 7.611e-02 2.759e-01
##  Residual                          2.540e-01 5.040e-01
## Number of obs: 166, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        3.44615    0.41320   8.340
## TransplantContamLC                 0.12028    0.16264   0.740
## OriginContamLC                     0.01237    0.32251   0.038
## InjectionPBS                      -0.02402    0.13371  -0.180
## SizeEnd                           -0.10078    0.03386  -2.976
## TransplantContamLC:OriginContamLC -0.09921    0.20628  -0.481
## TransplantContamLC:InjectionPBS    0.21346    0.15883   1.344
## OriginContamLC:InjectionPBS        0.02625    0.15977   0.164
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.196                                          
## OrignCntmLC -0.412  0.204                                   
## InjectinPBS -0.148  0.325  0.152                            
## SizeEnd     -0.830 -0.010  0.030 -0.031                     
## TrnCLC:OCLC  0.121 -0.610 -0.326 -0.042  0.004              
## TrnCLC:IPBS  0.048 -0.508 -0.020 -0.614  0.069  0.051       
## OrgCLC:IPBS  0.109 -0.025 -0.245 -0.537 -0.018  0.016  0.025
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    log(ratOXI + 1) ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(ratOXI + 1)
##                    Chisq Df Pr(>Chisq)    
## (Intercept)      71.3344  1  < 2.2e-16 ***
## TransplantContam  3.5864  1   0.058255 .  
## OriginContam      0.0053  1   0.942076    
## Injection         1.4256  1   0.232490    
## SizeEnd           9.5099  1   0.002044 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(ratOXI + 1) ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 271.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1871 -0.5425  0.0135  0.6022  2.0323 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.667e-02 1.291e-01
##  CagingSite            (Intercept) 3.888e-11 6.236e-06
##  OriginSite            (Intercept) 7.776e-02 2.789e-01
##  Residual                          2.567e-01 5.066e-01
## Number of obs: 166, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)         3.44289    0.40764   8.446
## TransplantContamLC  0.18166    0.09593   1.894
## OriginContamLC     -0.02147    0.29548  -0.073
## InjectionPBS        0.09488    0.07947   1.194
## SizeEnd            -0.10427    0.03381  -3.084
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.158                     
## OrignCntmLC -0.386  0.004              
## InjectinPBS -0.106  0.017  0.020       
## SizeEnd     -0.846  0.043  0.030  0.003
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    log(ratOXI + 1) ~ SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ratOXIRefined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(ratOXI + 1)
##               Chisq Df Pr(>Chisq)    
## (Intercept) 99.4348  1  < 2.2e-16 ***
## SizeEnd      9.2082  1   0.002409 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(ratOXI + 1) ~ SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 269
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3526 -0.4999  0.0010  0.6396  1.8761 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01830  0.1353  
##  CagingSite            (Intercept) 0.01190  0.1091  
##  OriginSite            (Intercept) 0.03728  0.1931  
##  Residual                          0.25728  0.5072  
## Number of obs: 166, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  3.53866    0.35487   9.972
## SizeEnd     -0.10109    0.03331  -3.034
## 
## Correlation of Fixed Effects:
##         (Intr)
## SizeEnd -0.940
```

```r
modRatOx <- mod5
RmodRatOx <- MuMIn::r.squaredGLMM(modRatOx)
```

The treatments has no effect on oxidative stress index but the oxidative stress index is lower in bigger fish 
Represent the relation between fish size and oxidative stress index

```r
plot(log(ratOXI + 1) ~ SizeEnd, data = dat2)
abline(b = -0.10109, a = 3.53866, col = "red")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ratOXISize-1.png)<!-- -->

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
mod5 <-
 lme4::lmer(
    log(ratOXI + 1) ~ SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ratOXISex-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(ratOXI + 1)
##              Chisq Df Pr(>Chisq)    
## (Intercept) 99.176  1  < 2.2e-16 ***
## SizeEnd      9.413  1   0.002155 ** 
## Sex          0.145  1   0.703328    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(ratOXI + 1) ~ SizeEnd + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 269.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2988 -0.5140  0.0161  0.6177  1.9481 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01693  0.1301  
##  CagingSite            (Intercept) 0.01374  0.1172  
##  OriginSite            (Intercept) 0.03700  0.1924  
##  Residual                          0.25788  0.5078  
## Number of obs: 165, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  3.53925    0.35539   9.959
## SizeEnd     -0.10284    0.03352  -3.068
## SexM         0.03204    0.08413   0.381
## 
## Correlation of Fixed Effects:
##         (Intr) SizEnd
## SizeEnd -0.934       
## SexM     0.006 -0.109
```
The sex effect is non-significant.
The mod5 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct

## Oxidative damage
### Check the data

```r
dat2 = dat1[is.na(dat1$mMH2O2) == F, ]
hist(log(dat2$mMH2O2 + 1))
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/OxDamIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(log(dat2$mMH2O2 + 1) ~ OriginContam, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ Injection, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam * Injection, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$mMH2O2 + 1) ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/OxDamViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(mMH2O2 + 1)
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             54.8494  1  1.301e-13 ***
## TransplantContam                         0.0387  1   0.843997    
## OriginContam                             0.5331  1   0.465301    
## Injection                                3.0090  1   0.082804 .  
## SizeEnd                                 12.6086  1   0.000384 ***
## TransplantContam:OriginContam            0.1036  1   0.747597    
## TransplantContam:Injection               5.9362  1   0.014833 *  
## OriginContam:Injection                   2.2515  1   0.133487    
## TransplantContam:OriginContam:Injection  3.4945  1   0.061573 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mMH2O2 + 1) ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 166.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.56602 -0.62429  0.00728  0.64432  2.55410 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01557  0.1248  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.04218  0.2054  
##  Residual                          0.11756  0.3429  
## Number of obs: 179, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                     2.11280    0.28528   7.406
## TransplantContamLC                             -0.02420    0.12299  -0.197
## OriginContamLC                                 -0.17654    0.24178  -0.730
## InjectionPBS                                   -0.17080    0.09846  -1.735
## SizeEnd                                        -0.08067    0.02272  -3.551
## TransplantContamLC:OriginContamLC               0.05790    0.17991   0.322
## TransplantContamLC:InjectionPBS                 0.33987    0.13949   2.436
## OriginContamLC:InjectionPBS                     0.22025    0.14679   1.500
## TransplantContamLC:OriginContamLC:InjectionPBS -0.38945    0.20834  -1.869
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.200                                                     
## OrignCntmLC -0.433  0.247                                              
## InjectinPBS -0.141  0.391  0.198                                       
## SizeEnd     -0.806 -0.012  0.024 -0.034                                
## TrnCLC:OCLC  0.133 -0.684 -0.372 -0.267  0.013                         
## TrnCLC:IPBS  0.061 -0.575 -0.138 -0.707  0.071  0.394                  
## OrgCLC:IPBS  0.101 -0.262 -0.303 -0.670  0.014  0.408      0.474       
## TCLC:OCLC:I -0.067  0.385  0.214  0.472 -0.015 -0.580     -0.667 -0.705
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(mMH2O2 + 1)
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   53.0417  1  3.265e-13 ***
## TransplantContam               0.3188  1  0.5723230    
## OriginContam                   0.1176  1  0.7316722    
## Injection                      0.9171  1  0.3382448    
## SizeEnd                       12.5772  1  0.0003905 ***
## TransplantContam:OriginContam  0.8666  1  0.3518981    
## TransplantContam:Injection     2.5028  1  0.1136463    
## OriginContam:Injection         0.0655  1  0.7980361    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mMH2O2 + 1) ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 168.6
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.69555 -0.59671  0.00027  0.63658  2.41208 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.560e-02 1.249e-01
##  CagingSite            (Intercept) 3.708e-10 1.926e-05
##  OriginSite            (Intercept) 4.090e-02 2.022e-01
##  Residual                          1.194e-01 3.456e-01
## Number of obs: 179, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        2.07527    0.28495   7.283
## TransplantContamLC                 0.06441    0.11408   0.565
## OriginContamLC                    -0.08016    0.23376  -0.343
## InjectionPBS                      -0.08375    0.08746  -0.958
## SizeEnd                           -0.08111    0.02287  -3.546
## TransplantContamLC:OriginContamLC -0.13701    0.14718  -0.931
## TransplantContamLC:InjectionPBS    0.16564    0.10470   1.582
## OriginContamLC:InjectionPBS        0.02685    0.10493   0.256
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.190                                          
## OrignCntmLC -0.425  0.186                                   
## InjectinPBS -0.125  0.257  0.115                            
## SizeEnd     -0.814 -0.007  0.029 -0.030                     
## TrnCLC:OCLC  0.116 -0.612 -0.316  0.010  0.006              
## TrnCLC:IPBS  0.022 -0.465  0.007 -0.597  0.081  0.011       
## OrgCLC:IPBS  0.078  0.014 -0.225 -0.540  0.004 -0.002  0.007
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ (TransplantContam + Injection) ^ 2 + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(mMH2O2 + 1)
##                              Chisq Df Pr(>Chisq)    
## (Intercept)                55.5203  1   9.25e-14 ***
## TransplantContam            0.0000  1  0.9989045    
## Injection                   0.9161  1  0.3384952    
## OriginContam                0.3939  1  0.5302729    
## SizeEnd                    12.6114  1  0.0003834 ***
## TransplantContam:Injection  2.5040  1  0.1135540    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mMH2O2 + 1) ~ (TransplantContam + Injection)^2 + OriginContam +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 164.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6627 -0.6294  0.0190  0.6121  2.3592 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.450e-02 1.204e-01
##  CagingSite            (Intercept) 5.086e-11 7.132e-06
##  OriginSite            (Intercept) 4.131e-02 2.032e-01
##  Residual                          1.191e-01 3.451e-01
## Number of obs: 179, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                      2.1004046  0.2818883   7.451
## TransplantContamLC               0.0001223  0.0890673   0.001
## InjectionPBS                    -0.0703594  0.0735099  -0.957
## OriginContamLC                  -0.1355283  0.2159505  -0.628
## SizeEnd                         -0.0810685  0.0228282  -3.551
## TransplantContamLC:InjectionPBS  0.1653988  0.1045228   1.582
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC InjPBS OrgCLC SizEnd
## TrnsplntCLC -0.151                            
## InjectinPBS -0.101  0.411                     
## OrignCntmLC -0.407 -0.006 -0.005              
## SizeEnd     -0.822 -0.005 -0.033  0.034       
## TrnCLC:IPBS  0.021 -0.586 -0.706  0.013  0.082
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ TransplantContam + Injection + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(mMH2O2 + 1)
##                    Chisq Df Pr(>Chisq)    
## (Intercept)      54.5200  1  1.539e-13 ***
## TransplantContam  1.4266  1  0.2323242    
## Injection         0.0508  1  0.8216143    
## OriginContam      0.4177  1  0.5180867    
## SizeEnd          13.4376  1  0.0002466 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mMH2O2 + 1) ~ TransplantContam + Injection + OriginContam +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 164.6
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.50161 -0.62349 -0.00387  0.65442  2.23814 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01254  0.1120  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.04173  0.2043  
##  Residual                          0.12105  0.3479  
## Number of obs: 179, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)         2.09001    0.28305   7.384
## TransplantContamLC  0.08377    0.07014   1.194
## InjectionPBS        0.01183    0.05248   0.225
## OriginContamLC     -0.13976    0.21624  -0.646
## SizeEnd            -0.08393    0.02290  -3.666
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC InjPBS OrgCLC
## TrnsplntCLC -0.169                     
## InjectinPBS -0.123 -0.004              
## OrignCntmLC -0.407  0.001  0.006       
## SizeEnd     -0.828  0.055  0.034  0.033
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/OxDamRefined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(mMH2O2 + 1)
##              Chisq Df Pr(>Chisq)    
## (Intercept) 70.728  1  < 2.2e-16 ***
## SizeEnd     13.668  1  0.0002182 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mMH2O2 + 1) ~ SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 157.7
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.61419 -0.65373  0.00172  0.62497  2.21411 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.400e-02 1.183e-01
##  CagingSite            (Intercept) 4.228e-11 6.502e-06
##  OriginSite            (Intercept) 3.266e-02 1.807e-01
##  Residual                          1.199e-01 3.463e-01
## Number of obs: 179, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  2.06737    0.24582   8.410
## SizeEnd     -0.08377    0.02266  -3.697
## 
## Correlation of Fixed Effects:
##         (Intr)
## SizeEnd -0.918
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modOXDam <- mod5
RmodOXDam <- MuMIn::r.squaredGLMM(modOXDam)
```
The treatments has no effect on oxidative damage but the oxidative damage are lower in bigger fish 
Represent the relation between fish size and oxidative damage

```r
plot(log(mMH2O2 + 1) ~ SizeEnd, data = dat2)
abline(b = -0.08377, a = 2.06737, col = "red")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/OxDamSize-1.png)<!-- -->

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    log(mMH2O2 + 1) ~ SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(mMH2O2 + 1)
##               Chisq Df Pr(>Chisq)    
## (Intercept) 70.9360  1  < 2.2e-16 ***
## SizeEnd     13.8796  1  0.0001949 ***
## Sex          0.0402  1  0.8411112    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(mMH2O2 + 1) ~ SizeEnd + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 158.3
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.50997 -0.63249  0.01078  0.65634  2.33837 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01358  0.1165  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.03354  0.1831  
##  Residual                          0.11895  0.3449  
## Number of obs: 178, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  2.06736    0.24546   8.422
## SizeEnd     -0.08459    0.02271  -3.726
## SexM         0.01120    0.05585   0.200
## 
## Correlation of Fixed Effects:
##         (Intr) SizEnd
## SizeEnd -0.912       
## SexM     0.005 -0.107
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is non-significant.
The mod5 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct

## Antiox. Capacity
### Check the data

```r
dat2 = dat1[is.na(dat1$mMHCLO) == F, ]
# transform variable to approx. normality
dat2$mMHCLO.p <-
  bimixt::boxcox(dat2$mMHCLO, car::powerTransform(dat2$mMHCLO)$lambda)
plot(dat2$mMHCLO.p, dat2$mMHCLO)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/antiOxIndex-1.png)<!-- -->

```r
hist(dat2$mMHCLO.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/antiOxIndex-2.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(mMHCLO.p ~ OriginContam, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam, data = dat2)
boxplot(mMHCLO.p ~ Injection, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam * Injection, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(mMHCLO.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/antiOxViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    mMHCLO.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: mMHCLO.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             233.3662  1    < 2e-16 ***
## TransplantContam                          0.4058  1    0.52413    
## OriginContam                              2.9468  1    0.08605 .  
## Injection                                 1.7544  1    0.18532    
## SizeEnd                                   0.3068  1    0.57967    
## TransplantContam:OriginContam             0.0314  1    0.85939    
## TransplantContam:Injection                0.6411  1    0.42330    
## OriginContam:Injection                    2.4429  1    0.11806    
## TransplantContam:OriginContam:Injection   3.0174  1    0.08238 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mMHCLO.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 541.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.7540 -0.5760  0.1430  0.6006  5.3445 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.662e-10 1.289e-05
##  CagingSite            (Intercept) 0.000e+00 0.000e+00
##  OriginSite            (Intercept) 4.487e-02 2.118e-01
##  Residual                          1.419e+00 1.191e+00
## Number of obs: 169, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    11.99105    0.78494  15.276
## TransplantContamLC                             -0.22155    0.34781  -0.637
## OriginContamLC                                 -0.72751    0.42380  -1.717
## InjectionPBS                                   -0.46049    0.34766  -1.325
## SizeEnd                                        -0.03909    0.07057  -0.554
## TransplantContamLC:OriginContamLC               0.09041    0.51039   0.177
## TransplantContamLC:InjectionPBS                 0.39216    0.48976   0.801
## OriginContamLC:InjectionPBS                     0.82024    0.52479   1.563
## TransplantContamLC:OriginContamLC:InjectionPBS -1.28948    0.74234  -1.737
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.222                                                     
## OrignCntmLC -0.311  0.417                                              
## InjectinPBS -0.227  0.510  0.420                                       
## SizeEnd     -0.929 -0.003  0.061  0.001                                
## TrnCLC:OCLC  0.122 -0.682 -0.614 -0.347  0.034                         
## TrnCLC:IPBS  0.113 -0.710 -0.293 -0.709  0.051  0.486                  
## OrgCLC:IPBS  0.166 -0.338 -0.598 -0.662 -0.017  0.495      0.469       
## TCLC:OCLC:I -0.074  0.469  0.421  0.468 -0.033 -0.688     -0.660 -0.706
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    mMHCLO.p~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: mMHCLO.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   230.6349  1     <2e-16 ***
## TransplantContam                0.0392  1     0.8430    
## OriginContam                    1.1930  1     0.2747    
## Injection                       0.3317  1     0.5646    
## SizeEnd                         0.3452  1     0.5569    
## TransplantContam:OriginContam   1.9466  1     0.1630    
## TransplantContam:Injection      0.2054  1     0.6504    
## OriginContam:Injection          0.2239  1     0.6361    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mMHCLO.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 545.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8958 -0.5804  0.1787  0.6118  5.4241 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.00000  0.0000  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.03975  0.1994  
##  Residual                          1.43820  1.1992  
## Number of obs: 169, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       11.87136    0.78170  15.187
## TransplantContamLC                 0.06125    0.30928   0.198
## OriginContamLC                    -0.41438    0.37938  -1.092
## InjectionPBS                      -0.17813    0.30927  -0.576
## SizeEnd                           -0.04141    0.07049  -0.588
## TransplantContamLC:OriginContamLC -0.52047    0.37304  -1.395
## TransplantContamLC:InjectionPBS   -0.16791    0.37048  -0.453
## OriginContamLC:InjectionPBS        0.17703    0.37413   0.473
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.214                                          
## OrignCntmLC -0.307  0.280                                   
## InjectinPBS -0.220  0.372  0.283                            
## SizeEnd     -0.936  0.013  0.084  0.019                     
## TrnCLC:OCLC  0.098 -0.560 -0.502 -0.040  0.016              
## TrnCLC:IPBS  0.086 -0.604 -0.023 -0.603  0.038  0.059       
## OrgCLC:IPBS  0.162 -0.011 -0.478 -0.531 -0.058  0.018  0.006
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    mMHCLO.p ~ (TransplantContam + OriginContam ) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: mMHCLO.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   240.1201  1     <2e-16 ***
## TransplantContam                0.0083  1     0.9274    
## OriginContam                    1.0066  1     0.3157    
## Injection                       1.0082  1     0.3153    
## SizeEnd                         0.2955  1     0.5867    
## TransplantContam:OriginContam   1.9226  1     0.1656    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mMHCLO.p ~ (TransplantContam + OriginContam)^2 + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 546.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.9016 -0.5463  0.1922  0.6084  5.4537 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0000   0.0000  
##  CagingSite            (Intercept) 0.0000   0.0000  
##  OriginSite            (Intercept) 0.0387   0.1967  
##  Residual                          1.4245   1.1935  
## Number of obs: 169, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       11.83934    0.76403  15.496
## TransplantContamLC                -0.02235    0.24524  -0.091
## OriginContamLC                    -0.33167    0.33057  -1.003
## InjectionPBS                      -0.18506    0.18430  -1.004
## SizeEnd                           -0.03801    0.06992  -0.544
## TransplantContamLC:OriginContamLC -0.51381    0.37056  -1.387
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.205                            
## OrignCntmLC -0.264  0.376                     
## InjectinPBS -0.142  0.008  0.034              
## SizeEnd     -0.948  0.045  0.065  0.019       
## TrnCLC:OCLC  0.092 -0.660 -0.564  0.008  0.015
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    mMHCLO.p ~ TransplantContam + Injection + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: mMHCLO.p
##                     Chisq Df Pr(>Chisq)    
## (Intercept)      241.0178  1    < 2e-16 ***
## TransplantContam   1.7770  1    0.18252    
## Injection          0.9884  1    0.32012    
## OriginContam       4.2989  1    0.03814 *  
## SizeEnd            0.3013  1    0.58308    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mMHCLO.p ~ TransplantContam + Injection + OriginContam + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 547.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.9991 -0.5639  0.1525  0.6239  5.3481 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.00000  0.000   
##  CagingSite            (Intercept) 0.00000  0.000   
##  OriginSite            (Intercept) 0.04579  0.214   
##  Residual                          1.43108  1.196   
## Number of obs: 169, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        11.95948    0.77035  15.525
## TransplantContamLC -0.24628    0.18475  -1.333
## InjectionPBS       -0.18366    0.18473  -0.994
## OriginContamLC     -0.59335    0.28618  -2.073
## SizeEnd            -0.03880    0.07069  -0.549
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC InjPBS OrgCLC
## TrnsplntCLC -0.192                     
## InjectinPBS -0.142  0.018              
## OrignCntmLC -0.261  0.006  0.044       
## SizeEnd     -0.952  0.074  0.019  0.085
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    mMHCLO.p ~ OriginContam +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/antiOxRefined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: mMHCLO.p
##                  Chisq Df Pr(>Chisq)    
## (Intercept)  4829.6889  1    < 2e-16 ***
## OriginContam    5.5915  1    0.01805 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mMHCLO.p ~ OriginContam + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 544.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1941 -0.5074  0.2039  0.6000  5.5700 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000000 0.00000 
##  CagingSite            (Intercept) 0.003901 0.06246 
##  OriginSite            (Intercept) 0.020520 0.14325 
##  Residual                          1.433492 1.19729 
## Number of obs: 169, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)     11.3444     0.1632  69.496
## OriginContamLC  -0.5580     0.2360  -2.365
## 
## Correlation of Fixed Effects:
##             (Intr)
## OrignCntmLC -0.667
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modantiOx <- mod5
RmodantiOx <- MuMIn::r.squaredGLMM(modantiOx)
```

The origin of the fish affects the non-enzymatic antioxidant capacity (measured in plasma), 
With HC fish having higher non-enzymatic antioxidant capacity than fish from LC sites (see boxplot and table below).

```r
boxplot(mMHCLO.p ~ OriginContam, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/antiOxOrigin-1.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:right;"> 195.3241 </td>
   <td style="text-align:right;"> 6.786571 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:right;"> 223.7893 </td>
   <td style="text-align:right;"> 8.853048 </td>
  </tr>
</tbody>
</table>

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    mMHCLO.p ~ OriginContam + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: mMHCLO.p
##                  Chisq Df Pr(>Chisq)    
## (Intercept)  5096.4526  1    < 2e-16 ***
## OriginContam    7.5057  1    0.00615 ** 
## Sex             0.5553  1    0.45615    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: mMHCLO.p ~ OriginContam + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 542
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1498 -0.5333  0.1974  0.5922  5.5104 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.642e-09 4.052e-05
##  CagingSite            (Intercept) 4.225e-03 6.500e-02
##  OriginSite            (Intercept) 6.740e-03 8.210e-02
##  Residual                          1.443e+00 1.201e+00
## Number of obs: 168, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)     11.4000     0.1597  71.389
## OriginContamLC  -0.5620     0.2051  -2.740
## SexM            -0.1414     0.1897  -0.745
## 
## Correlation of Fixed Effects:
##             (Intr) OrgCLC
## OrignCntmLC -0.571       
## SexM        -0.473 -0.002
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is non-significant.
The mod5 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct

## local inflammatory Response
### Check the data

```r
dat2 = dat1[is.na(dat1$IR) == F, ]
hist(dat2$IR)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/IRIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(IR ~ OriginContam, data = dat2)
boxplot(IR ~ TransplantContam, data = dat2)
boxplot(IR ~ Injection, data = dat2)
boxplot(IR ~ TransplantContam * Injection, data = dat2)
boxplot(IR ~ TransplantContam * OriginContam, data = dat2)
boxplot(IR ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/IRViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    IR ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                              2.0173  1     0.1555    
## TransplantContam                         1.5175  1     0.2180    
## OriginContam                             2.5784  1     0.1083    
## Injection                               49.8556  1  1.655e-12 ***
## SizeEnd                                  0.4385  1     0.5079    
## IRTime                                   2.0751  1     0.1497    
## TransplantContam:OriginContam            0.3677  1     0.5443    
## TransplantContam:Injection               0.3178  1     0.5729    
## OriginContam:Injection                   0.0781  1     0.7799    
## TransplantContam:OriginContam:Injection  0.1481  1     0.7004    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IR ~ (TransplantContam + OriginContam + Injection)^3 + SizeEnd +  
##     IRTime + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1939.2
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.61224 -0.67264 -0.07626  0.66819  2.77716 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.013e+01 3.182e+00
##  CagingSite            (Intercept) 1.495e+01 3.867e+00
##  OriginSite            (Intercept) 2.309e-08 1.519e-04
##  Residual                          1.735e+02 1.317e+01
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                  Estimate Std. Error t value
## (Intercept)                                    -1603.1248  1128.7057  -1.420
## TransplantContamLC                                -8.5645     6.9524  -1.232
## OriginContamLC                                    -6.1207     3.8118  -1.606
## InjectionPBS                                     -23.5048     3.3289  -7.061
## SizeEnd                                            0.4180     0.6313   0.662
## IRTime                                            34.0555    23.6410   1.441
## TransplantContamLC:OriginContamLC                  3.3518     5.5275   0.606
## TransplantContamLC:InjectionPBS                    2.6322     4.6689   0.564
## OriginContamLC:InjectionPBS                       -1.3575     4.8572  -0.279
## TransplantContamLC:OriginContamLC:InjectionPBS    -2.5928     6.7380  -0.385
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd IRTime TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC  0.632                                                            
## OrignCntmLC  0.040  0.291                                                     
## InjectinPBS  0.038  0.252  0.417                                              
## SizeEnd      0.217  0.165  0.055  0.022                                       
## IRTime      -1.000 -0.634 -0.042 -0.039 -0.222                                
## TrnCLC:OCLC  0.277 -0.187 -0.675 -0.275  0.070 -0.276                         
## TrnCLC:IPBS -0.022 -0.339 -0.296 -0.713  0.001  0.023  0.402                  
## OrgCLC:IPBS -0.041 -0.183 -0.599 -0.686 -0.023  0.042  0.400      0.489       
## TCLC:OCLC:I  0.027  0.242  0.431  0.494  0.004 -0.027 -0.568     -0.693 -0.721
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    IR~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                    1.9876  1     0.1586    
## TransplantContam               1.3775  1     0.2405    
## OriginContam                   2.5466  1     0.1105    
## Injection                     62.7080  1  2.397e-15 ***
## SizeEnd                        0.4401  1     0.5071    
## IRTime                         2.0443  1     0.1528    
## TransplantContam:OriginContam  0.2225  1     0.6372    
## TransplantContam:Injection     0.1702  1     0.6799    
## OriginContam:Injection         0.6470  1     0.4212    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IR ~ (TransplantContam + OriginContam + Injection)^2 + SizeEnd +  
##     IRTime + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1945
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.63906 -0.68254 -0.06801  0.67242  2.80334 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.024e+01 3.199e+00
##  CagingSite            (Intercept) 1.492e+01 3.863e+00
##  OriginSite            (Intercept) 2.530e-10 1.591e-05
##  Residual                          1.728e+02 1.314e+01
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                       -1590.5868  1128.2249  -1.410
## TransplantContamLC                   -7.9146     6.7435  -1.174
## OriginContamLC                       -5.4893     3.4398  -1.596
## InjectionPBS                        -22.8716     2.8882  -7.919
## SizeEnd                               0.4183     0.6306   0.663
## IRTime                               33.7869    23.6305   1.430
## TransplantContamLC:OriginContamLC     2.1474     4.5529   0.472
## TransplantContamLC:InjectionPBS       1.3857     3.3590   0.413
## OriginContamLC:InjectionPBS          -2.7037     3.3613  -0.804
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd IRTime TCLC:O TCLC:I
## TrnsplntCLC  0.645                                                 
## OrignCntmLC  0.032  0.213                                          
## InjectinPBS  0.028  0.157  0.259                                   
## SizeEnd      0.216  0.169  0.059  0.023                            
## IRTime      -1.000 -0.647 -0.033 -0.030 -0.221                     
## TrnCLC:OCLC  0.355 -0.063 -0.579  0.008  0.088 -0.354              
## TrnCLC:IPBS -0.006 -0.245  0.004 -0.591  0.005  0.006  0.014       
## OrgCLC:IPBS -0.032 -0.012 -0.460 -0.547 -0.029  0.033 -0.017 -0.021
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    IR ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                    2.0589  1    0.15132    
## TransplantContam               1.4011  1    0.23654    
## OriginContam                   4.8898  1    0.02702 *  
## Injection                     99.9115  1    < 2e-16 ***
## SizeEnd                        0.4089  1    0.52255    
## IRTime                         2.1186  1    0.14551    
## TransplantContam:OriginContam  0.2101  1    0.64666    
## TransplantContam:Injection     0.1563  1    0.69256    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IR ~ (TransplantContam + OriginContam)^2 + (TransplantContam +  
##     Injection)^2 + SizeEnd + IRTime + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1949.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6898 -0.6877 -0.0809  0.6640  2.7638 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)  10.34    3.215  
##  CagingSite            (Intercept)  14.86    3.855  
##  OriginSite            (Intercept)   0.00    0.000  
##  Residual                          172.45   13.132  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                       -1618.3861  1127.8888  -1.435
## TransplantContamLC                   -7.9789     6.7407  -1.184
## OriginContamLC                       -6.7624     3.0581  -2.211
## InjectionPBS                        -24.1425     2.4153  -9.996
## SizeEnd                               0.4030     0.6302   0.639
## IRTime                               34.3843    23.6227   1.456
## TransplantContamLC:OriginContamLC     2.0892     4.5577   0.458
## TransplantContamLC:InjectionPBS       1.3266     3.3553   0.395
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd IRTime TCLC:O
## TrnsplntCLC  0.645                                          
## OrignCntmLC  0.019  0.234                                   
## InjectinPBS  0.013  0.180  0.010                            
## SizeEnd      0.215  0.168  0.051  0.009                     
## IRTime      -1.000 -0.647 -0.021 -0.014 -0.220              
## TrnCLC:OCLC  0.354 -0.064 -0.661 -0.001  0.087 -0.354       
## TrnCLC:IPBS -0.006 -0.245 -0.007 -0.720  0.004  0.007  0.013
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    IR ~ TransplantContam + Injection + OriginContam +  SizeEnd + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##                     Chisq Df Pr(>Chisq)    
## (Intercept)        3.0220  1   0.082145 .  
## TransplantContam   1.2373  1   0.265988    
## Injection        196.5979  1  < 2.2e-16 ***
## OriginContam       6.7803  1   0.009217 ** 
## SizeEnd            0.3747  1   0.540431    
## IRTime             3.0957  1   0.078499 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IR ~ TransplantContam + Injection + OriginContam + SizeEnd +  
##     IRTime + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1959.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.74699 -0.69578 -0.05167  0.67464  2.83876 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)   9.211   3.035  
##  CagingSite            (Intercept)  14.842   3.853  
##  OriginSite            (Intercept)   0.000   0.000  
##  Residual                          171.806  13.107  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                      Estimate Std. Error t value
## (Intercept)        -1805.6127  1038.6776  -1.738
## TransplantContamLC    -7.1777     6.4527  -1.112
## InjectionPBS         -23.4627     1.6734 -14.021
## OriginContamLC        -5.8468     2.2454  -2.604
## SizeEnd                0.3811     0.6225   0.612
## IRTime                38.2893    21.7619   1.759
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC InjPBS OrgCLC SizEnd
## TrnsplntCLC  0.732                            
## InjectinPBS  0.008  0.005                     
## OrignCntmLC  0.364  0.266  0.021              
## SizeEnd      0.201  0.183  0.015  0.148       
## IRTime      -1.000 -0.734 -0.009 -0.365 -0.207
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    IR ~ OriginContam + Injection + IRTime +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
## unable to evaluate scaled gradient
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
## Model failed to converge: degenerate Hessian with 1 negative eigenvalues
```

```r
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/IRRefined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##                 Chisq Df Pr(>Chisq)    
## (Intercept)    1.7985  1    0.17990    
## OriginContam   5.9245  1    0.01493 *  
## Injection    199.6451  1    < 2e-16 ***
## IRTime         1.9059  1    0.16742    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## IR ~ OriginContam + Injection + IRTime + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1983.3
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.69902 -0.68555 -0.06849  0.67737  2.85502 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 9.957e+00  3.155436
##  CagingSite            (Intercept) 1.626e+01  4.032053
##  OriginSite            (Intercept) 1.502e-05  0.003876
##  Residual                          1.709e+02 13.072306
## Number of obs: 249, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    -968.190    721.950  -1.341
## OriginContamLC   -5.297      2.176  -2.434
## InjectionPBS    -23.495      1.663 -14.130
## IRTime           20.809     15.073   1.381
## 
## Correlation of Fixed Effects:
##             (Intr) OrgCLC InjPBS
## OrignCntmLC  0.254              
## InjectinPBS  0.004  0.024       
## IRTime      -1.000 -0.255 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## unable to evaluate scaled gradient
## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues
```
### Refining the model

```r
mod6 <-
 lme4::lmer(
    IR ~  Injection + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod6)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/IRRefined5-1.png)<!-- -->

```r
car::Anova(mod6, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##              Chisq Df Pr(>Chisq)    
## (Intercept) 116.67  1  < 2.2e-16 ***
## Injection   198.82  1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod6)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IR ~ Injection + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1998.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6216 -0.6995 -0.0447  0.6676  2.7812 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)  11.357   3.370  
##  CagingSite            (Intercept)   7.254   2.693  
##  OriginSite            (Intercept)   9.178   3.030  
##  Residual                          170.910  13.073  
## Number of obs: 249, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)    26.195      2.425    10.8
## InjectionPBS  -23.448      1.663   -14.1
## 
## Correlation of Fixed Effects:
##             (Intr)
## InjectinPBS -0.322
```

```r
modIR <- mod6
RmodIR <- MuMIn::r.squaredGLMM(modIR)
```
The immune challenge led to a significant skin swelling, and hence triggered the local immune response.


```r
boxplot(IR ~ Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/IRInjection-1.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> 2.806122 </td>
   <td style="text-align:right;"> 1.288356 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> 26.255052 </td>
   <td style="text-align:right;"> 1.214435 </td>
  </tr>
</tbody>
</table>

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    IR ~ Injection + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: IR
##                Chisq Df Pr(>Chisq)    
## (Intercept) 109.9042  1     <2e-16 ***
## Injection   195.5905  1     <2e-16 ***
## Sex           0.0782  1     0.7798    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: IR ~ Injection + Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 1979.6
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.62888 -0.68051 -0.04432  0.66882  2.78725 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)  12.217   3.495  
##  CagingSite            (Intercept)   6.526   2.555  
##  OriginSite            (Intercept)   9.260   3.043  
##  Residual                          171.764  13.106  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)   26.3880     2.5171   10.48
## InjectionPBS -23.4780     1.6788  -13.98
## SexM          -0.4957     1.7726   -0.28
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS
## InjectinPBS -0.337       
## SexM        -0.295  0.081
```
The sex effect is non-significant.
The mod6 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


## NL inflammatory Response
### compute NL ratio

```r
# compute NL ratio
dat2 = dat1[is.na(dat1$Lymphocytes) == F, ]
dat2 = dat2[is.na(dat2$Neutrophils) == F, ]
dat2$NLRatio = dat2$Neutrophils / dat2$Lymphocytes
# transform variable to approx. normality
dat2$NLRatio.p <-
  bimixt::boxcox(dat2$NLRatio + 1, car::powerTransform(dat2$NLRatio + 1)$lambda)
```

### Visualize

```r
par(mfrow=c(2,3))
boxplot(NLRatio.p ~ OriginContam, data = dat2)
boxplot(NLRatio.p ~ TransplantContam, data = dat2)
boxplot(NLRatio.p ~ Injection, data = dat2)
boxplot(NLRatio.p ~ TransplantContam * Injection, data = dat2)
boxplot(NLRatio.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(NLRatio.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/NLRatio.pViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: NLRatio.p
##                                          Chisq Df Pr(>Chisq)   
## (Intercept)                             0.3430  1   0.558086   
## TransplantContam                        0.7926  1   0.373303   
## OriginContam                            2.7926  1   0.094703 . 
## Injection                               0.0309  1   0.860455   
## SizeEnd                                 7.0077  1   0.008116 **
## TransplantContam:OriginContam           6.5866  1   0.010275 * 
## TransplantContam:Injection              0.1674  1   0.682424   
## OriginContam:Injection                  2.1609  1   0.141565   
## TransplantContam:OriginContam:Injection 2.0030  1   0.156987   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: NLRatio.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -977.9
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.37113 -0.70032 -0.04037  0.62365  2.54298 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.114e-05 3.337e-03
##  CagingSite            (Intercept) 1.699e-12 1.304e-06
##  OriginSite            (Intercept) 5.993e-05 7.742e-03
##  Residual                          5.743e-04 2.396e-02
## Number of obs: 229, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     0.008906   0.015206   0.586
## TransplantContamLC                             -0.005651   0.006348  -0.890
## OriginContamLC                                 -0.016786   0.010045  -1.671
## InjectionPBS                                   -0.001071   0.006094  -0.176
## SizeEnd                                         0.003564   0.001346   2.647
## TransplantContamLC:OriginContamLC               0.023390   0.009114   2.566
## TransplantContamLC:InjectionPBS                -0.003557   0.008693  -0.409
## OriginContamLC:InjectionPBS                     0.013173   0.008961   1.470
## TransplantContamLC:OriginContamLC:InjectionPBS -0.018051   0.012754  -1.415
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.227                                                     
## OrignCntmLC -0.343  0.312                                              
## InjectinPBS -0.195  0.464  0.294                                       
## SizeEnd     -0.886  0.024  0.018  0.001                                
## TrnCLC:OCLC  0.121 -0.695 -0.446 -0.323  0.026                         
## TrnCLC:IPBS  0.131 -0.662 -0.205 -0.701  0.005  0.461                  
## OrgCLC:IPBS  0.135 -0.316 -0.412 -0.680 -0.003  0.454      0.476       
## TCLC:OCLC:I -0.097  0.451  0.289  0.477  0.005 -0.650     -0.682 -0.702
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: NLRatio.p
##                                Chisq Df Pr(>Chisq)   
## (Intercept)                   0.1801  1   0.671308   
## TransplantContam              0.0842  1   0.771721   
## OriginContam                  1.8013  1   0.179559   
## Injection                     0.3106  1   0.577334   
## SizeEnd                       7.2018  1   0.007283 **
## TransplantContam:OriginContam 4.9634  1   0.025889 * 
## TransplantContam:Injection    3.4759  1   0.062269 . 
## OriginContam:Injection        0.4586  1   0.498268   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: NLRatio.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -982.8
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.44899 -0.72905 -0.04652  0.62828  2.62195 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 7.200e-06 0.002683
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 5.874e-05 0.007664
##  Residual                          5.797e-04 0.024077
## Number of obs: 229, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.006413   0.015112   0.424
## TransplantContamLC                -0.001616   0.005570  -0.290
## OriginContamLC                    -0.012745   0.009497  -1.342
## InjectionPBS                       0.002997   0.005378   0.557
## SizeEnd                            0.003617   0.001348   2.684
## TransplantContamLC:OriginContamLC  0.015062   0.006761   2.228
## TransplantContamLC:InjectionPBS   -0.011911   0.006389  -1.864
## OriginContamLC:InjectionPBS        0.004340   0.006409   0.677
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.204                                          
## OrignCntmLC -0.327  0.207                                   
## InjectinPBS -0.171  0.324  0.188                            
## SizeEnd     -0.892  0.025  0.018 -0.001                     
## TrnCLC:OCLC  0.073 -0.588 -0.350 -0.020  0.039              
## TrnCLC:IPBS  0.089 -0.554 -0.011 -0.583  0.012  0.033       
## OrgCLC:IPBS  0.095  0.002 -0.312 -0.551  0.000 -0.004 -0.004
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2 + (TransplantContam + Injection) ^ 2 + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: NLRatio.p
##                                Chisq Df Pr(>Chisq)   
## (Intercept)                   0.1401  1   0.708140   
## TransplantContam              0.0832  1   0.772984   
## OriginContam                  1.3888  1   0.238601   
## Injection                     1.2517  1   0.263235   
## SizeEnd                       7.1261  1   0.007597 **
## TransplantContam:OriginContam 4.8805  1   0.027162 * 
## TransplantContam:Injection    3.4859  1   0.061893 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: NLRatio.p ~ (TransplantContam + OriginContam)^2 + (TransplantContam +  
##     Injection)^2 + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -990.6
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.49767 -0.71902 -0.08241  0.62687  2.58596 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 8.585e-06 0.002930
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 5.974e-05 0.007729
##  Residual                          5.772e-04 0.024026
## Number of obs: 229, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.005638   0.015062   0.374
## TransplantContamLC                -0.001616   0.005601  -0.288
## OriginContamLC                    -0.010722   0.009098  -1.178
## InjectionPBS                       0.005011   0.004479   1.119
## SizeEnd                            0.003597   0.001347   2.669
## TransplantContamLC:OriginContamLC  0.015060   0.006817   2.209
## TransplantContamLC:InjectionPBS   -0.011904   0.006376  -1.867
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O
## TrnsplntCLC -0.206                                   
## OrignCntmLC -0.317  0.220                            
## InjectinPBS -0.143  0.387  0.020                     
## SizeEnd     -0.894  0.025  0.019 -0.001              
## TrnCLC:OCLC  0.074 -0.590 -0.370 -0.026  0.039       
## TrnCLC:IPBS  0.090 -0.550 -0.013 -0.702  0.012  0.033
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2 + Injection + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: NLRatio.p
##                                Chisq Df Pr(>Chisq)   
## (Intercept)                   0.2773  1   0.598502   
## TransplantContam              2.5359  1   0.111286   
## OriginContam                  1.4604  1   0.226867   
## Injection                     0.0717  1   0.788917   
## SizeEnd                       7.2706  1   0.007009 **
## TransplantContam:OriginContam 5.2920  1   0.021424 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: NLRatio.p ~ (TransplantContam + OriginContam)^2 + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -995.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.36244 -0.75384 -0.03035  0.63291  2.44956 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 6.332e-06 0.002516
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 5.984e-05 0.007736
##  Residual                          5.853e-04 0.024194
## Number of obs: 229, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                     Estimate Std. Error t value
## (Intercept)                        0.0079284  0.0150571   0.527
## TransplantContamLC                -0.0073654  0.0046252  -1.592
## OriginContamLC                    -0.0109687  0.0090765  -1.208
## InjectionPBS                      -0.0008599  0.0032118  -0.268
## SizeEnd                            0.0036515  0.0013542   2.696
## TransplantContamLC:OriginContamLC  0.0155111  0.0067427   2.300
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.186                            
## OrignCntmLC -0.315  0.252                     
## InjectinPBS -0.113  0.001  0.015              
## SizeEnd     -0.900  0.038  0.020  0.010       
## TrnCLC:OCLC  0.070 -0.683 -0.367 -0.004  0.039
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2  + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: NLRatio.p
##                                Chisq Df Pr(>Chisq)   
## (Intercept)                   0.2548  1   0.613743   
## TransplantContam              2.5332  1   0.111473   
## OriginContam                  1.4435  1   0.229574   
## SizeEnd                       7.2813  1   0.006967 **
## TransplantContam:OriginContam 5.2818  1   0.021550 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: NLRatio.p ~ (TransplantContam + OriginContam)^2 + SizeEnd + (1 |  
##     CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -1005
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.38697 -0.74246 -0.04835  0.65242  2.43644 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 6.626e-06 0.002574
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 6.021e-05 0.007760
##  Residual                          5.826e-04 0.024138
## Number of obs: 229, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.007545   0.014947   0.505
## TransplantContamLC                -0.007363   0.004626  -1.592
## OriginContamLC                    -0.010929   0.009096  -1.201
## SizeEnd                            0.003648   0.001352   2.698
## TransplantContamLC:OriginContamLC  0.015498   0.006743   2.298
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SizEnd
## TrnsplntCLC -0.187                     
## OrignCntmLC -0.317  0.251              
## SizeEnd     -0.904  0.038  0.019       
## TrnCLC:OCLC  0.070 -0.684 -0.366  0.039
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modNLRatio.p <- mod5
RmodNLRatio.p  <- MuMIn::r.squaredGLMM(modNLRatio.p )
```
Here, the interactions between fish origin and the transplant site are significant. Also, the bigger the fish, the higher the NL ratio and hence the immune inflammatory response.

```r
# check the relationships between th NlRatio and fish size
plot(NLRatio.p ~  SizeEnd, data = dat2)
abline(a = 0.007545 , b =  0.003648 , col = "red")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/NLRatio.pSize-1.png)<!-- -->

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    NLRatio.p ~ (TransplantContam + OriginContam) ^ 2  + SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: NLRatio.p
##                                Chisq Df Pr(>Chisq)   
## (Intercept)                   0.2193  1   0.639579   
## TransplantContam              2.3167  1   0.127989   
## OriginContam                  1.4037  1   0.236111   
## SizeEnd                       7.7020  1   0.005516 **
## Sex                           0.4454  1   0.504542   
## TransplantContam:OriginContam 4.9488  1   0.026109 * 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: NLRatio.p ~ (TransplantContam + OriginContam)^2 + SizeEnd + Sex +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -990.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.33274 -0.75679 -0.06933  0.65173  2.47006 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 5.147e-06 0.002269
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 5.835e-05 0.007639
##  Residual                          5.877e-04 0.024242
## Number of obs: 228, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.006994   0.014934   0.468
## TransplantContamLC                -0.007028   0.004617  -1.522
## OriginContamLC                    -0.010650   0.008989  -1.185
## SizeEnd                            0.003779   0.001362   2.775
## SexM                              -0.002234   0.003347  -0.667
## TransplantContamLC:OriginContamLC  0.014984   0.006735   2.225
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SizEnd SexM  
## TrnsplntCLC -0.187                            
## OrignCntmLC -0.314  0.253                     
## SizeEnd     -0.904  0.050  0.023              
## SexM         0.020 -0.109 -0.024 -0.105       
## TrnCLC:OCLC  0.071 -0.684 -0.371  0.028  0.086
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
The mod5 is thus the best model.

### Posthoc test (pairwise t-test)
The interactions between fish origin and the transplant site are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions.


```r
pairwise.t.test(dat2$NLRatio.p, dat2$CxO2, p.adjust.method = "fdr")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  dat2$NLRatio.p and dat2$CxO2 
## 
##       HC_HC HC_LC LC_HC
## HC_LC 0.074 -     -    
## LC_HC 0.219 0.529 -    
## LC_LC 0.402 0.351 0.531
## 
## P value adjustment method: fdr
```
Here fish from LC sites, have a marginally lower NL ratio than fish from HC site when transplanted in HC site. However, the posthoc are mostly non-significant.

Also,because interaction between the contamination in origin and transplant site of fish is significant
We test for parallel response between replicate populations.

### Test for parallel responses between replicate populations (from the same origin)
#### For Populations from High contamination sites (AUSCOR and RIOU)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: NLRatio.p ~ TransplantContam + OriginSite
## Model 2: NLRatio.p ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df   Deviance Pr(>Chi)
## 1       119   0.065380                       
## 2       118   0.065355  1 2.5656e-05   0.8296
```

```r
coef(mod.par)
```

```
##                       (Intercept)                TransplantContamLC 
##                       0.032098642                      -0.008723136 
##                    OriginSiteRIOU TransplantContamLC:OriginSiteRIOU 
##                       0.023609057                       0.001839351
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = NLRatio.p ~ TransplantContam * OriginSite, data = dat3)
## 
## Deviance Residuals: 
##       Min         1Q     Median         3Q        Max  
## -0.055708  -0.021626   0.000688   0.017677   0.058384  
## 
## Coefficients:
##                                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        0.032099   0.004448   7.217 5.57e-11 ***
## TransplantContamLC                -0.008723   0.006235  -1.399 0.164438    
## OriginSiteRIOU                     0.023609   0.006006   3.931 0.000143 ***
## TransplantContamLC:OriginSiteRIOU  0.001839   0.008546   0.215 0.829961    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.0005538531)
## 
##     Null deviance: 0.085831  on 121  degrees of freedom
## Residual deviance: 0.065355  on 118  degrees of freedom
## AIC: -562.68
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.

#### For Populations from Low contamination sites (ARIMAS and CELFIG)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: NLRatio.p ~ TransplantContam + OriginSite
## Model 2: NLRatio.p ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df   Deviance Pr(>Chi)
## 1       103   0.067919                       
## 2       102   0.067880  1 3.8896e-05    0.809
```

```r
coef(mod.par)
```

```
##                         (Intercept)                  TransplantContamLC 
##                         0.035659899                         0.008085113 
##                    OriginSiteCELFIG TransplantContamLC:OriginSiteCELFIG 
##                        -0.005996004                        -0.002423457
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = NLRatio.p ~ TransplantContam * OriginSite, data = dat4)
## 
## Deviance Residuals: 
##       Min         1Q     Median         3Q        Max  
## -0.043745  -0.017240  -0.002768   0.012915   0.057600  
## 
## Coefficients:
##                                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          0.035660   0.004965   7.183 1.15e-10 ***
## TransplantContamLC                   0.008085   0.007021   1.152    0.252    
## OriginSiteCELFIG                    -0.005996   0.007088  -0.846    0.400    
## TransplantContamLC:OriginSiteCELFIG -0.002423   0.010024  -0.242    0.809    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.0006654894)
## 
##     Null deviance: 0.070555  on 105  degrees of freedom
## Residual deviance: 0.067880  on 102  degrees of freedom
## AIC: -468.65
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of LC populations are parallel.

### Visualize 
The plot reported below correspond to the figure 2C from the manuscript

```r
NLratioPlot1()
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/NLRatio.pFinalViz-1.png)<!-- -->

## Available energy - Index (sum of lipids, proteins and carbohydrates)
### Check the data

```r
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
hist(log(dat2$AvailableEnerJ + 1))
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/AvailableEnerJIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(log(dat2$AvailableEnerJ + 1) ~ OriginContam, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ Injection, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam * Injection, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$AvailableEnerJ + 1) ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/AvailableEnerJViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$AvailableEnerJ + 1)
##                                             Chisq Df Pr(>Chisq)    
## (Intercept)                             1877.0660  1    < 2e-16 ***
## TransplantContam                           0.8135  1    0.36709    
## OriginContam                               0.1111  1    0.73890    
## Injection                                  0.0048  1    0.94484    
## SizeEnd                                    0.2945  1    0.58737    
## TransplantContam:OriginContam              5.6046  1    0.01791 *  
## TransplantContam:Injection                 1.2846  1    0.25705    
## OriginContam:Injection                     0.0026  1    0.95910    
## TransplantContam:OriginContam:Injection    0.3734  1    0.54115    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam +  
##     Injection)^3 + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 140.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1265 -0.5752 -0.0303  0.7151  3.1968 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 6.604e-11 8.127e-06
##  CagingSite            (Intercept) 4.647e-03 6.817e-02
##  OriginSite            (Intercept) 8.682e-03 9.318e-02
##  Residual                          9.059e-02 3.010e-01
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     8.294203   0.191441  43.325
## TransplantContamLC                              0.092137   0.102154   0.902
## OriginContamLC                                 -0.040483   0.121459  -0.333
## InjectionPBS                                   -0.005424   0.078395  -0.069
## SizeEnd                                         0.008824   0.016261   0.543
## TransplantContamLC:OriginContamLC              -0.253471   0.107067  -2.367
## TransplantContamLC:InjectionPBS                 0.122759   0.108310   1.133
## OriginContamLC:InjectionPBS                     0.005768   0.112469   0.051
## TransplantContamLC:OriginContamLC:InjectionPBS -0.094713   0.154994  -0.611
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.311                                                     
## OrignCntmLC -0.348  0.245                                              
## InjectinPBS -0.213  0.377  0.317                                       
## SizeEnd     -0.858  0.044  0.036  0.013                                
## TrnCLC:OCLC  0.135 -0.527 -0.465 -0.359  0.015                         
## TrnCLC:IPBS  0.143 -0.521 -0.229 -0.724  0.003  0.497                  
## OrgCLC:IPBS  0.145 -0.263 -0.442 -0.697 -0.006  0.501      0.504       
## TCLC:OCLC:I -0.097  0.364  0.320  0.506 -0.005 -0.690     -0.699 -0.726
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$AvailableEnerJ + 1)
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   1895.3183  1  < 2.2e-16 ***
## TransplantContam                 1.4584  1  0.2271831    
## OriginContam                     0.0212  1  0.8843552    
## Injection                        0.0775  1  0.7806793    
## SizeEnd                          0.2929  1  0.5883630    
## TransplantContam:OriginContam   14.8819  1  0.0001145 ***
## TransplantContam:Injection       0.9777  1  0.3227703    
## OriginContam:Injection           0.3257  1  0.5682284    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam +  
##     Injection)^2 + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 139.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1704 -0.5721 -0.0226  0.7025  3.2394 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000000 0.00000 
##  CagingSite            (Intercept) 0.004647 0.06817 
##  OriginSite            (Intercept) 0.008634 0.09292 
##  Residual                          0.090348 0.30058 
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        8.282686   0.190252  43.535
## TransplantContamLC                 0.114845   0.095098   1.208
## OriginContamLC                    -0.016697   0.114795  -0.145
## InjectionPBS                       0.018805   0.067539   0.278
## SizeEnd                            0.008788   0.016237   0.541
## TransplantContamLC:OriginContamLC -0.298608   0.077406  -3.858
## TransplantContamLC:InjectionPBS    0.076508   0.077376   0.989
## OriginContamLC:InjectionPBS       -0.044103   0.077283  -0.571
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.298                                          
## OrignCntmLC -0.336  0.146                                   
## InjectinPBS -0.191  0.241  0.190                            
## SizeEnd     -0.862  0.050  0.040  0.019                     
## TrnCLC:OCLC  0.094 -0.409 -0.356 -0.017  0.016              
## TrnCLC:IPBS  0.105 -0.400 -0.008 -0.600 -0.001  0.029       
## OrgCLC:IPBS  0.109  0.002 -0.322 -0.556 -0.014  0.001 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam) ^ 2 + Injection + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$AvailableEnerJ + 1)
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   1941.0315  1  < 2.2e-16 ***
## TransplantContam                 3.0309  1    0.08169 .  
## OriginContam                     0.1145  1    0.73503    
## Injection                        0.9462  1    0.33070    
## SizeEnd                          0.2887  1    0.59106    
## TransplantContam:OriginContam   15.1597  1  9.879e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam)^2 +  
##     Injection + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |      OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 134.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2055 -0.6493 -0.0609  0.6893  3.3458 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.076e-08 0.0001037
##  CagingSite            (Intercept) 4.724e-03 0.0687319
##  OriginSite            (Intercept) 8.570e-03 0.0925770
##  Residual                          9.007e-02 0.3001240
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        8.274302   0.187808  44.057
## TransplantContamLC                 0.152433   0.087558   1.741
## OriginContamLC                    -0.036669   0.108346  -0.338
## InjectionPBS                       0.037525   0.038578   0.973
## SizeEnd                            0.008708   0.016208   0.537
## TransplantContamLC:OriginContamLC -0.300800   0.077256  -3.894
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.283                            
## OrignCntmLC -0.320  0.164                     
## InjectinPBS -0.117  0.001  0.010              
## SizeEnd     -0.871  0.053  0.037  0.019       
## TrnCLC:OCLC  0.092 -0.431 -0.376  0.003  0.016
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam) ^ 2 + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$AvailableEnerJ + 1)
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   7672.7861  1  < 2.2e-16 ***
## TransplantContam                 2.9297  1    0.08697 .  
## OriginContam                     0.1164  1    0.73292    
## TransplantContam:OriginContam   15.3300  1  9.027e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam)^2 +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 124.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.0982 -0.6056 -0.0182  0.6854  3.3961 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.186e-09 3.444e-05
##  CagingSite            (Intercept) 4.786e-03 6.918e-02
##  OriginSite            (Intercept) 1.047e-02 1.023e-01
##  Residual                          8.965e-02 2.994e-01
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        8.38025    0.09567  87.594
## TransplantContamLC                 0.15013    0.08771   1.712
## OriginContamLC                    -0.03980    0.11663  -0.341
## TransplantContamLC:OriginContamLC -0.30173    0.07706  -3.915
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.467              
## OrignCntmLC -0.606  0.149       
## TrnCLC:OCLC  0.207 -0.430 -0.349
```

```r
modAvailableEnerJ <- mod4
RmodAvailableEnerJ  <- MuMIn::r.squaredGLMM(modAvailableEnerJ)
```
Here, the interactions between fish origin and the transplant site are significant. 

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam) ^ 2  + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$AvailableEnerJ + 1)
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   7513.2256  1  < 2.2e-16 ***
## TransplantContam                 2.8305  1  0.0924909 .  
## OriginContam                     0.1612  1  0.6880167    
## Sex                              0.2442  1  0.6211753    
## TransplantContam:OriginContam   14.7641  1  0.0001218 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$AvailableEnerJ + 1) ~ (TransplantContam + OriginContam)^2 +  
##     Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 126.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1252 -0.5996 -0.0031  0.6878  3.3782 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 5.742e-11 7.577e-06
##  CagingSite            (Intercept) 5.320e-03 7.294e-02
##  OriginSite            (Intercept) 9.999e-03 1.000e-01
##  Residual                          8.942e-02 2.990e-01
## Number of obs: 242, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        8.38668    0.09676  86.679
## TransplantContamLC                 0.15276    0.09080   1.682
## OriginContamLC                    -0.04607    0.11472  -0.402
## SexM                              -0.01974    0.03994  -0.494
## TransplantContamLC:OriginContamLC -0.29793    0.07754  -3.842
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SexM  
## TrnsplntCLC -0.468                     
## OrignCntmLC -0.584  0.148              
## SexM        -0.138 -0.055 -0.024       
## TrnCLC:OCLC  0.189 -0.418 -0.357  0.101
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
The mod5 is thus the best model.

### Posthoc test (pairwise t-test)
The interactions between fish origin and the transplant site are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions.


```r
pairwise.t.test(log(dat2$AvailableEnerJ + 1), dat2$CxO2, p.adjust.method = "fdr")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  log(dat2$AvailableEnerJ + 1) and dat2$CxO2 
## 
##       HC_HC  HC_LC  LC_HC  
## HC_LC 0.3430 -      -      
## LC_HC 0.0150 0.0012 -      
## LC_LC 0.0012 0.0150 1.3e-08
## 
## P value adjustment method: fdr
```
Here the available energy increases in LC fish when transplanted in HC sites. Similarly, the available energy increases in HC fish when transplanted in LC sites, suggesting that local condition within each study site could have driven the plastic responses rather than the level of contamination. 

Also,because interaction between the contamination in origin and transplant site of fish is significant
We test for parallel response between replicate populations.

### Test for parallel responses between replicate populations (from the same origin)
#### For Populations from High contamination sites (AUSCOR and RIOU)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: log(dat3$AvailableEnerJ + 1) ~ TransplantContam + OriginSite
## Model 2: log(dat3$AvailableEnerJ + 1) ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1       121     14.959                     
## 2       120     14.684  1  0.27469   0.1341
```

```r
coef(mod.par)
```

```
##                       (Intercept)                TransplantContamLC 
##                         8.2574774                         0.2463168 
##                    OriginSiteRIOU TransplantContamLC:OriginSiteRIOU 
##                         0.2419375                        -0.1888528
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = log(dat3$AvailableEnerJ + 1) ~ TransplantContam * 
##     OriginSite, data = dat3)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.94707  -0.21939   0.00947   0.22058   1.01919  
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        8.25748    0.06732 122.657  < 2e-16 ***
## TransplantContamLC                 0.24632    0.09078   2.713  0.00764 ** 
## OriginSiteRIOU                     0.24194    0.09141   2.647  0.00922 ** 
## TransplantContamLC:OriginSiteRIOU -0.18885    0.12605  -1.498  0.13670    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.1223704)
## 
##     Null deviance: 16.204  on 123  degrees of freedom
## Residual deviance: 14.684  on 120  degrees of freedom
## AIC: 97.344
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are not significantly different, we hence cannot reject the null hypothesis: the slopes of HC populations are parallel.

#### For Populations from Low contamination sites (ARIMAS and CELFIG)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: log(dat4$AvailableEnerJ + 1) ~ TransplantContam + OriginSite
## Model 2: log(dat4$AvailableEnerJ + 1) ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1       115     6.5419                       
## 2       114     6.2558  1  0.28616   0.0224 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
coef(mod.par)
```

```
##                         (Intercept)                  TransplantContamLC 
##                          8.34886418                         -0.04548398 
##                    OriginSiteCELFIG TransplantContamLC:OriginSiteCELFIG 
##                         -0.03205983                         -0.19760377
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = log(dat4$AvailableEnerJ + 1) ~ TransplantContam * 
##     OriginSite, data = dat4)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.61943  -0.13182  -0.00938   0.16106   0.66008  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          8.34886    0.04350 191.928   <2e-16 ***
## TransplantContamLC                  -0.04548    0.06006  -0.757   0.4504    
## OriginSiteCELFIG                    -0.03206    0.06327  -0.507   0.6133    
## TransplantContamLC:OriginSiteCELFIG -0.19760    0.08653  -2.284   0.0242 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.05487516)
## 
##     Null deviance: 7.7037  on 117  degrees of freedom
## Residual deviance: 6.2558  on 114  degrees of freedom
## AIC: -1.7178
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are significantly different, we hence reject the null hypothesis. here, the slopes of LC populations are not parallel.

### Visualize 

```r
AvailEnerPlot1()
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/AvailableEnerJFinalViz-1.png)<!-- -->

## Available energy - Lipids
### Check the data

```r
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
# transform variable to approx. normality
dat2$MuscleLipid.p <-
  bimixt::boxcox(dat2$MuscleLipid, car::powerTransform(dat2$MuscleLipid)$lambda)
```

### Visualize

```r
par(mfrow=c(2,3))
boxplot(MuscleLipid.p  ~ OriginContam, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam, data = dat2)
boxplot(MuscleLipid.p  ~ Injection, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam * Injection, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam * OriginContam, data = dat2)
boxplot(MuscleLipid.p  ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/LipidsViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleLipid.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             239.7471  1    < 2e-16 ***
## TransplantContam                          0.8741  1    0.34983    
## OriginContam                              0.0301  1    0.86220    
## Injection                                 0.1457  1    0.70266    
## SizeEnd                                   0.5445  1    0.46058    
## TransplantContam:OriginContam             4.7471  1    0.02935 *  
## TransplantContam:Injection                2.1557  1    0.14204    
## OriginContam:Injection                    0.0222  1    0.88161    
## TransplantContam:OriginContam:Injection   1.3112  1    0.25218    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleLipid.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 945.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3883 -0.5605  0.0452  0.6799  3.0051 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0000   0.0000  
##  CagingSite            (Intercept) 0.1360   0.3688  
##  OriginSite            (Intercept) 0.1395   0.3735  
##  Residual                          2.8308   1.6825  
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    15.62969    1.00942  15.484
## TransplantContamLC                              0.52629    0.56293   0.935
## OriginContamLC                                 -0.09958    0.57370  -0.174
## InjectionPBS                                   -0.16728    0.43822  -0.382
## SizeEnd                                         0.06477    0.08778   0.738
## TransplantContamLC:OriginContamLC              -1.30400    0.59850  -2.179
## TransplantContamLC:InjectionPBS                 0.88893    0.60544   1.468
## OriginContamLC:InjectionPBS                     0.09364    0.62870   0.149
## TransplantContamLC:OriginContamLC:InjectionPBS -0.99210    0.86641  -1.145
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.324                                                     
## OrignCntmLC -0.320  0.295                                              
## InjectinPBS -0.224  0.383  0.375                                       
## SizeEnd     -0.879  0.044  0.041  0.012                                
## TrnCLC:OCLC  0.143 -0.535 -0.550 -0.359  0.015                         
## TrnCLC:IPBS  0.150 -0.528 -0.271 -0.724  0.005  0.497                  
## OrgCLC:IPBS  0.153 -0.267 -0.523 -0.697 -0.005  0.501      0.504       
## TCLC:OCLC:I -0.102  0.369  0.379  0.506 -0.007 -0.690     -0.699 -0.726
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
     MuscleLipid.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleLipid.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   238.8706  1  < 2.2e-16 ***
## TransplantContam                2.1314  1     0.1443    
## OriginContam                    0.0801  1     0.7772    
## Injection                       0.0523  1     0.8190    
## SizeEnd                         0.5432  1     0.4611    
## TransplantContam:OriginContam  16.7946  1  4.165e-05 ***
## TransplantContam:Injection      0.8709  1     0.3507    
## OriginContam:Injection          0.9810  1     0.3220    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleLipid.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 948.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4605 -0.5650  0.0736  0.6661  3.0749 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 3.336e-09 5.776e-05
##  CagingSite            (Intercept) 1.360e-01 3.688e-01
##  OriginSite            (Intercept) 1.370e-01 3.702e-01
##  Residual                          2.835e+00 1.684e+00
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       15.50634    1.00329  15.455
## TransplantContamLC                 0.76417    0.52343   1.460
## OriginContamLC                     0.14962    0.52872   0.283
## InjectionPBS                       0.08655    0.37831   0.229
## SizeEnd                            0.06466    0.08773   0.737
## TransplantContamLC:OriginContamLC -1.77675    0.43355  -4.098
## TransplantContamLC:InjectionPBS    0.40446    0.43341   0.933
## OriginContamLC:InjectionPBS       -0.42876    0.43290  -0.990
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.311                                          
## OrignCntmLC -0.305  0.181                                   
## InjectinPBS -0.201  0.245  0.231                            
## SizeEnd     -0.884  0.050  0.047  0.018                     
## TrnCLC:OCLC  0.102 -0.416 -0.433 -0.017  0.014              
## TrnCLC:IPBS  0.111 -0.407 -0.009 -0.600  0.001  0.029       
## OrgCLC:IPBS  0.116  0.002 -0.391 -0.556 -0.015  0.001 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleLipid.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   245.6390  1  < 2.2e-16 ***
## TransplantContam                4.0056  1    0.04535 *  
## OriginContam                    0.0104  1    0.91884    
## Injection                       0.1728  1    0.67762    
## SizeEnd                         0.5289  1    0.46707    
## TransplantContam:OriginContam  17.0371  1  3.666e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleLipid.p ~ (TransplantContam + OriginContam)^2 + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 950.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4628 -0.5463  0.0406  0.6027  3.2011 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 3.042e-09 5.515e-05
##  CagingSite            (Intercept) 1.389e-01 3.727e-01
##  OriginSite            (Intercept) 1.356e-01 3.682e-01
##  Residual                          2.833e+00 1.683e+00
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       15.51410    0.98987  15.673
## TransplantContamLC                 0.96279    0.48106   2.001
## OriginContamLC                    -0.04942    0.48497  -0.102
## InjectionPBS                       0.08993    0.21634   0.416
## SizeEnd                            0.06374    0.08764   0.727
## TransplantContamLC:OriginContamLC -1.78812    0.43321  -4.128
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.296                            
## OrignCntmLC -0.284  0.209                     
## InjectinPBS -0.124  0.001  0.013              
## SizeEnd     -0.894  0.055  0.045  0.018       
## TrnCLC:OCLC  0.100 -0.440 -0.471  0.003  0.014
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam) ^ 2 +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleLipid.p
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   1213.6699  1  < 2.2e-16 ***
## TransplantContam                 3.8681  1    0.04921 *  
## OriginContam                     0.0155  1    0.90078    
## TransplantContam:OriginContam   17.2823  1  3.222e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## MuscleLipid.p ~ (TransplantContam + OriginContam)^2 + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 946.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5072 -0.5768  0.0792  0.6150  3.2119 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0000   0.0000  
##  CagingSite            (Intercept) 0.1401   0.3743  
##  OriginSite            (Intercept) 0.1966   0.4435  
##  Residual                          2.8104   1.6764  
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       16.19882    0.46498  34.838
## TransplantContamLC                 0.94566    0.48082   1.967
## OriginContamLC                    -0.06771    0.54309  -0.125
## TransplantContamLC:OriginContamLC -1.79370    0.43147  -4.157
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.527              
## OrignCntmLC -0.579  0.183       
## TrnCLC:OCLC  0.239 -0.439 -0.420
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modLipids <- mod4
RmodLipids  <- MuMIn::r.squaredGLMM(modLipids)
```

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    MuscleLipid.p ~ (TransplantContam + OriginContam) ^ 2  + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleLipid.p
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   1189.1135  1  < 2.2e-16 ***
## TransplantContam                 3.7936  1    0.05145 .  
## OriginContam                     0.0291  1    0.86448    
## Sex                              0.2211  1    0.63820    
## TransplantContam:OriginContam   16.7933  1  4.168e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleLipid.p ~ (TransplantContam + OriginContam)^2 + Sex + (1 |  
##     CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 943.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4675 -0.5731  0.0823  0.6265  3.1854 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0000   0.0000  
##  CagingSite            (Intercept) 0.1505   0.3879  
##  OriginSite            (Intercept) 0.1855   0.4307  
##  Residual                          2.8175   1.6786  
## Number of obs: 242, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       16.23357    0.47076  34.484
## TransplantContamLC                 0.95939    0.49257   1.948
## OriginContamLC                    -0.09114    0.53398  -0.171
## SexM                              -0.10539    0.22414  -0.470
## TransplantContamLC:OriginContamLC -1.78346    0.43521  -4.098
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SexM  
## TrnsplntCLC -0.522                     
## OrignCntmLC -0.555  0.184              
## SexM        -0.160 -0.057 -0.029       
## TrnCLC:OCLC  0.218 -0.432 -0.431  0.101
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is non-significant.
The mod4 is thus the best model.
Here, the interactions between fish origin and the transplant site are significant, confirming the pattern observed for the integrative measure of available energy previously observed.

## Available energy - Proteins
### Check the data

```r
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
hist(dat2$MuscleProtein)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ProteinsIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(MuscleProtein  ~ OriginContam, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam, data = dat2)
boxplot(MuscleProtein  ~ Injection, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam * Injection, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam * OriginContam, data = dat2)
boxplot(MuscleProtein  ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ProteinsViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    MuscleProtein ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleProtein
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             72.3614  1    < 2e-16 ***
## TransplantContam                         0.0505  1    0.82225    
## OriginContam                             0.5392  1    0.46276    
## Injection                                0.1729  1    0.67756    
## SizeEnd                                  0.0200  1    0.88757    
## TransplantContam:OriginContam            1.1311  1    0.28754    
## TransplantContam:Injection               1.0491  1    0.30571    
## OriginContam:Injection                   0.1112  1    0.73880    
## TransplantContam:OriginContam:Injection  2.9188  1    0.08755 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleProtein ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 3336.7
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.60471 -0.62743  0.01437  0.59565  2.93523 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.963e-09 4.430e-05
##  CagingSite            (Intercept) 0.000e+00 0.000e+00
##  OriginSite            (Intercept) 1.171e+04 1.082e+02
##  Residual                          7.769e+04 2.787e+02
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    1526.254    179.421   8.507
## TransplantContamLC                               15.825     70.440   0.225
## OriginContamLC                                  -95.483    130.032  -0.734
## InjectionPBS                                     30.187     72.600   0.416
## SizeEnd                                          -2.159     15.273  -0.141
## TransplantContamLC:OriginContamLC              -105.421     99.123  -1.064
## TransplantContamLC:InjectionPBS                -102.738    100.304  -1.024
## OriginContamLC:InjectionPBS                      34.730    104.155   0.333
## TransplantContamLC:OriginContamLC:InjectionPBS  245.234    143.540   1.708
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.255                                                     
## OrignCntmLC -0.388  0.285                                              
## InjectinPBS -0.211  0.507  0.275                                       
## SizeEnd     -0.859  0.058  0.030  0.015                                
## TrnCLC:OCLC  0.131 -0.707 -0.402 -0.360  0.017                         
## TrnCLC:IPBS  0.141 -0.699 -0.198 -0.724  0.003  0.497                  
## OrgCLC:IPBS  0.144 -0.353 -0.383 -0.697 -0.006  0.501      0.505       
## TCLC:OCLC:I -0.096  0.488  0.277  0.506 -0.005 -0.690     -0.699 -0.726
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
     MuscleProtein ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleProtein
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   75.1757  1    < 2e-16 ***
## TransplantContam               0.4846  1    0.48633    
## OriginContam                   1.5581  1    0.21195    
## Injection                      0.2680  1    0.60469    
## SizeEnd                        0.0182  1    0.89255    
## TransplantContam:OriginContam  0.0252  1    0.87389    
## TransplantContam:Injection     0.0558  1    0.81328    
## OriginContam:Injection         5.1861  1    0.02277 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleProtein ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 3351.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.69191 -0.66757  0.03888  0.63366  3.03282 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 0.000e+00 0.000e+00
##  CagingSite            (Intercept) 1.930e-11 4.393e-06
##  OriginSite            (Intercept) 1.190e+04 1.091e+02
##  Residual                          7.832e+04 2.799e+02
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       1556.133    179.477   8.670
## TransplantContamLC                 -42.962     61.714  -0.696
## OriginContamLC                    -157.057    125.824  -1.248
## InjectionPBS                       -32.552     62.882  -0.518
## SizeEnd                             -2.072     15.339  -0.135
## TransplantContamLC:OriginContamLC   11.433     72.029   0.159
## TransplantContamLC:InjectionPBS     17.016     72.040   0.236
## OriginContamLC:InjectionPBS        163.859     71.953   2.277
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.239                                          
## OrignCntmLC -0.378  0.178                                   
## InjectinPBS -0.189  0.345  0.162                            
## SizeEnd     -0.863  0.069  0.033  0.020                     
## TrnCLC:OCLC  0.090 -0.586 -0.302 -0.017  0.019              
## TrnCLC:IPBS  0.104 -0.573 -0.006 -0.600 -0.001  0.029       
## OrgCLC:IPBS  0.108  0.002 -0.274 -0.556 -0.014  0.001 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    MuscleProtein ~ (Injection + OriginContam) ^ 2 +  TransplantContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleProtein
##                          Chisq Df Pr(>Chisq)    
## (Intercept)            76.3940  1    < 2e-16 ***
## Injection               0.2228  1    0.63692    
## OriginContam            1.5859  1    0.20792    
## TransplantContam        0.6546  1    0.41846    
## SizeEnd                 0.0200  1    0.88740    
## Injection:OriginContam  5.2335  1    0.02216 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: MuscleProtein ~ (Injection + OriginContam)^2 + TransplantContam +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 3372.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.7069 -0.6835  0.0192  0.6125  3.0499 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)     0      0.0   
##  CagingSite            (Intercept)     0      0.0   
##  OriginSite            (Intercept) 11926    109.2   
##  Residual                          77683    278.7   
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                 1549.863    177.322   8.740
## InjectionPBS                 -23.641     50.086  -0.472
## OriginContamLC              -151.109    119.993  -1.259
## TransplantContamLC           -29.267     36.173  -0.809
## SizeEnd                       -2.163     15.278  -0.142
## InjectionPBS:OriginContamLC  163.928     71.657   2.288
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS OrgCLC TrnCLC SizEnd
## InjectinPBS -0.160                            
## OrignCntmLC -0.373  0.206                     
## TrnsplntCLC -0.223  0.003  0.003              
## SizeEnd     -0.872  0.024  0.040  0.136       
## InjPBS:OCLC  0.109 -0.699 -0.285  0.000 -0.014
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    MuscleProtein ~ (Injection + OriginContam) ^ 2 +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleProtein
##                           Chisq Df Pr(>Chisq)    
## (Intercept)            339.6044  1    < 2e-16 ***
## Injection                0.2213  1    0.63809    
## OriginContam             1.6859  1    0.19415    
## Injection:OriginContam   5.2605  1    0.02181 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## MuscleProtein ~ (Injection + OriginContam)^2 + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 3389.2
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.75840 -0.65962 -0.01681  0.57098  3.10906 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)     0      0     
##  CagingSite            (Intercept)     0      0     
##  OriginSite            (Intercept) 11026    105     
##  Residual                          77287    278     
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                  1513.01      82.10  18.428
## InjectionPBS                  -23.49      49.94  -0.470
## OriginContamLC               -150.65     116.02  -1.298
## InjectionPBS:OriginContamLC   163.91      71.47   2.294
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS OrgCLC
## InjectinPBS -0.299              
## OrignCntmLC -0.708  0.212       
## InjPBS:OCLC  0.209 -0.699 -0.294
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modProtein <- mod4
RmodProtein  <- MuMIn::r.squaredGLMM(modProtein)
```

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    MuscleProtein ~ (Injection + OriginContam) ^ 2   + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: MuscleProtein
##                           Chisq Df Pr(>Chisq)    
## (Intercept)            338.1336  1    < 2e-16 ***
## Injection                0.2654  1    0.60641    
## OriginContam             2.0614  1    0.15107    
## Sex                      0.3682  1    0.54396    
## Injection:OriginContam   6.3645  1    0.01164 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## MuscleProtein ~ (Injection + OriginContam)^2 + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 3356.6
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.76403 -0.63054  0.02422  0.60396  2.65019 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept)     0      0.0   
##  CagingSite            (Intercept)     0      0.0   
##  OriginSite            (Intercept) 10824    104.0   
##  Residual                          74589    273.1   
## Number of obs: 242, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                  1522.81      82.81  18.388
## InjectionPBS                  -25.33      49.16  -0.515
## OriginContamLC               -164.92     114.87  -1.436
## SexM                          -22.09      36.40  -0.607
## InjectionPBS:OriginContamLC   177.47      70.35   2.523
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS OrgCLC SexM  
## InjectinPBS -0.303                     
## OrignCntmLC -0.695  0.210              
## SexM        -0.195  0.062  0.010       
## InjPBS:OCLC  0.202 -0.696 -0.293  0.010
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is non-significant.
The mod4 is thus the best model.
Here, the interactions between fish origin and the immune challenge are significant, with LC fish site having a decreasing level of proteins in their muscle when immune challenged. On the contrary HC fish have a constant level of proteins, whatever the injection (control saline solution or antigen mixture).

```r
pairwise.t.test(dat2$MuscleProtein, dat2$O2xI, p.adjust.method = "fdr")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  dat2$MuscleProtein and dat2$O2xI 
## 
##         HC_AMIX HC_PBS LC_AMIX
## HC_PBS  0.8066  -      -      
## LC_AMIX 0.0057  0.0111 -      
## LC_PBS  0.8066  0.8066 0.0098 
## 
## P value adjustment method: fdr
```

## Available energy - Carbohydrates
### Check the data

```r
dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
hist(log(dat2$MuscleCarbohydrate))
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CarbohIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(log(dat2$MuscleCarbohydrate)  ~ OriginContam, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ Injection, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam * Injection, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam * OriginContam, data = dat2)
boxplot(log(dat2$MuscleCarbohydrate)  ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CarbohViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$MuscleCarbohydrate)
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             367.1817  1    < 2e-16 ***
## TransplantContam                          0.5896  1    0.44259    
## OriginContam                              4.3332  1    0.03738 *  
## Injection                                 0.5074  1    0.47625    
## SizeEnd                                   0.3274  1    0.56717    
## TransplantContam:OriginContam             6.0797  1    0.01367 *  
## TransplantContam:Injection                0.0196  1    0.88857    
## OriginContam:Injection                    1.7999  1    0.17973    
## TransplantContam:OriginContam:Injection   0.1076  1    0.74287    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam +  
##     Injection)^3 + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 119.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5702 -0.6259  0.1120  0.6593  2.5329 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0002886 0.01699 
##  CagingSite            (Intercept) 0.0179209 0.13387 
##  OriginSite            (Intercept) 0.0000000 0.00000 
##  Residual                          0.0824606 0.28716 
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     3.186285   0.166282  19.162
## TransplantContamLC                              0.117167   0.152594   0.768
## OriginContamLC                                  0.156032   0.074957   2.082
## InjectionPBS                                    0.053292   0.074811   0.712
## SizeEnd                                         0.007105   0.012416   0.572
## TransplantContamLC:OriginContamLC              -0.254236   0.103109  -2.466
## TransplantContamLC:InjectionPBS                 0.014481   0.103351   0.140
## OriginContamLC:InjectionPBS                    -0.143983   0.107322  -1.342
## TransplantContamLC:OriginContamLC:InjectionPBS  0.048517   0.147890   0.328
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.483                                                     
## OrignCntmLC -0.259  0.246                                              
## InjectinPBS -0.226  0.241  0.490                                       
## SizeEnd     -0.758  0.025  0.044  0.006                                
## TrnCLC:OCLC  0.152 -0.340 -0.725 -0.356  0.015                         
## TrnCLC:IPBS  0.149 -0.333 -0.354 -0.724  0.015  0.493                  
## OrgCLC:IPBS  0.156 -0.168 -0.683 -0.697 -0.002  0.497      0.505       
## TCLC:OCLC:I -0.101  0.232  0.495  0.506 -0.014 -0.684     -0.699 -0.726
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
     log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$MuscleCarbohydrate)
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   372.6047  1  < 2.2e-16 ***
## TransplantContam                0.5049  1   0.477360    
## OriginContam                    4.8750  1   0.027249 *  
## Injection                       0.4016  1   0.526289    
## SizeEnd                         0.3325  1   0.564180    
## TransplantContam:OriginContam   9.4054  1   0.002163 ** 
## TransplantContam:Injection      0.2694  1   0.603704    
## OriginContam:Injection          2.5818  1   0.108098    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam +  
##     Injection)^2 + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 117.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5977 -0.6287  0.1023  0.6809  2.5155 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0003481 0.01866 
##  CagingSite            (Intercept) 0.0179094 0.13383 
##  OriginSite            (Intercept) 0.0000000 0.00000 
##  Residual                          0.0821005 0.28653 
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        3.191944   0.165360  19.303
## TransplantContamLC                 0.105441   0.148393   0.711
## OriginContamLC                     0.143834   0.065144   2.208
## InjectionPBS                       0.040809   0.064399   0.634
## SizeEnd                            0.007155   0.012408   0.577
## TransplantContamLC:OriginContamLC -0.231120   0.075361  -3.067
## TransplantContamLC:InjectionPBS    0.038293   0.073770   0.519
## OriginContamLC:InjectionPBS       -0.118397   0.073685  -1.607
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.475                                          
## OrignCntmLC -0.241  0.156                                   
## InjectinPBS -0.203  0.147  0.319                            
## SizeEnd     -0.763  0.029  0.059  0.015                     
## TrnCLC:OCLC  0.115 -0.255 -0.610 -0.016  0.007              
## TrnCLC:IPBS  0.110 -0.244 -0.013 -0.600  0.006  0.029       
## OrgCLC:IPBS  0.120  0.001 -0.540 -0.556 -0.018  0.001 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$MuscleCarbohydrate)
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   386.4042  1  < 2.2e-16 ***
## TransplantContam                0.7383  1   0.390203    
## OriginContam                    2.5671  1   0.109107    
## Injection                       0.0070  1   0.933181    
## SizeEnd                         0.2962  1   0.586268    
## TransplantContam:OriginContam   9.4985  1   0.002056 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam)^2 +  
##     Injection + SizeEnd + (1 | CagingSite/CageSiteID) + (1 |      OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 113.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.7435 -0.5677  0.0726  0.6620  2.4868 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0003136 0.01771 
##  CagingSite            (Intercept) 0.0181006 0.13454 
##  OriginSite            (Intercept) 0.0000000 0.00000 
##  Residual                          0.0824140 0.28708 
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        3.214453   0.163526  19.657
## TransplantContamLC                 0.124211   0.144558   0.859
## OriginContamLC                     0.087790   0.054793   1.602
## InjectionPBS                       0.003094   0.036907   0.084
## SizeEnd                            0.006758   0.012417   0.544
## TransplantContamLC:OriginContamLC -0.232115   0.075314  -3.082
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.469                            
## OrignCntmLC -0.210  0.186                     
## InjectinPBS -0.123  0.000  0.019              
## SizeEnd     -0.771  0.032  0.059  0.016       
## TrnCLC:OCLC  0.113 -0.255 -0.724  0.003  0.007
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam) ^ 2 +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$MuscleCarbohydrate)
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   1043.8093  1  < 2.2e-16 ***
## TransplantContam                 0.7218  1   0.395558    
## OriginContam                     2.5004  1   0.113818    
## TransplantContam:OriginContam    9.6379  1   0.001906 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam)^2 +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 102.4
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.74057 -0.58246  0.07299  0.69077  2.51534 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000265 0.01628 
##  CagingSite            (Intercept) 0.017795 0.13340 
##  OriginSite            (Intercept) 0.000000 0.00000 
##  Residual                          0.081869 0.28613 
## Number of obs: 243, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        3.28444    0.10166  32.308
## TransplantContamLC                 0.12175    0.14330   0.850
## OriginContamLC                     0.08596    0.05436   1.581
## TransplantContamLC:OriginContamLC -0.23238    0.07485  -3.104
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.709              
## OrignCntmLC -0.261  0.185       
## TrnCLC:OCLC  0.189 -0.256 -0.726
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modCarboh <- mod4
RmodCarboh  <- MuMIn::r.squaredGLMM(modCarboh)
```

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam) ^ 2  + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: log(dat2$MuscleCarbohydrate)
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   1013.2175  1  < 2.2e-16 ***
## TransplantContam                 0.7518  1   0.385901    
## OriginContam                     2.4458  1   0.117843    
## Sex                              0.5670  1   0.451469    
## TransplantContam:OriginContam    9.7814  1   0.001763 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log(dat2$MuscleCarbohydrate) ~ (TransplantContam + OriginContam)^2 +  
##     Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 106.9
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.75737 -0.57667  0.03485  0.66901  2.49046 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0001979 0.01407 
##  CagingSite            (Intercept) 0.0182223 0.13499 
##  OriginSite            (Intercept) 0.0000000 0.00000 
##  Residual                          0.0823006 0.28688 
## Number of obs: 242, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        3.29392    0.10348  31.831
## TransplantContamLC                 0.12559    0.14484   0.867
## OriginContamLC                     0.08535    0.05458   1.564
## SexM                              -0.02888    0.03835  -0.753
## TransplantContamLC:OriginContamLC -0.23545    0.07528  -3.128
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SexM  
## TrnsplntCLC -0.699                     
## OrignCntmLC -0.248  0.183              
## SexM        -0.124 -0.034 -0.049       
## TrnCLC:OCLC  0.172 -0.254 -0.728  0.101
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is non-significant.
The mod4 is thus the best model.
Here, the interactions between fish origin and the transplant site are significant, confirming the pattern observed for the integrative measure of available energy previously observed.


## Daily mass changes
### Check the data

```r
dat2 = dat1[is.na(dat1$DailyMassChange) == F, ]
# transform variable to approx. normality
dat2$DailyMassChange.p <-
  bimixt::boxcox(dat2$DailyMassChange + 3,
         car::powerTransform(dat2$DailyMassChange + 3)$lambda)
hist(dat2$DailyMassChange.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/DMCIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(DailyMassChange.p ~ OriginContam, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam, data = dat2)
boxplot(DailyMassChange.p ~ Injection, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam * Injection, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DailyMassChange.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/DMCViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DailyMassChange.p
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             15.8885  1  6.719e-05 ***
## TransplantContam                         0.1775  1   0.673520    
## OriginContam                             1.7041  1   0.191755    
## Injection                                0.5038  1   0.477828    
## SizeEnd                                  2.7096  1   0.099745 .  
## TransplantContam:OriginContam            7.6762  1   0.005596 ** 
## TransplantContam:Injection               0.3860  1   0.534387    
## OriginContam:Injection                   1.4633  1   0.226406    
## TransplantContam:OriginContam:Injection  0.8882  1   0.345957    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DailyMassChange.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 93.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.4412 -0.5700  0.0571  0.5996  3.1697 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.003799 0.06164 
##  CagingSite            (Intercept) 0.059924 0.24479 
##  OriginSite            (Intercept) 0.005396 0.07346 
##  Residual                          0.070161 0.26488 
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                     0.94405    0.23684   3.986
## TransplantContamLC                              0.10781    0.25587   0.421
## OriginContamLC                                  0.13745    0.10529   1.305
## InjectionPBS                                    0.04748    0.06690   0.710
## SizeEnd                                         0.02351    0.01428   1.646
## TransplantContamLC:OriginContamLC              -0.29247    0.10556  -2.771
## TransplantContamLC:InjectionPBS                -0.05833    0.09387  -0.621
## OriginContamLC:InjectionPBS                    -0.11757    0.09719  -1.210
## TransplantContamLC:OriginContamLC:InjectionPBS  0.12739    0.13517   0.942
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.549                                                     
## OrignCntmLC -0.239  0.103                                              
## InjectinPBS -0.143  0.124  0.302                                       
## SizeEnd     -0.607  0.014  0.033  0.014                                
## TrnCLC:OCLC  0.101 -0.205 -0.510 -0.301  0.016                         
## TrnCLC:IPBS  0.095 -0.177 -0.215 -0.712  0.001  0.430                  
## OrgCLC:IPBS  0.097 -0.086 -0.429 -0.688 -0.008  0.428      0.490       
## TCLC:OCLC:I -0.065  0.123  0.308  0.495 -0.002 -0.602     -0.694 -0.719
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam + Injection) ^ 2  +  SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DailyMassChange.p
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   16.4593  1  4.971e-05 ***
## TransplantContam               0.0945  1   0.758506    
## OriginContam                   1.1422  1   0.285195    
## Injection                      0.0787  1   0.779052    
## SizeEnd                        2.7144  1   0.099448 .  
## TransplantContam:OriginContam  7.5825  1   0.005894 ** 
## TransplantContam:Injection     0.0022  1   0.962993    
## OriginContam:Injection         0.5876  1   0.443337    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DailyMassChange.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 92.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.5140 -0.5651  0.0643  0.5870  3.1142 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.003851 0.06206 
##  CagingSite            (Intercept) 0.059988 0.24492 
##  OriginSite            (Intercept) 0.005354 0.07317 
##  Residual                          0.070103 0.26477 
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.958781   0.236327   4.057
## TransplantContamLC                 0.078117   0.254084   0.307
## OriginContamLC                     0.106900   0.100026   1.069
## InjectionPBS                       0.016304   0.058113   0.281
## SizeEnd                            0.023513   0.014271   1.648
## TransplantContamLC:OriginContamLC -0.232617   0.084476  -2.754
## TransplantContamLC:InjectionPBS    0.003133   0.067528   0.046
## OriginContamLC:InjectionPBS       -0.051771   0.067535  -0.767
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.546                                          
## OrignCntmLC -0.231  0.070                                   
## InjectinPBS -0.127  0.074  0.182                            
## SizeEnd     -0.608  0.015  0.035  0.017                     
## TrnCLC:OCLC  0.078 -0.165 -0.429 -0.005  0.018              
## TrnCLC:IPBS  0.069 -0.128 -0.001 -0.590  0.000  0.021       
## OrgCLC:IPBS  0.073  0.004 -0.314 -0.551 -0.014 -0.009 -0.018
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2 + Injection + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DailyMassChange.p
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   17.0593  1  3.623e-05 ***
## TransplantContam               0.1008  1    0.75085    
## OriginContam                   0.7564  1    0.38446    
## Injection                      0.0443  1    0.83334    
## SizeEnd                        2.7067  1    0.09992 .  
## TransplantContam:OriginContam  7.7238  1    0.00545 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DailyMassChange.p ~ (TransplantContam + OriginContam)^2 + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 86.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.5818 -0.5506  0.0588  0.6014  3.1688 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.003751 0.06124 
##  CagingSite            (Intercept) 0.060026 0.24500 
##  OriginSite            (Intercept) 0.005448 0.07381 
##  Residual                          0.069723 0.26405 
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.970727   0.235026   4.130
## TransplantContamLC                 0.080002   0.251963   0.318
## OriginContamLC                     0.082800   0.095204   0.870
## InjectionPBS                      -0.007081   0.033652  -0.210
## SizeEnd                            0.023431   0.014242   1.645
## TransplantContamLC:OriginContamLC -0.233196   0.083908  -2.779
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.545                            
## OrignCntmLC -0.220  0.073                     
## InjectinPBS -0.078 -0.002  0.008              
## SizeEnd     -0.609  0.015  0.032  0.016       
## TrnCLC:OCLC  0.077 -0.163 -0.451  0.005  0.018
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2  + SizeEnd + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DailyMassChange.p
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   17.0349  1   3.67e-05 ***
## TransplantContam               0.1006  1   0.751141    
## OriginContam                   0.7581  1   0.383911    
## SizeEnd                        2.7346  1   0.098194 .  
## TransplantContam:OriginContam  7.7194  1   0.005463 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DailyMassChange.p ~ (TransplantContam + OriginContam)^2 + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 81.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.6078 -0.5608  0.0519  0.5984  3.1870 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.003781 0.06149 
##  CagingSite            (Intercept) 0.060029 0.24501 
##  OriginSite            (Intercept) 0.005469 0.07395 
##  Residual                          0.069423 0.26348 
## Number of obs: 248, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        0.96658    0.23419   4.127
## TransplantContamLC                 0.07991    0.25197   0.317
## OriginContamLC                     0.08299    0.09531   0.871
## SizeEnd                            0.02351    0.01422   1.654
## TransplantContamLC:OriginContamLC -0.23314    0.08391  -2.778
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SizEnd
## TrnsplntCLC -0.547                     
## OrignCntmLC -0.220  0.073              
## SizeEnd     -0.609  0.015  0.032       
## TrnCLC:OCLC  0.078 -0.163 -0.450  0.018
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2 + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DailyMassChange.p
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   46.0496  1  1.153e-11 ***
## TransplantContam               0.0872  1    0.76776    
## OriginContam                   0.5827  1    0.44526    
## TransplantContam:OriginContam  4.9114  1    0.02668 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DailyMassChange.p ~ (TransplantContam + OriginContam)^2 + (1 |  
##     CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 107.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.4880 -0.5227  0.0623  0.5611  3.1177 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.006937 0.08329 
##  CagingSite            (Intercept) 0.057970 0.24077 
##  OriginSite            (Intercept) 0.000000 0.00000 
##  Residual                          0.077979 0.27925 
## Number of obs: 252, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        1.20243    0.17719   6.786
## TransplantContamLC                 0.07397    0.25048   0.295
## OriginContamLC                     0.05348    0.07005   0.763
## TransplantContamLC:OriginContamLC -0.21739    0.09809  -2.216
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC
## TrnsplntCLC -0.707              
## OrignCntmLC -0.194  0.138       
## TrnCLC:OCLC  0.139 -0.194 -0.714
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modDMC <- mod5
RmodDMC  <- MuMIn::r.squaredGLMM(modDMC)
```
Here, the interactions between fish origin and the transplant site are significant. 

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
 modSex <-
 lme4::lmer(
    DailyMassChange.p ~ (TransplantContam + OriginContam) ^ 2  + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DailyMassChange.p
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   46.4776  1  9.267e-12 ***
## TransplantContam               0.0955  1   0.757281    
## OriginContam                   2.6068  1   0.106406    
## Sex                            0.5753  1   0.448174    
## TransplantContam:OriginContam  8.9250  1   0.002813 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DailyMassChange.p ~ (TransplantContam + OriginContam)^2 + Sex +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 62.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8836 -0.5478  0.0778  0.5956  3.4584 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 5.185e-03 7.201e-02
##  CagingSite            (Intercept) 5.904e-02 2.430e-01
##  OriginSite            (Intercept) 8.010e-12 2.830e-06
##  Residual                          6.446e-02 2.539e-01
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        1.21094    0.17762   6.817
## TransplantContamLC                 0.07746    0.25064   0.309
## OriginContamLC                     0.10133    0.06276   1.615
## SexM                              -0.02610    0.03441  -0.758
## TransplantContamLC:OriginContamLC -0.26263    0.08791  -2.987
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC SexM  
## TrnsplntCLC -0.705                     
## OrignCntmLC -0.168  0.122              
## SexM        -0.064 -0.018 -0.045       
## TrnCLC:OCLC  0.117 -0.172 -0.716  0.081
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
Adding the sex as covariate does no change the significance of the interactions, also sex effect is non-significant.
The mod5 is thus the best model.

### Posthoc test (pairwise t-test)
The interactions between fish origin and the transplant site are significant. Hence, perform posthoc test (pairwise t-test) according to significant interactions.


```r
pairwise.t.test(dat2$DailyMassChange.p, dat2$CxO2, p.adjust.method = "fdr")
```

```
## 
## 	Pairwise comparisons using t tests with pooled SD 
## 
## data:  dat2$DailyMassChange.p and dat2$CxO2 
## 
##       HC_HC HC_LC LC_HC
## HC_LC 0.378 -     -    
## LC_HC 0.382 0.851 -    
## LC_LC 0.132 0.014 0.014
## 
## P value adjustment method: fdr
```
Here the posthoc are non significant, except between the HC and LC fish when exposed to LC sites, likely because responses were very different depending on the population considered (see non-parallel plastic responses below). 
Also,because interaction between the contamination in origin and transplant site of fish is significant
We test for parallel response between replicate populations.

### Test for parallel responses between replicate populations (from the same origin)
#### For Populations from High contamination sites (AUSCOR and RIOU)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: DailyMassChange.p ~ TransplantContam + OriginSite
## Model 2: DailyMassChange.p ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1       125     6.4448                          
## 2       124     5.0924  1   1.3524 9.546e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
coef(mod.par)
```

```
##                       (Intercept)                TransplantContamLC 
##                         0.9855391                         0.2785145 
##                    OriginSiteRIOU TransplantContamLC:OriginSiteRIOU 
##                         0.4367652                        -0.4118958
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = DailyMassChange.p ~ TransplantContam * OriginSite, 
##     data = dat3)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -0.52373  -0.12763   0.00892   0.13518   0.59893  
## 
## Coefficients:
##                                   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        0.98554    0.03763  26.189  < 2e-16 ***
## TransplantContamLC                 0.27851    0.05158   5.400 3.27e-07 ***
## OriginSiteRIOU                     0.43677    0.05122   8.526 4.38e-14 ***
## TransplantContamLC:OriginSiteRIOU -0.41190    0.07178  -5.739 6.91e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.04106755)
## 
##     Null deviance: 8.1850  on 127  degrees of freedom
## Residual deviance: 5.0924  on 124  degrees of freedom
## AIC: -39.46
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are significantly different, we hence reject the null hypothesis, here the slopes of HC populations are not parallel.

#### For Populations from Low contamination sites (ARIMAS and CELFIG)

```r
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
```

```
## Analysis of Deviance Table
## 
## Model 1: DailyMassChange.p ~ TransplantContam + OriginSite
## Model 2: DailyMassChange.p ~ TransplantContam * OriginSite
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1       116     13.396                          
## 2       115     11.297  1   2.0997 3.778e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
coef(mod.par)
```

```
##                         (Intercept)                  TransplantContamLC 
##                          1.02261170                          0.08832977 
##                    OriginSiteCELFIG TransplantContamLC:OriginSiteCELFIG 
##                          0.54826564                         -0.53245772
```

```r
summary(mod.par)
```

```
## 
## Call:
## glm(formula = DailyMassChange.p ~ TransplantContam * OriginSite, 
##     data = dat4)
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.19383  -0.18877   0.00355   0.18266   0.91144  
## 
## Coefficients:
##                                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                          1.02261    0.05820  17.570  < 2e-16 ***
## TransplantContamLC                   0.08833    0.08036   1.099    0.274    
## OriginSiteCELFIG                     0.54827    0.08382   6.541 1.76e-09 ***
## TransplantContamLC:OriginSiteCELFIG -0.53246    0.11517  -4.623 9.95e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.09823347)
## 
##     Null deviance: 16.343  on 118  degrees of freedom
## Residual deviance: 11.297  on 115  degrees of freedom
## AIC: 67.51
## 
## Number of Fisher Scoring iterations: 2
```
The null model and the model including the site of origin in interaction with the level of contaminant in transplant sites are significantly different, we hence reject the null hypothesis, here the slopes of LC populations are not parallel.

### Visualize 

```r
DMCPlot1()
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/DMCFinalViz-1.png)<!-- -->

## HSI
### Check the data

```r
dat2 = dat1[is.na(dat1$HSI) == F, ]
# transform variable to approx. normality
dat2$HSI.p <- bimixt::boxcox(dat2$HSI, car::powerTransform(dat2$HSI)$lambda)
hist(dat2$HSI.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/HSIIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(HSI.p ~ OriginContam, data = dat2)
boxplot(HSI.p ~ TransplantContam, data = dat2)
boxplot(HSI.p ~ Injection, data = dat2)
boxplot(HSI.p ~ TransplantContam * Injection, data = dat2)
boxplot(HSI.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(HSI.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/HSIViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    HSI.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: HSI.p
##                                          Chisq Df Pr(>Chisq)  
## (Intercept)                             0.1222  1    0.72664  
## TransplantContam                        0.0241  1    0.87661  
## OriginContam                            0.0383  1    0.84479  
## Injection                               0.1593  1    0.68984  
## SizeEnd                                 5.0481  1    0.02465 *
## TransplantContam:OriginContam           1.9608  1    0.16143  
## TransplantContam:Injection              0.0554  1    0.81391  
## OriginContam:Injection                  0.0203  1    0.88683  
## TransplantContam:OriginContam:Injection 0.1900  1    0.66290  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: HSI.p ~ (TransplantContam + OriginContam + Injection)^3 + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 240.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5377 -0.4674  0.0100  0.4845  4.0412 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01483  0.1218  
##  CagingSite            (Intercept) 0.04700  0.2168  
##  OriginSite            (Intercept) 0.05889  0.2427  
##  Residual                          0.12635  0.3555  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                     0.11082    0.31699   0.350
## TransplantContamLC                              0.03793    0.24427   0.155
## OriginContamLC                                  0.05252    0.26825   0.196
## InjectionPBS                                    0.03585    0.08982   0.399
## SizeEnd                                        -0.04535    0.02018  -2.247
## TransplantContamLC:OriginContamLC              -0.22386    0.15987  -1.400
## TransplantContamLC:InjectionPBS                 0.02967    0.12607   0.235
## OriginContamLC:InjectionPBS                     0.01865    0.13105   0.142
## TransplantContamLC:OriginContamLC:InjectionPBS -0.07929    0.18189  -0.436
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.399                                                     
## OrignCntmLC -0.432  0.097                                              
## InjectinPBS -0.144  0.175  0.159                                       
## SizeEnd     -0.640  0.021  0.018  0.015                                
## TrnCLC:OCLC  0.116 -0.323 -0.304 -0.266  0.014                         
## TrnCLC:IPBS  0.097 -0.249 -0.113 -0.712 -0.002  0.381                  
## OrgCLC:IPBS  0.098 -0.120 -0.229 -0.685 -0.009  0.384      0.488       
## TCLC:OCLC:I -0.067  0.173  0.165  0.494  0.001 -0.537     -0.693 -0.720
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    HSI.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: HSI.p
##                                Chisq Df Pr(>Chisq)  
## (Intercept)                   0.1042  1    0.74686  
## TransplantContam              0.0548  1    0.81493  
## OriginContam                  0.0738  1    0.78590  
## Injection                     0.5004  1    0.47933  
## SizeEnd                       5.0740  1    0.02429 *
## TransplantContam:OriginContam 3.7542  1    0.05267 .
## TransplantContam:Injection    0.0085  1    0.92639  
## OriginContam:Injection        0.0615  1    0.80419  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: HSI.p ~ (TransplantContam + OriginContam + Injection)^2 + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 238.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5179 -0.4467  0.0328  0.4906  4.0196 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01489  0.1220  
##  CagingSite            (Intercept) 0.04696  0.2167  
##  OriginSite            (Intercept) 0.05887  0.2426  
##  Residual                          0.12588  0.3548  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        0.102002   0.316004   0.323
## TransplantContamLC                 0.056299   0.240521   0.234
## OriginContamLC                     0.071857   0.264532   0.272
## InjectionPBS                       0.055155   0.077970   0.707
## SizeEnd                           -0.045386   0.020149  -2.253
## TransplantContamLC:OriginContamLC -0.261344   0.134881  -1.938
## TransplantContamLC:InjectionPBS   -0.008382   0.090722  -0.092
## OriginContamLC:InjectionPBS       -0.022495   0.090734  -0.248
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.394                                          
## OrignCntmLC -0.428  0.071                                   
## InjectinPBS -0.128  0.104  0.091                            
## SizeEnd     -0.641  0.021  0.018  0.017                     
## TrnCLC:OCLC  0.095 -0.278 -0.259 -0.002  0.017              
## TrnCLC:IPBS  0.070 -0.182  0.002 -0.591 -0.002  0.014       
## OrgCLC:IPBS  0.072  0.007 -0.161 -0.547 -0.012 -0.005 -0.022
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    HSI.p ~ (TransplantContam + OriginContam) ^ 2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: HSI.p
##                                Chisq Df Pr(>Chisq)  
## (Intercept)                   0.1246  1    0.72405  
## TransplantContam              0.0491  1    0.82462  
## OriginContam                  0.0552  1    0.81426  
## Injection                     0.7845  1    0.37577  
## SizeEnd                       5.1540  1    0.02319 *
## TransplantContam:OriginContam 3.7545  1    0.05267 .
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: HSI.p ~ (TransplantContam + OriginContam)^2 + Injection + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 233
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5095 -0.4566  0.0265  0.5062  4.0599 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01502  0.1226  
##  CagingSite            (Intercept) 0.04697  0.2167  
##  OriginSite            (Intercept) 0.05890  0.2427  
##  Residual                          0.12478  0.3532  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        0.11081    0.31385   0.353
## TransplantContamLC                 0.05241    0.23652   0.222
## OriginContamLC                     0.06135    0.26115   0.235
## InjectionPBS                       0.03997    0.04513   0.886
## SizeEnd                           -0.04555    0.02006  -2.270
## TransplantContamLC:OriginContamLC -0.26144    0.13492  -1.938
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.391                            
## OrignCntmLC -0.425  0.074                     
## InjectinPBS -0.079 -0.003  0.002              
## SizeEnd     -0.642  0.021  0.016  0.016       
## TrnCLC:OCLC  0.095 -0.280 -0.263  0.007  0.017
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    HSI.p ~ TransplantContam + Injection + OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: HSI.p
##                   Chisq Df Pr(>Chisq)  
## (Intercept)      0.3260  1    0.56801  
## TransplantContam 0.1166  1    0.73275  
## Injection        0.7948  1    0.37264  
## OriginContam     0.0793  1    0.77825  
## SizeEnd          5.1853  1    0.02278 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: HSI.p ~ TransplantContam + Injection + OriginContam + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 234.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5314 -0.4699  0.0166  0.4858  3.9932 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01893  0.1376  
##  CagingSite            (Intercept) 0.04553  0.2134  
##  OriginSite            (Intercept) 0.05804  0.2409  
##  Residual                          0.12490  0.3534  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)         0.17794    0.31163   0.571
## TransplantContamLC -0.07694    0.22532  -0.341
## InjectionPBS        0.04026    0.04515   0.892
## OriginContamLC     -0.07083    0.25154  -0.282
## SizeEnd            -0.04577    0.02010  -2.277
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC InjPBS OrgCLC
## TrnsplntCLC -0.379                     
## InjectinPBS -0.080 -0.001              
## OrignCntmLC -0.417  0.000  0.004       
## SizeEnd     -0.650  0.027  0.016  0.021
```
### Refining the model

```r
mod5 <-
 lme4::lmer(
    HSI.p ~ SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/HSIRefined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: HSI.p
##              Chisq Df Pr(>Chisq)  
## (Intercept) 0.1886  1    0.66411  
## SizeEnd     4.7807  1    0.02878 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: HSI.p ~ SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 228.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5897 -0.4568  0.0296  0.4640  4.0940 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01891  0.1375  
##  CagingSite            (Intercept) 0.02755  0.1660  
##  OriginSite            (Intercept) 0.03462  0.1861  
##  Residual                          0.12481  0.3533  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.10156    0.23389   0.434
## SizeEnd     -0.04355    0.01992  -2.186
## 
## Correlation of Fixed Effects:
##         (Intr)
## SizeEnd -0.832
```

```r
modHSI <- mod5
RmodHSI  <- MuMIn::r.squaredGLMM(modHSI)
```
The treatments has no effect on HSI but the HSI is higher in bigger fish 
Represent the relation between fish size and HSI

```r
plot(HSI.p ~ SizeEnd, data = dat2)
abline(b = -0.04355, a = 0.10156, col = "red")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/HSISize-1.png)<!-- -->

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    HSI.p ~ SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: HSI.p
##              Chisq Df Pr(>Chisq)  
## (Intercept) 0.1307  1    0.71769  
## SizeEnd     3.8317  1    0.05029 .
## Sex         1.6701  1    0.19624  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: HSI.p ~ SizeEnd + Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 231.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6703 -0.4511  0.0584  0.5046  4.2012 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.01821  0.1349  
##  CagingSite            (Intercept) 0.02858  0.1691  
##  OriginSite            (Intercept) 0.03231  0.1798  
##  Residual                          0.12472  0.3532  
## Number of obs: 247, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.08426    0.23304   0.362
## SizeEnd     -0.03932    0.02009  -1.957
## SexM        -0.06291    0.04868  -1.292
## 
## Correlation of Fixed Effects:
##         (Intr) SizEnd
## SizeEnd -0.831       
## SexM     0.040 -0.143
```
The sex effect is non-significant.
The mod5 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct

## GSI
### Check the data

```r
dat2 = dat1[is.na(dat1$GSI) == F, ]
dat2 = dat2[is.na(dat2$Sex) == F, ]
# transform variable to approx. normality
dat2$GSI.p <- bimixt::boxcox(dat2$GSI, car::powerTransform(dat2$GSI)$lambda)
hist(dat2$GSI.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GSIIndex-1.png)<!-- -->

### Visualize

```r
par(mfrow=c(2,3))
boxplot(GSI.p ~ OriginContam, data = dat2)
boxplot(GSI.p ~ TransplantContam, data = dat2)
boxplot(GSI.p ~ Injection, data = dat2)
boxplot(GSI.p ~ TransplantContam * Injection, data = dat2)
boxplot(GSI.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(GSI.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GSIViz-1.png)<!-- -->

### Build the full model

```r
modfull <-
 lme4::lmer(
    GSI.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: GSI.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                               4.9342  1    0.02633 *  
## TransplantContam                          0.4563  1    0.49936    
## OriginContam                              0.9120  1    0.33960    
## Injection                                 0.7498  1    0.38654    
## SizeEnd                                   0.1893  1    0.66350    
## Sex                                     630.9592  1    < 2e-16 ***
## TransplantContam:OriginContam             1.3168  1    0.25117    
## TransplantContam:Injection                0.5318  1    0.46585    
## OriginContam:Injection                    0.3518  1    0.55307    
## TransplantContam:OriginContam:Injection   0.0025  1    0.96048    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: GSI.p ~ (TransplantContam + OriginContam + Injection)^3 + SizeEnd +  
##     Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 289.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6652 -0.3581  0.0312  0.5165  3.8586 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 6.498e-09 8.061e-05
##  CagingSite            (Intercept) 0.000e+00 0.000e+00
##  OriginSite            (Intercept) 4.845e-02 2.201e-01
##  Residual                          1.678e-01 4.096e-01
## Number of obs: 244, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                     0.63484    0.28580   2.221
## TransplantContamLC                             -0.06934    0.10265  -0.676
## OriginContamLC                                 -0.23282    0.24380  -0.955
## InjectionPBS                                   -0.09019    0.10416  -0.866
## SizeEnd                                         0.01003    0.02304   0.435
## SexM                                           -1.39931    0.05571 -25.119
## TransplantContamLC:OriginContamLC               0.16830    0.14667   1.148
## TransplantContamLC:InjectionPBS                 0.10648    0.14601   0.729
## OriginContamLC:InjectionPBS                     0.09070    0.15290   0.593
## TransplantContamLC:OriginContamLC:InjectionPBS -0.01050    0.21188  -0.050
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd SexM   TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.215                                                            
## OrignCntmLC -0.438  0.217                                                     
## InjectinPBS -0.177  0.495  0.208                                              
## SizeEnd     -0.797  0.055  0.022  0.003                                       
## SexM         0.047 -0.132 -0.045 -0.029 -0.133                                
## TrnCLC:OCLC  0.113 -0.706 -0.310 -0.348  0.004  0.150                         
## TrnCLC:IPBS  0.119 -0.699 -0.151 -0.715  0.002  0.084  0.495                  
## OrgCLC:IPBS  0.127 -0.348 -0.295 -0.684 -0.016  0.100  0.498      0.494       
## TCLC:OCLC:I -0.089  0.489  0.215  0.494  0.012 -0.111 -0.692     -0.693 -0.725
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod2 <-
 lme4::lmer(
    GSI.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: GSI.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                     4.9720  1    0.02576 *  
## TransplantContam                0.5600  1    0.45425    
## OriginContam                    0.9356  1    0.33341    
## Injection                       0.9407  1    0.33211    
## SizeEnd                         0.1898  1    0.66308    
## Sex                           641.8109  1    < 2e-16 ***
## TransplantContam:OriginContam   2.3911  1    0.12203    
## TransplantContam:Injection      0.9342  1    0.33377    
## OriginContam:Injection          0.6577  1    0.41736    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: GSI.p ~ (TransplantContam + OriginContam + Injection)^2 + SizeEnd +  
##     Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 288
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6703 -0.3597  0.0321  0.5143  3.8634 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 1.248e-10 1.117e-05
##  CagingSite            (Intercept) 0.000e+00 0.000e+00
##  OriginSite            (Intercept) 4.844e-02 2.201e-01
##  Residual                          1.671e-01 4.087e-01
## Number of obs: 244, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        0.63378    0.28423   2.230
## TransplantContamLC                -0.06686    0.08934  -0.748
## OriginContamLC                    -0.23023    0.23802  -0.967
## InjectionPBS                      -0.08764    0.09036  -0.970
## SizeEnd                            0.01002    0.02299   0.436
## SexM                              -1.39960    0.05525 -25.334
## TransplantContamLC:OriginContamLC  0.16327    0.10559   1.546
## TransplantContamLC:InjectionPBS    0.10146    0.10497   0.967
## OriginContamLC:InjectionPBS        0.08520    0.10505   0.811
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd SexM   TCLC:O TCLC:I
## TrnsplntCLC -0.197                                                 
## OrignCntmLC -0.431  0.131                                          
## InjectinPBS -0.153  0.334  0.120                                   
## SizeEnd     -0.799  0.056  0.020 -0.003                            
## SexM         0.037 -0.090 -0.022  0.030 -0.133                     
## TrnCLC:OCLC  0.071 -0.583 -0.228 -0.009  0.017  0.102              
## TrnCLC:IPBS  0.079 -0.573 -0.003 -0.594  0.015  0.010  0.028       
## OrgCLC:IPBS  0.091  0.012 -0.208 -0.543 -0.011  0.028 -0.008 -0.019
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod3 <-
 lme4::lmer(
    GSI.p ~ TransplantContam + OriginContam + Injection +  SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: GSI.p
##                     Chisq Df Pr(>Chisq)    
## (Intercept)        3.9463  1    0.04698 *  
## TransplantContam   1.2417  1    0.26514    
## OriginContam       0.2125  1    0.64483    
## Injection          0.0094  1    0.92281    
## SizeEnd            0.1580  1    0.69104    
## Sex              654.4211  1    < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: GSI.p ~ TransplantContam + OriginContam + Injection + SizeEnd +  
##     Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 283.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7392 -0.4029  0.0402  0.4865  3.8015 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0006149 0.0248  
##  CagingSite            (Intercept) 0.0000000 0.0000  
##  OriginSite            (Intercept) 0.0491028 0.2216  
##  Residual                          0.1672205 0.4089  
## Number of obs: 244, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                     Estimate Std. Error t value
## (Intercept)         0.561239   0.282524   1.987
## TransplantContamLC  0.060024   0.053866   1.114
## OriginContamLC     -0.105105   0.228015  -0.461
## InjectionPBS        0.005104   0.052679   0.097
## SizeEnd             0.009155   0.023035   0.397
## SexM               -1.409437   0.055096 -25.582
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.192                            
## OrignCntmLC -0.422 -0.001                     
## InjectinPBS -0.096 -0.012  0.007              
## SizeEnd     -0.807  0.123  0.023 -0.002       
## SexM         0.028 -0.044  0.007  0.087 -0.137
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
### Refining the model

```r
mod4 <-
 lme4::lmer(
    GSI.p ~ Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod4)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GSIRefined3-1.png)<!-- -->

```r
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: GSI.p
##               Chisq Df Pr(>Chisq)    
## (Intercept)  37.318  1  1.004e-09 ***
## Sex         673.900  1  < 2.2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: GSI.p ~ Sex + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 270.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8345 -0.3850  0.0610  0.5162  3.7582 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0006987 0.02643 
##  CagingSite            (Intercept) 0.0000000 0.00000 
##  OriginSite            (Intercept) 0.0380456 0.19505 
##  Residual                          0.1658745 0.40728 
## Number of obs: 244, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.63031    0.10318   6.109
## SexM        -1.40539    0.05414 -25.960
## 
## Correlation of Fixed Effects:
##      (Intr)
## SexM -0.200
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modGSI <- mod4
RmodGSI <- MuMIn::r.squaredGLMM(modGSI)
```

The treatments has no effect on GSI but the GSI is higher in females.
Represent the relation between fish sex and GSI

```r
boxplot(GSI.p ~ Sex, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GSISize-1.png)<!-- -->

Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct

## Summary of the best models for traits excepted gene expression (Table 3 from the manuscript)

The following table report the results of the best models tested above, this table correspond to the Table 3  in the manuscript. Excepted for the survival (GLM), the table report the marginal and conditional Rsquared (R2m and R2c respectively).
<table class=" lightable-classic-2" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;">  </th>
   <th style="text-align:center;"> Estimate </th>
   <th style="text-align:center;"> Std. Error </th>
   <th style="text-align:center;"> t or z value </th>
   <th style="text-align:center;"> df </th>
   <th style="text-align:center;"> Chisq </th>
   <th style="text-align:center;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="4"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Survival | n = 268</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> -19.6 </td>
   <td style="text-align:center;"> 1350 </td>
   <td style="text-align:center;"> -0.0144 </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> NA </td>
   <td style="text-align:center;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC) </td>
   <td style="text-align:center;"> 16.1 </td>
   <td style="text-align:center;"> 1350 </td>
   <td style="text-align:center;"> 0.0119 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 2.68 </td>
   <td style="text-align:center;"> 0.102 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> 18.3 </td>
   <td style="text-align:center;"> 1350 </td>
   <td style="text-align:center;"> 0.0135 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 22 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC): Origin (LC) </td>
   <td style="text-align:center;"> -17.9 </td>
   <td style="text-align:center;"> 1350 </td>
   <td style="text-align:center;"> -0.0132 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 6.59 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="6"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Bioaccumulation | n = 248 | R2m = 0.0498 | R2c = 0.385</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 0.00786 </td>
   <td style="text-align:center;"> 0.957 </td>
   <td style="text-align:center;"> 0.00821 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.0000675 </td>
   <td style="text-align:center;"> 0.993 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC) </td>
   <td style="text-align:center;"> 0.358 </td>
   <td style="text-align:center;"> 1.35 </td>
   <td style="text-align:center;"> 0.264 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.0699 </td>
   <td style="text-align:center;"> 0.791 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> 0.499 </td>
   <td style="text-align:center;"> 0.352 </td>
   <td style="text-align:center;"> 1.42 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 2.01 </td>
   <td style="text-align:center;"> 0.156 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Imm. challenge (PBS) </td>
   <td style="text-align:center;"> 0.18 </td>
   <td style="text-align:center;"> 0.325 </td>
   <td style="text-align:center;"> 0.554 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.306 </td>
   <td style="text-align:center;"> 0.58 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC): Origin (LC) </td>
   <td style="text-align:center;"> -1.09 </td>
   <td style="text-align:center;"> 0.491 </td>
   <td style="text-align:center;"> -2.22 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4.91 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC): imm. Challenge (PBS) </td>
   <td style="text-align:center;"> -0.972 </td>
   <td style="text-align:center;"> 0.452 </td>
   <td style="text-align:center;"> -2.15 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4.62 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="2"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Oxidative stress index | n = 166 | R2m = 0.0629 | R2c = 0.258</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 3.54 </td>
   <td style="text-align:center;"> 0.355 </td>
   <td style="text-align:center;"> 9.97 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 99.4 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Size </td>
   <td style="text-align:center;"> -0.101 </td>
   <td style="text-align:center;"> 0.0333 </td>
   <td style="text-align:center;"> -3.03 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 9.21 </td>
   <td style="text-align:center;"> &lt;0.01 </td>
  </tr>
  <tr grouplength="2"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Oxidative damage | n = 179 | R2m = 0.0837 | R2c = 0.34</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 2.07 </td>
   <td style="text-align:center;"> 0.246 </td>
   <td style="text-align:center;"> 8.41 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 70.7 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Size </td>
   <td style="text-align:center;"> -0.0838 </td>
   <td style="text-align:center;"> 0.0227 </td>
   <td style="text-align:center;"> -3.7 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 13.7 </td>
   <td style="text-align:center;"> &lt;0.01 </td>
  </tr>
  <tr grouplength="2"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Antioxidant capacity | n = 169 | R2m = 0.0502 | R2c = 0.0661</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 11.3 </td>
   <td style="text-align:center;"> 0.163 </td>
   <td style="text-align:center;"> 69.5 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4830 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> -0.558 </td>
   <td style="text-align:center;"> 0.236 </td>
   <td style="text-align:center;"> -2.36 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 5.59 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="2"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Local immune response | n = 249 | R2m = 0.409 | R2c = 0.492</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 26.2 </td>
   <td style="text-align:center;"> 2.43 </td>
   <td style="text-align:center;"> 10.8 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 117 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Imm. challenge (PBS) </td>
   <td style="text-align:center;"> -23.4 </td>
   <td style="text-align:center;"> 1.66 </td>
   <td style="text-align:center;"> -14.1 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 199 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr grouplength="5"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>NL inflammatory immune response | n = 229 | R2m = 0.0714 | R2c = 0.167</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 0.00754 </td>
   <td style="text-align:center;"> 0.0149 </td>
   <td style="text-align:center;"> 0.505 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.255 </td>
   <td style="text-align:center;"> 0.614 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC) </td>
   <td style="text-align:center;"> -0.00736 </td>
   <td style="text-align:center;"> 0.00463 </td>
   <td style="text-align:center;"> -1.59 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 2.53 </td>
   <td style="text-align:center;"> 0.111 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> -0.0109 </td>
   <td style="text-align:center;"> 0.0091 </td>
   <td style="text-align:center;"> -1.2 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 1.44 </td>
   <td style="text-align:center;"> 0.23 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Size </td>
   <td style="text-align:center;"> 0.00365 </td>
   <td style="text-align:center;"> 0.00135 </td>
   <td style="text-align:center;"> 2.7 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 7.28 </td>
   <td style="text-align:center;"> &lt;0.01 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC): Origin (LC) </td>
   <td style="text-align:center;"> 0.0155 </td>
   <td style="text-align:center;"> 0.00674 </td>
   <td style="text-align:center;"> 2.3 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 5.28 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="4"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Available energy | n = 243 | R2m = 0.13 | R2c = 0.256</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 8.38 </td>
   <td style="text-align:center;"> 0.0957 </td>
   <td style="text-align:center;"> 87.6 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 7670 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC) </td>
   <td style="text-align:center;"> 0.15 </td>
   <td style="text-align:center;"> 0.0877 </td>
   <td style="text-align:center;"> 1.71 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 2.93 </td>
   <td style="text-align:center;"> 0.087 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> -0.0398 </td>
   <td style="text-align:center;"> 0.117 </td>
   <td style="text-align:center;"> -0.341 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.116 </td>
   <td style="text-align:center;"> 0.733 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC): Origin (LC) </td>
   <td style="text-align:center;"> -0.302 </td>
   <td style="text-align:center;"> 0.0771 </td>
   <td style="text-align:center;"> -3.92 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 15.3 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr grouplength="4"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Daily mass change | n = 252 | R2m = 0.028 | R2c = 0.47</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 1.2 </td>
   <td style="text-align:center;"> 0.177 </td>
   <td style="text-align:center;"> 6.79 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 46 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC) </td>
   <td style="text-align:center;"> 0.074 </td>
   <td style="text-align:center;"> 0.25 </td>
   <td style="text-align:center;"> 0.295 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.0872 </td>
   <td style="text-align:center;"> 0.768 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> 0.0535 </td>
   <td style="text-align:center;"> 0.0701 </td>
   <td style="text-align:center;"> 0.763 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.583 </td>
   <td style="text-align:center;"> 0.445 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Transplant (LC): Origin (LC) </td>
   <td style="text-align:center;"> -0.217 </td>
   <td style="text-align:center;"> 0.0981 </td>
   <td style="text-align:center;"> -2.22 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4.91 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="2"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>HSI | n = 247 | R2m = 0.0211 | R2c = 0.407</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 0.102 </td>
   <td style="text-align:center;"> 0.234 </td>
   <td style="text-align:center;"> 0.434 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 0.189 </td>
   <td style="text-align:center;"> 0.664 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Size </td>
   <td style="text-align:center;"> -0.0436 </td>
   <td style="text-align:center;"> 0.0199 </td>
   <td style="text-align:center;"> -2.19 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4.78 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="2"><td colspan="7" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>GSI | n = 244 | R2m = 0.696 | R2c = 0.753</strong></td></tr>
<tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Intercept </td>
   <td style="text-align:center;"> 0.63 </td>
   <td style="text-align:center;"> 0.103 </td>
   <td style="text-align:center;"> 6.11 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 37.3 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Sex (M) </td>
   <td style="text-align:center;"> -1.41 </td>
   <td style="text-align:center;"> 0.0541 </td>
   <td style="text-align:center;"> -26 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 674 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
</tbody>
</table>

## Genes expression

The summary of the results of the best models selected below is reported in Table 4 from the manuscript and is also available in the section "Summary of the best models for gene expression (Table 4 from the manuscript)"

### Mtl
#### Check the data

```r
dat2 = dat1[is.na(dat1$DeltaCtMtl) == F, ]
# transform variable to approx. normality
dat2$DeltaCtMtl.p <-
  bimixt::boxcox(dat2$DeltaCtMtl, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtMtl.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/MtlIndex-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(DeltaCtMtl.p ~ OriginContam, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtMtl.p ~ Injection, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtMtl.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/MtlViz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    DeltaCtMtl.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtMtl.p
##                                           Chisq Df Pr(>Chisq)   
## (Intercept)                             10.2927  1   0.001336 **
## TransplantContam                         0.2395  1   0.624548   
## OriginContam                             6.7105  1   0.009585 **
## Injection                                1.2214  1   0.269086   
## SizeEnd                                  1.3946  1   0.237623   
## TransplantContam:OriginContam            0.1196  1   0.729470   
## TransplantContam:Injection               0.3075  1   0.579197   
## OriginContam:Injection                   3.4508  1   0.063221 . 
## TransplantContam:OriginContam:Injection  1.0149  1   0.313739   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtMtl.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 738.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.4967 -0.5453  0.0038  0.5515  3.3103 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.03839  0.1959  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.07785  0.2790  
##  Residual                          1.10408  1.0508  
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                     2.03364    0.63388   3.208
## TransplantContamLC                             -0.13847    0.28294  -0.489
## OriginContamLC                                 -1.03660    0.40016  -2.590
## InjectionPBS                                   -0.29320    0.26530  -1.105
## SizeEnd                                        -0.06670    0.05648  -1.181
## TransplantContamLC:OriginContamLC               0.13920    0.40250   0.346
## TransplantContamLC:InjectionPBS                 0.20641    0.37221   0.555
## OriginContamLC:InjectionPBS                     0.71604    0.38546   1.858
## TransplantContamLC:OriginContamLC:InjectionPBS -0.54224    0.53825  -1.007
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.267                                                     
## OrignCntmLC -0.341  0.355                                              
## InjectinPBS -0.211  0.446  0.316                                       
## SizeEnd     -0.896  0.049  0.033  0.013                                
## TrnCLC:OCLC  0.132 -0.700 -0.509 -0.313  0.028                         
## TrnCLC:IPBS  0.138 -0.636 -0.224 -0.712  0.004  0.447                  
## OrgCLC:IPBS  0.144 -0.307 -0.448 -0.688 -0.008  0.445      0.490       
## TCLC:OCLC:I -0.091  0.439  0.320  0.493 -0.008 -0.628     -0.692 -0.716
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    DeltaCtMtl.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtMtl.p
##                                Chisq Df Pr(>Chisq)   
## (Intercept)                   9.7828  1   0.001762 **
## TransplantContam              0.0026  1   0.959182   
## OriginContam                  5.7677  1   0.016323 * 
## Injection                     0.4896  1   0.484121   
## SizeEnd                       1.4036  1   0.236114   
## TransplantContam:OriginContam 0.1378  1   0.710449   
## TransplantContam:Injection    0.0394  1   0.842634   
## OriginContam:Injection        2.6489  1   0.103624   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtMtl.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 739.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5664 -0.5191 -0.0122  0.5910  3.3659 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.03628  0.1905  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.07756  0.2785  
##  Residual                          1.10544  1.0514  
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                        1.97302    0.63081   3.128
## TransplantContamLC                -0.01294    0.25290  -0.051
## OriginContamLC                    -0.90740    0.37783  -2.402
## InjectionPBS                      -0.16163    0.23101  -0.700
## SizeEnd                           -0.06690    0.05647  -1.185
## TransplantContamLC:OriginContamLC -0.11545    0.31098  -0.371
## TransplantContamLC:InjectionPBS   -0.05341    0.26902  -0.199
## OriginContamLC:InjectionPBS        0.43825    0.26927   1.628
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.253                                          
## OrignCntmLC -0.330  0.250                                   
## InjectinPBS -0.192  0.296  0.192                            
## SizeEnd     -0.901  0.058  0.038  0.019                     
## TrnCLC:OCLC  0.096 -0.605 -0.416 -0.005  0.029              
## TrnCLC:IPBS  0.105 -0.514 -0.004 -0.592 -0.002  0.022       
## OrgCLC:IPBS  0.114  0.012 -0.332 -0.552 -0.019 -0.009 -0.009
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    DeltaCtMtl.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtMtl.p
##                   Chisq Df Pr(>Chisq)   
## (Intercept)      9.2349  1   0.002374 **
## TransplantContam 0.3748  1   0.540388   
## OriginContam     5.6894  1   0.017068 * 
## Injection        0.0229  1   0.879776   
## SizeEnd          1.2851  1   0.256945   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtMtl.p ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 740.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6909 -0.5467 -0.0089  0.5865  3.2568 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.03112  0.1764  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.07763  0.2786  
##  Residual                          1.10777  1.0525  
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)         1.88316    0.61968   3.039
## TransplantContamLC -0.09415    0.15378  -0.612
## OriginContamLC     -0.75883    0.31814  -2.385
## InjectionPBS        0.02036    0.13462   0.151
## SizeEnd            -0.06396    0.05642  -1.134
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.236                     
## OrignCntmLC -0.303  0.003              
## InjectinPBS -0.116 -0.007  0.012       
## SizeEnd     -0.918  0.123  0.052  0.013
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    DeltaCtMtl.p ~  OriginContam +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )

car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtMtl.p
##                Chisq Df Pr(>Chisq)    
## (Intercept)  29.5067  1  5.572e-08 ***
## OriginContam  5.4731  1    0.01931 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtMtl.p ~ OriginContam + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 734
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6964 -0.5456  0.0223  0.5869  3.2087 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 2.107e-02 1.451e-01
##  CagingSite            (Intercept) 1.535e-10 1.239e-05
##  OriginSite            (Intercept) 7.854e-02 2.802e-01
##  Residual                          1.108e+00 1.052e+00
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)      1.2111     0.2229   5.432
## OriginContamLC  -0.7403     0.3164  -2.339
## 
## Correlation of Fixed Effects:
##             (Intr)
## OrignCntmLC -0.705
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
Mtl expression is significantly different according to fish origin. More specifically, HC fish have higher Mtl expression than LC fish.

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtMtl.p ~ OriginContam + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/MtlSex-1.png)<!-- -->

```r
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtMtl.p
##                Chisq Df Pr(>Chisq)    
## (Intercept)  31.3296  1  2.177e-08 ***
## OriginContam  5.1233  1    0.02361 *  
## Sex           4.2371  1    0.03955 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtMtl.p ~ OriginContam + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 728.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8237 -0.5927  0.0607  0.6000  3.3720 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 2.007e-02 1.417e-01
##  CagingSite            (Intercept) 2.039e-10 1.428e-05
##  OriginSite            (Intercept) 8.545e-02 2.923e-01
##  Residual                          1.093e+00 1.045e+00
## Number of obs: 245, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)      1.3248     0.2367   5.597
## OriginContamLC  -0.7395     0.3267  -2.263
## SexM            -0.2866     0.1392  -2.058
## 
## Correlation of Fixed Effects:
##             (Intr) OrgCLC
## OrignCntmLC -0.688       
## SexM        -0.234  0.013
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modMtl <- modSex
RmodMtl <- MuMIn::r.squaredGLMM(modMtl)
```
The sex effect is significant.
The modSex is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


```r
boxplot(DeltaCtMtl.p ~ OriginContam, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/MtlOrigin-1.png)<!-- -->

```r
boxplot(DeltaCtMtl.p ~ Sex, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/MtlOrigin-2.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:right;"> 0.4778226 </td>
   <td style="text-align:right;"> 0.0925794 </td>
   <td style="text-align:right;"> 117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:right;"> 1.2116222 </td>
   <td style="text-align:right;"> 0.1011425 </td>
   <td style="text-align:right;"> 128 </td>
  </tr>
</tbody>
</table>

### Cat
#### Check the data

```r
dat2 = dat1[is.na(dat1$DeltaCtCat) == F, ]
# transform variable to approx. normality
dat2$DeltaCtCat.p <-
  bimixt::boxcox(dat2$DeltaCtCat, car::powerTransform(dat2$DeltaCtCat)$lambda)
hist(dat2$DeltaCtCat.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CatIndex-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(DeltaCtCat.p ~ OriginContam, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtCat.p ~ Injection, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtCat.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CatViz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    DeltaCtCat.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCat.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             126.6869  1  < 2.2e-16 ***
## TransplantContam                          0.1362  1   0.712039    
## OriginContam                              0.5148  1   0.473082    
## Injection                                 3.3333  1   0.067889 .  
## SizeEnd                                   7.2135  1   0.007236 ** 
## TransplantContam:OriginContam             3.9098  1   0.048005 *  
## TransplantContam:Injection                0.0240  1   0.876997    
## OriginContam:Injection                    1.3774  1   0.240550    
## TransplantContam:OriginContam:Injection   0.9025  1   0.342112    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCat.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 144.8
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.46127 -0.65109  0.00029  0.65431  2.80017 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0001937 0.01392 
##  CagingSite            (Intercept) 0.0053361 0.07305 
##  OriginSite            (Intercept) 0.0745272 0.27300 
##  Residual                          0.0899628 0.29994 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    -3.01890    0.26821 -11.256
## TransplantContamLC                              0.03849    0.10429   0.369
## OriginContamLC                                 -0.20325    0.28328  -0.717
## InjectionPBS                                    0.13817    0.07568   1.826
## SizeEnd                                         0.04571    0.01702   2.686
## TransplantContamLC:OriginContamLC              -0.20956    0.10598  -1.977
## TransplantContamLC:InjectionPBS                -0.01643    0.10617  -0.155
## OriginContamLC:InjectionPBS                    -0.12905    0.10996  -1.174
## TransplantContamLC:OriginContamLC:InjectionPBS  0.14586    0.15353   0.950
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.218                                                     
## OrignCntmLC -0.536  0.094                                              
## InjectinPBS -0.142  0.346  0.127                                       
## SizeEnd     -0.638  0.038  0.014  0.012                                
## TrnCLC:OCLC  0.077 -0.499 -0.190 -0.340  0.032                         
## TrnCLC:IPBS  0.094 -0.492 -0.091 -0.713  0.003  0.485                  
## OrgCLC:IPBS  0.096 -0.238 -0.181 -0.688 -0.005  0.483      0.490       
## TCLC:OCLC:I -0.061  0.340  0.129  0.493 -0.009 -0.681     -0.691 -0.716
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    DeltaCtCat.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCat.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   125.8848  1  < 2.2e-16 ***
## TransplantContam                0.0025  1   0.960361    
## OriginContam                    0.7176  1   0.396928    
## Injection                       2.4353  1   0.118634    
## SizeEnd                         7.2547  1   0.007072 ** 
## TransplantContam:OriginContam   3.2785  1   0.070194 .  
## TransplantContam:Injection      0.4832  1   0.486969    
## OriginContam:Injection          0.4998  1   0.479585    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCat.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 143.8
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.41117 -0.62645 -0.00176  0.63321  2.86447 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0002758 0.01661 
##  CagingSite            (Intercept) 0.0053064 0.07284 
##  OriginSite            (Intercept) 0.0745138 0.27297 
##  Residual                          0.0898674 0.29978 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                       -3.003235   0.267672 -11.220
## TransplantContamLC                 0.004873   0.098038   0.050
## OriginContamLC                    -0.237975   0.280922  -0.847
## InjectionPBS                       0.102721   0.065825   1.561
## SizeEnd                            0.045826   0.017014   2.693
## TransplantContamLC:OriginContamLC -0.141069   0.077910  -1.811
## TransplantContamLC:InjectionPBS    0.053295   0.076668   0.695
## OriginContamLC:InjectionPBS       -0.054245   0.076729  -0.707
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.211                                          
## OrignCntmLC -0.533  0.054                                   
## InjectinPBS -0.129  0.218  0.074                            
## SizeEnd     -0.639  0.043  0.015  0.018                     
## TrnCLC:OCLC  0.049 -0.390 -0.140 -0.007  0.035              
## TrnCLC:IPBS  0.072 -0.379 -0.002 -0.592 -0.004  0.026       
## OrgCLC:IPBS  0.075  0.008 -0.127 -0.552 -0.016 -0.010 -0.009
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    DeltaCtCat.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCat.p
##                     Chisq Df Pr(>Chisq)    
## (Intercept)      126.0929  1  < 2.2e-16 ***
## TransplantContam   0.2040  1   0.651486    
## OriginContam       1.4996  1   0.220734    
## Injection          7.4079  1   0.006494 ** 
## SizeEnd            7.4541  1   0.006329 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCat.p ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 138.2
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.52701 -0.68507 -0.06431  0.63540  2.78093 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.001279 0.03576 
##  CagingSite            (Intercept) 0.005106 0.07145 
##  OriginSite            (Intercept) 0.073363 0.27086 
##  Residual                          0.089638 0.29940 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        -2.97561    0.26499 -11.229
## TransplantContamLC -0.03728    0.08254  -0.452
## OriginContamLC     -0.33552    0.27399  -1.225
## InjectionPBS        0.10420    0.03828   2.722
## SizeEnd             0.04647    0.01702   2.730
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.198                     
## OrignCntmLC -0.528  0.001              
## InjectinPBS -0.077 -0.004  0.004       
## SizeEnd     -0.647  0.066  0.018  0.012
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    DeltaCtCat.p ~  Injection + SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCat.p
##                Chisq Df Pr(>Chisq)    
## (Intercept) 199.6151  1  < 2.2e-16 ***
## Injection     7.4260  1   0.006429 ** 
## SizeEnd       7.5935  1   0.005858 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCat.p ~ Injection + SizeEnd + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 135.7
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.57273 -0.65230 -0.03892  0.63278  2.82713 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.001260 0.03550 
##  CagingSite            (Intercept) 0.002643 0.05141 
##  OriginSite            (Intercept) 0.085284 0.29204 
##  Residual                          0.089642 0.29940 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)  -3.16488    0.22401 -14.129
## InjectionPBS  0.10433    0.03828   2.725
## SizeEnd       0.04676    0.01697   2.756
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS
## InjectinPBS -0.090       
## SizeEnd     -0.740  0.012
```
Cat expression is significantly different according to immune challenge. More specifically, HC fish have higher Cat expression than LC fish. Also, bigger fish have higher expression of Cat.


```r
plot(DeltaCtCat.p ~ SizeEnd, data = dat2)
abline(a = -3.16488, b = 0.04676 , col = "red")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CatSize-1.png)<!-- -->

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtCat.p ~ Injection + SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CatSex-1.png)<!-- -->

```r
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCat.p
##                Chisq Df Pr(>Chisq)    
## (Intercept) 205.8893  1  < 2.2e-16 ***
## Injection     7.0398  1   0.007972 ** 
## SizeEnd       9.3052  1   0.002285 ** 
## Sex           5.8286  1   0.015767 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## DeltaCtCat.p ~ Injection + SizeEnd + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 133.1
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.75224 -0.65983 -0.02391  0.65897  2.88861 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000310 0.01761 
##  CagingSite            (Intercept) 0.003288 0.05734 
##  OriginSite            (Intercept) 0.082352 0.28697 
##  Residual                          0.088084 0.29679 
## Number of obs: 245, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)  -3.17559    0.22131 -14.349
## InjectionPBS  0.10112    0.03811   2.653
## SizeEnd       0.05157    0.01690   3.050
## SexM         -0.09578    0.03967  -2.414
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS SizEnd
## InjectinPBS -0.088              
## SizeEnd     -0.737  0.003       
## SexM         0.012  0.074 -0.117
```

```r
modCat<- modSex
RmodCat <- MuMIn::r.squaredGLMM(modCat)
```
The sex effect is significant.
The modSex is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


```r
boxplot(DeltaCtCat.p ~ OriginContam, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CatInjection-1.png)<!-- -->

```r
boxplot(DeltaCtCat.p ~ Sex, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/CatInjection-2.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> -2.589656 </td>
   <td style="text-align:right;"> 0.0414297 </td>
   <td style="text-align:right;"> 116 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> -2.702629 </td>
   <td style="text-align:right;"> 0.0358039 </td>
   <td style="text-align:right;"> 129 </td>
  </tr>
</tbody>
</table>

### GPX
#### Check the data

```r
dat2 = dat1[is.na(dat1$DeltaCtGpx) == F, ]
# transform variable to approx. normality
dat2$DeltaCtGpx.p <-
  bimixt::boxcox(dat2$DeltaCtGpx, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtGpx.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GpxIndex-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(DeltaCtGpx.p ~ OriginContam, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtGpx.p ~ Injection, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtGpx.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GpxViz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    DeltaCtGpx.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtGpx.p
##                                           Chisq Df Pr(>Chisq)    
## (Intercept)                             20.8492  1  4.969e-06 ***
## TransplantContam                         0.7396  1     0.3898    
## OriginContam                             1.3179  1     0.2510    
## Injection                                0.0458  1     0.8306    
## SizeEnd                                  1.6136  1     0.2040    
## TransplantContam:OriginContam            0.2413  1     0.6233    
## TransplantContam:Injection               0.1609  1     0.6883    
## OriginContam:Injection                   0.2995  1     0.5842    
## TransplantContam:OriginContam:Injection  0.7050  1     0.4011    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtGpx.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 467.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3815 -0.5235  0.1590  0.6273  2.6625 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.007445 0.08629 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.029427 0.17154 
##  Residual                          0.355216 0.59600 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    -1.65475    0.36240  -4.566
## TransplantContamLC                              0.13352    0.15526   0.860
## OriginContamLC                                 -0.26738    0.23291  -1.148
## InjectionPBS                                    0.03219    0.15044   0.214
## SizeEnd                                         0.04084    0.03215   1.270
## TransplantContamLC:OriginContamLC              -0.10854    0.22097  -0.491
## TransplantContamLC:InjectionPBS                 0.08466    0.21105   0.401
## OriginContamLC:InjectionPBS                     0.11962    0.21859   0.547
## TransplantContamLC:OriginContamLC:InjectionPBS -0.25629    0.30522  -0.840
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.258                                                     
## OrignCntmLC -0.346  0.334                                              
## InjectinPBS -0.209  0.462  0.308                                       
## SizeEnd     -0.892  0.049  0.032  0.013                                
## TrnCLC:OCLC  0.124 -0.699 -0.480 -0.324  0.029                         
## TrnCLC:IPBS  0.137 -0.657 -0.219 -0.713  0.004  0.462                  
## OrgCLC:IPBS  0.143 -0.318 -0.437 -0.688 -0.007  0.460      0.490       
## TCLC:OCLC:I -0.090  0.454  0.312  0.493 -0.008 -0.649     -0.691 -0.716
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    DeltaCtGpx.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtGpx.p
##                                 Chisq Df Pr(>Chisq)    
## (Intercept)                   21.7743  1  3.067e-06 ***
## TransplantContam               1.9614  1     0.1614    
## OriginContam                   0.8686  1     0.3513    
## Injection                      0.5214  1     0.4703    
## SizeEnd                        1.6117  1     0.2043    
## TransplantContam:OriginContam  1.8803  1     0.1703    
## TransplantContam:Injection     0.0626  1     0.8024    
## OriginContam:Injection         0.0059  1     0.9387    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtGpx.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 467.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4450 -0.5319  0.1816  0.6129  2.6032 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.006866 0.08286 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.029659 0.17222 
##  Residual                          0.355130 0.59593 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -1.68401    0.36089  -4.666
## TransplantContamLC                 0.19272    0.13761   1.400
## OriginContamLC                    -0.20628    0.22133  -0.932
## InjectionPBS                       0.09452    0.13090   0.722
## SizeEnd                            0.04081    0.03215   1.270
## TransplantContamLC:OriginContamLC -0.22885    0.16689  -1.371
## TransplantContamLC:InjectionPBS   -0.03815    0.15244  -0.250
## OriginContamLC:InjectionPBS       -0.01174    0.15258  -0.077
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.244                                          
## OrignCntmLC -0.336  0.225                                   
## InjectinPBS -0.190  0.308  0.186                            
## SizeEnd     -0.897  0.060  0.037  0.019                     
## TrnCLC:OCLC  0.086 -0.596 -0.381 -0.006  0.031              
## TrnCLC:IPBS  0.103 -0.536 -0.004 -0.592 -0.002  0.024       
## OrgCLC:IPBS  0.112  0.012 -0.322 -0.552 -0.019 -0.009 -0.009
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    DeltaCtGpx.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtGpx.p
##                    Chisq Df Pr(>Chisq)    
## (Intercept)      20.9880  1  4.622e-06 ***
## TransplantContam  0.5733  1    0.44894    
## OriginContam      2.8518  1    0.09127 .  
## Injection         0.8497  1    0.35665    
## SizeEnd           1.7297  1    0.18845    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtGpx.p ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 464.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.5478 -0.4710  0.1555  0.6057  2.5766 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.008683 0.09318 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.030534 0.17474 
##  Residual                          0.352196 0.59346 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        -1.63029    0.35586  -4.581
## TransplantContamLC  0.06476    0.08553   0.757
## OriginContamLC     -0.32844    0.19449  -1.689
## InjectionPBS        0.06996    0.07590   0.922
## SizeEnd             0.04221    0.03209   1.315
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.232                     
## OrignCntmLC -0.315  0.003              
## InjectinPBS -0.114 -0.007  0.011       
## SizeEnd     -0.909  0.125  0.048  0.013
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    DeltaCtGpx.p ~  OriginContam +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtGpx.p
##                 Chisq Df Pr(>Chisq)    
## (Intercept)  149.6900  1     <2e-16 ***
## OriginContam   6.5657  1     0.0104 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtGpx.p ~ OriginContam + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 455.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4423 -0.5022  0.1522  0.6138  2.7332 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.009036 0.09506 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.010510 0.10252 
##  Residual                          0.353388 0.59446 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)     -1.1464     0.0937 -12.235
## OriginContamLC  -0.3418     0.1334  -2.562
## 
## Correlation of Fixed Effects:
##             (Intr)
## OrignCntmLC -0.702
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
Gpx expression is significantly different according to fish origin. More specifically, HC fish have higher Gpx expression than LC fish.

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtGpx.p ~ OriginContam + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GpxSex-1.png)<!-- -->

```r
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtGpx.p
##                 Chisq Df Pr(>Chisq)    
## (Intercept)  112.5356  1    < 2e-16 ***
## OriginContam   6.4537  1    0.01107 *  
## Sex            5.2931  1    0.02141 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtGpx.p ~ OriginContam + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 452.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3444 -0.4585  0.1213  0.5956  2.5866 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 5.543e-03 7.445e-02
##  CagingSite            (Intercept) 4.261e-11 6.527e-06
##  OriginSite            (Intercept) 1.214e-02 1.102e-01
##  Residual                          3.508e-01 5.923e-01
## Number of obs: 245, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    -1.07457    0.10130 -10.608
## OriginContamLC -0.34854    0.13720  -2.540
## SexM           -0.18119    0.07875  -2.301
## 
## Correlation of Fixed Effects:
##             (Intr) OrgCLC
## OrignCntmLC -0.673       
## SexM        -0.309  0.017
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modGpx <- modSex
RmodGpx <- MuMIn::r.squaredGLMM(modGpx)
```
The sex effect is significant.
The modSex is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


```r
boxplot(DeltaCtGpx.p ~ OriginContam, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GpxOrigin-1.png)<!-- -->

```r
boxplot(DeltaCtGpx.p ~ Sex, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/GpxOrigin-2.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:right;"> -1.490080 </td>
   <td style="text-align:right;"> 0.0533207 </td>
   <td style="text-align:right;"> 117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:right;"> -1.149969 </td>
   <td style="text-align:right;"> 0.0560095 </td>
   <td style="text-align:right;"> 128 </td>
  </tr>
</tbody>
</table>

### Casp3
#### Check the data

```r
dat2 = dat1[is.na(dat1$DeltaCtCasp3) == F, ]
# transform variable to approx. normality
dat2$DeltaCtCasp3.p <-
  bimixt::boxcox(dat2$DeltaCtCasp3, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtCasp3.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/Casp3Index-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(DeltaCtCasp3.p ~ OriginContam, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtCasp3.p ~ Injection, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtCasp3.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/Casp3Viz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    DeltaCtCasp3.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCasp3.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             209.1082  1  < 2.2e-16 ***
## TransplantContam                          0.0021  1   0.963343    
## OriginContam                             10.4111  1   0.001253 ** 
## Injection                                 0.2313  1   0.630533    
## SizeEnd                                  43.1668  1  5.027e-11 ***
## TransplantContam:OriginContam             0.0000  1   0.999188    
## TransplantContam:Injection                0.4093  1   0.522341    
## OriginContam:Injection                    1.7747  1   0.182800    
## TransplantContam:OriginContam:Injection   0.0092  1   0.923781    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCasp3.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 548.2
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.18169 -0.63883 -0.09077  0.59191  2.29276 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.001552 0.03939 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.001942 0.04407 
##  Residual                          0.511377 0.71511 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                  Estimate Std. Error t value
## (Intercept)                                    -5.0390380  0.3484674 -14.461
## TransplantContamLC                              0.0081695  0.1777545   0.046
## OriginContamLC                                 -0.5997254  0.1858674  -3.227
## InjectionPBS                                   -0.0867806  0.1804259  -0.481
## SizeEnd                                         0.2100963  0.0319774   6.570
## TransplantContamLC:OriginContamLC              -0.0002577  0.2531360  -0.001
## TransplantContamLC:InjectionPBS                -0.1619011  0.2530731  -0.640
## OriginContamLC:InjectionPBS                     0.3492433  0.2621585   1.332
## TransplantContamLC:OriginContamLC:InjectionPBS -0.0350206  0.3660468  -0.096
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.300                                                     
## OrignCntmLC -0.303  0.479                                              
## InjectinPBS -0.258  0.484  0.463                                       
## SizeEnd     -0.929  0.049  0.048  0.013                                
## TrnCLC:OCLC  0.156 -0.699 -0.690 -0.339  0.025                         
## TrnCLC:IPBS  0.165 -0.688 -0.329 -0.713  0.011  0.484                  
## OrgCLC:IPBS  0.179 -0.333 -0.657 -0.688 -0.010  0.482      0.490       
## TCLC:OCLC:I -0.111  0.476  0.470  0.493 -0.011 -0.680     -0.691 -0.716
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    DeltaCtCasp3.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCasp3.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   211.8814  1  < 2.2e-16 ***
## TransplantContam                0.0109  1  0.9170049    
## OriginContam                   12.9746  1  0.0003158 ***
## Injection                       0.2494  1  0.6175203    
## SizeEnd                        43.0676  1  5.288e-11 ***
## TransplantContam:OriginContam   0.0082  1  0.9278250    
## TransplantContam:Injection      0.9595  1  0.3273181    
## OriginContam:Injection          3.2908  1  0.0696711 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCasp3.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 548.1
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -2.19213 -0.63792 -0.09036  0.59237  2.28990 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.001766 0.04203 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.002009 0.04482 
##  Residual                          0.509045 0.71347 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -5.04030    0.34627 -14.556
## TransplantContamLC                 0.01628    0.15623   0.104
## OriginContamLC                    -0.59139    0.16418  -3.602
## InjectionPBS                      -0.07823    0.15665  -0.499
## SizeEnd                            0.20981    0.03197   6.563
## TransplantContamLC:OriginContamLC -0.01681    0.18557  -0.091
## TransplantContamLC:InjectionPBS   -0.17869    0.18242  -0.980
## OriginContamLC:InjectionPBS        0.33126    0.18261   1.814
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.283                                          
## OrignCntmLC -0.286  0.330                                   
## InjectinPBS -0.235  0.325  0.300                            
## SizeEnd     -0.936  0.062  0.060  0.021                     
## TrnCLC:OCLC  0.110 -0.583 -0.572 -0.006  0.023              
## TrnCLC:IPBS  0.123 -0.565 -0.006 -0.592  0.004  0.026       
## OrgCLC:IPBS  0.144  0.012 -0.519 -0.552 -0.026 -0.010 -0.009
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    DeltaCtCasp3.p ~ TransplantContam + (OriginContam + Injection)^2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCasp3.p
##                           Chisq Df Pr(>Chisq)    
## (Intercept)            211.6011  1  < 2.2e-16 ***
## TransplantContam         0.6898  1    0.40622    
## OriginContam            19.4837  1  1.015e-05 ***
## Injection                1.7977  1    0.17999    
## SizeEnd                 42.4736  1  7.164e-11 ***
## OriginContam:Injection   3.2660  1    0.07073 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCasp3.p ~ TransplantContam + (OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 545.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.1546 -0.6130 -0.1134  0.5822  2.2985 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000000 0.00000 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.002616 0.05114 
##  Residual                          0.508083 0.71280 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                 -4.98732    0.34285 -14.547
## TransplantContamLC          -0.07621    0.09176  -0.831
## OriginContamLC              -0.59881    0.13566  -4.414
## InjectionPBS                -0.16913    0.12614  -1.341
## SizeEnd                      0.20914    0.03209   6.517
## OriginContamLC:InjectionPBS  0.32963    0.18240   1.807
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.258                            
## OrignCntmLC -0.277  0.002                     
## InjectinPBS -0.204 -0.007  0.453              
## SizeEnd     -0.952  0.132  0.089  0.029       
## OrgCLC:IPBS  0.147  0.003 -0.634 -0.692 -0.026
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    DeltaCtCasp3.p ~  TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCasp3.p
##                     Chisq Df Pr(>Chisq)    
## (Intercept)      220.7014  1  < 2.2e-16 ***
## TransplantContam   0.6933  1     0.4050    
## OriginContam      17.4701  1  2.919e-05 ***
## Injection          0.0156  1     0.9005    
## SizeEnd           42.3555  1  7.610e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtCasp3.p ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 547.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.2414 -0.6378 -0.1475  0.6448  2.3176 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000000 0.00000 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.002793 0.05285 
##  Residual                          0.512812 0.71611 
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        -5.07491    0.34161 -14.856
## TransplantContamLC -0.07676    0.09218  -0.833
## OriginContamLC     -0.44336    0.10607  -4.180
## InjectionPBS       -0.01144    0.09152  -0.125
## SizeEnd             0.21029    0.03231   6.508
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.260                     
## OrignCntmLC -0.241  0.004              
## InjectinPBS -0.143 -0.008  0.025       
## SizeEnd     -0.959  0.133  0.093  0.015
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod5 <-
 lme4::lmer(
    DeltaCtCasp3.p ~  OriginContam +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/Casp3Refined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCasp3.p
##                Chisq Df Pr(>Chisq)    
## (Intercept)  270.215  1  < 2.2e-16 ***
## OriginContam  20.109  1  7.314e-06 ***
## SizeEnd       48.992  1  2.570e-12 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## DeltaCtCasp3.p ~ OriginContam + SizeEnd + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 542.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.2329 -0.6460 -0.1196  0.6673  2.3653 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.000000 0.0000  
##  CagingSite            (Intercept) 0.000000 0.0000  
##  OriginSite            (Intercept) 0.001289 0.0359  
##  Residual                          0.510566 0.7145  
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    -5.19004    0.31573 -16.438
## OriginContamLC -0.44163    0.09848  -4.484
## SizeEnd         0.21743    0.03106   6.999
## 
## Correlation of Fixed Effects:
##             (Intr) OrgCLC
## OrignCntmLC -0.245       
## SizeEnd     -0.976  0.098
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modCasp3 <- mod5
RmodCasp3 <- MuMIn::r.squaredGLMM(modCasp3)
```

Casp3 expression is significantly different according to fish origin. More specifically, HC fish have higher Casp3 expression than LC fish. Also, the bigger the fish, the higher the level of Casp3 expression. 


```r
plot(DeltaCtCasp3.p ~ SizeEnd, data = dat2)
abline(a = -5.19004, b = 0.21743, col = "red")
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/Casp3Size-1.png)<!-- -->

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtCasp3.p ~ OriginContam + SizeEnd + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtCasp3.p
##                 Chisq Df Pr(>Chisq)    
## (Intercept)  251.6319  1  < 2.2e-16 ***
## OriginContam  17.5314  1  2.826e-05 ***
## SizeEnd       44.7003  1  2.296e-11 ***
## Sex            0.0736  1     0.7862    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## DeltaCtCasp3.p ~ OriginContam + SizeEnd + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 544
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.2358 -0.6397 -0.1252  0.6708  2.3654 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.00000  0.0000  
##  CagingSite            (Intercept) 0.00000  0.0000  
##  OriginSite            (Intercept) 0.00255  0.0505  
##  Residual                          0.51416  0.7170  
## Number of obs: 245, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                Estimate Std. Error t value
## (Intercept)    -5.16841    0.32582 -15.863
## OriginContamLC -0.44057    0.10522  -4.187
## SizeEnd         0.21420    0.03204   6.686
## SexM            0.02565    0.09455   0.271
## 
## Correlation of Fixed Effects:
##             (Intr) OrgCLC SizEnd
## OrignCntmLC -0.250              
## SizeEnd     -0.968  0.095       
## SexM        -0.051  0.021 -0.066
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is not significant.
The mod5 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


```r
boxplot(DeltaCtCasp3.p ~ OriginContam, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/Casp3Origin-1.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> LC </td>
   <td style="text-align:right;"> -3.542152 </td>
   <td style="text-align:right;"> 0.0677479 </td>
   <td style="text-align:right;"> 117 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HC </td>
   <td style="text-align:right;"> -3.030752 </td>
   <td style="text-align:right;"> 0.0741425 </td>
   <td style="text-align:right;"> 128 </td>
  </tr>
</tbody>
</table>

### Pcx
#### Check the data

```r
dat2 = dat1[is.na(dat1$DeltaCtPcx) == F, ]
# transform variable to approx. normality
dat2$DeltaCtPcx.p <-
  bimixt::boxcox(dat2$DeltaCtPcx, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtPcx.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PcxIndex-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(DeltaCtPcx.p ~ OriginContam, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtPcx.p ~ Injection, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtPcx.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PcxViz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    DeltaCtPcx.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPcx.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             107.6861  1     <2e-16 ***
## TransplantContam                          0.0189  1     0.8905    
## OriginContam                              0.0681  1     0.7941    
## Injection                                 2.2942  1     0.1299    
## SizeEnd                                   1.4059  1     0.2357    
## TransplantContam:OriginContam             0.6035  1     0.4372    
## TransplantContam:Injection                0.3826  1     0.5362    
## OriginContam:Injection                    0.5533  1     0.4570    
## TransplantContam:OriginContam:Injection   0.0928  1     0.7606    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPcx.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 267.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5255 -0.6242  0.0045  0.6279  3.6097 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.007666 0.08755 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.105554 0.32489 
##  Residual                          0.151573 0.38932 
## Number of obs: 240, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    -3.49927    0.33721 -10.377
## TransplantContamLC                              0.01544    0.11217   0.138
## OriginContamLC                                 -0.08953    0.34302  -0.261
## InjectionPBS                                    0.14892    0.09832   1.515
## SizeEnd                                         0.02767    0.02334   1.186
## TransplantContamLC:OriginContamLC              -0.12166    0.15660  -0.777
## TransplantContamLC:InjectionPBS                 0.08742    0.14132   0.619
## OriginContamLC:InjectionPBS                    -0.10626    0.14285  -0.744
## TransplantContamLC:OriginContamLC:InjectionPBS -0.06149    0.20181  -0.305
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.225                                                     
## OrignCntmLC -0.518  0.155                                              
## InjectinPBS -0.149  0.418  0.136                                       
## SizeEnd     -0.696  0.099  0.016  0.015                                
## TrnCLC:OCLC  0.116 -0.710 -0.225 -0.299 -0.006                         
## TrnCLC:IPBS  0.113 -0.622 -0.095 -0.696 -0.024  0.444                  
## OrgCLC:IPBS  0.101 -0.288 -0.194 -0.688 -0.008  0.424      0.479       
## TCLC:OCLC:I -0.075  0.435  0.137  0.487  0.011 -0.611     -0.700 -0.708
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    DeltaCtPcx.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPcx.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   109.0217  1    < 2e-16 ***
## TransplantContam                0.0903  1    0.76375    
## OriginContam                    0.0490  1    0.82480    
## Injection                       3.6415  1    0.05636 .  
## SizeEnd                         1.4181  1    0.23372    
## TransplantContam:OriginContam   1.4858  1    0.22286    
## TransplantContam:Injection      0.3234  1    0.56958    
## OriginContam:Injection          1.8527  1    0.17347    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPcx.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 266.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5506 -0.6470  0.0109  0.6336  3.5963 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 7.646e-03 8.744e-02
##  CagingSite            (Intercept) 9.086e-11 9.532e-06
##  OriginSite            (Intercept) 1.055e-01 3.248e-01
##  Residual                          1.510e-01 3.885e-01
## Number of obs: 240, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -3.50685    0.33586 -10.441
## TransplantContamLC                 0.03030    0.10083   0.301
## OriginContamLC                    -0.07519    0.33964  -0.221
## InjectionPBS                       0.16352    0.08569   1.908
## SizeEnd                            0.02774    0.02329   1.191
## TransplantContamLC:OriginContamLC -0.15083    0.12374  -1.219
## TransplantContamLC:InjectionPBS    0.05727    0.10070   0.569
## OriginContamLC:InjectionPBS       -0.13707    0.10070  -1.361
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.214                                          
## OrignCntmLC -0.514  0.107                                   
## InjectinPBS -0.129  0.262  0.080                            
## SizeEnd     -0.696  0.104  0.014  0.011                     
## TrnCLC:OCLC  0.089 -0.623 -0.180 -0.001  0.001              
## TrnCLC:IPBS  0.085 -0.493  0.001 -0.569 -0.023  0.028       
## OrgCLC:IPBS  0.068  0.032 -0.138 -0.557  0.000 -0.015 -0.033
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    DeltaCtPcx.p ~ TransplantContam + (OriginContam + Injection)^2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPcx.p
##                           Chisq Df Pr(>Chisq)    
## (Intercept)            110.3238  1  < 2.2e-16 ***
## TransplantContam         0.0756  1   0.783360    
## OriginContam             0.2075  1   0.648767    
## Injection                7.4937  1   0.006191 ** 
## SizeEnd                  1.4519  1   0.228225    
## OriginContam:Injection   1.8522  1   0.173526    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPcx.p ~ TransplantContam + (OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 262.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6471 -0.6634  0.0107  0.5713  3.5715 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.007977 0.08932 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.103693 0.32201 
##  Residual                          0.150707 0.38821 
## Number of obs: 240, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                             Estimate Std. Error t value
## (Intercept)                 -3.48672    0.33196 -10.504
## TransplantContamLC          -0.01731    0.06297  -0.275
## OriginContamLC              -0.15097    0.33145  -0.455
## InjectionPBS                 0.19271    0.07040   2.737
## SizeEnd                      0.02804    0.02327   1.205
## OriginContamLC:InjectionPBS -0.13685    0.10055  -1.361
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.194                            
## OrignCntmLC -0.509 -0.004                     
## InjectinPBS -0.100 -0.018  0.104              
## SizeEnd     -0.702  0.150  0.015 -0.003       
## OrgCLC:IPBS  0.073  0.011 -0.144 -0.700 -0.001
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    DeltaCtPcx.p ~  TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPcx.p
##                     Chisq Df Pr(>Chisq)    
## (Intercept)      108.8221  1     <2e-16 ***
## TransplantContam   0.0672  1     0.7954    
## OriginContam       0.4344  1     0.5099    
## Injection          6.2241  1     0.0126 *  
## SizeEnd            1.4435  1     0.2296    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPcx.p ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 261.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.5499 -0.6893  0.0111  0.6220  3.4685 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.008103 0.09002 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.103298 0.32140 
##  Residual                          0.151211 0.38886 
## Number of obs: 240, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        -3.45390    0.33109 -10.432
## TransplantContamLC -0.01639    0.06320  -0.259
## OriginContamLC     -0.21581    0.32744  -0.659
## InjectionPBS        0.12562    0.05035   2.495
## SizeEnd             0.02800    0.02331   1.201
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.196                     
## OrignCntmLC -0.505 -0.002              
## InjectinPBS -0.070 -0.015  0.005       
## SizeEnd     -0.705  0.150  0.015 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod5 <-
 lme4::lmer(
    DeltaCtPcx.p ~  Injection +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )

car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPcx.p
##                Chisq Df Pr(>Chisq)    
## (Intercept) 410.9888  1    < 2e-16 ***
## Injection     6.2643  1    0.01232 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPcx.p ~ Injection + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 254.3
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.4705 -0.6975 -0.0188  0.6402  3.4955 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.008078 0.08988 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.099706 0.31576 
##  Residual                          0.150697 0.38820 
## Number of obs: 240, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)  -3.29796    0.16268 -20.273
## InjectionPBS  0.12580    0.05026   2.503
## 
## Correlation of Fixed Effects:
##             (Intr)
## InjectinPBS -0.146
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

Pcx expression is significantly different according to the immune challenge. More specifically, immune challenged fish injected with the antigen mixture (AMIX) have a lower Pcx gene expression compare to the fish injected with the control saline solution (PBS). 

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtPcx.p ~ Injection + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PcxSex-1.png)<!-- -->

```r
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPcx.p
##                Chisq Df Pr(>Chisq)    
## (Intercept) 392.0753  1    < 2e-16 ***
## Injection     5.9462  1    0.01475 *  
## Sex           4.9243  1    0.02648 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPcx.p ~ Injection + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 250.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.3234 -0.6714 -0.0291  0.7023  3.4292 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.004934 0.07024 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.100530 0.31706 
##  Residual                          0.148851 0.38581 
## Number of obs: 239, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)  -3.25501    0.16439 -19.801
## InjectionPBS  0.12242    0.05021   2.438
## SexM         -0.11662    0.05255  -2.219
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS
## InjectinPBS -0.156       
## SexM        -0.135  0.083
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modPcx <- modSex
RmodPcx <- MuMIn::r.squaredGLMM(modPcx)
```
The sex effect is significant.
The modSex is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


```r
boxplot(DeltaCtPcx.p ~ Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PcxOrigin-1.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> -3.171033 </td>
   <td style="text-align:right;"> 0.0463890 </td>
   <td style="text-align:right;"> 114 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> -3.307949 </td>
   <td style="text-align:right;"> 0.0419337 </td>
   <td style="text-align:right;"> 125 </td>
  </tr>
</tbody>
</table>

### Pygl
#### Check the data

```r
dat2 = dat1[is.na(dat1$DeltaCtPygl) == F, ]
# transform variable to approx. normality
dat2$DeltaCtPygl.p <-
  bimixt::boxcox(dat2$DeltaCtPygl, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$DeltaCtPygl.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PyglIndex-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(DeltaCtPygl.p ~ OriginContam, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam, data = dat2)
boxplot(DeltaCtPygl.p ~ Injection, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam * Injection, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(DeltaCtPygl.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PyglViz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    DeltaCtPygl.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPygl.p
##                                            Chisq Df Pr(>Chisq)    
## (Intercept)                             124.1855  1     <2e-16 ***
## TransplantContam                          1.2894  1     0.2562    
## OriginContam                              0.4564  1     0.4993    
## Injection                                 1.5179  1     0.2179    
## SizeEnd                                   1.2137  1     0.2706    
## TransplantContam:OriginContam             0.3061  1     0.5801    
## TransplantContam:Injection                0.8515  1     0.3561    
## OriginContam:Injection                    0.1938  1     0.6598    
## TransplantContam:OriginContam:Injection   1.9743  1     0.1600    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPygl.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 221.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7762 -0.4925  0.0060  0.6019  2.7845 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.005033 0.07094 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.007040 0.08390 
##  Residual                          0.134533 0.36679 
## Number of obs: 226, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                Estimate Std. Error t value
## (Intercept)                                    -2.70225    0.24249 -11.144
## TransplantContamLC                              0.12416    0.10934   1.136
## OriginContamLC                                 -0.09012    0.13340  -0.676
## InjectionPBS                                    0.12528    0.10168   1.232
## SizeEnd                                         0.02411    0.02189   1.102
## TransplantContamLC:OriginContamLC              -0.08167    0.14760  -0.553
## TransplantContamLC:InjectionPBS                 0.13129    0.14228   0.923
## OriginContamLC:InjectionPBS                     0.06204    0.14095   0.440
## TransplantContamLC:OriginContamLC:InjectionPBS -0.27621    0.19658  -1.405
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.370                                                     
## OrignCntmLC -0.334  0.387                                              
## InjectinPBS -0.240  0.430  0.346                                       
## SizeEnd     -0.920  0.177  0.060  0.056                                
## TrnCLC:OCLC  0.214 -0.729 -0.547 -0.315 -0.066                         
## TrnCLC:IPBS  0.201 -0.646 -0.249 -0.717 -0.072  0.474                  
## OrgCLC:IPBS  0.173 -0.310 -0.480 -0.721 -0.040  0.436      0.517       
## TCLC:OCLC:I -0.140  0.467  0.346  0.518  0.047 -0.633     -0.723 -0.718
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    DeltaCtPygl.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPygl.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   130.7321  1    < 2e-16 ***
## TransplantContam                4.0936  1    0.04305 *  
## OriginContam                    0.0409  1    0.83976    
## Injection                       5.2326  1    0.02217 *  
## SizeEnd                         1.3619  1    0.24321    
## TransplantContam:OriginContam   3.4698  1    0.06250 .  
## TransplantContam:Injection      0.0183  1    0.89242    
## OriginContam:Injection          0.6638  1    0.41521    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPygl.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 221.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8555 -0.4702  0.0152  0.5567  2.6701 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.004984 0.07060 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.007048 0.08395 
##  Residual                          0.135180 0.36767 
## Number of obs: 226, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -2.75029    0.24054 -11.434
## TransplantContamLC                 0.19585    0.09680   2.023
## OriginContamLC                    -0.02533    0.12528  -0.202
## InjectionPBS                       0.19937    0.08716   2.287
## SizeEnd                            0.02557    0.02191   1.167
## TransplantContamLC:OriginContamLC -0.21294    0.11432  -1.863
## TransplantContamLC:InjectionPBS   -0.01332    0.09847  -0.135
## OriginContamLC:InjectionPBS       -0.08014    0.09836  -0.815
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.348                                          
## OrignCntmLC -0.307  0.272                                   
## InjectinPBS -0.198  0.248  0.208                            
## SizeEnd     -0.924  0.176  0.047  0.038                     
## TrnCLC:OCLC  0.163 -0.633 -0.451  0.021 -0.047              
## TrnCLC:IPBS  0.145 -0.506  0.001 -0.579 -0.055  0.030       
## OrgCLC:IPBS  0.104  0.041 -0.356 -0.587 -0.009 -0.035 -0.005
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    DeltaCtPygl.p ~ (TransplantContam + OriginContam)^2 + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPygl.p
##                                  Chisq Df Pr(>Chisq)    
## (Intercept)                   133.3751  1  < 2.2e-16 ***
## TransplantContam                5.3113  1   0.021187 *  
## OriginContam                    0.2787  1   0.597543    
## Injection                       9.4640  1   0.002095 ** 
## SizeEnd                         1.3406  1   0.246924    
## TransplantContam:OriginContam   3.5640  1   0.059047 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPygl.p ~ (TransplantContam + OriginContam)^2 + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 216.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.8260 -0.4845  0.0207  0.5715  2.7511 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.005119 0.07155 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.006988 0.08360 
##  Residual                          0.134274 0.36643 
## Number of obs: 226, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                   Estimate Std. Error t value
## (Intercept)                       -2.72511    0.23596 -11.549
## TransplantContamLC                 0.19237    0.08347   2.305
## OriginContamLC                    -0.06169    0.11686  -0.528
## InjectionPBS                       0.15064    0.04897   3.076
## SizeEnd                            0.02525    0.02181   1.158
## TransplantContamLC:OriginContamLC -0.21580    0.11431  -1.888
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd
## TrnsplntCLC -0.329                            
## OrignCntmLC -0.294  0.357                     
## InjectinPBS -0.094 -0.044 -0.002              
## SizeEnd     -0.931  0.172  0.047  0.001       
## TrnCLC:OCLC  0.166 -0.716 -0.498  0.031 -0.045
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    DeltaCtPygl.p ~  TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPygl.p
##                     Chisq Df Pr(>Chisq)    
## (Intercept)      130.4728  1  < 2.2e-16 ***
## TransplantContam   1.7193  1   0.189788    
## OriginContam       2.9271  1   0.087103 .  
## Injection          9.7301  1   0.001813 ** 
## SizeEnd            1.2323  1   0.266962    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPygl.p ~ TransplantContam + OriginContam + Injection +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 217.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6998 -0.5206  0.0379  0.5447  2.6484 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.007478 0.08648 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.006357 0.07973 
##  Residual                          0.134472 0.36670 
## Number of obs: 226, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                    Estimate Std. Error t value
## (Intercept)        -2.65959    0.23284 -11.422
## TransplantContamLC  0.08078    0.06161   1.311
## OriginContamLC     -0.17138    0.10017  -1.711
## InjectionPBS        0.15283    0.04900   3.119
## SizeEnd             0.02420    0.02180   1.110
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.304                     
## OrignCntmLC -0.245  0.000              
## InjectinPBS -0.101 -0.030  0.016       
## SizeEnd     -0.938  0.190  0.028  0.002
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod5 <-
 lme4::lmer(
    DeltaCtPygl.p ~  Injection +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(mod5)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PyglRefined4-1.png)<!-- -->

```r
car::Anova(mod5, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPygl.p
##                Chisq Df Pr(>Chisq)    
## (Intercept) 1087.830  1  < 2.2e-16 ***
## Injection     10.084  1   0.001496 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod5)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPygl.p ~ Injection + (1 | CagingSite/CageSiteID) + (1 |  
##     OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 209.9
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7510 -0.5203  0.0299  0.5532  2.5889 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.006913 0.08315 
##  CagingSite            (Intercept) 0.000000 0.00000 
##  OriginSite            (Intercept) 0.016728 0.12934 
##  Residual                          0.134468 0.36670 
## Number of obs: 226, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)  -2.47089    0.07492 -32.982
## InjectionPBS  0.15549    0.04896   3.175
## 
## Correlation of Fixed Effects:
##             (Intr)
## InjectinPBS -0.309
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modPygl <- mod5
RmodPygl <- MuMIn::r.squaredGLMM(modPygl)
```

Pygl expression is significantly different according to the immune challenge. More specifically, immune challenged fish injected with the antigen mixture (AMIX) have a lower Pygl gene expression compare to the fish injected with the control saline solution (PBS). 

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    DeltaCtPygl.p ~ Injection + Sex + 
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )

car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: DeltaCtPygl.p
##                Chisq Df Pr(>Chisq)    
## (Intercept) 978.8302  1  < 2.2e-16 ***
## Injection     9.7481  1   0.001795 ** 
## Sex           0.2304  1   0.631264    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: DeltaCtPygl.p ~ Injection + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: 213.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.6951 -0.5427  0.0298  0.5821  2.5559 
## 
## Random effects:
##  Groups                Name        Variance Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.00643  0.08019 
##  CagingSite            (Intercept) 0.00000  0.00000 
##  OriginSite            (Intercept) 0.01703  0.13050 
##  Residual                          0.13576  0.36846 
## Number of obs: 225, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept)  -2.46144    0.07867 -31.286
## InjectionPBS  0.15460    0.04952   3.122
## SexM         -0.02499    0.05207  -0.480
## 
## Correlation of Fixed Effects:
##             (Intr) InjPBS
## InjectinPBS -0.323       
## SexM        -0.285  0.095
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
The sex effect is not significant.
The mod5 is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


```r
boxplot(DeltaCtPygl.p ~ Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/PyglOrigin-1.png)<!-- -->

```r
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
```

<table class=" lightable-classic" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> mean </th>
   <th style="text-align:right;"> Se </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> PBS </td>
   <td style="text-align:right;"> -2.323065 </td>
   <td style="text-align:right;"> 0.0396051 </td>
   <td style="text-align:right;"> 107 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AMIX </td>
   <td style="text-align:right;"> -2.486398 </td>
   <td style="text-align:right;"> 0.0343619 </td>
   <td style="text-align:right;"> 118 </td>
  </tr>
</tbody>
</table>

### CqActb (β-actin)
#### Check the data

```r
dat2 = dat1[is.na(dat1$CqActb) == F, ]
# transform variable to approx. normality
dat2$CqActb.p <-
  bimixt::boxcox(dat2$CqActb, car::powerTransform(dat2$DeltaCtMtl)$lambda)
hist(dat2$CqActb.p)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ActbIndex-1.png)<!-- -->

#### Visualize

```r
par(mfrow=c(2,3))
boxplot(CqActb.p ~ OriginContam, data = dat2)
boxplot(CqActb.p ~ TransplantContam, data = dat2)
boxplot(CqActb.p ~ Injection, data = dat2)
boxplot(CqActb.p ~ TransplantContam * Injection, data = dat2)
boxplot(CqActb.p ~ TransplantContam * OriginContam, data = dat2)
boxplot(CqActb.p ~ TransplantContam * OriginContam * Injection, data = dat2)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ActbViz-1.png)<!-- -->

#### Build the full model

```r
modfull <-
 lme4::lmer(
    CqActb.p ~ (TransplantContam + OriginContam + Injection) ^ 3 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(modfull, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: CqActb.p
##                                             Chisq Df Pr(>Chisq)    
## (Intercept)                             4800.4064  1    < 2e-16 ***
## TransplantContam                           0.8446  1    0.35808    
## OriginContam                               1.6791  1    0.19504    
## Injection                                  1.3007  1    0.25409    
## SizeEnd                                    5.3729  1    0.02045 *  
## TransplantContam:OriginContam              0.9971  1    0.31800    
## TransplantContam:Injection                 0.2766  1    0.59897    
## OriginContam:Injection                     0.7924  1    0.37337    
## TransplantContam:OriginContam:Injection    0.1447  1    0.70368    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modfull)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: CqActb.p ~ (TransplantContam + OriginContam + Injection)^3 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -535
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4702 -0.6521  0.0416  0.5684  2.5732 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev. 
##  CageSiteID:CagingSite (Intercept) 2.614e-05 5.112e-03
##  CagingSite            (Intercept) 4.127e-12 2.031e-06
##  OriginSite            (Intercept) 3.204e-03 5.661e-02
##  Residual                          5.145e-03 7.173e-02
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                                 Estimate Std. Error t value
## (Intercept)                                     4.049539   0.058448  69.285
## TransplantContamLC                             -0.016481   0.017934  -0.919
## OriginContamLC                                 -0.077057   0.059466  -1.296
## InjectionPBS                                   -0.020642   0.018099  -1.140
## SizeEnd                                         0.009387   0.004050   2.318
## TransplantContamLC:OriginContamLC              -0.025505   0.025541  -0.999
## TransplantContamLC:InjectionPBS                 0.013351   0.025388   0.526
## OriginContamLC:InjectionPBS                     0.023410   0.026298   0.890
## TransplantContamLC:OriginContamLC:InjectionPBS  0.013967   0.036720   0.380
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TrCLC:OCLC TCLC:I OCLC:I
## TrnsplntCLC -0.188                                                     
## OrignCntmLC -0.518  0.151                                              
## InjectinPBS -0.155  0.481  0.145                                       
## SizeEnd     -0.696  0.051  0.015  0.011                                
## TrnCLC:OCLC  0.085 -0.699 -0.217 -0.337  0.033                         
## TrnCLC:IPBS  0.102 -0.684 -0.103 -0.713  0.004  0.481                  
## OrgCLC:IPBS  0.105 -0.331 -0.206 -0.688 -0.005  0.479      0.490       
## TCLC:OCLC:I -0.066  0.473  0.147  0.493 -0.009 -0.676     -0.691 -0.716
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod2 <-
 lme4::lmer(
    CqActb.p ~ (TransplantContam + OriginContam + Injection) ^ 2 +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod2, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: CqActb.p
##                                   Chisq Df Pr(>Chisq)    
## (Intercept)                   4832.5358  1    < 2e-16 ***
## TransplantContam                 1.5556  1    0.21232    
## OriginContam                     1.8664  1    0.17188    
## Injection                        2.3380  1    0.12625    
## SizeEnd                          5.3960  1    0.02018 *  
## TransplantContam:OriginContam    1.0097  1    0.31497    
## TransplantContam:Injection       1.1990  1    0.27351    
## OriginContam:Injection           2.7838  1    0.09522 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod2)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: CqActb.p ~ (TransplantContam + OriginContam + Injection)^2 +  
##     SizeEnd + (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -539.6
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.4962 -0.6592  0.0368  0.5907  2.6011 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0000298 0.005459
##  CagingSite            (Intercept) 0.0000000 0.000000
##  OriginSite            (Intercept) 0.0032064 0.056625
##  Residual                          0.0051237 0.071580
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                                    Estimate Std. Error t value
## (Intercept)                        4.051127   0.058276  69.516
## TransplantContamLC                -0.019717   0.015809  -1.247
## OriginContamLC                    -0.080383   0.058838  -1.366
## InjectionPBS                      -0.024034   0.015718  -1.529
## SizeEnd                            0.009389   0.004042   2.323
## TransplantContamLC:OriginContamLC -0.018944   0.018852  -1.005
## TransplantContamLC:InjectionPBS    0.020043   0.018304   1.095
## OriginContamLC:InjectionPBS        0.030569   0.018322   1.668
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS SizEnd TCLC:O TCLC:I
## TrnsplntCLC -0.179                                          
## OrignCntmLC -0.515  0.094                                   
## InjectinPBS -0.141  0.323  0.084                            
## SizeEnd     -0.697  0.062  0.017  0.018                     
## TrnCLC:OCLC  0.054 -0.584 -0.162 -0.006  0.036              
## TrnCLC:IPBS  0.077 -0.560 -0.002 -0.592 -0.002  0.025       
## OrgCLC:IPBS  0.083  0.012 -0.145 -0.552 -0.017 -0.010 -0.009
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod3 <-
 lme4::lmer(
    CqActb.p ~ TransplantContam + OriginContam + Injection +  SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod3, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: CqActb.p
##                      Chisq Df Pr(>Chisq)    
## (Intercept)      4897.3886  1    < 2e-16 ***
## TransplantContam    4.1955  1    0.04053 *  
## OriginContam        1.7623  1    0.18434    
## Injection           0.0097  1    0.92168    
## SizeEnd             5.7249  1    0.01673 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod3)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: CqActb.p ~ TransplantContam + OriginContam + Injection + SizeEnd +  
##     (1 | CagingSite/CageSiteID) + (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -553
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3786 -0.6177 -0.0041  0.6109  2.6272 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 1.954e-05 0.004421
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 3.162e-03 0.056233
##  Residual                          5.176e-03 0.071946
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                      Estimate Std. Error t value
## (Intercept)         4.0407558  0.0577405  69.981
## TransplantContamLC -0.0193609  0.0094523  -2.048
## OriginContamLC     -0.0756946  0.0570200  -1.328
## InjectionPBS        0.0009043  0.0091975   0.098
## SizeEnd             0.0097049  0.0040561   2.393
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC OrgCLC InjPBS
## TrnsplntCLC -0.178                     
## OrignCntmLC -0.508  0.001              
## InjectinPBS -0.085 -0.007  0.005       
## SizeEnd     -0.707  0.137  0.021  0.012
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```
#### Refining the model

```r
mod4 <-
 lme4::lmer(
    CqActb.p ~  TransplantContam + SizeEnd +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
car::Anova(mod4, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: CqActb.p
##                      Chisq Df Pr(>Chisq)    
## (Intercept)      6027.3927  1    < 2e-16 ***
## TransplantContam    4.2031  1    0.04035 *  
## SizeEnd             5.6332  1    0.01762 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(mod4)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: CqActb.p ~ TransplantContam + SizeEnd + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -562.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3849 -0.6200 -0.0148  0.6192  2.6233 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 2.177e-05 0.004666
##  CagingSite            (Intercept) 0.000e+00 0.000000
##  OriginSite            (Intercept) 3.999e-03 0.063237
##  Residual                          5.153e-03 0.071785
## Number of obs: 246, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                     Estimate Std. Error t value
## (Intercept)         4.004158   0.051576  77.636
## TransplantContamLC -0.019377   0.009452  -2.050
## SizeEnd             0.009624   0.004055   2.373
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC
## TrnsplntCLC -0.199       
## SizeEnd     -0.779  0.137
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

Now that we have identified the best model, add sex as covariate to check whether results are sensitive
NB: sample size is reduced when adding sex variable within the model, for this reason the sex effect on results is checked on the final model

#### Test for the sex effect

```r
dat2 = dat2[is.na(dat2$Sex) == F, ]
modSex <-
 lme4::lmer(
    CqActb.p ~  TransplantContam + SizeEnd + Sex +
      (1 | CagingSite /  CageSiteID) + (1| OriginSite),
    data = dat2,
    na.action = na.exclude
  )
performance::check_model(modSex)
```

![](Rmarkdown-Adaptive_responses_of_fish_in_multistress_context_files/figure-html/ActbSex-1.png)<!-- -->

```r
car::Anova(modSex, type = "3")
```

```
## Analysis of Deviance Table (Type III Wald chisquare tests)
## 
## Response: CqActb.p
##                      Chisq Df Pr(>Chisq)    
## (Intercept)      6037.4561  1    < 2e-16 ***
## TransplantContam    3.8189  1    0.05068 .  
## SizeEnd             6.0967  1    0.01354 *  
## Sex                 1.1895  1    0.27544    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
summary(modSex)
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: 
## CqActb.p ~ TransplantContam + SizeEnd + Sex + (1 | CagingSite/CageSiteID) +  
##     (1 | OriginSite)
##    Data: dat2
## 
## REML criterion at convergence: -553
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.3943 -0.6446  0.0074  0.6235  2.6828 
## 
## Random effects:
##  Groups                Name        Variance  Std.Dev.
##  CageSiteID:CagingSite (Intercept) 0.0000548 0.007403
##  CagingSite            (Intercept) 0.0000000 0.000000
##  OriginSite            (Intercept) 0.0039459 0.062816
##  Residual                          0.0051468 0.071741
## Number of obs: 245, groups:  
## CageSiteID:CagingSite, 24; CagingSite, 4; OriginSite, 4
## 
## Fixed effects:
##                     Estimate Std. Error t value
## (Intercept)         4.003488   0.051524  77.701
## TransplantContamLC -0.019078   0.009763  -1.954
## SizeEnd             0.010094   0.004088   2.469
## SexM               -0.010466   0.009596  -1.091
## 
## Correlation of Fixed Effects:
##             (Intr) TrnCLC SizEnd
## TrnsplntCLC -0.199              
## SizeEnd     -0.778  0.136       
## SexM         0.024 -0.041 -0.117
## optimizer (nloptwrap) convergence code: 0 (OK)
## boundary (singular) fit: see ?isSingular
```

```r
modActb <- modSex
RmodActb <- MuMIn::r.squaredGLMM(modActb)
```
The sex effect is  significant.
The modSex is thus the best model.
Here, there is no interaction, and hence no posthoc test nor parallelism test to conduct


## Summary of the best models for gene expression (Table 4 from the manuscript)

The following table report the results of the best models tested above, this table correspond to the Table 4 in the manuscript. The table report the marginal and conditional Rsquared (R2m and R2c respectively).
<table class=" lightable-classic-2" style="font-size: 10px; font-family: arial; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;">  </th>
   <th style="text-align:center;">  </th>
   <th style="text-align:center;"> Estimate </th>
   <th style="text-align:center;"> Std. Error </th>
   <th style="text-align:center;"> t or z value </th>
   <th style="text-align:center;"> df </th>
   <th style="text-align:center;"> Chisq </th>
   <th style="text-align:center;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr grouplength="3"><td colspan="8" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Mtl - Metallothionein | n = 245 | R2m = 0.113 | R2c = 0.191</strong></td></tr>
<tr>
   <td style="text-align:center;vertical-align: middle !important;padding-left: 2em;" rowspan="3" indentlevel="1"> Metal sequestration </td>
   <td style="text-align:center;"> Intercept </td>
   <td style="text-align:center;"> 1.32 </td>
   <td style="text-align:center;"> 0.237 </td>
   <td style="text-align:center;"> 5.6 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 31.3 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> -0.739 </td>
   <td style="text-align:center;"> 0.327 </td>
   <td style="text-align:center;"> -2.26 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 5.12 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Sex (M) </td>
   <td style="text-align:center;"> -0.287 </td>
   <td style="text-align:center;"> 0.139 </td>
   <td style="text-align:center;"> -2.06 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4.24 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="4"><td colspan="8" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Cat - Catalase | n = 245 | R2m = 0.0582 | R2c = 0.523</strong></td></tr>
<tr>
   <td style="text-align:center;vertical-align: middle !important;padding-left: 2em;" rowspan="4" indentlevel="1"> Antioxidant - Cat </td>
   <td style="text-align:center;"> Intercept </td>
   <td style="text-align:center;"> -3.18 </td>
   <td style="text-align:center;"> 0.221 </td>
   <td style="text-align:center;"> -14.3 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 206 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Imm. challenge (PBS) </td>
   <td style="text-align:center;"> 0.101 </td>
   <td style="text-align:center;"> 0.0381 </td>
   <td style="text-align:center;"> 2.65 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 7.04 </td>
   <td style="text-align:center;"> &lt;0.01 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Size </td>
   <td style="text-align:center;"> 0.0516 </td>
   <td style="text-align:center;"> 0.0169 </td>
   <td style="text-align:center;"> 3.05 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 9.31 </td>
   <td style="text-align:center;"> &lt;0.01 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Sex (M) </td>
   <td style="text-align:center;"> -0.0958 </td>
   <td style="text-align:center;"> 0.0397 </td>
   <td style="text-align:center;"> -2.41 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 5.83 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="3"><td colspan="8" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Gpx - Glutathione peroxidase | n = 245 | R2m = 0.0918 | R2c = 0.135</strong></td></tr>
<tr>
   <td style="text-align:center;vertical-align: middle !important;padding-left: 2em;" rowspan="3" indentlevel="1"> Antioxidant - Gpx </td>
   <td style="text-align:center;"> Intercept </td>
   <td style="text-align:center;"> -1.07 </td>
   <td style="text-align:center;"> 0.101 </td>
   <td style="text-align:center;"> -10.6 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 113 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> -0.349 </td>
   <td style="text-align:center;"> 0.137 </td>
   <td style="text-align:center;"> -2.54 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 6.45 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Sex (M) </td>
   <td style="text-align:center;"> -0.181 </td>
   <td style="text-align:center;"> 0.0788 </td>
   <td style="text-align:center;"> -2.3 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 5.29 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="3"><td colspan="8" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Casp3 - Caspase 3 | n = 246 | R2m = 0.253 | R2c = 0.255</strong></td></tr>
<tr>
   <td style="text-align:center;vertical-align: middle !important;padding-left: 2em;" rowspan="3" indentlevel="1"> Apoptosis effector </td>
   <td style="text-align:center;"> Intercept </td>
   <td style="text-align:center;"> -5.19 </td>
   <td style="text-align:center;"> 0.316 </td>
   <td style="text-align:center;"> -16.4 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 270 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Origin (LC) </td>
   <td style="text-align:center;"> -0.442 </td>
   <td style="text-align:center;"> 0.0985 </td>
   <td style="text-align:center;"> -4.48 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 20.1 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Size </td>
   <td style="text-align:center;"> 0.217 </td>
   <td style="text-align:center;"> 0.0311 </td>
   <td style="text-align:center;"> 7 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 49 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr grouplength="3"><td colspan="8" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Pcx - Pyruvate carboxylase | n = 239 | R2m = 0.0289 | R2c = 0.432</strong></td></tr>
<tr>
   <td style="text-align:center;vertical-align: middle !important;padding-left: 2em;" rowspan="3" indentlevel="1"> Energy metabolism (oxaloacetate synthesis &amp; gluconeogenesis) </td>
   <td style="text-align:center;"> Intercept </td>
   <td style="text-align:center;"> -3.26 </td>
   <td style="text-align:center;"> 0.164 </td>
   <td style="text-align:center;"> -19.8 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 392 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Imm. challenge (PBS) </td>
   <td style="text-align:center;"> 0.122 </td>
   <td style="text-align:center;"> 0.0502 </td>
   <td style="text-align:center;"> 2.44 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 5.95 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Sex (M) </td>
   <td style="text-align:center;"> -0.117 </td>
   <td style="text-align:center;"> 0.0526 </td>
   <td style="text-align:center;"> -2.22 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 4.92 </td>
   <td style="text-align:center;"> &lt;0.05 </td>
  </tr>
  <tr grouplength="2"><td colspan="8" style="background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;"><strong>Pygl - Glycogen phosphorylase | n = 226 | R2m = 0.0369 | R2c = 0.181</strong></td></tr>
<tr>
   <td style="text-align:center;vertical-align: middle !important;padding-left: 2em;" rowspan="2" indentlevel="1"> Energy metabolism (glycogen breakdown to produce glucose) </td>
   <td style="text-align:center;"> Intercept </td>
   <td style="text-align:center;"> -2.47 </td>
   <td style="text-align:center;"> 0.0749 </td>
   <td style="text-align:center;"> -33 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 1090 </td>
   <td style="text-align:center;"> &lt;0.0001 </td>
  </tr>
  <tr>
   
   <td style="text-align:center;padding-left: 2em;" indentlevel="1"> Imm. challenge (PBS) </td>
   <td style="text-align:center;"> 0.155 </td>
   <td style="text-align:center;"> 0.049 </td>
   <td style="text-align:center;"> 3.18 </td>
   <td style="text-align:center;"> 1 </td>
   <td style="text-align:center;"> 10.1 </td>
   <td style="text-align:center;"> &lt;0.01 </td>
  </tr>
</tbody>
</table>

# R session informations


```r
sessionInfo()
```

```
## R version 4.1.0 (2021-05-18)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 22621)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=French_France.1252  LC_CTYPE=French_France.1252   
## [3] LC_MONETARY=French_France.1252 LC_NUMERIC=C                  
## [5] LC_TIME=French_France.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
##  [1] httr_1.4.3            tidyr_1.2.0           sass_0.4.0           
##  [4] jsonlite_1.7.2        viridisLite_0.4.0     splines_4.1.0        
##  [7] carData_3.0-4         bslib_0.3.1           datawizard_0.2.3     
## [10] assertthat_0.2.1      stats4_4.1.0          highr_0.9            
## [13] cellranger_1.1.0      yaml_2.2.1            bayestestR_0.11.5    
## [16] ggrepel_0.9.1         pillar_1.7.0          lattice_0.20-44      
## [19] glue_1.6.2            pROC_1.17.0.1         digest_0.6.27        
## [22] rvest_1.0.3           minqa_1.2.4           colorspace_2.0-2     
## [25] MuMIn_1.43.17         plyr_1.8.6            htmltools_0.5.2      
## [28] Matrix_1.3-3          pkgconfig_2.0.3       haven_2.4.1          
## [31] purrr_0.3.4           patchwork_1.1.1       scales_1.1.1         
## [34] webshot_0.5.2         svglite_2.1.0.9000    openxlsx_4.2.4       
## [37] rio_0.5.27            lme4_1.1-27           tibble_3.1.6         
## [40] mgcv_1.8-35           generics_0.1.0        farver_2.1.0         
## [43] car_3.0-10            ggplot2_3.3.5         bimixt_1.0           
## [46] ellipsis_0.3.2        cli_3.2.0             magrittr_2.0.2       
## [49] crayon_1.4.1          readxl_1.3.1          evaluate_0.15        
## [52] fansi_0.5.0           nlme_3.1-152          MASS_7.3-54          
## [55] forcats_0.5.1         xml2_1.3.2            foreign_0.8-81       
## [58] tools_4.1.0           data.table_1.14.2     hms_1.1.0            
## [61] lifecycle_1.0.1       see_0.6.9             stringr_1.4.0        
## [64] munsell_0.5.0         zip_2.2.0             kableExtra_1.3.4.9000
## [67] compiler_4.1.0        jquerylib_0.1.4       systemfonts_1.0.4    
## [70] rlang_1.0.2           grid_4.1.0            nloptr_1.2.2.2       
## [73] rstudioapi_0.13       labeling_0.4.2        rmarkdown_2.14       
## [76] boot_1.3-28           gtable_0.3.0          abind_1.4-5          
## [79] DBI_1.1.1             curl_4.3.1            R6_2.5.0             
## [82] knitr_1.39            dplyr_1.0.8           performance_0.8.0    
## [85] fastmap_1.1.0         utf8_1.2.1            insight_0.16.0       
## [88] stringi_1.6.1         Rcpp_1.0.7            vctrs_0.3.8          
## [91] tidyselect_1.1.1      xfun_0.30
```

# References

Costantini, D., Dell’Omo, G., 2006. Effects of T-cell-mediated immune response on avian oxidative stress. Comp. Biochem. Physiol. A. Mol. Integr. Physiol. 145, 137–142. https://doi.org/10.1016/j.cbpa.2006.06.002.

Jacquin, L., Reader, S.M., Boniface, A., Mateluna, J., Patalas, I., Pérez-Jvostov, F., Hendry, A.P., 2016. Parallel and nonparallel behavioural evolution in response to parasitism and predation in Trinidadian guppies. J. Evol. Biol. 29, 1406–1422. https://doi.org/10.1111/jeb.12880.

Petitjean, Q., Jacquin, L., Riem, L., Pitout, M., Perrault, A., Cousseau, M., Laffaille, P., Jean, S., 2020a. Intraspecific variability of responses to combined metal contamination and immune challenge among wild fish populations. Environ. Pollut. 116042. https://doi.org/10.1016/j.envpol.2020.116042.

Petitjean, Q., Jean, S., Côte, J., Larcher, T., Angelier, F., Ribout, C., Perrault, A., Laffaille, P., Jacquin, L., 2020b. Direct and indirect effects of multiple environmental stressors on fish health in human-altered rivers. Sci. Total Environ. 742, 140657. https://doi.org/10.1016/j.scitotenv.2020.140657.
