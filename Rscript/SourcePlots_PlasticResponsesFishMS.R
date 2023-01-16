#########################################################################################################################################
# This is a set of function used to draw the different plots displayed in                                                                    # 
# title: "'Adaptive plastic responses to metal contamination in a multistress context: a field experiment in fish'"                     #
# author:  "Quentin PETITJEAN[q.petitjean1@gmail.com], Pascal LAFFAILLE, Annie PERRAULT, Myriam COUSSEAU, Séverine JEAN, Lisa JACQUIN"  #                                                                                        #
# date: "25/06/2022"                                                                                                                    #
#########################################################################################################################################


# Create the variability among populations plots for survival (with legend)
Survivplot <- function() {
  dat2 = dat1[is.na(dat1$Death) == F, ]
  # create a new dataset containing survival rate within each cage
  names(dat2)
  head(dat2)
  dat2$t2 = paste(dat2$OriginSite, dat2$TransplantContam,  sep = "_")
  n = as.data.frame(dplyr::count(dat2, CageID))
  names(n)[1] <- "CageID"
  names(n)[2] <- "n"
  alive <- as.data.frame(dplyr::count(dat2[dat2$Death == "0", ], CageID))
  dead <- as.data.frame(dplyr::count(dat2[dat2$Death == "1", ], CageID))
  DeathSum <- dplyr::left_join(alive, dead, by = "CageID")
  DeathSum$n.y[is.na(DeathSum$n.y)] <- 0
  DeathSum <- dplyr::left_join(DeathSum, n, by = "CageID")
  names(DeathSum)[2] <- "alive"
  names(DeathSum)[3] <- "dead"
  DeathSum$survival <- DeathSum$alive / DeathSum$n
  DeathSum <-  dplyr::left_join(DeathSum,
                                dat2[match(DeathSum$CageID,  dat2$CageID), c(
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
                                )], by = "CageID")
  
  names(DeathSum)
  DeathSum$t2 = paste(DeathSum$OriginSite, DeathSum$TransplantContam, sep =
                        "_")
  
  # average the results according to study sites
  n = as.data.frame(dplyr::count(DeathSum, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(DeathSum$survival, list(DeathSum$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "survival"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("OriginSite", "TransplantContam"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$OriginSite, Ga$TransplantContam, sep = "_")
  Ga
  
  Ga3 = aggregate(DeathSum$survival, list(DeathSum$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$OriginContam = c("LC", "LC", "HC", "HC", "LC", "LC", "HC", "HC")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2$Or = paste(Ga2$OriginContam, Ga2$TransplantContam, sep = "_")
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$OriginSite, Ga2$TransplantContam, sep = "_")
  Ga2$Orcolor <-
    ifelse(Ga2$OriginContam == "LC", "#336633", "#990000")
  Ga2$OrSymbols <-
    ifelse(Ga2$OriginSite == "ARIMAS",
           24,
           ifelse(
             Ga2$OriginSite == "AUSCOR",
             22,
             ifelse(Ga2$OriginSite == "CELFIG", 23, 21)
           ))
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  
  # Create a custom jitter
  jitt = 0.35
  
  ## for LC
  Ga2LC <- Ga2[which(Ga2$TransplantContam == "LC"),]
  Ga2LC[order(Ga2LC$OriginContam),]
  for(x in  seq(nrow(Ga2LC))){
    Ga2LC$CNumJitter[x] <- Ga2LC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  ## for HC
  Ga2HC <- Ga2[which(Ga2$TransplantContam == "HC"),]
  Ga2HC[order(Ga2HC$OriginContam),]
  for(x in  seq(nrow(Ga2HC))){
    Ga2HC$CNumJitter[x] <- Ga2HC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }

  Ga2 <- merge(Ga2, rbind(Ga2LC[c("t2","CNumJitter")], Ga2HC[c("t2","CNumJitter")]), by = "t2")
  
  # Create a break interval on Y axis
  breakInt = c(0.2, 0.7)
  ylim = c(0, 1)
  step = 0.1
  
  plotScale <- seq(ylim[1], ylim[2], by = step)
  plotScale <-
    plotScale[which(plotScale <= breakInt[1] |
                      plotScale >= breakInt[2])]
  toAdd <- length(which(plotScale <= breakInt[1]))
  finalScale <- seq(breakInt[2], ylim[2], by = step)
  toAddfinal <- c()
  for (h in (seq(toAdd))) {
    toAdd_temp <- min(finalScale) - (step * h)
    toAddfinal <- c(toAddfinal, toAdd_temp)
  }
  finalScale <- sort(c(toAddfinal, finalScale))
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(min(finalScale), 
             max(finalScale)+((max(finalScale)-min(finalScale))/(length(finalScale)-1)/2))
  )
  
  title(
    ylab = "Survival",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  
  # draw y axis
  plotScale <- unlist(rapply(as.list(plotScale), sprintf, fmt = "%0.1f", how = "replace"))
  segments(
    x0 = 0,
    y0 = max(breakInt) - 5 / 100 * max(breakInt),
    x1 = 0,
    y1 = 1
  )
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 0,
    y1 = max(breakInt) - 10 / 100 * max(breakInt)
  )
  text(0,
       max(breakInt) - 5 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(0,
       max(breakInt) - 10 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(
    rep(0, length(plotScale)),
    finalScale,
    as.character(plotScale),
    xpd = TRUE,
    srt = 0,
    adj = 1.5
  )
  text(
    rep(0, length(plotScale)),
    finalScale,
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 2,
    y1 = min(finalScale)
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  
  # some formating to add the individuals to the plot
  dat2$Orcolor <-
    ifelse(dat2$OriginContam == "LC", "#336633", "#990000")
  dat2$OrSymbols <-
    ifelse(dat2$OriginSite == "ARIMAS",
           24,
           ifelse(
             dat2$OriginSite == "AUSCOR",
             22,
             ifelse(dat2$OriginSite == "CELFIG", 23, 21)
           ))
  
  # Because we have modifier the scale of the plot (start at 0.4 instead of 0) 
  # we need to change the value corresponding to dead fish from 0 to 0.4
  dat2$Surv[dat2$Death == 1] <- min(finalScale)
  dat2$Surv[dat2$Death == 0] <- max(finalScale)
  
  # associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
    dat2[dat2$t2 == i, "SurvJitter"] <- ifelse(grepl("#990000", dat2[dat2$t2 == i, "Orcolor"]), 
                                               ifelse(grepl("RIOU", dat2[dat2$t2 == i, "t2"]),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]+0.03),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]+0.015)
                                               ),
                                               ifelse(grepl("CELFIG", dat2[dat2$t2 == i, "t2"]),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]-0.03),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]-0.015)
                                               ))
    
    
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 5)
  dat2$SurvJitter <- jitter(dat2$SurvJitter, 2)
  # Force the jittered value that are below 0 at 0 
  dat2$SurvJitter[which(dat2$SurvJitter < min(finalScale))] <- min(finalScale)
  
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      points(
        CNumJitter[k],
        SurvJitter[k],
        pch = OrSymbols[k],
        col = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Orcolor[k], alpha.f = 0.2),
        cex = 1
      )
    )}
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(
      Ga2,
      segments(
        CNumJitter[j],
        survival[j],
        CNumJitter[j + 1],
        survival[j + 1],
        Ga2$Orcolor[j],
        lwd = 2
      )
    )
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(
      Ga2,
      segments(
        CNumJitter[k],
        survival[k] - Se[k],
        CNumJitter[k],
        survival[k] + Se[k],
        col = "black",
        lwd = 1.2
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        survival[k],
        pch = OrSymbols[k],
        col = "black",
        bg = adjustcolor(Orcolor[k], alpha.f = 0.7),
        cex = 1.5
      )
    )
  }

  # Add significance stars above NC fish slopes
  text(
    1.1,
    0.9,
    "**",
    xpd = TRUE,
    srt = 0,
    cex = 1.3,
    col = "#336633"
  )
  
  # Create a legend
  text(
    c(0.3, 0.3),
    c(0.6, 0.56),
    c("ARIMAS", "CELFIG"),
    xpd = FALSE,
    srt = 0,
    col = "#336633",
    cex = 0.9
  )
  text(
    c(0.3, 0.3),
    c(0.52, 0.48),
    c("AUSCOR", "RIOU"),
    xpd = FALSE,
    srt = 0,
    col = "#990000",
    cex = 0.9
  )
  text(
    c(0.5, 0.5),
    c(0.58, 0.50),
    "}",
    srt = 0,
    cex = 2.3,
    family = "sans"
  )
  text(
    c(0.6, 0.6),
    c(0.58, 0.50),
    c("LC", "HC"),
    xpd = FALSE,
    srt = 0,
    col = c("#336633", "#990000"),
    cex = 0.9
  )
  text(
    0.35,
    0.64,
    "Origin site",
    srt = 0,
    cex = 1,
    family = "sans"
  )
  
}

# Create the variability among populations plots for survival (without legend)
Survivplot2 <- function() {
  dat2 = dat1[is.na(dat1$Death) == F, ]
  # create a new dataset containing survival rate within each cage
  names(dat2)
  head(dat2)
  dat2$t2 = paste(dat2$OriginSite, dat2$TransplantContam,  sep = "_")
  n = as.data.frame(dplyr::count(dat2, CageID))
  names(n)[1] <- "CageID"
  names(n)[2] <- "n"
  alive <- as.data.frame(dplyr::count(dat2[dat2$Death == "0", ], CageID))
  dead <- as.data.frame(dplyr::count(dat2[dat2$Death == "1", ], CageID))
  DeathSum <- dplyr::left_join(alive, dead, by = "CageID")
  DeathSum$n.y[is.na(DeathSum$n.y)] <- 0
  DeathSum <- dplyr::left_join(DeathSum, n, by = "CageID")
  names(DeathSum)[2] <- "alive"
  names(DeathSum)[3] <- "dead"
  DeathSum$survival <- DeathSum$alive / DeathSum$n
  DeathSum <-  dplyr::left_join(DeathSum,
                                dat2[match(DeathSum$CageID,  dat2$CageID), c(
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
                                )], by = "CageID")
  
  names(DeathSum)
  DeathSum$t2 = paste(DeathSum$OriginSite, DeathSum$TransplantContam, sep =
                        "_")
  n = as.data.frame(dplyr::count(DeathSum, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(DeathSum$survival, list(DeathSum$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "survival"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("OriginSite", "TransplantContam"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$OriginSite, Ga$TransplantContam, sep = "_")
  Ga
  
  Ga3 = aggregate(DeathSum$survival, list(DeathSum$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$OriginContam = c("LC", "LC", "HC", "HC", "LC", "LC", "HC", "HC")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2$Or = paste(Ga2$OriginContam, Ga2$TransplantContam, sep = "_")
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$OriginSite, Ga2$TransplantContam, sep = "_")
  Ga2$Orcolor <-
    ifelse(Ga2$OriginContam == "LC", "#336633", "#990000")
  Ga2$OrSymbols <-
    ifelse(Ga2$OriginSite == "ARIMAS",
           24,
           ifelse(
             Ga2$OriginSite == "AUSCOR",
             22,
             ifelse(Ga2$OriginSite == "CELFIG", 23, 21)
           ))
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  
  # Create a custom jitter
  jitt = 0.35
  
  ## for LC
  Ga2LC <- Ga2[which(Ga2$TransplantContam == "LC"),]
  Ga2LC[order(Ga2LC$OriginContam),]
  for(x in  seq(nrow(Ga2LC))){
    Ga2LC$CNumJitter[x] <- Ga2LC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  ## for HC
  Ga2HC <- Ga2[which(Ga2$TransplantContam == "HC"),]
  Ga2HC[order(Ga2HC$OriginContam),]
  for(x in  seq(nrow(Ga2HC))){
    Ga2HC$CNumJitter[x] <- Ga2HC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  
  Ga2 <- merge(Ga2, rbind(Ga2LC[c("t2","CNumJitter")], Ga2HC[c("t2","CNumJitter")]), by = "t2")
  
  # Create a break interval on Y axis
  breakInt = c(0.2, 0.7)
  ylim = c(0, 1)
  step = 0.1
  
  plotScale <- seq(ylim[1], ylim[2], by = step)
  plotScale <-
    plotScale[which(plotScale <= breakInt[1] |
                      plotScale >= breakInt[2])]
  toAdd <- length(which(plotScale <= breakInt[1]))
  finalScale <- seq(breakInt[2], ylim[2], by = step)
  toAddfinal <- c()
  for (h in (seq(toAdd))) {
    toAdd_temp <- min(finalScale) - (step * h)
    toAddfinal <- c(toAddfinal, toAdd_temp)
  }
  finalScale <- sort(c(toAddfinal, finalScale))
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(min(finalScale), 
             max(finalScale)+((max(finalScale)-min(finalScale))/(length(finalScale)-1)/2))
  )
  
  title(
    ylab = "Survival",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  # draw y axis
  plotScale <- unlist(rapply(as.list(plotScale), sprintf, fmt = "%0.1f", how = "replace"))
  segments(
    x0 = 0,
    y0 = max(breakInt) - 5 / 100 * max(breakInt),
    x1 = 0,
    y1 = 1
  )
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 0,
    y1 = max(breakInt) - 10 / 100 * max(breakInt)
  )
  text(0,
       max(breakInt) - 5 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(0,
       max(breakInt) - 10 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(
    rep(0, length(plotScale)),
    finalScale,
    as.character(plotScale),
    xpd = TRUE,
    srt = 0,
    adj = 1.5
  )
  text(
    rep(0, length(plotScale)),
    finalScale,
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 2,
    y1 = min(finalScale)
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  
  # some formating to add the individuals to the plot
  dat2$Orcolor <-
    ifelse(dat2$OriginContam == "LC", "#336633", "#990000")
  dat2$OrSymbols <-
    ifelse(dat2$OriginSite == "ARIMAS",
           24,
           ifelse(
             dat2$OriginSite == "AUSCOR",
             22,
             ifelse(dat2$OriginSite == "CELFIG", 23, 21)
           ))
  
  # Because we have modifier the scale of the plot (start at 0.4 instead of 0) 
  # we need to change the value corresponding to dead fish from 0 to 0.4
  dat2$Surv[dat2$Death == 1] <- min(finalScale)
  dat2$Surv[dat2$Death == 0] <- max(finalScale)
  
  # associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
    dat2[dat2$t2 == i, "SurvJitter"] <- ifelse(grepl("#990000", dat2[dat2$t2 == i, "Orcolor"]), 
                                               ifelse(grepl("RIOU", dat2[dat2$t2 == i, "t2"]),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]+0.03),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]+0.015)
                                               ),
                                               ifelse(grepl("CELFIG", dat2[dat2$t2 == i, "t2"]),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]-0.03),
                                                      ifelse(dat2[dat2$t2 == i, "Surv"] == min(finalScale), 
                                                             dat2[dat2$t2 == i, "Surv"], 
                                                             dat2[dat2$t2 == i, "Surv"]-0.015)
                                               ))
    
    
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 2.5)
  dat2$SurvJitter <- jitter(dat2$SurvJitter, 5)
  dat2[which(dat2$SurvJitter < min(finalScale)), "SurvJitter"] <- min(finalScale)
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      points(
        CNumJitter[k],
        SurvJitter[k],
        pch = OrSymbols[k],
        col = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Orcolor[k], alpha.f = 0.2),
        cex = .75
      )
    )}
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(
      Ga2,
      segments(
        CNumJitter[j],
        survival[j],
        CNumJitter[j + 1],
        survival[j + 1],
        Ga2$Orcolor[j],
        lwd = 2
      )
    )
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(
      Ga2,
      segments(
        CNumJitter[k],
        survival[k] - Se[k],
        CNumJitter[k],
        survival[k] + Se[k],
        col = "black",
        lwd = 1.2
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        survival[k],
        pch = OrSymbols[k],
        col = "black",
        bg = adjustcolor(Orcolor[k], alpha.f = 0.7),
        cex = 1.5
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  text(
    1.1,
    0.9,
    "**",
    xpd = TRUE,
    srt = 0,
    cex = 1.3,
    col = "#336633"
  )
  
}

# Create the variability among populations plots for bioacc (without legend)
BioaccPlot1 <- function() {
  dat2 = dat1[is.na(dat1$MuscleCd) == F, ]
  dat2$Cdscaled = scale(log(dat2$MuscleCd),
                        center = T,
                        scale = T)
  dat2$Cuscaled = scale(log(dat2$MuscleCu),
                        center = T,
                        scale = T)
  dat2$Znscaled = scale(log(dat2$MuscleZn),
                        center = T,
                        scale = T)
  dat2$Bioacc = dat2$Cdscaled + dat2$Cuscaled + dat2$Znscaled
  dat2$t2 = paste(dat2$OriginSite, dat2$TransplantContam, sep = "_")
  n = as.data.frame(dplyr::count(dat2, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(dat2$Bioacc, list(dat2$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "Bioacc"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("OriginSite", "TransplantContam"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$OriginSite, Ga$TransplantContam, sep = "_")
  Ga
  
  Ga3 = aggregate(dat2$Bioacc, list(dat2$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$OriginContam = c("LC", "LC", "HC", "HC", "LC", "LC", "HC", "HC")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2$Or = paste(Ga2$OriginContam, Ga2$TransplantContam, sep = "_")
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$OriginSite, Ga2$TransplantContam, sep = "_")
  Ga2$Orcolor <-
    ifelse(Ga2$OriginContam == "LC", "#336633", "#990000")
  Ga2$OrSymbols <-
    ifelse(Ga2$OriginSite == "ARIMAS",
           24,
           ifelse(
             Ga2$OriginSite == "AUSCOR",
             22,
             ifelse(Ga2$OriginSite == "CELFIG", 23, 21)
           ))
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  
  # Create a custom jitter
  jitt = 0.35
  
  ## for LC
  Ga2LC <- Ga2[which(Ga2$TransplantContam == "LC"),]
  Ga2LC[order(Ga2LC$OriginContam),]
  for(x in  seq(nrow(Ga2LC))){
    Ga2LC$CNumJitter[x] <- Ga2LC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  ## for HC
  Ga2HC <- Ga2[which(Ga2$TransplantContam == "HC"),]
  Ga2HC[order(Ga2HC$OriginContam),]
  for(x in  seq(nrow(Ga2HC))){
    Ga2HC$CNumJitter[x] <- Ga2HC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  
  Ga2 <- merge(Ga2, rbind(Ga2LC[c("t2","CNumJitter")], Ga2HC[c("t2","CNumJitter")]), by = "t2")
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-5, 8)
  )
  
  title(
    ylab = "Metal bioaccumulation",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  
  # draw y axis
  plotScale <- c("-4.0", "-2.0", "0.0", "2.0", "4.0", "6.0", "8.0")
  
  segments(x0 = 0,
           y0 = -5,
           x1 = 0,
           y1 = 8)
  text(
    rep(0, length(plotScale)),
    as.numeric(plotScale),
    plotScale,
    xpd = TRUE,
    srt = 0,
    adj = 1.5
  )
  text(
    rep(0, length(plotScale)),
    as.numeric(plotScale),
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = -5,
    x1 = 2,
    y1 = -5
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  
  # some formatting to add the individuals to the plot
  dat2$Orcolor <-
    ifelse(dat2$OriginContam == "LC", "#336633", "#990000")
  dat2$OrSymbols <-
    ifelse(dat2$OriginSite == "ARIMAS",
           24,
           ifelse(
             dat2$OriginSite == "AUSCOR",
             22,
             ifelse(dat2$OriginSite == "CELFIG", 23, 21)
           ))
  
  # add individual to the plot
  ## associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 1.5)
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      points(
        CNumJitter[k],
        Bioacc[k],
        pch = OrSymbols[k],
        col = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Orcolor[k], alpha.f = 0.2),
        cex = .75
      )
    )}
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(Ga2,
         segments(
           CNumJitter[j],
           Bioacc[j],
           CNumJitter[j + 1],
           Bioacc[j + 1],
           Ga2$Orcolor[j],
           lwd = 2
         ))
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(
      Ga2,
      segments(
        CNumJitter[k],
        Bioacc[k] - Se[k],
        CNumJitter[k],
        Bioacc[k] + Se[k],
        col = "black",
        lwd = 1.2
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        Bioacc[k],
        pch = OrSymbols[k],
        col = "black",
        bg = adjustcolor(Orcolor[k], alpha.f = 0.7),
        cex = 1.5
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  text(
    c(1.25, 1.25),
    c(1.5,-0.25),
    "*",
    xpd = TRUE,
    srt = 0,
    cex = 1.3,
    col = "#336633"
  )
  
  # Create a legend
  #text(c(0.3,0.3), c(0.6,0.56), c("ARIMAS", "CELFIG"), xpd = FALSE, srt = 0, col= "#336633", cex = 0.9)
  #text(c(0.3,0.3), c(0.52,0.48), c("AUSCOR", "RIOU"), xpd = FALSE, srt = 0, col= "#990000", cex = 0.9)
  #text(c(0.5,0.5), c(0.58,0.50), "}", srt = 0, cex = 2.3, family = "sans")
  #text(c(0.6,0.6), c(0.58,0.50), c("LC", "HC"), xpd = FALSE, srt = 0, col= c("#336633", "#990000"), cex = 0.9)
  #text(0.35, 0.64, "Origin site", srt = 0, cex = 1, family = "sans")
}

# Create the Multiple stressors plot for bioacc (without legend)
BioaccPlot2 <- function() {
  dat2 = dat1[is.na(dat1$MuscleCd) == F, ]
  dat2$Cdscaled = scale(log(dat2$MuscleCd),
                        center = T,
                        scale = T)
  dat2$Cuscaled = scale(log(dat2$MuscleCu),
                        center = T,
                        scale = T)
  dat2$Znscaled = scale(log(dat2$MuscleZn),
                        center = T,
                        scale = T)
  dat2$Bioacc = dat2$Cdscaled + dat2$Cuscaled + dat2$Znscaled
  names(dat2)
  dat2$t2 = paste(dat2$TransplantContam, dat2$Injection, sep = "_")
  n = as.data.frame(dplyr::count(dat2, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(dat2$Bioacc, list(dat2$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "Bioacc"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("TransplantContam", "Injection"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$TransplantContam, Ga$Injection, sep = "_")
  Ga
  
  Ga3 = aggregate(dat2$Bioacc, list(dat2$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$Injection = c("AMIX", "PBS", "AMIX", "PBS")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$TransplantContam, Ga2$Injection, sep = "_")
  Ga2$Injectioncolor <-
    ifelse(Ga2$Injection == "PBS", "#808080", "#470C8E")
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  Ga2$lty <- ifelse(Ga2$Injection == "PBS", 2, 1)
  Ga2 <- Ga2[order(Ga2$Injection), ]
  
  # Create a custom jitter
  jitt = 0.2
  counterHC <- 0
  counterLC <- 0
  for (x in seq(nrow(Ga2))) {
    counterHC <- ifelse(Ga2$CNum[x] == 1.75,  counterHC + 1, counterHC)
    counterLC <-  ifelse(Ga2$CNum[x] == 0.25,  counterLC + 1, counterLC)
    if(Ga2$CNum[x] == 0.25){
    Ga2$CNumJitter[x] <-
      ifelse(counterLC == 1,
             Ga2$CNum[x] + jitt / 2,
             Ga2$CNum[x] - jitt / 2)
    } else if(Ga2$CNum[x] == 1.75){
      Ga2$CNumJitter[x] <-
        ifelse(counterHC == 1,
               Ga2$CNum[x] + jitt / 2,
               Ga2$CNum[x] - jitt / 2)
    }
  }
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-5, 8)
  )
  
  title(
    ylab = "Metal bioaccumulation",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  
  # draw y axis
  plotScale <- c("-4.0", "-2.0", "0.0", "2.0", "4.0", "6.0", "8.0")
  segments(x0 = 0,
           y0 = -5,
           x1 = 0,
           y1 = 8)
  text(
    rep(0, length(plotScale)),
    as.numeric(plotScale),
    plotScale,
    xpd = TRUE,
    srt = 0,
    adj = 1.5
  )
  text(
    rep(0, length(plotScale)),
    as.numeric(plotScale),
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = -5,
    x1 = 2,
    y1 = -5
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  # some formating to add the individuals to the plot
  dat2$Injectioncolor <-
    ifelse(dat2$Injection == "PBS", "#808080", "#470C8E")

  # add individual to the plot
  ## associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 1.5)
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      graphics::symbols(
        CNumJitter[k],
        Bioacc[k],
        stars = cbind(1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5),
        add = T,
        inches = 0.02,
        fg = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Injectioncolor[k], alpha.f = 0.2),
      )
    )
    }

  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(
      Ga2,
      segments(
        CNumJitter[j],
        Bioacc[j],
        CNumJitter[j + 1],
        Bioacc[j + 1],
        Injectioncolor[j],
        lty = lty[j],
        lwd = 2
      )
    )
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(
      Ga2,
      segments(
        CNumJitter[k],
        Bioacc[k] - Se[k],
        CNumJitter[k],
        Bioacc[k] + Se[k],
        col = "black",
        lwd = 1.2
      )
    )
    with(
      Ga2,
      graphics::symbols(
        CNumJitter[k],
        Bioacc[k],
        stars = cbind(1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5, 1, 0.5),
        add = T,
        inches = 0.06,
        fg = "black",
        bg = adjustcolor(Injectioncolor[k], alpha.f = 0.7),
      )
    )
  }

  # Add significance stars above NC fish slopes
  text(
    1,
    -0.75,
    "*",
    xpd = TRUE,
    srt = 0,
    cex = 1.5,
    col = "#808080"
  )
  
  # Create a legend
  #text(c(0.3,0.3), c(-1.4,-1.6), c("PBS", "AMIX"), xpd = FALSE, srt = 0, col= c("#808080", "#000000"), cex = 0.9, family = "sans")
  #text(c(0.6,0.6), c(0.58,0.50), c("LC", "HC"), xpd = FALSE, srt = 0, col= c("#336633", "#990000"), cex = 0.9)
  #text(0.43, -1.2, "Imm. Challenge", srt = 0, cex = 1, family = "sans")
  #segments(0.4,-1.42,
  #0.6,-1.42,
  #"#808080",
  #lwd = 2)
  #segments(0.4,-1.62,
  #0.6,-1.62,
  #"#000000",
  #lwd = 2)
}

# Create the Multiple stressors plot for bioacc (with legend)
BioaccPlot3 <- function() {
  plot.new()
  opar <- par(no.readonly = TRUE)
  BioaccPlot2()
  legend2()
  par(opar)
}

# Create the variability among populations plots for neutrophils/lymphocytes ratio 
NLratioPlot1 <- function() {
  dat2 = dat1[is.na(dat1$Lymphocytes) == F, ]
  dat2 = dat2[is.na(dat2$Neutrophils) == F, ]
  dat2$NLRatio = dat2$Neutrophils / dat2$Lymphocytes
  # scale numeric variables
  dat2$NLRatio = scale(dat2$NLRatio, center = TRUE, scale = TRUE)
  dat2$NLRatio.p <-
    bimixt::boxcox(dat2$NLRatio + 1, car::powerTransform(dat2$NLRatio + 1)$lambda)
  dat2$t2 = paste(dat2$OriginSite, dat2$TransplantContam, sep = "_")
  n = as.data.frame(dplyr::count(dat2, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(dat2$NLRatio.p, list(dat2$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "NLRatio.p"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("OriginSite", "TransplantContam"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$OriginSite, Ga$TransplantContam, sep = "_")
  Ga
  
  Ga3 = aggregate(dat2$NLRatio.p, list(dat2$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$OriginContam = c("LC", "LC", "HC", "HC", "LC", "LC", "HC", "HC")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2$Or = paste(Ga2$OriginContam, Ga2$TransplantContam, sep = "_")
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$OriginSite, Ga2$TransplantContam, sep = "_")
  Ga2$Orcolor <-
    ifelse(Ga2$OriginContam == "LC", "#336633", "#990000")
  Ga2$OrSymbols <-
    ifelse(Ga2$OriginSite == "ARIMAS",
           24,
           ifelse(
             Ga2$OriginSite == "AUSCOR",
             22,
             ifelse(Ga2$OriginSite == "CELFIG", 23, 21)
           ))
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  
  # Create a custom jitter
  jitt = 0.35
  
  ## for LC
  Ga2LC <- Ga2[which(Ga2$TransplantContam == "LC"),]
  Ga2LC[order(Ga2LC$OriginContam),]
  for(x in  seq(nrow(Ga2LC))){
    Ga2LC$CNumJitter[x] <- Ga2LC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  ## for HC
  Ga2HC <- Ga2[which(Ga2$TransplantContam == "HC"),]
  Ga2HC[order(Ga2HC$OriginContam),]
  for(x in  seq(nrow(Ga2HC))){
    Ga2HC$CNumJitter[x] <- Ga2HC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  
  Ga2 <- merge(Ga2, rbind(Ga2LC[c("t2","CNumJitter")], Ga2HC[c("t2","CNumJitter")]), by = "t2")
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-1.65, 1.65)
  )
  
  title(
    ylab = "NL inflammatory immune response",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  
  # draw y axis
  plotScale <- c("-1.5", "-1.0", "-0.5", "0.0", "0.5", "1.0", "1.5")
  segments(
    x0 = 0,
    y0 = -1.65,
    x1 = 0,
    y1 = 1.5
  )
  text(
    rep(0, length(plotScale)),
    as.numeric(plotScale),
    plotScale,
    xpd = TRUE,
    srt = 0,
    adj = 1.5
  )
  text(
    rep(0, length(plotScale)),
    as.numeric(plotScale),
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = -1.65,
    x1 = 2,
    y1 = -1.65
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  
  # some formatting to add the individuals to the plot
  dat2$Orcolor <-
    ifelse(dat2$OriginContam == "LC", "#336633", "#990000")
  dat2$OrSymbols <-
    ifelse(dat2$OriginSite == "ARIMAS",
           24,
           ifelse(
             dat2$OriginSite == "AUSCOR",
             22,
             ifelse(dat2$OriginSite == "CELFIG", 23, 21)
           ))
  
  # add individual to the plot
  ## associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 1.25)
  
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      points(
        CNumJitter[k],
        NLRatio.p[k],
        pch = OrSymbols[k],
        col = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Orcolor[k], alpha.f = 0.2),
        cex = .75
      )
    )}
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(
      Ga2,
      segments(
        CNumJitter[j],
        NLRatio.p[j],
        CNumJitter[j + 1],
        NLRatio.p[j + 1],
        Ga2$Orcolor[j],
        lwd = 2
      )
    )
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(
      Ga2,
      segments(
        CNumJitter[k],
        NLRatio.p[k] - Se[k],
        CNumJitter[k],
        NLRatio.p[k] + Se[k],
        col = "black",
        lwd = 1.2
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        NLRatio.p[k],
        pch = OrSymbols[k],
        col = "black",
        bg = adjustcolor(Orcolor[k], alpha.f = 0.7),
        cex = 1.5
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  #text(c(1.1, 1.1), c(0.9, -0.7),   "*", xpd = TRUE, srt = 0, cex = 1.3, col = "#336633")
  # Create a legend
  #text(c(0.3,0.3), c(0.6,0.56), c("ARIMAS", "CELFIG"), xpd = FALSE, srt = 0, col= "#336633", cex = 0.9)
  #text(c(0.3,0.3), c(0.52,0.48), c("AUSCOR", "RIOU"), xpd = FALSE, srt = 0, col= "#990000", cex = 0.9)
  #text(c(0.5,0.5), c(0.58,0.50), "}", srt = 0, cex = 2.3, family = "sans")
  #text(c(0.6,0.6), c(0.58,0.50), c("LC", "HC"), xpd = FALSE, srt = 0, col= c("#336633", "#990000"), cex = 0.9)
  #text(0.35, 0.64, "Origin site", srt = 0, cex = 1, family = "sans")
}

# Create the variability among populations plots for available energy (sum of lipid, protein and carbohydrate) 
AvailEnerPlot1 <- function() {
  dat2 = dat1[is.na(dat1$AvailableEnerJ) == F, ]
  dat2$t2 = paste(dat2$OriginSite, dat2$TransplantContam, sep = "_")
  n = as.data.frame(dplyr::count(dat2, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(log(dat2$AvailableEnerJ + 1), list(dat2$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "log(AvailableEnerJ)"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("OriginSite", "TransplantContam"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$OriginSite, Ga$TransplantContam, sep = "_")
  Ga
  
  Ga3 = aggregate(log(dat2$AvailableEnerJ + 1), list(dat2$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$OriginContam = c("LC", "LC", "HC", "HC", "LC", "LC", "HC", "HC")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2$Or = paste(Ga2$OriginContam, Ga2$TransplantContam, sep = "_")
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$OriginSite, Ga2$TransplantContam, sep = "_")
  Ga2$Orcolor <-
    ifelse(Ga2$OriginContam == "LC", "#336633", "#990000")
  Ga2$OrSymbols <-
    ifelse(Ga2$OriginSite == "ARIMAS",
           24,
           ifelse(
             Ga2$OriginSite == "AUSCOR",
             22,
             ifelse(Ga2$OriginSite == "CELFIG", 23, 21)
           ))
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  names(Ga2)[3] <- "AE"
  
  # Create a custom jitter
  jitt = 0.35
  
  ## for LC
  Ga2LC <- Ga2[which(Ga2$TransplantContam == "LC"),]
  Ga2LC[order(Ga2LC$OriginContam),]
  for(x in  seq(nrow(Ga2LC))){
    Ga2LC$CNumJitter[x] <- Ga2LC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  ## for HC
  Ga2HC <- Ga2[which(Ga2$TransplantContam == "HC"),]
  Ga2HC[order(Ga2HC$OriginContam),]
  for(x in  seq(nrow(Ga2HC))){
    Ga2HC$CNumJitter[x] <- Ga2HC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  
  Ga2 <- merge(Ga2, rbind(Ga2LC[c("t2","CNumJitter")], Ga2HC[c("t2","CNumJitter")]), by = "t2")
  
  # Create a break interval on Y axis
  breakInt = c(0, 7.5)
  ylim = c(0, 10)
  step = 0.5
  
  plotScale <- seq(ylim[1], ylim[2], by = step)
  plotScale <-
    plotScale[which(plotScale <= breakInt[1] |
                      plotScale >= breakInt[2])]
  toAdd <- length(which(plotScale <= breakInt[1]))
  finalScale <- seq(breakInt[2], ylim[2], by = step)
  toAddfinal <- c()
  for (h in (seq(toAdd))) {
    toAdd_temp <- min(finalScale) - (step * h)
    toAddfinal <- c(toAddfinal, toAdd_temp)
  }
  finalScale <- sort(c(toAddfinal, finalScale))
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(min(finalScale), max(finalScale))
  )
  
  title(
    ylab = "Available energy",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  
  # draw y axis
  plotScale <- unlist(rapply(as.list(plotScale), sprintf, fmt = "%0.2f", how = "replace"))
 
  segments(
    x0 = 0,
    y0 = max(breakInt) - 2 / 100 * max(breakInt),
    x1 = 0,
    y1 = max(as.numeric(plotScale))
  )
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 0,
    y1 = max(breakInt) - 4 / 100 * max(breakInt)
  )
  text(0,
       max(breakInt) - 2 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(0,
       max(breakInt) - 4 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(
    rep(0, length(plotScale)),
    finalScale,
    as.character(plotScale),
    xpd = TRUE,
    srt = 0,
    adj = 1.25
  )
  text(
    rep(0, length(plotScale)),
    finalScale,
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 2,
    y1 = min(finalScale)
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  
  # some formatting to add the individuals to the plot
  dat2$Orcolor <-
    ifelse(dat2$OriginContam == "LC", "#336633", "#990000")
  dat2$OrSymbols <-
    ifelse(dat2$OriginSite == "ARIMAS",
           24,
           ifelse(
             dat2$OriginSite == "AUSCOR",
             22,
             ifelse(dat2$OriginSite == "CELFIG", 23, 21)
           ))
  
  # add individual to the plot
  ## associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 1.25)
  
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      points(
        CNumJitter[k],
        log(dat2$AvailableEnerJ + 1)[k],
        pch = OrSymbols[k],
        col = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Orcolor[k], alpha.f = 0.2),
        cex = .75
      )
    )}
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(Ga2,
         segments(CNumJitter[j],
                  AE[j],
                  CNumJitter[j + 1],
                  AE[j + 1],
                  Ga2$Orcolor[j],
                  lwd = 2))
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(Ga2,
         segments(
           CNumJitter[k],
           AE[k] - Se[k],
           CNumJitter[k],
           AE[k] + Se[k],
           col = "black",
           lwd = 1.2
         ))
    with(
      Ga2,
      points(
        CNumJitter[k],
        AE[k],
        pch = OrSymbols[k],
        col = "black",
        bg = adjustcolor(Orcolor[k], alpha.f = 0.7),
        cex = 1.5
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  text(
    c(1.1),
    c(8.25),
    "*",
    xpd = TRUE,
    srt = 0,
    cex = 1.5,
    col = "#336633"
  )
  text(
    c(1.1),
    c(8.45),
    "*",
    xpd = TRUE,
    srt = 0,
    cex = 1.5,
    col = "#990000"
  )
  
}

# Create the variability among populations plots for Daily mass change 
DMCPlot1 <- function() {
  dat2 = dat1[is.na(dat1$DailyMassChange) == F, ]
  # transform variable to approx. normality
  dat2$DailyMassChange.p <-
    bimixt::boxcox(dat2$DailyMassChange + 3,
                   car::powerTransform(dat2$DailyMassChange + 3)$lambda)
  dat2$t2 = paste(dat2$OriginSite, dat2$TransplantContam, sep = "_")
  n = as.data.frame(dplyr::count(dat2, t2))
  n
  names(n)[1] <- "t2"
  names(n)[2] <- "n"
  GAmean = aggregate(dat2$DailyMassChange.p, list(dat2$t2) , mean)
  GAmean
  names(GAmean)[1] <- "t2"
  names(GAmean)[2] <- "dat2$DailyMassChange.p"
  Ga <- dplyr::left_join(GAmean, n, by = "t2")
  Ga = tidyr::separate(
    data = Ga,
    col = t2,
    into = c("OriginSite", "TransplantContam"),
    sep = "_"
  )
  Ga$t2 = paste(Ga$OriginSite, Ga$TransplantContam, sep = "_")
  Ga
  
  Ga3 = aggregate(dat2$DailyMassChange.p, list(dat2$t2), sd)
  Ga3
  names(Ga3)[1] <- "t2"
  names(Ga3)[2] <- "Sd"
  Ga2 <- dplyr::left_join(Ga, Ga3, by = "t2")
  Ga2
  Ga2$Se = Ga2$Sd / sqrt(Ga2$n)
  Ga2
  Ga2$OriginContam = c("LC", "LC", "HC", "HC", "LC", "LC", "HC", "HC")
  Ga2$TransplantContam[which(Ga2$TransplantContam == "HC")] <- "HC"
  Ga2$TransplantContam[which(Ga2$TransplantContam == "LC")] <- "LC"
  Ga2$Or = paste(Ga2$OriginContam, Ga2$TransplantContam, sep = "_")
  Ga2
  
  # define some graphic elements
  Ga2$treat = paste(Ga2$OriginSite, Ga2$TransplantContam, sep = "_")
  Ga2$Orcolor <-
    ifelse(Ga2$OriginContam == "LC", "#336633", "#990000")
  Ga2$OrSymbols <-
    ifelse(Ga2$OriginSite == "ARIMAS",
           24,
           ifelse(
             Ga2$OriginSite == "AUSCOR",
             22,
             ifelse(Ga2$OriginSite == "CELFIG", 23, 21)
           ))
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  names(Ga2)[3] <- "DMC"
  
  # Create a custom jitter
  jitt = 0.35
  
  ## for LC
  Ga2LC <- Ga2[which(Ga2$TransplantContam == "LC"),]
  Ga2LC[order(Ga2LC$OriginContam),]
  for(x in  seq(nrow(Ga2LC))){
    Ga2LC$CNumJitter[x] <- Ga2LC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  ## for HC
  Ga2HC <- Ga2[which(Ga2$TransplantContam == "HC"),]
  Ga2HC[order(Ga2HC$OriginContam),]
  for(x in  seq(nrow(Ga2HC))){
    Ga2HC$CNumJitter[x] <- Ga2HC$CNum[x] - jitt/2 + (jitt/3)*(x-1)
  }
  
  Ga2 <- merge(Ga2, rbind(Ga2LC[c("t2","CNumJitter")], Ga2HC[c("t2","CNumJitter")]), by = "t2")
  
  # Create a break interval on Y axis
  #breakInt = c(0.0, 0.8)
  #ylim = c(0, 1.8)
 # step = 0.2
  
  #plotScale <- seq(ylim[1], ylim[2], by = step)
  #plotScale <-
  #  plotScale[which(plotScale <= breakInt[1] |
  #                    plotScale >= breakInt[2])]
  #toAdd <- length(which(plotScale <= breakInt[1]))
  #finalScale <- seq(breakInt[2], ylim[2], by = step)
  #toAddfinal <- c()
  #for (h in (seq(toAdd))) {
  #  toAdd_temp <- min(finalScale) - (step * h)
  #  toAddfinal <- c(toAddfinal, toAdd_temp)
  # }
  #finalScale <- sort(c(toAddfinal, finalScale))
  finalScale = seq(0, 2.4, 0.4)
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-0.05, max(finalScale))
  )
  
  title(
    ylab = "Daily mass change",
    line = 1.5,
    cex.lab = 1.2,
    family = "sans"
  )
  title(
    xlab = "Transplant site",
    line = 0.5,
    cex.lab = 1.2,
    family = "sans"
  )
  
  # draw y axis
  segments(
    x0 = 0,
    y0 = -0.05,
    x1 = 0,
    y1 = max(finalScale)
  )

  text(
    rep(0, length(finalScale)),
    finalScale,
    unlist(rapply(as.list(finalScale), sprintf, fmt = "%0.2f", how = "replace")),
    xpd = TRUE,
    srt = 0,
    adj = 1.25
  )
  text(
    rep(0, length(finalScale)),
    finalScale,
    "-",
    xpd = TRUE,
    srt = 0,
    adj = 0.9
  )
  
  # draw x axis and add x axis labels
  segments(
    x0 = 0,
    y0 = -0.05,
    x1 = 2,
    y1 = -0.05
  )
  for (i in unique(Ga2$TransplantContam)) {
    with(Ga2,
         mtext(
           i,
           side = 1,
           line = 0,
           at = unique(CNum[which(TransplantContam == i)]),
           col = unique(Contcolor[which(TransplantContam == i)]),
           family = "sans",
           cex = 1.2
         ))
  }
  
  # some formatting to add the individuals to the plot
  dat2$Orcolor <-
    ifelse(dat2$OriginContam == "LC", "#336633", "#990000")
  dat2$OrSymbols <-
    ifelse(dat2$OriginSite == "ARIMAS",
           24,
           ifelse(
             dat2$OriginSite == "AUSCOR",
             22,
             ifelse(dat2$OriginSite == "CELFIG", 23, 21)
           ))
  
  # add individual to the plot
  ## associate the custom jitter previously generated for each site with individuals
  for(i in unique(dat2$t2)){
    dat2[dat2$t2 == i, "CNum"] <- Ga2[Ga2$t2 == i, "CNumJitter"]
  }
  
  dat2$CNumJitter <- jitter(dat2$CNum, 1.25)
  
  # plot the individuals
  for (k in seq(nrow(dat2))) {
    with(
      dat2,
      points(
        CNumJitter[k],
        DailyMassChange.p[k],
        pch = OrSymbols[k],
        col = adjustcolor("black", alpha.f = 0.3),
        bg = adjustcolor(Orcolor[k], alpha.f = 0.2),
        cex = .75
      )
    )}
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(Ga2,
         segments(CNumJitter[j],
                  DMC[j],
                  CNumJitter[j + 1],
                  DMC[j + 1],
                  Ga2$Orcolor[j],
                  lwd = 2))
  }
  # Add error bar and symbols at the origin and end of reaction norm
  for (k in seq(nrow(Ga2))) {
    with(Ga2,
         segments(
           CNumJitter[k],
           DMC[k] - Se[k],
           CNumJitter[k],
           DMC[k] + Se[k],
           col = "black",
           lwd = 1.2
         ))
    with(
      Ga2,
      points(
        CNumJitter[k],
        DMC[k],
        pch = OrSymbols[k],
        col = "black",
        bg = adjustcolor(Orcolor[k], alpha.f = 0.7),
        cex = 1.5
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  #text(c(1.1), c(8.25),   "*", xpd = TRUE, srt = 0, cex = 1.5, col = "#336633")
  #text(c(1.1), c(8.45),   "*", xpd = TRUE, srt = 0, cex = 1.5, col = "#990000")
  
}

# Create panel plot to groups variability among populations plots (survival +  bioacc + NLRatio)
Fig1Merged <- function() {
  plot.new()
  opar <- par(no.readonly = TRUE)
  par(
    mfrow = c(2, 2),
    # 2x2 layout
    oma = c(1, 1, 1, 1),
    # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 1, 0),
    # space for one row of text at ticks and to tidyr::separate plots
    mgp = c(3, 1, 0),
    # axis label at 2 rows distance, tick labels at 1 row
    mai = c(0.1, 0.3, 0.1, 0.15),
    xpd = NA,
    # allow content to protrude into outer margin (and beyond)
    family = "sans"
  )
  
  Survivplot2()
  text(0.1,
       1.05,
       "A",
       srt = 0,
       cex = 1.2,
       family = "sans")
  BioaccPlot1()
  text(0.1,
       8.0,
       "B",
       srt = 0,
       cex = 1.2,
       family = "sans")
  NLratioPlot1()
  text(0.1,
       1.5,
       "C",
       srt = 0,
       cex = 1.2,
       family = "sans")
  legend()
  
  par(opar)
}

# Create a panel plot to groups variability among populations plots (in supplementary material - available energy + daily mass loss)
Fig3Merged <- function() {
  plot.new()
  opar <- par(no.readonly = TRUE)
  par(
    mfrow = c(1, 2),
    # 1x2 layout
    oma = c(1, 1, 1, 1),
    # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 1, 0),
    # space for one row of text at ticks and to tidyr::separate plots
    mgp = c(3, 1, 0),
    # axis label at 2 rows distance, tick labels at 1 row
    mai = c(0.1, 0.5, 0.1, 0.15),
    xpd = NA,
    # allow content to protrude into outer margin (and beyond)
    family = "sans"
  )
  AvailEnerPlot1()
  text(0.1,
       10.0,
       "A",
       srt = 0,
       cex = 1.2,
       family = "sans")
  legend3()
  DMCPlot1()
  text(0.1,
       2.4,
       "B",
       srt = 0,
       cex = 1.2,
       family = "sans")
  par(opar)
}

# create a legend for variability among populations plots (survival +  bioacc + NLRatio)
legend <- function() {
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(0.4, 1)
  )
  # Create a legend
  text(
    c(1, 1),
    c(0.71, 0.63),
    c("ARIMAS", "CELFIG"),
    xpd = FALSE,
    srt = 0,
    col = "#336633",
    cex = 1.2
  )
  text(
    c(1, 1),
    c(0.55, 0.47),
    c("AUSCOR", "RIOU"),
    xpd = FALSE,
    srt = 0,
    col = "#990000",
    cex = 1.2
  )
  points(
    c(0.6, 0.6, 0.6, 0.6),
    c(0.71, 0.63, 0.55, 0.47),
    pch = c(24, 23, 22, 21),
    bg = c("#336633", "#336633", "#990000", "#990000"),
    col = "black",
    cex = 1.5
  )
  text(
    c(1.4, 1.4),
    c(0.67, 0.51),
    "}",
    srt = 0,
    cex = 3.5,
    family = "sans"
  )
  text(
    c(1.6, 1.6),
    c(0.67, 0.51),
    c("LC", "HC"),
    xpd = FALSE,
    srt = 0,
    col = c("#336633", "#990000"),
    cex = 1.2
  )
  text(
    1.1,
    0.8,
    "Origin site",
    srt = 0,
    cex = 1.4,
    family = "sans"
  )
}

# create a legend for Multiple stressors plots (Bioacc + NLRatio)
legend2 <- function() {
  # Create a legend
  text(
    c(0.8, 0.8),
    c(-3, -4),
    c("PBS", "AMIX"),
    xpd = FALSE,
    srt = 0,
    col = c("#808080", "#470C8E"),
    cex = 0.9,
    family = "sans"
  )
  text(
    1.05,
    -2,
    "Imm. Challenge",
    srt = 0,
    cex = 1,
    family = "sans"
  )
  segments(1, -3,
           1.2, -3,
           "#808080",
           lty = 2,
           lwd = 2)
  segments(1, -4,
           1.2, -4,
           "#470C8E",
           lwd = 2)
}

# create a legend for variability among populations plots (in supplementary material - available energy + daily mass loss)
legend3 <- function() {
  # Create a legend
  text(
    c(0.45, 0.45),
    c(7.41, 7.23),
    c("ARIMAS", "CELFIG"),
    xpd = FALSE,
    srt = 0,
    col = "#336633",
    cex = 1
  )
  text(
    c(1.46, 1.46),
    c(7.41, 7.23),
    c("AUSCOR", "RIOU"),
    xpd = FALSE,
    srt = 0,
    col = "#990000",
    cex = 1
  )
  points(
    c(0.1, 0.1, 1.10, 1.10),
    c(7.41, 7.23, 7.41, 7.23),
    pch = c(24, 23, 22, 21),
    bg = c("#336633", "#336633", "#990000", "#990000"),
    col = "black",
    cex = 1.5
  )
  text(
    c(0.75, 1.79),
    c(7.32, 7.32),
    "}",
    srt = 0,
    cex = 3.5,
    family = "sans"
  )
  text(
    c(0.875, 1.95),
    c(7.32, 7.32),
    c("LC", "HC"),
    xpd = FALSE,
    srt = 0,
    col = c("#336633", "#990000"),
    cex = 1
  )
  text(
    1.05,
    7.57,
    "Origin site",
    srt = 0,
    cex = 1.2,
    family = "sans"
  )
}

# NB: to plot an element run the selected function, e.g., BioaccPlot1() 
