#########################################################################################################################################
# This is a set of function used to make the differents tables displayed in                                                                    # 
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
  dat2$t2 = paste(dat2$Transfert, dat2$OriginSite, sep = "_")
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
  jitt = 0.3
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
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
    ylim = c(min(finalScale), max(finalScale))
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
  plotScale <- as.character(plotScale)
  plotScale[which(plotScale == "1")] <- "1.0"
  plotScale[which(plotScale == "0")] <- "0.0"
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
        Ga2$Orcolor[k],
        lwd = 1
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        survival[k],
        pch = Ga2$OrSymbols[k],
        col = "black",
        bg = Ga2$Orcolor[k],
        cex = 2
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
  dat2$t2 = paste(dat2$Transfert, dat2$OriginSite, sep = "_")
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
  jitt = 0.3
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
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
    ylim = c(min(finalScale), max(finalScale))
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
  plotScale <- as.character(plotScale)
  plotScale[which(plotScale == "1")] <- "1.0"
  plotScale[which(plotScale == "0")] <- "0.0"
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
        Ga2$Orcolor[k],
        lwd = 1
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        survival[k],
        pch = Ga2$OrSymbols[k],
        col = "black",
        bg = Ga2$Orcolor[k],
        cex = 2
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
  jitt = 0.3
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-2, 2)
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
  plotScale <- c("-2.0", "-1.0", "0.0", "1.0", "2.0")
  segments(x0 = 0,
           y0 = -2,
           x1 = 0,
           y1 = 2)
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
    y0 = -2,
    x1 = 2,
    y1 = -2
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
        Ga2$Orcolor[k],
        lwd = 1
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        Bioacc[k],
        pch = Ga2$OrSymbols[k],
        col = "black",
        bg = Ga2$Orcolor[k],
        cex = 2
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  text(
    c(1.1, 1.1),
    c(0.9,-0.7),
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
    ifelse(Ga2$Injection == "PBS", "#808080", "#000000")
  Ga2$InjectionSymbols <-
    ifelse(Ga2$Injection == "PBS", 4, 4)
  Ga2$Contcolor <-
    ifelse(Ga2$TransplantContam == "LC", "#336633", "#990000")
  Ga2$CNum <- ifelse(Ga2$TransplantContam == "LC", 0.25, 1.75)
  Ga2$lty <- ifelse(Ga2$Injection == "PBS", 2, 1)
  Ga2 <- Ga2[order(Ga2$Injection), ]
  # Create a custom jitter
  jitt = 0.2
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-2, 2)
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
  plotScale <- c("-2.0", "-1.0", "0.0", "1.0", "2.0")
  segments(x0 = 0,
           y0 = -2,
           x1 = 0,
           y1 = 2)
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
    y0 = -2,
    x1 = 2,
    y1 = -2
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
  
  # Add reaction norm
  for (j in seq(1, nrow(Ga2), by = 2)) {
    with(
      Ga2,
      segments(
        CNumJitter[j],
        Bioacc[j],
        CNumJitter[j + 1],
        Bioacc[j + 1],
        Ga2$Injectioncolor[j],
        lty = Ga2$lty[j],
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
        Ga2$Injectioncolor[k],
        lwd = 1
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        Bioacc[k],
        pch = InjectionSymbols[k],
        col = "black",
        bg = Injectioncolor[k],
        cex = 2
      )
    )
  }
  
  # Add significance stars above NC fish slopes
  text(
    1.1,
    -0.0,
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
  jitt = 0.3
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
  # Create an empty plot
  plot(
    NULL,
    ylab = "",
    xlab = "",
    axes = FALSE,
    xlim = c(0, 2),
    ylim = c(-1.5, 0.5)
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
  plotScale <- c("-1.5", "-1.0", "-0.5", "0.0", "0.5")
  segments(
    x0 = 0,
    y0 = -1.5,
    x1 = 0,
    y1 = 0.5
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
    y0 = -1.5,
    x1 = 2,
    y1 = -1.5
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
        Ga2$Orcolor[k],
        lwd = 1
      )
    )
    with(
      Ga2,
      points(
        CNumJitter[k],
        NLRatio.p[k],
        pch = Ga2$OrSymbols[k],
        col = "black",
        bg = Ga2$Orcolor[k],
        cex = 2
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
  jitt = 0.3
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
  # Create a break interval on Y axis
  breakInt = c(0, 8)
  ylim = c(0, 9)
  step = 0.2
  
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
  plotScale <- as.character(plotScale)
  plotScale[which(plotScale == "9")] <- "9.0"
  plotScale[which(plotScale == "8")] <- "8.0"
  plotScale[which(plotScale == "0")] <- "0.0"
  segments(
    x0 = 0,
    y0 = max(breakInt) - 1 / 100 * max(breakInt),
    x1 = 0,
    y1 = max(as.numeric(plotScale))
  )
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 0,
    y1 = max(breakInt) - 1.5 / 100 * max(breakInt)
  )
  text(0,
       max(breakInt) - 1 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(0,
       max(breakInt) - 1.5 / 100 * max(breakInt),
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
           Ga2$Orcolor[k],
           lwd = 1
         ))
    with(
      Ga2,
      points(
        CNumJitter[k],
        AE[k],
        pch = Ga2$OrSymbols[k],
        col = "black",
        bg = Ga2$Orcolor[k],
        cex = 2
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
  jitt = 0.3
  for (x in seq(nrow(Ga2))) {
    Ga2$CNumJitter[x] <- Ga2$CNum[x] + (x / nrow(Ga2) * jitt)
  }
  
  # Create a break interval on Y axis
  breakInt = c(0.0, 0.8)
  ylim = c(0, 1.8)
  step = 0.2
  
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
  plotScale <- as.character(plotScale)
  plotScale[which(plotScale == "1")] <- "1.0"
  plotScale[which(plotScale == "0")] <- "0.0"
  segments(
    x0 = 0,
    y0 = max(breakInt) - 10 / 100 * max(breakInt),
    x1 = 0,
    y1 = max(as.numeric(plotScale))
  )
  segments(
    x0 = 0,
    y0 = min(finalScale),
    x1 = 0,
    y1 = max(breakInt) - 15 / 100 * max(breakInt)
  )
  text(0,
       max(breakInt) - 10 / 100 * max(breakInt),
       "\\",
       xpd = F,
       srt = 90)
  text(0,
       max(breakInt) - 15 / 100 * max(breakInt),
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
           Ga2$Orcolor[k],
           lwd = 1
         ))
    with(
      Ga2,
      points(
        CNumJitter[k],
        DMC[k],
        pch = Ga2$OrSymbols[k],
        col = "black",
        bg = Ga2$Orcolor[k],
        cex = 2
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
       1.0,
       "A",
       srt = 0,
       cex = 1.2,
       family = "sans")
  BioaccPlot1()
  text(0.1,
       2.0,
       "B",
       srt = 0,
       cex = 1.2,
       family = "sans")
  NLratioPlot1()
  text(0.1,
       0.5,
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
       9.0,
       "A",
       srt = 0,
       cex = 1.2,
       family = "sans")
  legend3()
  DMCPlot1()
  text(0.1,
       1.8,
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
    cex = 2
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
    c(0.3, 0.3),
    c(-1.5, -1.7),
    c("PBS", "AMIX"),
    xpd = FALSE,
    srt = 0,
    col = c("#808080", "#000000"),
    cex = 0.9,
    family = "sans"
  )
  text(
    0.43,
    -1.3,
    "Imm. Challenge",
    srt = 0,
    cex = 1,
    family = "sans"
  )
  segments(0.5, -1.52,
           0.7, -1.52,
           "#808080",
           lty = 2,
           lwd = 2)
  segments(0.5, -1.72,
           0.7, -1.72,
           "#000000",
           lwd = 2)
}

# create a legend for variability among populations plots (in supplementary material - available energy + daily mass loss)
legend3 <- function() {
  # Create a legend
  text(
    c(0.45, 0.45),
    c(8.91, 8.83),
    c("ARIMAS", "CELFIG"),
    xpd = FALSE,
    srt = 0,
    col = "#336633",
    cex = 1
  )
  text(
    c(1.46, 1.46),
    c(8.91, 8.83),
    c("AUSCOR", "RIOU"),
    xpd = FALSE,
    srt = 0,
    col = "#990000",
    cex = 1
  )
  points(
    c(0.1, 0.1, 1.10, 1.10),
    c(8.91, 8.83, 8.91, 8.83),
    pch = c(24, 23, 22, 21),
    bg = c("#336633", "#336633", "#990000", "#990000"),
    col = "black",
    cex = 1.5
  )
  text(
    c(0.75, 1.79),
    c(8.87, 8.87),
    "}",
    srt = 0,
    cex = 3.5,
    family = "sans"
  )
  text(
    c(0.95, 2.0),
    c(8.87, 8.87),
    c("LC", "HC"),
    xpd = FALSE,
    srt = 0,
    col = c("#336633", "#990000"),
    cex = 1
  )
  text(
    1.05,
    8.97,
    "Origin site",
    srt = 0,
    cex = 1.2,
    family = "sans"
  )
}

# NB: to plot an element run the selected function, e.g., BioaccPlot1() 