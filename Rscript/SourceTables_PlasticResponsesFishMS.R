#########################################################################################################################################
# This is a set of function used to make the different tables displayed in                                                                    # 
# title: "'Adaptive plastic responses to metal contamination in a multistress context: a field experiment in fish'"                     #
# author:  "Quentin PETITJEAN[q.petitjean1@gmail.com], Pascal LAFFAILLE, Annie PERRAULT, Myriam COUSSEAU, Séverine JEAN, Lisa JACQUIN"  #                                                                                        #
# date: "25/06/2022"                                                                                                                    #
#########################################################################################################################################

#########################################
#           Table 3 from the MS         #
#   All traits except genes expression  #
#########################################

TraitsTable <- function() {
  # create a temporary table grouping the names of the best models and the name of the corresponding response variable
  BestMods <- data.frame(
    mods = c(
      "modSurvival",
      "modBioacc",
      "modRatOx",
      "modOXDam",
      "modantiOx",
      "modIR",
      "modNLRatio.p",
      "modAvailableEnerJ",
      "modDMC",
      "modHSI",
      "modGSI"
    ),
    modsId = c(
      "Survival",
      "Bioaccumulation",
      "Oxidative stress index",
      "Oxidative damage",
      "Antioxidant capacity",
      "Local immune response",
      "NL inflammatory immune response",
      "Available energy",
      "Daily mass change",
      "HSI",
      "GSI"
    )
  )
  
  # add the sample size
  BestMods[["modsn"]] <- rep(NA, nrow(BestMods))
  for (r in seq(nrow(BestMods))) {
    if ("lm" %in% class(get(BestMods[r, "mods"]))) {
      BestMods[r, "modsn"] <-
        paste0("n = ", nrow(model.frame(get(BestMods[r, "mods"]))))
    } else{
      BestMods[r, "modsn"] <-
        paste0("n = ", summary(get(BestMods[r, "mods"]))$devcomp$dims[["n"]])
    }
  }
  # add the Rsquared
  BestMods[["modsR2m"]] <- rep(NA, nrow(BestMods))
  BestMods[["modsR2c"]] <- rep(NA, nrow(BestMods))
  for (r in seq(nrow(BestMods))) {
    if ("lm" %in% class(get(BestMods[r, "mods"]))) {
      BestMods[r, "modsR2m"] <- NA
      BestMods[r, "modsR2c"] <- NA
    } else{
      BestMods[r, "modsR2m"] <-
        paste0("R2m = ", signif(get(paste0(
          "R", BestMods[r, "mods"]
        ))[[1]], digits = 3))
      BestMods[r, "modsR2c"] <-
        paste0("R2c = ", signif(get(paste0(
          "R", BestMods[r, "mods"]
        ))[[2]], digits = 3))
    }
  }
  
  # retrieve the useful informations from the models
  options(scipen = 999)
  ValuesRes <- data.frame()
  for (r in seq(nrow(BestMods))) {
    if ("lm" %in% class(get(BestMods[r, "mods"]))) {
      Values <- cbind(
        signif(summary(get(BestMods[r, "mods"]))$coefficient[, c(1:3)], digits = 3),
        df = c(NA, signif(
          car::Anova(get(BestMods[r, "mods"]), type = "3")[["Df"]], digits = 3
        )),
        Chisq = c(NA, signif(
          car::Anova(get(BestMods[r, "mods"]), type = "3")[["LR Chisq"]], digits = 3
        )),
        p.value = c(NA, signif(
          car::Anova(get(BestMods[r, "mods"]), type = "3")[["Pr(>Chisq)"]], digits = 3
        ))
      )
      colnames(Values)[which(colnames(Values) == "z value")] <-
        "t or z value"
    } else{
      Values <- cbind(
        summary(get(BestMods[r, "mods"]))$coefficient,
        df = car::Anova(get(BestMods[r, "mods"]), type = "3")[["Df"]],
        Chisq = car::Anova(get(BestMods[r, "mods"]), type = "3")[["Chisq"]],
        p.value = car::Anova(get(BestMods[r, "mods"]), type = "3")[["Pr(>Chisq)"]]
      )
      colnames(Values)[which(colnames(Values) == "t value")] <-
        "t or z value"
      Values <- signif(Values, digits = 3)
    }
    Values <- cbind(rownames(Values), Values)
    rownames(Values) <- NULL
    Values <- cbind(rep(BestMods[r, "modsId"], nrow(Values)), Values)
    ValuesRes <- rbind(ValuesRes, Values)
  }
  # leave the two first column without names
  names(ValuesRes)[c(1, 2)] <- c("", "")
  
  
  # specify the new predictor names
  PredictorsNames <-
    data.frame(
      toreplace = unique(ValuesRes[[2]]),
      replacement = c(
        "Intercept",
        "Transplant (LC)",
        "Origin (LC)",
        "Transplant (LC): Origin (LC)",
        "Imm. challenge (PBS)",
        "Transplant (LC): imm. Challenge (PBS)",
        "Size",
        "Sex (M)"
      )
    )
  
  # rename the predictors
  for (pn in PredictorsNames[["toreplace"]]) {
    ValuesRes[[2]][which(ValuesRes[[2]] == pn)] <-
      PredictorsNames[["replacement"]][which(PredictorsNames[["toreplace"]] == pn)]
  }
  
  # replace p.value with large decimal by approximations
  ValuesRes[["p.value"]][which(ValuesRes[["p.value"]] < 0.0001)] <-
    "<0.0001"
  ValuesRes[["p.value"]][which(ValuesRes[["p.value"]] > 0.0001 &
                                 ValuesRes[["p.value"]] < 0.01)] <- "<0.01"
  ValuesRes[["p.value"]][which(ValuesRes[["p.value"]] > 0.01 &
                                 ValuesRes[["p.value"]] < 0.05)] <- "<0.05"
  options(scipen = 0)
  
  l <- vector("list", nrow(BestMods))
  l[[1]] <- kableExtra::kable_classic_2(
    kableExtra::kbl(ValuesRes[-1], align = "c"),
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F,
    html_font = "arial",
    font_size = 10
  )
  
  for (i in seq(nrow(BestMods))) {
    l[[i + 1]] <- kableExtra::pack_rows(l[[i]],
                                        if (is.na(BestMods[i, "modsR2m"]) &
                                            is.na(BestMods[i, "modsR2c"])) {
                                          paste(BestMods[i, "modsId"], BestMods[i, "modsn"], sep = " | ")
                                        } else{
                                          paste(BestMods[i, "modsId"], BestMods[i, "modsn"], BestMods[i, "modsR2m"], BestMods[i, "modsR2c"], sep = " | ")
                                        },
                                        min(which(ValuesRes[[1]] ==  BestMods[i, "modsId"])),
                                        max(which(ValuesRes[[1]] ==  BestMods[i, "modsId"])),
                                        label_row_css = "background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;")
  }
  l[[length(l)]]
}


#######################
# Table 4 from the MS #
#   Genes expression  #
#######################

GenesExpTable <- function() {
  # create a temporary table grouping the names of the best models and the name of the corresponding response variable
  BestGenesMods <- data.frame(
    mods = c("modMtl",
             "modCat",
             "modGpx",
             "modCasp3",
             "modPcx",
             "modPygl"),
    modsId = c(
      "Mtl - Metallothionein",
      "Cat - Catalase",
      "Gpx - Glutathione peroxidase",
      "Casp3 - Caspase 3",
      "Pcx - Pyruvate carboxylase",
      "Pygl - Glycogen phosphorylase"
    ),
    modFunc = c(
      "Metal sequestration",
      "Antioxidant - Cat",
      "Antioxidant - Gpx ",
      "Apoptosis effector",
      "Energy metabolism (oxaloacetate synthesis & gluconeogenesis)",
      "Energy metabolism (glycogen breakdown to produce glucose)"
    )
  )
  
  # add the sample size
  BestGenesMods[["modsn"]] <- rep(NA, nrow(BestGenesMods))
  for (r in seq(nrow(BestGenesMods))) {
    if ("lm" %in% class(get(BestGenesMods[r, "mods"]))) {
      BestGenesMods[r, "modsn"] <-
        paste0("n = ", nrow(model.frame(get(
          BestGenesMods[r, "mods"]
        ))))
    } else{
      BestGenesMods[r, "modsn"] <-
        paste0("n = ", summary(get(BestGenesMods[r, "mods"]))$devcomp$dims[["n"]])
    }
  }
  # add the Rsquared
  BestGenesMods[["modsR2m"]] <- rep(NA, nrow(BestGenesMods))
  BestGenesMods[["modsR2c"]] <- rep(NA, nrow(BestGenesMods))
  for (r in seq(nrow(BestGenesMods))) {
    if ("lm" %in% class(get(BestGenesMods[r, "mods"]))) {
      BestGenesMods[r, "modsR2m"] <- NA
      BestGenesMods[r, "modsR2c"] <- NA
    } else{
      BestGenesMods[r, "modsR2m"] <-
        paste0("R2m = ", signif(get(paste0(
          "R", BestGenesMods[r, "mods"]
        ))[[1]], digits = 3))
      BestGenesMods[r, "modsR2c"] <-
        paste0("R2c = ", signif(get(paste0(
          "R", BestGenesMods[r, "mods"]
        ))[[2]], digits = 3))
    }
  }
  
  # retrieve the useful informations from the models
  options(scipen = 999)
  ValuesRes <- data.frame()
  for (r in seq(nrow(BestGenesMods))) {
    if ("lm" %in% class(get(BestGenesMods[r, "mods"]))) {
      Values <- cbind(
        signif(summary(get(
          BestGenesMods[r, "mods"]
        ))$coefficient[, c(1:3)], digits = 3),
        df = c(NA, signif(
          car::Anova(get(BestGenesMods[r, "mods"]), type = "3")[["Df"]], digits = 3
        )),
        Chisq = c(NA, signif(
          car::Anova(get(BestGenesMods[r, "mods"]), type = "3")[["LR Chisq"]], digits = 3
        )),
        p.value = c(NA, signif(
          car::Anova(get(BestGenesMods[r, "mods"]), type = "3")[["Pr(>Chisq)"]], digits = 3
        ))
      )
      colnames(Values)[which(colnames(Values) == "z value")] <-
        "t or z value"
    } else{
      Values <- cbind(
        summary(get(BestGenesMods[r, "mods"]))$coefficient,
        df = car::Anova(get(BestGenesMods[r, "mods"]), type = "3")[["Df"]],
        Chisq = car::Anova(get(BestGenesMods[r, "mods"]), type = "3")[["Chisq"]],
        p.value = car::Anova(get(BestGenesMods[r, "mods"]), type = "3")[["Pr(>Chisq)"]]
      )
      colnames(Values)[which(colnames(Values) == "t value")] <-
        "t or z value"
      Values <- signif(Values, digits = 3)
    }
    Values <- cbind(rownames(Values), Values)
    rownames(Values) <- NULL
    Values <-
      cbind(rep(BestGenesMods[r, "modsId"], nrow(Values)), Values)
    Values <-
      cbind(rep(BestGenesMods[r, "modFunc"], nrow(Values)), Values)
    ValuesRes <- rbind(ValuesRes, Values)
  }
  # leave the two first column without names
  names(ValuesRes)[c(1, 2, 3)] <- c("", "", "")
  
  # specify the new predictor names
  PredictorsNames <-
    data.frame(
      toreplace = unique(ValuesRes[[3]]),
      replacement = c("Intercept", "Origin (LC)", "Sex (M)", "Imm. challenge (PBS)", "Size")
    )
  
  # rename the predictors
  for (pn in PredictorsNames[["toreplace"]]) {
    ValuesRes[[3]][which(ValuesRes[[3]] == pn)] <-
      PredictorsNames[["replacement"]][which(PredictorsNames[["toreplace"]] == pn)]
  }
  
  # replace p.value with large decimal by approximations
  ValuesRes[["p.value"]][which(ValuesRes[["p.value"]] < 0.0001)] <-
    "<0.0001"
  ValuesRes[["p.value"]][which(ValuesRes[["p.value"]] > 0.0001 &
                                 ValuesRes[["p.value"]] < 0.01)] <-
    "<0.01"
  ValuesRes[["p.value"]][which(ValuesRes[["p.value"]] > 0.01 &
                                 ValuesRes[["p.value"]] < 0.05)] <-
    "<0.05"
  options(scipen = 0)
  
  # create an empty list to interate over the newly created table
  l <- vector("list", nrow(BestGenesMods))
  
  # remove the column carrying response variable names
  ValuesResnoResp <- ValuesRes[-2]
  colnames(ValuesResnoResp)[2] <- ""
  l[[1]] <- kableExtra::kable_classic_2(
    kableExtra::collapse_rows(
      kableExtra::kbl(ValuesResnoResp, align = "c"),
      columns = 1,
      valign = "middle"
    ),
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = F,
    html_font = "arial",
    font_size = 10
  )
  
  for (i in seq(nrow(BestGenesMods))) {
    l[[i + 1]] <- kableExtra::pack_rows(l[[i]],
                                        if (is.na(BestGenesMods[i, "modsR2m"]) &
                                            is.na(BestGenesMods[i, "modsR2c"])) {
                                          paste(BestGenesMods[i, "modsId"], BestGenesMods[i, "modsn"], sep = " | ")
                                        } else{
                                          paste(BestGenesMods[i, "modsId"],
                                                BestGenesMods[i, "modsn"],
                                                BestGenesMods[i, "modsR2m"],
                                                BestGenesMods[i, "modsR2c"],
                                                sep = " | ")
                                        },
                                        min(which(ValuesRes[[2]] ==  BestGenesMods[i, "modsId"])),
                                        max(which(ValuesRes[[2]] ==  BestGenesMods[i, "modsId"])),
                                        label_row_css = "background-color: #fff; color: #000; border-bottom: 1px solid; border-top: 1px solid; font-style: italic;")
  }
  l[[length(l)]]
}
