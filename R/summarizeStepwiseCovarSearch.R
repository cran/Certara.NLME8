
# Summarizes the results from a stepwise covariate search run.
# generates Overall.csv and StatusWindows.txt
summarizeStepwiseCovarSearch <- function(localWorkingDir, scenarios) {
  OverallFilename <- file.path(localWorkingDir, "Overall.csv")

  GlobalSummaryLine1 <- sprintf("Summarizing stepwise covariate search results for %d scenarios", length(scenarios))
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
  UpdateProgressMessages()
  OverallDF <- data.frame()
  for (scenario in scenarios) {
    if (scenario$status == "Completed") {
      currentRow <- cbind.data.frame(
        Scenario = paste(scenario$scenarioName, scenario$scenarioDescription),
        RetCode = scenario$returnCode,
        LogLik = scenario$logLike,
        `-2LL` = scenario$logLikelihood,
        AIC = scenario$AIC,
        BIC = scenario$BIC,
        nParm = scenario$nParam,
        nObs = scenario$nObs,
        nSub = scenario$nSub,
        EpsShrinkage = scenario$shrinkage,
        Condition = scenario$Condition
      )
      OverallDF <- rbind.data.frame(OverallDF, currentRow)

    } else {

      currentRow <- cbind.data.frame(
        Scenario = paste(scenario$scenarioName, scenario$scenarioDescription),
        RetCode = NA,
        LogLik = NA,
        `-2LL` = NA,
        AIC = NA,
        BIC = NA,
        nParm = NA,
        nObs = NA,
        nSub = NA,
        EpsShrinkage = NA,
        Condition = NA
      )
      OverallDF <- rbind.data.frame(OverallDF, currentRow)
    }
  }

  write.csv(OverallDF, file = OverallFilename, na = "", row.names = FALSE)
  assign("OverallDF", OverallDF, envir = nlmeEnv)

  filenames <- c()
  progressfilenames <- c()
  scenarioNames <- c()
  for (s in scenarios) {
    if (scenario$status == "Completed") {
      filename <- file.path(localWorkingDir, paste0("status.txt.", s$index))
      filenames <- c(filenames, filename)
      fileToRead <- file.path(localWorkingDir, paste0("progress.txt.", s$index))
      progressfilenames <- c(progressfilenames, fileToRead)
      scenarioNames <- c(scenarioNames, s$scenarioName)
    }
  }

  generateStatusWindow(filenames, progressfilenames, scenarioNames)

  GlobalSummaryLine1 <- "\nFinished summarizing results. Transferring data and loading the results...\n"
  assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
  UpdateProgressMessages()
}
