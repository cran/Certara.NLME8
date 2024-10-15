generateSecondary <- function(jobList, OutFileNames, localWorkingDir) {
  secondaryTable <- data.frame()
  jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
  jobsBaseDirectory <-
    getBatchDirectoryLocation(jobsDirectoryRoot)

  filesAreMissing <- TRUE
  GlobalSummaryLine2Reported <- FALSE
  for (job in jobList) {
    rDumpFile <-
      figureOutDmpFileLocation(job,
                               jobsBaseDirectory)

    dmp.txt <- .get_dmptxt(rDumpFile)
    if (length(dmp.txt$coefficients$secondary) == 0) next
    filesAreMissing <- FALSE
    #---------------
    nobs <- dmp.txt$nObs
    nparam <- dmp.txt$nParm
    degOfFreedom <- as.numeric(nobs) - as.numeric(nparam)
    confidenceLevel <- 95
    if (degOfFreedom < 1) {
      xcilow <- NA
    } else {
      xcilow <- qt((100 - confidenceLevel) / 200,
                   degOfFreedom)
    }

    # if there's something to prepare in secondary - report it
    if (!GlobalSummaryLine2Reported) {
      GlobalSummaryLine2 <- sprintf("Generating Secondary.csv")
      assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
      UpdateProgressMessages()
      GlobalSummaryLine2Reported <- TRUE
    }

    secondary <- dmp.txt$coefficients$secondary
    stderrSecondary <- dmp.txt$stderrSecondary

    if (is.null(stderrSecondary)) {
      low <- high <- cv <- stderrSecondary <- rep("", length(secondary))
    } else {
      low <- secondary + xcilow * as.double(stderrSecondary)
      high <- secondary - xcilow * as.double(stderrSecondary)
      cv <- (100 * as.double(stderrSecondary)) / secondary
    }

    secondaryFromOut <- .parse_OutFileStructure(OutFileNames[job], ToCapture = "secondary")

    # they should have the same order
    secondaryUnits <-
      attr(secondaryFromOut, "units")

    secondaryVIF <-
      .parse_OutFileStructure(OutFileNames[job], ToCapture = "varSecondaryInf")

    secondariesOutput <-
      cbind.data.frame(
        Secondary = names(secondary),
        Estimate = secondary,
        Units = secondaryUnits,
        Stderr = stderrSecondary,
        `CV%` = cv,
        `2.5% CI` = low,
        `97.5% CI` = high,
        `Var.Inf.factor` = secondaryVIF
      )

    SortScenarioDF <- .get_SortsScenarioDF(job)
    SecondaryOutput <-
      cbind.data.frame(.get_SortsScenarioDF(job),
                       secondariesOutput,
                       row.names = NULL)

    secondaryTable <- rbind.data.frame(secondaryTable, SecondaryOutput)

  }

  if (filesAreMissing == FALSE) {
    write.csv(secondaryTable,
      file = file.path(localWorkingDir, "Secondary.csv"),
      row.names = FALSE, quote = FALSE
    )
  }
}
