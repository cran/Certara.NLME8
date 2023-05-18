generateThetaCovCorFiles <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           localWorkingDir,
           OutFileNames,
           ReturnedFilesPattern) {
    filesToGenerate <-
      c("theta.csv",
        "thetaCorrelation.csv",
        "thetaCovariance.csv",
        "Covariance.csv")
    report_filesToGenerate(filesToGenerate, ReturnedFilesPattern)

    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <-
      getBatchDirectoryLocation(jobsDirectoryRoot)
    if (exists("fixefUnits", envir = nlmeEnv)) {
      fixefUnits <- get("fixefUnits", envir = nlmeEnv)
    } else {
      fixefUnits <- ""
    }

    ThetaOutputFile <- file.path(localWorkingDir, "theta.csv")
    ThetaCorrelationOutputFile <-
      file.path(localWorkingDir, "thetaCorrelation.csv")
    ThetaCovarianceOutputFile <-
      file.path(localWorkingDir, "thetaCovariance.csv")

    # if hash is given, it is likely Phoenix remote run where thetaVarCovar.csv is expected
    if (Sys.getenv("NLME_HASH") == "") {
      CovarianceOutputFile <- file.path(localWorkingDir, "Covariance.csv")
    } else {
      CovarianceOutputFile <-
        file.path(localWorkingDir, "thetaVarCovar.csv")
    }

    thetaFull <- data.frame()
    thetaCorrelationFull <- data.frame()
    thetaCovarianceFull <- data.frame()
    CovarianceFull <- data.frame()

    for (job in jobList) {
      rDumpFile <-
        figureOutDmpFileLocation(job,
                                 jobsBaseDirectory)

      dmp.txt <- .get_dmptxt(rDumpFile)
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

      thetas <- dmp.txt$coefficients$fixed
      fixedEffectNames <- names(thetas)
      numFixedEffects <- length(fixedEffectNames)
      varFix <- dmp.txt$varFix

      # out file is required for VIF
      outFile <- OutFileNames[job]
      # figure out VIF if presented
      VIFs <- .get_VIFs(outFile)
      if (!is.null(names(fixefUnits))) {
        matchedFixefs <- match(fixefUnits, fixedEffectNames)
        # units are in names
        names(fixedEffectNames)[matchedFixefs] <-
          names(fixefUnits)[matchedFixefs]
        names(fixedEffectNames)[is.na(names(fixedEffectNames))] <-
          ""
      } else {
        names(fixedEffectNames) <- rep("", numFixedEffects)
      }

      VIFsGiven <- .get_VIFs(outFile)
      if (!is.null(names(VIFsGiven))) {
        matchedFixefs <- match(fixedEffectNames, names(VIFsGiven))
        VIFs <- VIFsGiven[matchedFixefs]
        names(VIFs) <- fixedEffectNames
      } else {
        VIFs <- rep(NA, numFixedEffects)
      }

      # Figure out Stderr, CV%, CI%
      if (length(varFix) > 0) {
        CurrentVariance <- diag(varFix)
        SDs <- sqrt(CurrentVariance)
        CVs <- (100 * as.double(SDs)) / thetas
        LowerCIs <- thetas + xcilow * as.double(SDs)
        HigherCIs <- thetas - xcilow * as.double(SDs)
      } else {
        SDs <-
          CVs <-
          LowerCIs <- HigherCIs <- rep(NA, length(fixedEffectNames))
      }

      LowerCIsName <-
        paste0(round((100 - confidenceLevel) / 2, 1), "%CI")
      UpperCIsName <-
        paste0(round((100 + confidenceLevel) / 2, 1), "%CI")
      thetaOutput <-
        cbind.data.frame(
          fixedEffectNames,
          thetas,
          names(fixedEffectNames),
          SDs,
          CVs,
          LowerCIs,
          HigherCIs,
          VIFs
        )

      names(thetaOutput) <- c(
        "Parameter",
        "Estimate",
        "Units",
        "Stderr",
        "CV%",
        LowerCIsName,
        UpperCIsName,
        "Var.Inf.factor"
      )

      SortScenarioDF <- .get_SortsScenarioDF(job)

      thetaOutput <-
        cbind.data.frame(SortScenarioDF,
                         thetaOutput,
                         row.names = NULL)

      thetaFull <- rbind.data.frame(thetaFull, thetaOutput)

      if (length(varFix) == 0)
        next

      # if SE was computed
      suppressWarnings(thetaCorrelationOutput <- cov2cor(varFix))
      thetaCorrelationOutput[upper.tri(thetaCorrelationOutput)] <-
        NA

      thetaCovarianceOutput <- varFix
      thetaCovarianceOutput[upper.tri(thetaCovarianceOutput)] <- NA

      CovarianceOutput <- dmp.txt$Covariance
      CovarianceOutput[upper.tri(CovarianceOutput)] <- NA

      thetaCorrelationOutput <-
        cbind.data.frame(SortScenarioDF, thetaCorrelationOutput, row.names = NULL)

      thetaCovarianceOutput <-
        cbind.data.frame(SortScenarioDF, thetaCovarianceOutput, row.names = NULL)

      CovarianceOutput <-
        cbind.data.frame(
          SortScenarioDF,
          'Var Name' = colnames(dmp.txt$Covariance),
          CovarianceOutput,
          row.names = NULL
        )

      thetaCorrelationFull <-
        rbind.data.frame(thetaCorrelationFull, thetaCorrelationOutput)
      thetaCovarianceFull <-
        rbind.data.frame(thetaCovarianceFull, thetaCovarianceOutput)
      CovarianceFull <-
        rbind.data.frame(CovarianceFull, CovarianceOutput)
    }

    if (nrow(thetaFull) > 0 &&
        grepl(ReturnedFilesPattern, "theta.csv")) {
      write.csv(
        thetaFull,
        ThetaOutputFile,
        row.names = FALSE,
        na = "",
        quote = FALSE
      )
    }

    if (nrow(thetaCorrelationFull) > 0 &&
        grepl(ReturnedFilesPattern, "thetaCorrelation.csv")) {
      write.csv(
        thetaCorrelationFull,
        ThetaCorrelationOutputFile,
        row.names = FALSE,
        na = "",
        quote = FALSE
      )
    }

    if (nrow(thetaCovarianceFull) > 0 &&
        grepl(ReturnedFilesPattern, "thetaCovariance.csv")) {
      write.csv(
        thetaCovarianceFull,
        ThetaCovarianceOutputFile,
        row.names = FALSE,
        na = "",
        quote = FALSE
      )
    }

    if (nrow(CovarianceFull) > 0 &&
        grepl(ReturnedFilesPattern, "Covariance.csv")) {
      write.csv(
        CovarianceFull,
        CovarianceOutputFile,
        row.names = FALSE,
        na = "",
        quote = FALSE
      )
    }

  }
