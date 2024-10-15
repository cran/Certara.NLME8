generateNonparamSummary <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           jobsBaseDirectory,
           localWorkingDir,
           ReturnedFilesPattern) {
    nonParSupportResultFile <-
      file.path(localWorkingDir, "nonParSupportResult.csv")
    nonParStackedResultFile <-
      file.path(localWorkingDir, "nonParStackedResult.csv")
    nonParEtaResultFile <-
      file.path(localWorkingDir, "nonParEtaResult.csv")
    nonParOverallResultFile <-
      file.path(localWorkingDir, "nonParOverallResult.csv")
    nparsupportFile <- "nparsupport.asc"
    etameansnpFile <- "etameansnp.asc"

    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    resultsDataframe <- NULL
    resultsEtaDataframe <- NULL
    resultsStackedDataframe <- NULL
    resultsOverallDataframe <- NULL
    first <- TRUE
    missingFile <- TRUE

    for (job in jobList) {
      jobBaseIndx <- job %% 100
      nparsupportFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                nparsupportFile)

      etameansnpFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                etameansnpFile)

      IdEtaFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "IdEta.txt")

      if (!file.exists(nparsupportFileFull) ||
          file.info(nparsupportFileFull)$size == 0) {
        next
      }

      # used for ID data frame
      if (!file.exists(IdEtaFileFull) &&
          file.info(IdEtaFileFull)$size != 0) {
        next
      }

      nparsupport <-
        data.table::fread(nparsupportFileFull, data.table = FALSE)
      etameansnp <- data.table::fread(etameansnpFileFull)

      if (nrow(nparsupport) == 0) {
        next
      }

      if (first) {
        # reporting
        GlobalSummaryLine2 <-
          sprintf("Generating NonParam support summaries")
        assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
        UpdateProgressMessages()
      }

      currentEtaStacked.csv <-
        data.table::fread(IdEtaFileFull, header = FALSE)
      nIDCols <- 5
      IDCols <- paste0("id", (6 - nIDCols):5)
      idsDF <- unique(currentEtaStacked.csv[, 1:5])
      colnames(idsDF) <- IDCols

      randomEffectNames <- unique(currentEtaStacked.csv[, 6])
      colnames(etameansnp) <- randomEffectNames
      supportColnames <- c("p", randomEffectNames)
      colnames(nparsupport) <- supportColnames

      etameansnp <- cbind(idsDF, etameansnp)
      nEffects <- length(randomEffectNames)

      means <- c()
      for (ndx in 1:nEffects) {
        means <- c(means, sum(nparsupport[, 1] * nparsupport[, ndx + 1]))
      }

      covar <- as.list(rep(0, nEffects * nEffects))

      for (row in 1:nrow(nparsupport)) {
        prob <- nparsupport[row, 1]
        for (j in 1:nEffects) {
          mean1 <- means[j]
          val1 <- nparsupport[row, (j + 1)]
          vals2 <- nparsupport[row, ((1:nEffects) + 1)]
          dcov <- (val1 - mean1) * (vals2 - means[1:nEffects])
          covar[((j - 1) * nEffects + (1:nEffects))] <-
            covar[((j - 1) * nEffects + (1:nEffects))] + dcov * prob
        }
      }

      overallDf <-
        data.frame(type = c("Mean", rep("omega", nEffects)))
      for (i in 1:nEffects) {
        coln <- c(means[i], covar[((1:nEffects) - 1) * nEffects + i])
        overallDf[randomEffectNames[i]] <- unlist(coln)
      }

      nparsupportOrdered <- data.table::as.data.table(nparsupport)
      NonParStacked <- data.frame()
      for (ndx in 1:nEffects) {
        data.table::setorderv(nparsupportOrdered, colnames(nparsupportOrdered)[ndx + 1], 1)
        etaValues <- nparsupportOrdered[, ndx + 1]
        prob <- nparsupportOrdered[, 1]
        stacked <-
          cbind.data.frame(
            "eta" = randomEffectNames[ndx],
            "value" = etaValues,
            "P" = nparsupportOrdered[1],
            "CumP" = cumsum(prob)
          )
        NonParStacked <- rbind.data.frame(NonParStacked, stacked)
      }

      cols1Text <- .get_cols1Text(dirname(IdEtaFileFull))
      etameansnp <- .rename_IDs(cols1Text, etameansnp)

      # Add the scenario name
      SortScenarioDF <- .get_SortsScenarioDF(job)

      NonParStacked <-
        cbind.data.frame(SortScenarioDF, NonParStacked)
      nparsupport <-
        cbind.data.frame(SortScenarioDF, nparsupport)
      etameansnp <-
        cbind.data.frame(SortScenarioDF, etameansnp)
      overallDf <-
        cbind.data.frame(SortScenarioDF, overallDf)

      if (first == TRUE) {
        resultsDataframe <- nparsupport
        resultsStackedDataframe <- NonParStacked
        resultsEtaDataframe <- etameansnp
        resultsOverallDataframe <- overallDf
        first <- FALSE
        missingFile <- FALSE
      } else {
        resultsDataframe <- rbind(resultsDataframe, nparsupport)
        resultsStackedDataframe <-
          rbind(resultsStackedDataframe, NonParStacked)
        resultsEtaDataframe <-
          rbind(resultsEtaDataframe, etameansnp)
        resultsOverallDataframe <-
          rbind(resultsOverallDataframe, overallDf)
      }

      if (missingFile) next

      if (grepl(ReturnedFilesPattern, "nonParSupportResult.csv")) {
        write.csv(
          resultsDataframe,
          file = nonParSupportResultFile,
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

      if (grepl(ReturnedFilesPattern, "nonParStackedResult.csv")) {
        write.csv(
          resultsStackedDataframe,
          file = nonParStackedResultFile,
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

      if (grepl(ReturnedFilesPattern, "nonParEtaResult.csv")) {
        write.csv(
          resultsEtaDataframe,
          file = nonParEtaResultFile,
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

      if (grepl(ReturnedFilesPattern, "nonParOverallResult.csv")) {
        write.csv(
          resultsOverallDataframe,
          file = nonParOverallResultFile,
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

    }

    return(TRUE)
  }
