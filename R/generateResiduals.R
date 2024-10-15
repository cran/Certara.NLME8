# no resid2
generateResiduals <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           jobsBaseDirectory,
           localWorkingDir) {
    outputName <- file.path(localWorkingDir, "residuals.csv")
    num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
    sort_column_names <- get("sort_column_names", envir = nlmeEnv)
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    resultsDataframe <- NULL
    first <- TRUE
    missingFile <- TRUE
    control_lines <- get("control_lines", envir = nlmeEnv)
    for (job in jobList) {
      rDumpFile <-
        figureOutDmpFileLocation(
          job,
          jobsBaseDirectory
        )
      if (!file.exists(rDumpFile) ||
          file.info(rDumpFile)$size == 0)
        next

      jobBaseIndx <- job %% 100
      fName <- getRunSuccessFilename(control_lines[job])
      fileToRead <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                fName)
      newFileToRead <-
        reformatResidualsFile(fileToRead, localWorkingDir)
      dataf <- read.table(newFileToRead, header = TRUE)
      if (nrow(dataf) == 0)
        next

      # substituting values without meaning
      dataf$PREDSE[dataf$PREDSE %in% c(0, 3.3e-151)] <- NA
      # adding VIF if available
      dataf <- .get_VIFobs(fileToRead, dataf)

      cols1Text <- .get_cols1Text(dirname(fileToRead))
      dataf <- .rename_IDs(cols1Text, dataf)

      # Add the scenario name
      dataf <-
        cbind("Scenario" = scenarioNames[(job - 1) %% length(scenarioNames) + 1], dataf)
      if (num_sort_columns > 0) {
        scenariosIteration <- (job - 1) %/% length(scenarioNames) + 1
        SortsDF <-
          unique_sorted_values[scenariosIteration, , drop = FALSE]
        dataf <- cbind(SortsDF, dataf, row.names = NULL)
      }

      if (first == TRUE) {
        assign("GlobalSummaryLine2",
               "Generating Residuals.csv",
               envir = nlmeEnv)
        UpdateProgressMessages()

        resultsDataframe <- dataf
        first <- FALSE
        missingFile <- FALSE
      } else {
        resultsDataframe <- rbind(resultsDataframe, dataf)
      }
    }


    if (missingFile == FALSE) {
      # swap obsname and TadSeq for PHX compatibility
      ObsNamePos <- which(colnames(resultsDataframe) == "ObsName")
      TadSeqColumnNamePos <- which(colnames(resultsDataframe) == "WhichDose")
      resultsDataframe <-
        resultsDataframe[c(
          seq(1:(ObsNamePos - 1)),
          TadSeqColumnNamePos,
          ObsNamePos,
          (TadSeqColumnNamePos + 1):ncol(resultsDataframe)
        )]

      write.csv(
        resultsDataframe,
        file = outputName,
        row.names = FALSE,
        quote = FALSE,
        na = ""
      )
    }
  }
