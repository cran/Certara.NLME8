#
# Summarizes the results from estimation runs sorted by particular columns
# generates Overall.csv and StatusWindows.txt
#
summarizeSimpleEstimation <-
  function(localWorkingDir,
           jobList,
           control_lines,
           sorting = FALSE) {
    SortColumns <- .check_numSortColumns()
    num_sort_columns <- SortColumns$num_sort_columns
    unique_sorted_values <- SortColumns$unique_sorted_values
    sort_column_names <- SortColumns$sort_column_names

    if (!sorting) {
      workflow_name <- get("workflow_name", envir = nlmeEnv)
    }

    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <-
      getBatchDirectoryLocation(jobsDirectoryRoot)

    if (length(jobList) > 1) {
      GlobalSummaryLine2 <-
        sprintf("Summarizing sort column estimation %d runs",
                length(jobList))
      assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
      UpdateProgressMessages()
    }

    jobsDirectory <-
      file.path(jobsBaseDirectory,
                "jobs",
                sprintf("%02d", jobList %% 100),
                jobList)
    # Lets try to detect simulation jobs here and get out
    if (length(jobList) == 1 &&
        !file.exists(file.path(jobsDirectory[1], "nlme7engine.log")) &&
        !file.exists(file.path(jobsDirectory[1], "progress.txt"))) {
      simTablesExist <-
        list.files(
          jobsDirectory[1],
          pattern = "*.csv",
          all.files = TRUE,
          full.names = TRUE
        )

      GlobalSummaryLine2 <-
        sprintf("Copying tables %s", paste(basename(simTablesExist), collapse = ", "))
      assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
      UpdateProgressMessages()

      return(file.copy(from = simTablesExist,
                       to = localWorkingDir,
                       overwrite = TRUE))
    }

    OutFileNames <-
      file.path(jobsDirectory, getRunSuccessFilename(control_lines))
    progressfilenames <- file.path(jobsDirectory, "progress.txt")
    statusFiles <-
      file.path(jobsDirectoryRoot, sprintf("S_%03d.status", as.integer(jobList)))

    scenarioNames <- unique(getScenarioName(control_lines))

    generateStatusWindow(statusFiles, progressfilenames, scenarioNames)

    # patterns written for each scenario are the same for now,
    # so read the first one
    ReturnedFilesPatternVec <-
      unlist(strsplit(get_ReturnedFilesPattern(control_lines[1]), split = " +"))

    ReturnedFilesPattern <-
      paste0("(", ReturnedFilesPatternVec , ")", collapse = "|")
    assign("ReturnedFilesPattern", ReturnedFilesPattern, envir = nlmeEnv)

    if (any(grepl(ReturnedFilesPattern, "Overall.csv"))) {
      generateOverallSummary(OutFileNames, scenarioNames, sorting = sorting)
    }

    if (any(grepl(ReturnedFilesPattern, "EtaEta.csv"))) {
      generateEtaEtaTable(jobList,
                          unique_sorted_values,
                          scenarioNames,
                          localWorkingDir)
    }


    covariateNamesDefined <- FALSE

    filesInreadEtaCovTxtFile <-
      c("EtaCov.csv", "EtaCovariate.csv", "EtaCovariateCat.csv")
    if (any(grepl(ReturnedFilesPattern, filesInreadEtaCovTxtFile))) {
      covariateNames <-
        getCovariateNames(file.path(localWorkingDir, "test.mdl"))
      covariateNamesDefined <- TRUE

      readEtaCovTxtFile(
        jobList,
        unique_sorted_values,
        scenarioNames,
        covariateNames,
        localWorkingDir,
        ReturnedFilesPattern
      )
    }

    filesInreadStrCovTxtFile <-
      c("StrCovariate.csv", "StrCovariateCat.csv")
    if (any(grepl(ReturnedFilesPattern, filesInreadStrCovTxtFile))) {
      if (!covariateNamesDefined) {
        covariateNames <-
          getCovariateNames(file.path(localWorkingDir, "test.mdl"))
      }

      readStrCovTxtFile(
        jobList,
        unique_sorted_values,
        scenarioNames,
        covariateNames,
        localWorkingDir,
        ReturnedFilesPattern
      )
    }

    filesgenerateEtaSpreadsheet <-
      c("Eta.csv", "EtaStacked.csv", "bluptable.dat")
    if (any(grepl(ReturnedFilesPattern, filesgenerateEtaSpreadsheet))) {
      generateEtaSpreadsheet(
        jobList,
        unique_sorted_values,
        scenarioNames,
        localWorkingDir,
        ReturnedFilesPattern
      )
    }

    if (any(grepl(ReturnedFilesPattern, "Secondary.csv"))) {
      generateSecondary(jobList,
                        OutFileNames,
                        localWorkingDir)
    }

    if (any(grepl(ReturnedFilesPattern, "ConvergenceData.csv"))) {
      generateConvergenceFile(
        jobList,
        unique_sorted_values,
        progressfilenames,
        scenarioNames,
        localWorkingDir
      )
    }

    # iniest.csv is obligatory returned
    concatenateInitEstimates(jobList,
                             unique_sorted_values,
                             scenarioNames,
                             localWorkingDir)

    # doses.csv is obligatory returned
    generateDoseTable(jobList,
                      unique_sorted_values,
                      scenarioNames,
                      localWorkingDir)

    if (any(grepl(ReturnedFilesPattern, "ParDer.csv"))) {
      generateVIFParDerTables(
        jobList,
        unique_sorted_values,
        scenarioNames,
        localWorkingDir
      )
    }

    filesIngenerateOmegaEtas <-
      c("omega.csv", "omega_stderr.csv")
    if (any(grepl(ReturnedFilesPattern, filesIngenerateOmegaEtas))) {
      # do not differentiate omega.csv omega_stderr.csv in ReturnedFilesPattern
      generateOmegaEtas(jobList,
                        unique_sorted_values,
                        scenarioNames,
                        localWorkingDir)
    }

    filesIngenerateThetaCovCorFiles <-
      c("theta.csv",
        "thetaCorrelation.csv",
        "thetaCovariance.csv",
        "Covariance.csv")
    if (any(grepl(ReturnedFilesPattern, filesIngenerateThetaCovCorFiles))) {
      generateThetaCovCorFiles(
        jobList,
        unique_sorted_values,
        scenarioNames,
        localWorkingDir,
        OutFileNames,
        ReturnedFilesPattern
      )
    }

    if (any(grepl(ReturnedFilesPattern, "residuals.csv"))) {
      generateResiduals(
        jobList,
        unique_sorted_values,
        scenarioNames,
        jobsBaseDirectory,
        localWorkingDir
      )
    }

    posthocTableNames <-
      getTableNames(columnDefinitionFilename = NULL,
                    columnDefinitionText = .get_cols1Text(DirectoryToRead = jobsDirectory[1]),
                    simtbl = FALSE)

    if (posthocTableNames != "") {
      for (name in unlist(strsplit(gsub("^\\s+", "", posthocTableNames), " "))) {
        # the user requested tables, so we do not filter it
        # except posthoc.csv which is built-in
        if ((name == "posthoc.csv" &&
             any(grepl(ReturnedFilesPattern, c("posthoc.csv", "posthocStacked.csv")))) ||
            name != "posthoc.csv") {
          GlobalSummaryLine2 <-
            sprintf("Generating %s", name)
          assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
          UpdateProgressMessages()

          collateTables(
            jobList = jobList,
            scenarioNames = scenarioNames,
            inputName = name,
            outputName = file.path(localWorkingDir, name),
            ReturnedFilesPattern = ReturnedFilesPattern
          )
        }
      }
    }

    filesIngenerateNonparamSummary <-
      c(
        "nonParSupportResult.csv",
        "nonParStackedResult.csv",
        "nonParEtaResult.csv",
        "nonParOverallResult.csv"
      )
    if (any(grepl(ReturnedFilesPattern, filesIngenerateNonparamSummary))) {
      generateNonparamSummary(
        jobList,
        unique_sorted_values,
        scenarioNames,
        jobsBaseDirectory,
        localWorkingDir,
        ReturnedFilesPattern
      )
    }
  }
