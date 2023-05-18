

# read StrCov.txt file and generates :
#
#  StrCovariateCat.csv
#  StrCovariate.csv
readStrCovTxtFile <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           covariateNames,
           localWorkingDir,
           ReturnedFilesPattern) {
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)

    # Figure out categorical and noncategorical covariate names
    catnames <- names(covariateNames)[covariateNames]
    noncatnames <- setdiff(names(covariateNames), catnames)

    strCovariateTable <- data.frame()
    strCovariateCatTable <- data.frame()
    filesAreMissing <- TRUE
    GlobalSummaryLine2Reported <- FALSE
    for (job in jobList) {
      jobBaseIndx <- job %% 100
      StrCovFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "StrCov.txt")
      rDumpFile <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "dmp.txt")

      if (file.exists(StrCovFileFull) &&
          file.info(StrCovFileFull)$size != 0) {
        # there's something to do - report it
        if (!GlobalSummaryLine2Reported) {
          GlobalSummaryLine2 <- sprintf("Generating StrCovariate.csv")
          assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
          UpdateProgressMessages()
          GlobalSummaryLine2Reported <- TRUE
          filesAreMissing <- FALSE
        }

        StrCov <- data.table::fread(StrCovFileFull, header = FALSE)
        if (nrow(StrCov) == 0)
          next
        IDs <- paste0("id", 1:5)
        colnames(StrCov) <-
          c(IDs, "StrName", "CovrName", "Str", "Covr")
        data.table::setcolorder(StrCov, c(IDs, "CovrName", "StrName", "Covr", "Str"))
        dmp.txt <- .get_dmptxt(rDumpFile)

        StrCov <- .rename_IDs(dmp.txt$cols1.txt, StrCov)

        # Add scenario name
        SortScenarioDF <- .get_SortsScenarioDF(job)
        StrCov <-
          cbind.data.frame(SortScenarioDF, StrCov, row.names = NULL)

        # splitting
        StrCovCat <- StrCov[StrCov$CovrName %in% catnames,]
        StrCov <- StrCov[StrCov$CovrName %in% noncatnames,]

        strCovariateTable <-
          rbind.data.frame(strCovariateTable, StrCov)
        strCovariateCatTable <-
          rbind.data.frame(strCovariateCatTable, StrCovCat)
      }
    }

    if (filesAreMissing == FALSE) {
      if (grepl(ReturnedFilesPattern, "StrCovariate.csv")) {
        write.csv(
          strCovariateTable,
          file = file.path(localWorkingDir, "StrCovariate.csv"),
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

      if (grepl(ReturnedFilesPattern, "StrCovariateCat.csv")) {
        write.csv(
          strCovariateCatTable,
          file = file.path(localWorkingDir, "StrCovariateCat.csv"),
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }
    }
  }
