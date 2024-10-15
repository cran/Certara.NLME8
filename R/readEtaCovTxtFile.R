

# read EtaCov.txt file and generates :
#
# "EtaCov.csv", "EtaCovariate.csv", "EtaCovariateCat.csv"
readEtaCovTxtFile <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           covariateNames,
           localWorkingDir,
           ReturnedFilesPattern) {
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)

    catnames <- names(covariateNames)[covariateNames]
    noncatnames <- setdiff(names(covariateNames), catnames)

    etaCovTable <- data.frame()
    etaCovariateTable <- data.frame()
    etaCovariateCatTable <- data.frame()
    filesAreMissing <- TRUE
    GlobalSummaryLine2Reported <- FALSE
    for (job in jobList) {
      jobBaseIndx <- job %% 100
      EtaCovFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "EtaCov.txt")
      etameansnpFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "etameansnp.asc")
      rDumpFile <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "dmp.txt")

      if (file.exists(EtaCovFileFull) &&
          file.info(EtaCovFileFull)$size != 0) {
        # there's something to do - report it
        if (!GlobalSummaryLine2Reported) {
          filesToGenerate <- c("EtaCov.csv", "EtaCovariate.csv", "EtaCovariateCat.csv")
          report_filesToGenerate(filesToGenerate, ReturnedFilesPattern)
          GlobalSummaryLine2Reported <- TRUE
        }

        EtaCovariate <-
          data.table::fread(EtaCovFileFull, header = FALSE)
        if (nrow(EtaCovariate) == 0)
          next

        dmp.txt <- .get_dmptxt(rDumpFile)

        # preparing EtaCov
        IDs <- paste0("id", 1:5)
        colnames(EtaCovariate) <-
          c(IDs, "EtaName", "CovrName", "Eta", "Covr")
        data.table::setcolorder(EtaCovariate,
                                c(IDs, "CovrName", "EtaName", "Covr", "Eta"))
        # preserve the order of Etas
        EtaCovariate$EtaName <-
          factor(EtaCovariate$EtaName, levels = unique(EtaCovariate$EtaName))
        # casting covariates
        IDColsCollapsed <- paste0(IDs, collapse = "+")
        IdEtaVSCovFormula <-
          as.formula(paste0(IDColsCollapsed, " + EtaName + Eta ~ CovrName"))
        EtaCovrCasted <-
          data.table::dcast.data.table(EtaCovariate, IdEtaVSCovFormula, value.var = "Covr")
        # casting etas
        IdCovVSEtaFormula <-
          as.formula(paste0(
            IDColsCollapsed,
            " + ",
            paste0(unique(EtaCovariate$CovrName), collapse = "+"),
            " ~ EtaName"
          ))
        EtaCov <-
          data.table::dcast.data.table(EtaCovrCasted, IdCovVSEtaFormula, value.var = "Eta")

        # adding npEta to EtaCovariate if present
        if (file.exists(etameansnpFileFull) &&
            file.info(etameansnpFileFull)$size != 0) {
          npData <- data.table::fread(etameansnpFileFull, header = FALSE)
          randomEffectNames <- unlist(dimnames(dmp.txt$omega)[1])
          colnames(npData) <- randomEffectNames
          npData <- cbind(EtaCov[, 1:5], npData)
          npDataMolten <-
            data.table::melt.data.table(
              npData,
              id.vars = IDs,
              variable.name = "EtaName",
              value.name = "NPEta"
            )
          EtaCovariate <-
            data.table::merge.data.table(EtaCovariate, npDataMolten, by = c(IDs, "EtaName"))
        } else {
          EtaCovariate$NPEta <- NA
        }

        cols1Text <- .get_cols1Text(dirname(EtaCovFileFull))
        EtaCovariate <- .rename_IDs(cols1Text, EtaCovariate)
        EtaCov <- .rename_IDs(cols1Text, EtaCov)

        # Add the scenario name
        SortScenarioDF <- .get_SortsScenarioDF(job)

        EtaCov <-
          cbind.data.frame(SortScenarioDF, EtaCov, row.names = NULL)
        EtaCovariate <-
          cbind.data.frame(SortScenarioDF, EtaCovariate, row.names = NULL)

        # splitting EtaCovariate and EtaCovariateCat
        EtaCovariateCat <-
          EtaCovariate[EtaCovariate$CovrName %in% catnames,]
        EtaCovariate <-
          EtaCovariate[EtaCovariate$CovrName %in% noncatnames,]

        etaCovTable <- rbind.data.frame(etaCovTable, EtaCov)
        etaCovariateTable <-
          rbind.data.frame(etaCovariateTable, EtaCovariate)
        etaCovariateCatTable <-
          rbind.data.frame(etaCovariateCatTable, EtaCovariateCat)
        filesAreMissing <- FALSE
      }
    }

    if (grepl(ReturnedFilesPattern, "EtaCov.csv")) {
      write.csv(
        etaCovTable,
        file = file.path(localWorkingDir, "EtaCov.csv"),
        row.names = FALSE,
        quote = FALSE,
        na = ""
      )
    }

    if (grepl(ReturnedFilesPattern, "EtaCovariate.csv")) {
      write.csv(
        etaCovariateTable,
        file = file.path(localWorkingDir, "EtaCovariate.csv"),
        row.names = FALSE,
        quote = FALSE,
        na = ""
      )
    }

    if (grepl(ReturnedFilesPattern, "EtaCovariateCat.csv")) {
      write.csv(
        etaCovariateCatTable,
        file = file.path(localWorkingDir, "EtaCovariateCat.csv"),
        row.names = FALSE,
        quote = FALSE,
        na = ""
      )
    }
  }
