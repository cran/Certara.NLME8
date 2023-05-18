


generateEtaEtaTable <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           localWorkingDir) {
    generateGenericTable(
      jobList,
      unique_sorted_values,
      scenarioNames,
      "EtaEta.txt",
      file.path(localWorkingDir, "EtaEta.csv"),
      c(
        "id1",
        "id2",
        "id3",
        "id4",
        "id5",
        "Eta1Name",
        "Eta2Name",
        "Eta1",
        "Eta2",
        "NPEta1",
        "NPEta2"
      ),
      "etameansnp.asc"
    )
  }


generateDoseTable <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           localWorkingDir) {
    generateGenericTable(
      jobList,
      unique_sorted_values,
      scenarioNames,
      "doses.csv",
      file.path(localWorkingDir, "doses.csv"),
      c(
        "id1",
        "id2",
        "id3",
        "id4",
        "id5",
        "time",
        "dosepoint",
        "amt",
        "rate",
        "path",
        "strip"
      )
    )
  }

generateVIFParDerTables <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           localWorkingDir) {
      generateGenericTable(
        jobList,
        unique_sorted_values,
        scenarioNames,
        "ParDer.csv",
        file.path(localWorkingDir, "ParDer.csv"),
        c(
          "id1",
          "id2",
          "id3",
          "id4",
          "id5",
          "WhichReset",
          "Time",
          "IPred",
          "Deriv",
          "PredName",
          "ObsName",
          "Parameter"
        )
      )
  }

generateGenericTable <- function(jobList,
                                 unique_sorted_values,
                                 scenarioNames,
                                 inputPrefix,
                                 outputName,
                                 colNames,
                                 optionalMergeFile = "") {
  jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
  jobsBaseDirectory <- getBatchDirectoryLocation(jobsDirectoryRoot)
  first <- TRUE
  for (job in jobList) {
    rDumpFile <- "dmp"
    jobBaseIndx <- job %% 100
    fileToRead <-
      sprintf("%s/jobs/%02d/%d/%s",
              jobsBaseDirectory,
              jobBaseIndx,
              job,
              inputPrefix)
    if (optionalMergeFile != "") {
      optionalFileToRead <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                optionalMergeFile)
      rDumpFile <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                rDumpFile)
    } else {
      optionalFileToRead <- ""
    }

    if (file.exists(fileToRead) &&
        file.info(fileToRead)$size != 0) {
      dataf <- data.table::fread(fileToRead, header = FALSE)
      if (all(as.character(dataf[1, 1:5]) == c("id1", "id2", "id3", "id4", "id5"))) {
        # doses data frame has header
        dataf <- dataf[-1, ]
      }
      colnames(dataf) <- colNames[1:ncol(dataf)]

      if (optionalMergeFile != "" &&
          file.exists(optionalFileToRead) &&
          file.info(optionalFileToRead)$size != 0) {
        rDumpFile <-
          file.path(dirname(outputName), basename(paste0(rDumpFile, ".txt")))
        dmp.txt <- .get_dmptxt(rDumpFile)

        randomEffectNames <- unlist(dimnames(dmp.txt$omega)[1])

        optionalDataf <-
          data.table::fread(optionalFileToRead, header = FALSE)
        colnames(optionalDataf) <- randomEffectNames
        # bind with ids from the main data file
        IDsDF <- dataf[!duplicated(dataf[, 1:5]), 1:5]
        optionalDataf <- cbind.data.frame(IDsDF, optionalDataf)
        optionalDatafMelted <-
          reshape::melt(optionalDataf, id = colNames[1:5])
        colnames(optionalDatafMelted)[6:7] <-
          c("Eta1Name", "NPEta1")
        # first merge to get NPEta1
        dataf <-
          merge(dataf,
                optionalDatafMelted,
                by = c(colNames[1:5], "Eta1Name"))
        colnames(optionalDatafMelted)[6:7] <-
          c("Eta2Name", "NPEta2")
        # second merge to get NPEta2
        dataf <-
          merge(dataf,
                optionalDatafMelted,
                by = c(colNames[1:5], "Eta2Name"))
      }

      if (first == TRUE) {
        cols1Path <- file.path(dirname(fileToRead), "cols1.txt")
        cols1Text <- readLines(cols1Path)
      }

      dataf <- .rename_IDs(cols1Text, dataf)

      if (nrow(dataf) > 0) {
        # update progress here only if there's something to do
        if (first == TRUE) {
          GlobalSummaryLine2 <- paste("Generating", basename(outputName))
          assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
          UpdateProgressMessages()
        }

        SortScenarioDF <- .get_SortsScenarioDF(job)

        dataf <-
          cbind.data.frame(SortScenarioDF, dataf, row.names = NULL)
      } else {
        num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
        sort_column_names <-
          get("sort_column_names", envir = nlmeEnv)
        internalColNames <- colnames(dataf)
        dataf <-
          data.frame(matrix(nrow = 0, ncol = num_sort_columns + 1 + ncol(dataf)))
        newColNames <-
          c(sort_column_names, "Scenario", internalColNames)
        if (ncol(dataf) == length(newColNames)) {
          colnames(dataf) <-
            c(sort_column_names, "Scenario", internalColNames)
        }
      }

      if (first == TRUE) {
        resultsDataframe <- dataf
        first <- FALSE
      } else {
        resultsDataframe <- rbind(resultsDataframe, dataf)
      }
    }
  }

  if (first == FALSE) {
    write.csv(
      resultsDataframe,
      file = outputName,
      row.names = FALSE,
      quote = FALSE,
      na = ""
    )
  }
}
