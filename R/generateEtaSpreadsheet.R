generateEtaSpreadsheet <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           localWorkingDir,
           ReturnedFilesPattern) {
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <-
      getBatchDirectoryLocation(jobsDirectoryRoot)

    Eta.csv <- data.frame()
    EtaStacked.csv <- data.frame()
    missingFile <- TRUE
    for (job in jobList) {
      jobBaseIndx <- job %% 100
      IdEtaFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "IdEta.txt")
      shrinkFileFull <-
        sprintf(
          "%s/jobs/%02d/%d/%s",
          jobsBaseDirectory,
          jobBaseIndx,
          job,
          "EtaShrinkageBySubject.txt"
        )
      bluptableFileFull <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "bluptable.dat")

      rDumpFile <-
        sprintf("%s/jobs/%02d/%d/%s",
                jobsBaseDirectory,
                jobBaseIndx,
                job,
                "dmp.txt")

      if (file.exists(IdEtaFileFull) &&
          file.info(IdEtaFileFull)$size != 0) {
        currentEtaStacked.csv <-
          data.table::fread(IdEtaFileFull, header = FALSE)
        nIDCols <- 5
        IDCols <- paste0("id", (6 - nIDCols):5)
        colnames(currentEtaStacked.csv) <- c(IDCols, "Eta", "Value")
        IDColsCollapsed <- paste0(IDCols, collapse = "+")
        IdEtaFormula <-
          as.formula(paste0(IDColsCollapsed, " ~ Eta"))
        currentEtaStacked.csv$Eta <-
          factor(currentEtaStacked.csv$Eta,
                 levels = unique(currentEtaStacked.csv$Eta))
        cdata <-
          reshape::cast(currentEtaStacked.csv, IdEtaFormula, value = "Value")

        shrink_names <-
          sapply(colnames(cdata[(nIDCols + 1):ncol(cdata)]),
                 function(colName) {
                   paste0(colName, "_shrinkage")
                 })

        if (file.exists(shrinkFileFull) &&
            file.info(shrinkFileFull)$size != 0) {
          shrink_data <- data.table::fread(shrinkFileFull, header = FALSE)
          shrink_data[1] <- NULL
          colnames(shrink_data) <- shrink_names
          currentEta.csv <- cbind.data.frame(cdata, shrink_data)
        } else {
          # there are some examples where eta shrinkage file is not created
          # we do not create that columns then
          warning(
            "File ",
            shrinkFileFull,
            " was not created and eta shrinkage info is not added to Eta.csv"
          )
          currentEta.csv <- cdata
        }

        if (nrow(currentEtaStacked.csv) == 0)
          next

        dmp.txt <- .get_dmptxt(rDumpFile)

        if (grepl(ReturnedFilesPattern, "bluptable.dat")) {
          currentbluptable.dat <- .get_bluptable(bluptableFileFull,
                                                 colnames(dmp.txt$omega),
                                                 currentEta.csv)

          cols1Text <- .get_cols1Text(dirname(rDumpFile))

          currentEtaStacked.csv <-
            .rename_IDs(cols1Text, currentEtaStacked.csv)
          currentEta.csv <-
            .rename_IDs(cols1Text, currentEta.csv)
          currentbluptable.dat <-
            .rename_IDs(cols1Text, currentbluptable.dat)

        }


        SortScenarioDF <- .get_SortsScenarioDF(job)

        currentEta.csv <- cbind.data.frame(SortScenarioDF,
                                           currentEta.csv,
                                           row.names = NULL)

        currentEtaStacked.csv <- cbind.data.frame(SortScenarioDF,
                                                  currentEtaStacked.csv,
                                                  row.names = NULL)

        if (grepl(ReturnedFilesPattern, "bluptable.dat") &&
            nrow(currentbluptable.dat) != 0) {
          currentbluptable.dat <- cbind.data.frame(SortScenarioDF,
                                                   currentbluptable.dat,
                                                   row.names = NULL)
        } else {
          currentbluptable.dat <- data.frame()
        }

        if (missingFile == TRUE) {
          # reporting
          filesToGenerate <-
            c("Eta.csv", "EtaStacked.csv", "bluptable.dat")
          report_filesToGenerate(filesToGenerate, ReturnedFilesPattern)

          Eta.csv <- currentEta.csv
          EtaStacked.csv <- currentEtaStacked.csv

          bluptable.dat <- currentbluptable.dat

          missingFile <- FALSE
        } else {
          # see above: if eta shrinkage is not given
          if (ncol(Eta.csv) > ncol(currentEta.csv)) {
            Eta.csv <- Eta.csv[, 1:ncol(currentEta.csv)]
          } else if (ncol(Eta.csv) < ncol(currentEta.csv)) {
            currentEta.csv <- currentEta.csv[, 1:ncol(Eta.csv)]
          }

          Eta.csv <- rbind(Eta.csv, currentEta.csv)
          EtaStacked.csv <-
            rbind(EtaStacked.csv, currentEtaStacked.csv)
          bluptable.dat <- rbind(bluptable.dat, currentbluptable.dat)
        }
      }
    }

    if (!missingFile) {
      if (grepl(ReturnedFilesPattern, "Eta.csv") && nrow(Eta.csv) > 0) {
        write.csv(
          Eta.csv,
          file = file.path(localWorkingDir, "Eta.csv"),
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

      if (grepl(ReturnedFilesPattern, "EtaStacked.csv") && nrow(EtaStacked.csv) > 0) {
        write.csv(
          EtaStacked.csv,
          file = file.path(localWorkingDir, "EtaStacked.csv"),
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }

      if (grepl(ReturnedFilesPattern, "bluptable.dat")  && nrow(bluptable.dat) > 0) {
        write.csv(
          bluptable.dat,
          file = file.path(localWorkingDir, "bluptable.dat"),
          row.names = FALSE,
          quote = FALSE,
          na = ""
        )
      }
    }
  }

.get_dmptxt <- function(rDumpFile) {
  dmp.txtCached <- paste0("dmp.txt[[", rDumpFile, "]]")
  if (exists(dmp.txtCached, envir = nlmeEnv)) {
    dmp.txt <- get(dmp.txtCached, envir = nlmeEnv)
  } else {
    dmpLines <- shrinkDmpDotTxt(rDumpFile)
    source(textConnection(dmpLines), local = TRUE)
    assign(dmp.txtCached, dmp.txt, envir = nlmeEnv)
  }

  dmp.txt
}

.get_outtxt <- function(outFile) {
  out.txtCached <- paste0("out.txt[[", outFile, "]]")
  if (exists(out.txtCached, envir = nlmeEnv)) {
    out.txt <- get(out.txtCached, envir = nlmeEnv)
  } else {
    out.txt <- readLines(outFile, warn = FALSE)
    assign(out.txtCached, out.txt, envir = nlmeEnv)
  }

  out.txt
}
