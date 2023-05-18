generateOmegaEtas <-
  function(jobList,
           unique_sorted_values,
           scenarioNames,
           localWorkingDir) {
    num_sort_columns <- get("num_sort_columns", envir = nlmeEnv)
    sort_column_names <- get("sort_column_names", envir = nlmeEnv)
    jobsDirectoryRoot <- get("jobsDirectoryRoot", envir = nlmeEnv)
    jobsBaseDirectory <-
      getBatchDirectoryLocation(jobsDirectoryRoot)

    OmegaOutputFlag <- TRUE
    OmegaSderrOutputFlag <- TRUE
    OmegaSEDF <- data.frame()
    sc <- 1
    uIndex <- 1
    control_lines <- get("control_lines", envir = nlmeEnv)

    # get headerline
    if (num_sort_columns > 0) {
      for (sortColumnIndex in 1:num_sort_columns) {
        sortColumn <- sort_column_names[sortColumnIndex]
        if (sortColumnIndex == 1) {
          headerLine <- sortColumn
        } else {
          headerLine <- paste(headerLine,
                              sortColumn,
                              sep = ",",
                              collapse = ",")
        }
      }
    } else {
      headerLine <- character(0)
    }

    for (job in jobList) {
      rDumpFile <- figureOutDmpFileLocation(
        job,
        jobsBaseDirectory
      )

      if (file.exists(rDumpFile) &&
          file.info(rDumpFile)$size != 0) {
        dmp.txt <- .get_dmptxt(rDumpFile)

        randomEffectNames <- unlist(dimnames(dmp.txt$omega)[1])
        numRandomEffects <- length(randomEffectNames)
        if (numRandomEffects == 0) {
          next()
        }

        omegas <- dmp.txt$omega
        etas <- dmp.txt$eta

        SortScenarioPart <- character(0)
        if (num_sort_columns > 0) {
          for (sortColumnIndex in 1:num_sort_columns) {
            val <- unlist(unique_sorted_values[[sortColumnIndex]])[uIndex]
            if (is.na(val)) {
              val <- unlist(unique_sorted_values[[sortColumnIndex]])[uIndex + 1]
            }
            val <- sub("^\\s+", "", val)
            if (sortColumnIndex == 1) {
              SortScenarioPart <- val
            } else {
              SortScenarioPart <- paste(SortScenarioPart,
                                        val,
                                        sep = ",",
                                        collapse = ",")
            }
          }

          SortScenarioPart <-
            paste(SortScenarioPart,
                  scenarioNames[sc],
                  collapse = ",",
                  sep = ",")
        } else {
          if (nzchar(scenarioNames[sc])) {
            SortScenarioPart <-
              paste(scenarioNames[sc], collapse = ",", sep = ",")
          } else {
            SortScenarioPart <-
              paste(" ", collapse = ",", sep = ",")
          }

        }



        # omega stuff
        OmegaOutputFile <- file.path(localWorkingDir, "omega.csv")

        if (OmegaOutputFlag) {
          # first sort/scenario
          GlobalSummaryLine2 <- sprintf("Generating omega.csv")
          assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
          UpdateProgressMessages()
          headerLine <- paste(
            c(headerLine,
              "Scenario",
              "Label",
              randomEffectNames),
            collapse = ",",
            sep = ","
          )
          cat(headerLine, file = OmegaOutputFile, sep = "\n")
          OmegaOutputFlag <- FALSE
        }

        firstLine <- paste(SortScenarioPart,
                           "Omega",
                           collapse = ",",
                           sep = ",")
        cat(firstLine,
            file = OmegaOutputFile,
            sep = "\n",
            append = TRUE)

        SortScenario <-
          data.frame(t(
            scan(
              text = SortScenarioPart,
              sep = ",",
              quiet = TRUE,
              what = "character"
            )
          ))
        omegaToWrite <- cbind.data.frame(SortScenario,
                                         Label = randomEffectNames,
                                         omegas)

        write.table(
          omegaToWrite,
          file = OmegaOutputFile,
          append = TRUE,
          row.names = FALSE,
          sep = ",",
          dec = ".",
          qmethod = "double",
          col.names = FALSE,
          quote = FALSE
        )

        # correlation stuff
        firstLineCorrelation <-
          paste(SortScenarioPart,
                "Correlation",
                collapse = ",",
                sep = ",")
        cat(
          firstLineCorrelation,
          file = OmegaOutputFile,
          sep = "\n",
          append = TRUE
        )
        omegaCorToWrite <- cbind.data.frame(SortScenario,
                                            Label = randomEffectNames,
                                            cov2cor(omegas))

        write.table(
          omegaCorToWrite,
          file = OmegaOutputFile,
          append = TRUE,
          row.names = FALSE,
          sep = ",",
          dec = ".",
          qmethod = "double",
          col.names = FALSE,
          quote = FALSE
        )

        # eta shrinkage stuff
        ShrinkageLine <- paste(c(SortScenarioPart,
                                 "Shrinkage",
                                 etas[, "shrinkage"]),
                               collapse = ",")

        cat(ShrinkageLine,
            file = OmegaOutputFile,
            sep = "\n",
            append = TRUE)

        if (!is.null(dmp.txt[["omegaSE"]]) ||
            !is.null(dmp.txt[["Correlation"]])) {
          if (OmegaSderrOutputFlag) {
            # generate the message only if omega stderr really found
            GlobalSummaryLine2 <-
              sprintf("Generating omega_stderr.csv")
            assign("GlobalSummaryLine2", GlobalSummaryLine2, envir = nlmeEnv)
            UpdateProgressMessages()
            OmegaSderrOutputFlag <- FALSE
          }


          if (!is.null(dmp.txt[["omegaSE"]])) {
            # OmegaSE stuff
            OmegaSE <- dmp.txt[["omegaSE"]]
            OmegaSEDFCurrent <- cbind.data.frame(SortScenario,
                                                 Label = randomEffectNames,
                                                 OmegaSE)
          } else {
            # if(!is.null(dmp.txt[["Correlation"]]))
            # this part is for compatiblity only with old NLME executables
            FullCorr <- diag(dmp.txt[["Correlation"]])
            CorrelationFirstOmega <-
              paste0(randomEffectNames[1], "-", randomEffectNames[1])
            CorrelationFirstOmegaIndex <-
              which(CorrelationFirstOmega == names(FullCorr), arr.ind = TRUE)
            if (length(CorrelationFirstOmegaIndex) > 0) {
              OmegaCorr <-
                FullCorr[CorrelationFirstOmegaIndex:length(FullCorr)]
              OmegaSE <- matrix(
                nrow = numRandomEffects,
                ncol = numRandomEffects,
                dimnames = list(randomEffectNames, randomEffectNames)
              )
              # fill in the upper triangle and then transpose
              OmegaSE[upper.tri(OmegaSE, diag = TRUE)] <- OmegaCorr
              OmegaSE[lower.tri(OmegaSE)] <-
                t(OmegaSE)[lower.tri(OmegaSE)]
            } else {
              warning(
                "Cannot find omega name ",
                CorrelationFirstOmega,
                " in ",
                rDumpFile,
                " to build SE."
              )
              next()
            }
          }

          OmegaSEDFCurrent <- cbind.data.frame(SortScenario,
                                               Label = randomEffectNames,
                                               OmegaSE)
          if (nrow(OmegaSEDF) == 0) {
            OmegaSEDF <- OmegaSEDFCurrent
            # header is the same as for omega.csv
            colnames(OmegaSEDF) <-
              scan(
                text = headerLine,
                sep = ",",
                quiet = TRUE,
                what = "character"
              )
          } else {
            colnames(OmegaSEDFCurrent) <- colnames(OmegaSEDF)
            OmegaSEDF <-
              rbind.data.frame(OmegaSEDF, OmegaSEDFCurrent)
          }
        }
      }

      # increment scenario
      sc <- sc + 1
      if (sc > length(scenarioNames)) {
        sc <- 1
        uIndex <- uIndex + 1
      }
    }

    if (nrow(OmegaSEDF) != 0) {
      OmegaSEOutputFile <- file.path(localWorkingDir, "omega_stderr.csv")
      write.csv(OmegaSEDF, file = OmegaSEOutputFile, row.names = FALSE, quote = FALSE)
    }
  }
