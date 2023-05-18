.isnew_SummaryLine <- function(SummaryLine, DetailInfoLine) {
  if (all(is.null(c(SummaryLine, DetailInfoLine)))) {
    return(FALSE)
  } else if (!is.character(SummaryLine)) {
    return(FALSE)
  } else if (SummaryLine == "") {
    return(FALSE)
  } else if (is.null(DetailInfoLine)) {
    return(TRUE)
  } else if (is.na(DetailInfoLine)) {
    return(TRUE)
  } else if (is.character(DetailInfoLine) &
    identical(SummaryLine, DetailInfoLine)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

.output_line <- function(length = 70) {
  reportProgress <- get("reportProgress", envir = nlmeEnv)
  if (reportProgress) {
    cat("\n", rep("-", length), "\n", sep = "")
  }
}

.output_StartedJobName <-
  function(jobType,
           num_samples,
           samplesProcessed,
           workflow_name) {
    printSomething <- TRUE
    if (jobType == "BOOTSTRAP" &&
      num_samples > samplesProcessed) {
      # for bootstrap report that new replicate is started
      # only if there's something to do
      .output_line(60)
      .output_progress(paste0("Processing replicate ",
                              samplesProcessed + 1,
                              " of ",
                              num_samples,
                              "\n"),
                        output_newline = FALSE
      )
    } else if (jobType %in% c("COVAR_SEARCH", "STEPWISE_SEARCH") &&
      workflow_name != "WorkFlow") {
      .output_line(60)
      .output_progress(paste0("Processing scenario: ", workflow_name, ":\n"),
                       output_newline = FALSE)
    } else if (workflow_name != "WorkFlow" &&
      jobType != "BOOTSTRAP") {
      .output_line(60)
      .output_progress(paste0("Progress for ", workflow_name, ":\n"),
                       output_newline = FALSE)
    } else {
      .output_line(0)
      printSomething <- FALSE
    }

    printSomething
  }

.output_progress <- function(msg, output_newline = TRUE) {
  runFromShiny <- Sys.getenv("RUN_TYPE") == "shiny"
  reportProgress <- get("reportProgress", envir = nlmeEnv)
  if (runFromShiny) {
    message(msg)
  } else if (reportProgress) {
    if (output_newline) {
      cat("\n")
    }
    cat(msg)
  }
}

UpdateProgressMessages <-
  function(currentJobDirectory = "",
           progressStage = "",
           FinalCall = FALSE) {
    reportProgress <- get("reportProgress", envir = nlmeEnv)
    ProgressStatus <- get("ProgressStatus", envir = nlmeEnv)
    progress <- ProgressStatus
    if (progressStage != "") {
      progress$ProgressStage <- progressStage
    } else {
      progress$ProgressStage <- ""
    }

    if (currentJobDirectory == "") {
      GlobalSummaryLine1 <- get("GlobalSummaryLine1", envir = nlmeEnv)
      GlobalSummaryLine2 <- get("GlobalSummaryLine2", envir = nlmeEnv)
      GlobalSummaryLine3 <- get("GlobalSummaryLine3", envir = nlmeEnv)
      if (.isnew_SummaryLine(GlobalSummaryLine1, progress$DetailInfoLine1)) {
        progress$DetailInfoLine1 <- GlobalSummaryLine1
        .output_progress(GlobalSummaryLine1)
      }

      if (.isnew_SummaryLine(GlobalSummaryLine2, progress$DetailInfoLine2)) {
        progress$DetailInfoLine2 <- GlobalSummaryLine2
        .output_progress(GlobalSummaryLine2)
      }

      if (.isnew_SummaryLine(GlobalSummaryLine3, progress$DetailInfoLine3)) {
        progress$DetailInfoLine3 <- GlobalSummaryLine3
        .output_progress(GlobalSummaryLine3)
      }
    } else {
      # read all
      lines <- readProgressUpdate(currentJobDirectory, 10000)
      # save the progress results into progress.xml, so PHX can read it
      progress$DetailInfoLine1 <- lines[1]
      progress$DetailInfoLine2 <- lines[2]

      # if the run is not job-parallelized, output all progress
      num_samples <- get("num_samples", envir = nlmeEnv)
      parallelMethod <-
        tolower(get("parallelMethod", envir = nlmeEnv))
      jobType <- get("jobType", envir = nlmeEnv)
      workflow_name <- get("workflow_name", envir = nlmeEnv)

      # used by sortfit
      scenario <- ""
      Sorts <- ""

      if (jobType == "STEPWISE_SEARCH") {
        IndexScenario <- as.numeric(basename(currentJobDirectory))
        scenarioIndexes <- get("scenarioIndexes", envir = nlmeEnv)
        scenarios <-
          get("scenarios", envir = nlmeEnv)[scenarioIndexes]
        if (!is.na(IndexScenario) &&
          length(scenarios) >= IndexScenario) {
          workflow_name <- scenarios[[IndexScenario]]$scenarioDescription
          if (workflow_name == "") {
            workflow_name <- "Base Model, no covariates"
          }
        }
      } else if (jobType == "ESTIMATION_RUN" &&
        grepl("\\d+", basename(currentJobDirectory))) {
        # sortfit
        jobNumber <- as.numeric(basename(currentJobDirectory))

        scenarioNames <-
          unique(getScenarioName(get("control_lines", envir = nlmeEnv)))
        if (any(scenarioNames != "WorkFlow")) {
          scenario <- paste0(
            " Scenario = ",
            scenarioNames[(jobNumber - 1) %% length(scenarioNames) + 1]
          )
        }

        num_sort_columns <-
          as.numeric(get("num_sort_columns", envir = nlmeEnv))
        if (num_sort_columns > 0) {
          sort_column_names <- get("sort_column_names", envir = nlmeEnv)
          unique_sorted_values <-
            get("unique_sorted_values", envir = nlmeEnv)
          scenariosIteration <-
            (jobNumber - 1) %/% length(scenarioNames) + 1
          Sorts <-
            paste(
              sort_column_names,
              unique_sorted_values[scenariosIteration, ],
              sep = "=",
              collapse = ", "
            )
        }

        if (Sorts != "" | scenario != "") {
          workflow_name <- paste0("'", Sorts, scenario, "'")
        }
      } else if (jobType == "COVAR_SEARCH" &&
        grepl("\\d+", basename(currentJobDirectory))) {
        # shotgun
        fullScenarioName <-
          getScenarioName(get("control_lines", envir = nlmeEnv)[as.numeric(basename(currentJobDirectory))])
        workflow_name <- gsub("^cshot\\d+ *", "", fullScenarioName)
        if (workflow_name == "") {
          workflow_name <- "Base Model, no covariates"
        }
      }

      if (!is.null(lines) && length(lines) > 0 && !is.na(lines[1]) &&
        (num_samples == 1 ||
          tolower(parallelMethod) %in% c("none", "local_mpi", "mpi"))) {
        progressDF <- read.delim(text = lines)[length(lines):1, -1]
        progressDF <- progressDF[!is.na(progressDF$Iteration), ]
        colnames(progressDF)[colnames(progressDF) == "X.2LL"] <-
          "-2LL"
        samplesProcessed <-
          progress$NumOfSamplesCompleted +
          progress$NumOfSamplesFailed +
          progress$NumOfSamplesErrored

        if (!exists("ProgressList", envir = nlmeEnv)) {
          # no jobs were reported
          ProgressList <- list()

          .output_StartedJobName(jobType, num_samples, samplesProcessed, workflow_name)

          if (Sys.getenv("RUN_TYPE") == "shiny" &&
            nrow(progressDF) > 0) {
            message(paste0(colnames(progressDF), collapse = "\t"))
            message(paste0("\t", apply(
              progressDF, 1, paste0,
              collapse = "\t"
            ), collapse = "\n"))
          } else if (reportProgress) {
            print(progressDF, row.names = FALSE, max = 10000)
          }
        } else {
          ProgressList <- get("ProgressList", envir = nlmeEnv)
          if (is.null(ProgressList[[currentJobDirectory]])) {
            # other job was reported and this one is not reported yet
            # firstly report overall status
            reportCurrentStatus(
              num_samples,
              progress$NumOfSamplesCompleted,
              progress$NumOfSamplesFailed
            )

            .output_StartedJobName(
              jobType,
              num_samples,
              samplesProcessed,
              workflow_name
            )

            if (Sys.getenv("RUN_TYPE") == "shiny" &&
              nrow(progressDF) > 0) {
              ProgressDFtoPrint <-
                capture.output(print(
                  progressDF,
                  quote = FALSE,
                  row.names = FALSE,
                  max = 10000
                ))

              message(paste0(
                             lapply(ProgressDFtoPrint, paste0, collapse = "\t"),
                             collapse = "\n"
              ))
            } else if (reportProgress)  {
              print(progressDF, row.names = FALSE, max = 10000)
            }
          } else {
            # some progress rows already reported
            nrowProgressList <-
              nrow(ProgressList[[currentJobDirectory]])
            nrowProgressDF <- nrow(progressDF)
            rowsToReport <- nrowProgressDF - nrowProgressList
            if (nrowProgressDF > nrowProgressList) {
              ProgressDFtoPrint <-
                capture.output(print(
                  progressDF,
                  quote = FALSE,
                  row.names = FALSE,
                  max = 10000
                ))
              if (Sys.getenv("RUN_TYPE") == "shiny" &&
                length(ProgressDFtoPrint) > 0) {
                message(paste0(
                  lapply(ProgressDFtoPrint[(nrowProgressList + 2):(nrowProgressDF + 1)], paste0, collapse = "\t"),
                  collapse = "\n"
                ))
              } else if (reportProgress) {
                cat(ProgressDFtoPrint[(nrowProgressList + 2):(nrowProgressDF + 1)], sep = "\n")
              }
            }
          }
        }

        ProgressList[[currentJobDirectory]] <- progressDF

        assign("ProgressList", ProgressList, envir = nlmeEnv)
      } else if (is.null(lines) && FinalCall) {
        if (.output_StartedJobName(jobType, num_samples, samplesProcessed, workflow_name)) {
          message("No optimization iterations found\n")
        }
      }
    }

    assign("ProgressStatus", progress, envir = nlmeEnv)
    ReportProgress(progress)
  }
