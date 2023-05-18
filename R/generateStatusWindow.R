# Takes a list of jobs and generates StatusWindow.txt
generateStatusWindow <- function(statusFiles, progressFiles, scenarioNames) {
  localWorkingDir <- get("localWorkingDir", envir = nlmeEnv)
  maxPrint <- getOption("max.print")
  old <- options()
  on.exit(options(old))
  tryCatch(
    {
      options(max.print = 1000000)
      StatusFilename <- file.path(localWorkingDir, "StatusWindow.txt")
      appendFlag <- FALSE
      workflow_name <- get("workflow_name", envir = nlmeEnv)
      num_samples <- get("num_samples", envir = nlmeEnv)
      parallelMethod <- tolower(get("parallelMethod", envir = nlmeEnv))
      jobType <- get("jobType", envir = nlmeEnv)

      for (progressFile in progressFiles) {
        # find corresponding status file
        if (jobType == "STEPWISE_SEARCH") {
          jobNumber <- as.numeric(tools::file_ext(progressFile))
          statusFile <- statusFiles[which(paste0("status.txt.", jobNumber) == basename(statusFiles))]
        } else {
          jobNumber <- as.numeric(basename(dirname(progressFile)))
          statusFile <- statusFiles[which(sprintf("S_%03d.status", jobNumber) == basename(statusFiles))]
        }

        if (jobType == "STEPWISE_SEARCH") {
          scenarios <- get("scenarios", envir = nlmeEnv)
          if (!is.na(jobNumber) && length(scenarios) >= jobNumber) {
            workflow_Scenarios <- scenarios[[jobNumber]]$scenarioDescription
            if (workflow_Scenarios == "") {
              workflow_Scenarios <- "Base Model, no covariates"
            }

            workflow_name <- paste("Scenario:", workflow_Scenarios)
          }
        } else if (jobType == "ESTIMATION_RUN" &&
          !is.na(jobNumber)) {
          # sortfit
          scenario <- ""
          Sorts <- ""
          scenarioNames <- unique(getScenarioName(get("control_lines", envir = nlmeEnv)))
          if (any(scenarioNames != "WorkFlow")) {
            scenario <- paste0(
              " Scenario = ",
              scenarioNames[(jobNumber - 1) %% length(scenarioNames) + 1]
            )
          }

          num_sort_columns <- as.numeric(get("num_sort_columns", envir = nlmeEnv))
          if (num_sort_columns > 0) {
            sort_column_names <- get("sort_column_names", envir = nlmeEnv)
            unique_sorted_values <- get("unique_sorted_values", envir = nlmeEnv)
            scenariosIteration <- (jobNumber - 1) %/% length(scenarioNames) + 1
            Sorts <- paste(sort_column_names, unique_sorted_values[scenariosIteration, ],
              sep = "=", collapse = ", "
            )
          }

          if (Sorts != "" | scenario != "") {
            workflow_name <- paste0(Sorts, scenario)
          }
        } else if (jobType == "COVAR_SEARCH" &&
          !is.na(jobNumber)) {
          # shotgun
          fullScenarioName <-
            getScenarioName(get("control_lines", envir = nlmeEnv)[jobNumber])
          workflow_name <- gsub("^cshot\\d+ *", "", fullScenarioName)
          if (workflow_name == "") {
            workflow_name <- "Scenario: Base Model, no covariates"
          } else {
            workflow_name <- paste("Scenario:", workflow_name)
          }
        } else if (jobType == "PROFILE_RUN" &&
          !is.na(jobNumber)) {
          # profiling
          scenario <- ""
          Sorts <- ""
          scenarioNames <- unique(getScenarioName(get("control_lines", envir = nlmeEnv)))
          if (any(scenarioNames != "WorkFlow")) {
            scenario <- paste0(
              "Scenario = ",
              scenarioNames[(jobNumber - 1) %% length(scenarioNames) + 1]
            )
          }

          num_sort_columns <- as.numeric(get("num_sort_columns", envir = nlmeEnv))
          if (num_sort_columns > 0) {
            sort_column_names <- get("sort_column_names", envir = nlmeEnv)
            unique_sorted_values <- get("unique_sorted_values", envir = nlmeEnv)
            scenariosIteration <- (jobNumber - 1) %/% length(scenarioNames) + 1
            Sorts <- paste(sort_column_names, unique_sorted_values[scenariosIteration, ],
              sep = "=", collapse = ", "
            )
          }

          Perturbation <- getExePostfix(get("control_lines", envir = nlmeEnv)[jobNumber])
          workflow_name <- paste("Perturbation: ", Perturbation, Sorts, scenario)
          if (Sorts != "" | scenario != "") {
            workflow_name <- paste0(Sorts, scenario)
          }
        }

        statusFileLines <- readLines(statusFile)
        if (length(grep("FAILED", statusFileLines)) > 0) {
          cat_filesWarnLong(workflow_name, "\n", file = StatusFilename, append = appendFlag)
          appendFlag <- TRUE
          next()
        }

        startTime <- strptime(statusFileLines[2], "%m/%d/%Y %H:%M:%S")
        stopTime <- strptime(statusFileLines[3], "%m/%d/%Y %H:%M:%S")
        if (is.na(startTime)) {
          startTime <- strptime(statusFileLines[2], "%Y/%m/%d %H:%M:%S")
          stopTime <- strptime(statusFileLines[3], "%Y/%m/%d %H:%M:%S")
        }

        timeDiff <- as.numeric(difftime(stopTime, startTime))

        if (file.exists(progressFile) && file.info(progressFile)$size != 0) {
          # we need to parse nObs and nSubj only
          # other info is handled by readProgressDotTxt()
          nSubjnObslines <- readLines(progressFile, n = 3L)
          if (length(nSubjnObslines) >= 2) {
            nSubj <- gsub(")", "", unlist(strsplit(nSubjnObslines[1], " = "))[2], fixed = TRUE)
            nObs <- gsub(")", "", unlist(strsplit(nSubjnObslines[2], " = "))[2], fixed = TRUE)
          }

          if (length(nSubjnObslines) == 2) {
            cat("\n", workflow_name, file = StatusFilename, append = appendFlag)
            appendFlag <- TRUE
            cat(sprintf("\nStart %s  Run : %s nSubj: %s nObs : %s\n", startTime, timeDiff, nSubj, nObs), file = StatusFilename, append = TRUE)
            cat("No optimization steps found\n", file = StatusFilename, append = TRUE)
          } else if (length(nSubjnObslines) > 2) {
            dfLong <- readProgressDotTxt(progressFile)
            dfLong$Parameter <- factor(dfLong$Parameter, levels = unique(dfLong$Parameter))
            df <- reshape::cast(dfLong, formula = Iter ~ Parameter, value = "Value")
            cat("\n", workflow_name, file = StatusFilename, append = appendFlag)
            appendFlag <- TRUE
            cat(sprintf("\nStart %s  Run : %s nSubj: %s nObs : %s\n", startTime, timeDiff, nSubj, nObs), file = StatusFilename, append = TRUE)
            cat(capture.output(print(df, row.names = FALSE)), file = StatusFilename, sep = "\n", append = TRUE)
          }
        } else {
          nSubj <- nObs <- 0
          cat("\n", workflow_name, file = StatusFilename, append = appendFlag)
          appendFlag <- TRUE
          cat(sprintf("\nStart %s  Run : %s nSubj: %s nObs : %s\n", startTime, timeDiff, nSubj, nObs), file = StatusFilename, append = TRUE)
        }
      }
    },
    error = function(ex) {
      warning(ex)
    }
  )

  options(old)
}
