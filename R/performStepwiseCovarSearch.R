




#' NLME stepwise covariate search
#'
#' This function runs a stepwise covariate NLME job in parallel
#' It is designated to be called in commandline (Rscript)
#'
#' @param args a vector of arguments provided as the following:
#' c(method, install_directory, shared_directory, localWorkingDir,
#'  modelFile, nlmeArgsFile, listOfFilesToCopy, numCovariates,
#'   CovariateNames, NCriteria, addPValue, removePValue, NumProc, workflowName)
#' @param reportProgress whether it is required to report the progress
#' (for local jobs usually)
#' @return Directory path where NLME job was executed
#' @keywords NLME StepwiseCovariateSearch
#' @export
performStepwiseCovarSearch <-
  function(args, reportProgress = FALSE) {
    requiredargs <-
      c(
        "method",
        "install_directory",
        "shared_directory",
        "localWorkingDir",
        "modelFile",
        "nlmeArgsFile",
        "listOfFilesToCopy",
        "numCovariates",
        "CovariateNames",
        "NCriteria",
        "addPValue",
        "removePValue",
        "NumProc",
        "workflowName"
      )

    if (length(args) != 14) {
      check_Arguments(requiredargs, args, "performStepwiseCovarSearch")
    }

    rm(list = ls(envir = nlmeEnv), envir = nlmeEnv)
    assign("reportProgress", reportProgress, envir = nlmeEnv)

    tryCatch({
      jobHomeDirectories <- c()
      parallelMethod <- args[1]
      installDir <- args[2]
      sharedDir <- args[3]
      localWorkingDir <- args[4]
      modelFilename <- args[5]
      nlmeArgsFilename <- args[6]
      inputFileArray <- args[7]
      numCovariates <- args[8]
      covarNamesArray <- args[9]
      criteriaString <- args[10]
      addPValue <- as.numeric(args[11])
      removePValue <- as.numeric(args[12])
      numCores <- args[13]
      workflow_name <- args[14]

      localWorkingDir <-
        gsub("\\", "/", localWorkingDir, fixed = TRUE)
      assign("localWorkingDir", localWorkingDir, envir = nlmeEnv)
      updateInitialStatus("Stepwise Covariate Search",
                          parallelMethod,
                          localWorkingDir)
      # since the path is written to globaldefs, need to use / in the path
      controlFilename <-
        normalizePath(
          file.path(localWorkingDir, "nlmeControlFile.txt"),
          winslash = "/",
          mustWork = FALSE
        )
      nlmeArgsFile <-
        file.path(localWorkingDir, "nlmeargsCombined.txt")

      tokens <- unlist(strsplit(criteriaString, split = ":"))
      criteria <- tokens[1]
      if (length(tokens) == 2) {
        degreesOfFreedomString <- tokens[2]
      } else {
        degreesOfFreedomString <-
          paste(as.character(rep(1, times = numCovariates)), collapse = ",")
      }

      nxtScenario <- 0
      assign("workflow_name", workflow_name, envir = nlmeEnv)

      # Step 1 .  Run zero-N searches
      scenarios <- list()
      assign("scenarios", scenarios, envir = nlmeEnv)
      scenarioIndexes <-
        generateInitialScenarios(
          controlFilename,
          nlmeArgsFile,
          modelFilename,
          nlmeArgsFilename,
          inputFileArray,
          numCovariates,
          covarNamesArray,
          nxtScenario,
          localWorkingDir
        )

      scenarios <- get("scenarios", envir = nlmeEnv)
      nxtScenario <- get("nxtScenario", envir = nlmeEnv)
      selectedVariables <- c()
      argList <- c(
        "STEPWISE_SEARCH",
        parallelMethod,
        installDir,
        sharedDir,
        localWorkingDir,
        controlFilename,
        numCores,
        workflow_name
      )

      outputFilenames <- c()
      scenarioNames <- c()
      progressFilenames <- c()
      statusFilenames <- c()
      scenariosOutput <-
        sapply(scenarios[scenarioIndexes], "[", c("scenarioDescription"))
      scenariosOutput[scenariosOutput == ""] <-
        "Base Model, no covariates"
      .output_line()
      .output_progress(paste0("Processing scenarios:\n",
                              paste0(dQuote(
                                scenariosOutput
                              ), collapse = ", ")),
                       output_newline = FALSE)
      .output_line()

      # Run the jobs
      jobHome <-
        performParallelNLMERun(
          argList,
          partialJob = TRUE,
          allowIntermediateResults = FALSE,
          progressStage = "Initial Effect To Add",
          func = "moveStepwiseOutputFiles",
          func_arg = scenarioIndexes,
          reportProgress = reportProgress
        )

      if (!IsJobCanceled() && !IsEarlyTerminationRequested()) {
        jobHomeDirectories <- c(jobHomeDirectories, jobHome)

        bestResultsSofar <- 0.0
        currentBestIndex <- -1

        # Pick the best run
        stepwiseFilename <-
          file.path(localWorkingDir, "Stepwise.txt")

        currentBestIndex <-
          getBestResults(
            localWorkingDir,
            scenarioIndexes,
            criteria,
            degreesOfFreedomString,
            addPValue,
            stepwiseFilename,
            currentBestIndex,
            "add",
            TRUE
          )

        scenarios <- get("scenarios", envir = nlmeEnv)

        # Nothing todo, the best results is the one with no covariates
        if (currentBestIndex == -1) {
          message(
            "\nModel without covariate effects has the best fit",
            "\nor other unexpected errors occured"
          )
        } else {
          s <- scenarioIndexes[currentBestIndex]
          currentBestScenario <- scenarios[[s]]$index
          currentBestIndex <- currentBestIndex - 1

          selectedVariables <-
            c(selectedVariables, currentBestIndex)

          numTodo <- as.integer(numCovariates)
          submodels <- c()
          for (i in 1:numTodo) {
            submodels <- c(submodels, FALSE)
          }
          submodels[currentBestIndex] <- TRUE
          notDone <- 1

          # Keep adding another variable and see if the results et improved
          originaldegreesOfFreedomString <- degreesOfFreedomString
          while (notDone) {
            # Add all the remaining variables one at a time in this iteration
            appendFlag <- FALSE
            lines <- c()
            outputFilenames <- c()

            cat_filesWarnLong(modelFilename, file = controlFilename, sep = "\n")
            for (ifn in inputFileArray) {
              cat(ifn,
                  file = controlFilename,
                  sep = " ",
                  append = TRUE)
            }
            cat("",
                file = controlFilename,
                sep = "\n",
                append = TRUE)
            cat(
              "*.csv *.txt *.log *.LOG",
              file = controlFilename,
              sep = "\n",
              append = TRUE
            )
            scenarioIndexes <- c()
            indx <- 1
            scenarioAlreadyRan <- FALSE
            newDegreesOfFreedomString <- ""
            firstTime <- TRUE
            for (i in 1:numTodo) {
              if (submodels[i] == FALSE) {
                submodels[i] <- TRUE
                degOfFreedom <-
                  unlist(strsplit(degreesOfFreedomString, ","))[i]
                if (firstTime == TRUE) {
                  newDegreesOfFreedomString <- degOfFreedom
                } else {
                  newDegreesOfFreedomString <-
                    paste(newDegreesOfFreedomString,
                          degOfFreedom,
                          sep = ",")
                }
                firstTime <- FALSE
                subArgs <- submodels
                ret <-
                  generateSelCovarSearchArgsLine(
                    controlFilename,
                    nlmeArgsFile,
                    modelFilename,
                    nlmeArgsFilename,
                    inputFileArray,
                    numCovariates,
                    covarNamesArray,
                    subArgs,
                    nxtScenario,
                    appendFlag,
                    indx,
                    localWorkingDir
                  )

                if (length(scenarios[[ret$key]]) != 0) {
                  if (scenarios[[ret$key]]$status == "Completed") {
                    scenarioAlreadyRan <- TRUE
                  }
                }
                if (scenarioAlreadyRan == FALSE) {
                  indx <- indx + 1
                  scenarios[[ret$key]] <- ret
                  appendFlag <- TRUE
                  line <- ret$line
                  outputFilename <- ret$outputFilename
                  outputFilenames <-
                    c(outputFilenames, outputFilename)
                  assign("scenarios", scenarios, envir = nlmeEnv)
                  lines <- c(lines, line)
                  nxtScenario <- nxtScenario + 1
                }
                submodels[i] <- FALSE
                scenarioIndexes <- c(scenarioIndexes, ret$index)
              }
            }
            cat((length(lines)),
                file = controlFilename,
                sep = "\n",
                append = TRUE)
            for (l in lines) {
              cat(l,
                  file = controlFilename,
                  sep = "\n",
                  append = TRUE)
            }
            if (length(lines) > 0) {
              scenariosOutput <-
                sapply(scenarios[scenarioIndexes], "[", c("scenarioDescription"))
              scenariosOutput[scenariosOutput == ""] <-
                "Base Model, no covariates"
              .output_line()
              .output_progress(paste0(
                "Processing scenarios:\n",
                paste0(dQuote(scenariosOutput), collapse = ", ")
              ),
              output_newline = FALSE)
              .output_line()

              jobHome <-
                performParallelNLMERun(
                  argList,
                  partialJob = TRUE,
                  progressStage = "Add Effects",
                  func = "moveStepwiseOutputFiles",
                  func_arg = scenarioIndexes,
                  reportProgress = reportProgress
                )
              jobHomeDirectories <- c(jobHomeDirectories, jobHome)
            }
            if (IsJobCanceled() ||
                IsEarlyTerminationRequested()) {
              break
            }
            bestIndex <-
              getBestResults(
                localWorkingDir,
                scenarioIndexes,
                criteria,
                newDegreesOfFreedomString,
                addPValue,
                stepwiseFilename,
                currentBestScenario,
                "add",
                FALSE
              )
            scenarios <- get("scenarios", envir = nlmeEnv)

            if (bestIndex == -1) {
              notDone <- 0
              break
            }
            bestScenario <-
              scenarios[[scenarioIndexes[bestIndex]]]$index
            if (criteria == "-2LL") {
              newBest <- scenarios[[bestScenario]]$logLikelihood
              oldBest <-
                scenarios[[currentBestScenario]]$logLikelihood
              degOfFreedom <-
                unlist(strsplit(newDegreesOfFreedomString, ","))[bestIndex]
              chisq <-
                qchisq(as.numeric(1.0 - addPValue), df = as.integer(degOfFreedom))
              if (is.nan(chisq)) {
                chisq <- 0.0
              }
            }
            if (criteria == "AIC") {
              newBest <- scenarios[[bestScenario]]$AIC
              oldBest <- scenarios[[currentBestScenario]]$AIC
              chisq <- addPValue
            }
            if (criteria == "BIC") {
              newBest <- scenarios[[bestScenario]]$BIC
              oldBest <- scenarios[[currentBestScenario]]$BIC
              chisq <- addPValue
            }
            if (newBest < (oldBest - addPValue)) {
              currentBestIndex <- bestIndex
              currentBestScenario <- bestScenario
              mask <- scenarios[[bestScenario]]$key
              # Keep track of which variable we have used so far
              selectedVariables <- c(selectedVariables, bestIndex)
              # Reset submodel flags based on what was picked
              for (i in 1:nchar(mask)) {
                if (unlist(strsplit(mask, split = ""))[i] == "0") {
                  submodels[i] <- FALSE
                } else {
                  submodels[i] <- TRUE
                }
              }
            } else {
              message(
                paste(
                  "Did not improve on LL",
                  scenarios[[bestIndex]]$logLikelihood,
                  scenarios[[currentBestScenario]]$logLikelihood
                )
              )
              notDone <- FALSE
            }
          }

          # Keep subtracting one variable and see if the results are improved
          notDone <- 1
          while (notDone) {
            # Remove the selected variables one at a time in this iteration
            appendFlag <- FALSE
            lines <- c()
            outputFilenames <- c()

            cat(modelFilename, file = controlFilename, sep = "\n")
            for (ifn in inputFileArray) {
              cat(ifn,
                  file = controlFilename,
                  sep = " ",
                  append = TRUE)
            }

            cat("",
                file = controlFilename,
                sep = "\n",
                append = TRUE)
            cat(
              "*.csv *.txt *.log *.LOG",
              file = controlFilename,
              sep = "\n",
              append = TRUE
            )
            scenarioIndexes <- c()
            scenarioIndexesToRun <- c()
            indx <- 1
            firstTime <- TRUE
            for (i in 1:numTodo) {
              scenarioAlreadyRan <- FALSE
              if (submodels[i] == TRUE) {
                submodels[i] <- FALSE
                degOfFreedom <-
                  unlist(strsplit(degreesOfFreedomString, ","))[i]
                if (firstTime == TRUE) {
                  newDegreesOfFreedomString <- degOfFreedom
                } else {
                  newDegreesOfFreedomString <-
                    paste(newDegreesOfFreedomString,
                          degOfFreedom,
                          sep = ",")
                }
                firstTime <- FALSE
                subArgs <- submodels
                ret <-
                  generateSelCovarSearchArgsLine(
                    controlFilename,
                    nlmeArgsFile,
                    modelFilename,
                    nlmeArgsFilename,
                    inputFileArray,
                    numCovariates,
                    covarNamesArray,
                    subArgs,
                    nxtScenario,
                    appendFlag,
                    indx,
                    localWorkingDir
                  )
                if (length(scenarios[[ret$key]]) != 0 &&
                    scenarios[[ret$key]]$status == "Completed") {
                  scenarioAlreadyRan <- TRUE
                } else {
                  scenarioIndexesToRun <- c(scenarioIndexesToRun, ret$index)
                }

                scenarioIndexes <- c(scenarioIndexes, ret$index)
                if (scenarioAlreadyRan == FALSE) {
                  scenarios[[ret$key]] <- ret
                  appendFlag <- TRUE
                  line <- ret$line
                  outputFilenames <-
                    c(outputFilenames, ret$outputFilename)
                  assign("scenarios", scenarios, envir = nlmeEnv)
                  indx <- indx + 1
                  lines <- c(lines, line)
                  nxtScenario <- nxtScenario + 1
                }
                submodels[i] <- TRUE
              }
            }

            cat((length(lines)),
                file = controlFilename,
                sep = "\n",
                append = TRUE)

            for (l in lines) {
              cat(l,
                  file = controlFilename,
                  sep = "\n",
                  append = TRUE)
            }

            if (length(scenarioIndexesToRun) > 0) {
              jobHome <- performParallelNLMERun(
                argList,
                partialJob = TRUE,
                progressStage = "Remove Effects",
                func = "moveStepwiseOutputFiles",
                func_arg = scenarioIndexesToRun,
                reportProgress = reportProgress
              )
              jobHomeDirectories <- c(jobHomeDirectories, jobHome)
            }

            if (IsJobCanceled() ||
                IsEarlyTerminationRequested()) {
              break
            }
            bestIndex <- getBestResults(
              localWorkingDir,
              scenarioIndexes,
              criteria,
              newDegreesOfFreedomString,
              removePValue,
              stepwiseFilename,
              currentBestScenario,
              "subtract",
              FALSE
            )
            scenarios <- get("scenarios", envir = nlmeEnv)

            if (bestIndex == -1) {
              notDone <- 0
              break
            }
            bestScenario <-
              scenarios[[scenarioIndexes[bestIndex]]]$index
            newBest <- scenarios[[bestScenario]]$logLikelihood
            oldBest <-
              scenarios[[currentBestScenario]]$logLikelihood
            if (criteria == "-2LL") {
              newBest <- scenarios[[bestScenario]]$logLikelihood
              oldBest <-
                scenarios[[currentBestScenario]]$logLikelihood
              degOfFreedom <-
                unlist(strsplit(newDegreesOfFreedomString, ","))[bestIndex]
              chisq <-
                qchisq(as.numeric(1.0 - removePValue), df = as.integer(degOfFreedom))
              if (is.nan(chisq)) {
                chisq <- 0.0
              }
            } else if (criteria == "AIC") {
              newBest <- scenarios[[bestScenario]]$AIC
              oldBest <- scenarios[[currentBestScenario]]$AIC
              chisq <- removePValue
            } else if (criteria == "BIC") {
              newBest <- scenarios[[bestScenario]]$BIC
              oldBest <- scenarios[[currentBestScenario]]$BIC
              chisq <- removePValue
            }

            chisq <- chisq * -1
            if (newBest < (oldBest - chisq)) {
              currentBestIndex <- bestIndex
              currentBestScenario <- bestScenario
              mask <- scenarios[[bestScenario]]$key
              # Keep track of which variable we have used so far
              selectedVariables <- c(selectedVariables, bestIndex)
              # Reset submodel flags based on what was picked
              for (i in 1:nchar(mask)) {
                if (unlist(strsplit(mask, split = ""))[i] == "0") {
                  submodels[i] <- FALSE
                } else {
                  submodels[i] <- TRUE
                }
              }
            } else {
              message(
                paste(
                  "Did not improve on LL",
                  scenarios[[bestIndex]]$logLikelihood,
                  scenarios[[currentBestScenario]]$logLikelihood
                )
              )
              notDone <- FALSE
            }
          }
        }
      }
    },
    error = function(ex) {
      warning(
        "Failed to performStepwiseCovarSearch. Error is: ",
        ex,
        call. = FALSE,
        immediate. = TRUE
      )
      FailProgress()
    })

    if (!IsJobCanceled()) {
      summarizeStepwiseCovarSearch(localWorkingDir, scenarios)
      CompleteProgress()
    }

    for (jh in jobHomeDirectories) {
      removeTempWorkingDirectory(jh)
    }

    assign("jobHomeDirectories", jobHomeDirectories, envir = nlmeEnv)
    if (exists("OverallDF", envir = nlmeEnv)) {
      get0("OverallDF", envir = nlmeEnv)
    }
  }
