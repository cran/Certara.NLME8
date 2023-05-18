# Reads summary information from a list of results files from different
# scenarios .
#
# Return the index of best results in the list of output files
getBestResults <-
  function(localDir,
           scenarioIndexes,
           criteria,
           degreesOfFreedomString,
           addPValue,
           stepwiseFilename,
           currentBest,
           mode,
           firstTime) {
    scenarios <- get("scenarios", envir = nlmeEnv)
    bestListIndex <- -1
    bestScenario <- ""
    if (currentBest == -1) {
      bestScenarioIndex <- -1
      bestLL <- 1e+17
      initialLL <- 1e+20
      degreesOfFreedomString <- paste0("1,", degreesOfFreedomString)
    } else {
      bestScenarioIndex <- -1
      if (criteria == "-2LL") {
        bestLL <- scenarios[[currentBest]]$logLikelihood
      } else if (criteria == "AIC") {
        bestLL <- scenarios[[currentBest]]$AIC
      } else if (criteria == "BIC") {
        bestLL <- scenarios[[currentBest]]$BIC
      }

      initialLL <- bestLL
    }

    first <- TRUE
    listIndex <- 1

    for (si in scenarioIndexes) {
      scenarioFileName <- scenarios[[si]]$outputFilename
      key <- getScenarioKey(scenarioFileName)
      if (criteria == "-2LL") {
        degOfFreedom <-
          unlist(strsplit(degreesOfFreedomString, ","))[listIndex]
        chisq <-
          qchisq(as.numeric(1.0 - addPValue), df = as.integer(degOfFreedom))

        if (is.nan(chisq)) {
          chisq <- 0.0
        } else if (mode == "subtract") {
          chisq <- chisq * -1
        }
      } else {
        # criteria != "-2LL"
        if (mode == "subtract") {
          chisq <- addPValue * -1
        } else {
          chisq <- addPValue
        }
      }

      fullPath <- file.path(localDir, scenarioFileName)
      if (file.exists(fullPath)) {
        overallDF <- .get_outputDF(fullPath)
        if (nrow(overallDF) == 0) {
          warning(
            "The row with 'ReturnCode' word is not found in ",
            fullPath,
            "\nScenarioName = ",
            scenarios[[si]]$scenarioName,
            "\nScenarioDescription = ",
            scenarios[[si]]$scenarioDescription
          )
          next()
        } else {
          scenarios[[si]]$returnCode <- overallDF$RetCode
          scenarios[[si]]$shrinkage <- overallDF$EpsShrinkage
          scenarios[[si]]$nParam <- overallDF$nParm
          scenarios[[si]]$nObs <- overallDF$nObs
          scenarios[[si]]$nSub <- overallDF$nSub
          scenarios[[si]]$logLikelihood <- ll <- overallDF$`-2LL`
          scenarios[[si]]$logLike <- overallDF$LogLik
          scenarios[[si]]$AIC <- overallDF$AIC
          scenarios[[si]]$BIC <- overallDF$BIC
          scenarios[[si]]$Condition <- overallDF$Condition
        }

        valueToCompare <- ll
        if (criteria == "AIC") {
          valueToCompare <- scenarios[[si]]$AIC
        } else if (criteria == "BIC") {
          valueToCompare <- scenarios[[si]]$BIC
        }

        if (firstTime && (si == 1)) {
          initialLL <- valueToCompare
          cat(
            sprintf(
              "%s %s %s, %s = %f ",
              scenarios[[si]]$scenarioName,
              scenarios[[si]]$scenarioDescription,
              scenarios[[si]]$key,
              criteria,
              valueToCompare
            ),
            file = stepwiseFilename,
            sep = "\n",
            append = FALSE
          )
          FindEffectText <-
            paste0("Find effect to add that reduces ", criteria, " the most:")
          cat(
            FindEffectText,
            file = stepwiseFilename,
            sep = "\n",
            append = TRUE
          )
          .output_progress(paste0(FindEffectText, "\n"),
                           output_newline = FALSE)
        } else {
          if (first) {
            cat(" ",
                file = stepwiseFilename,
                sep = "\n",
                append = TRUE)
            if (mode == "add") {
              FindEffectText <-
                paste0("Find effect to add that reduces ",
                       criteria,
                       " the most:")
            } else {
              FindEffectText <-
                paste0("Find effect to subtract that increases ",
                       criteria,
                       " the least:")
            }
            cat(
              FindEffectText,
              file = stepwiseFilename,
              sep = "\n",
              append = TRUE
            )
            .output_progress(paste0(FindEffectText, "\n"),
                             output_newline = FALSE)
          }
          if ((valueToCompare + chisq) < (initialLL)) {
            textComparison <- sprintf(
              "%s %s %s     %f ( %f %+f ) < %f )",
              scenarios[[si]]$scenarioName,
              scenarios[[si]]$scenarioDescription,
              scenarios[[si]]$key,
              (valueToCompare + chisq),
              valueToCompare,
              chisq,
              initialLL
            )
            if ((valueToCompare + chisq) < bestLL) {
              bestScenario <- key
              bestScenarioIndex <- si
              bestNetValue <- valueToCompare
              bestLL <- valueToCompare + chisq
              bestListIndex <- listIndex
            }
          } else {
            textComparison <- sprintf(
              "%s %s %s   X %f ( %f %+f ) > %f )",
              scenarios[[si]]$scenarioName,
              scenarios[[si]]$scenarioDescription,
              scenarios[[si]]$key,
              (valueToCompare + chisq),
              valueToCompare,
              chisq,
              initialLL
            )
          }

          cat(
            textComparison,
            file = stepwiseFilename,
            sep = "\n",
            append = TRUE
          )
          .output_progress(paste0(textComparison, "\n"),
                           output_newline = FALSE)
        }
        first <- FALSE
        scenarios[[si]]$status <- "Completed"
      } else {
        cat(
          sprintf("ERROR : Unable to read %s", scenarioFileName),
          file = stepwiseFilename,
          sep = "\n",
          append = TRUE
        )
      }
      listIndex <- listIndex + 1
    }

    if (bestScenarioIndex != -1) {
      scenarioChosenText <- sprintf(
        "%s %s %s chosen, %s = %f",
        scenarios[[bestScenarioIndex]]$scenarioName,
        scenarios[[bestScenarioIndex]]$scenarioDescription,
        scenarios[[bestScenarioIndex]]$key,
        criteria,
        bestNetValue
      )
    } else {
      scenarioChosenText <- sprintf("\nNo effect chosen to %s\n", mode)
    }

    cat(
      scenarioChosenText,
      file = stepwiseFilename,
      sep = "\n",
      append = TRUE
    )
    .output_progress(scenarioChosenText,
                     output_newline = FALSE)

    if (bestScenarioIndex == -1 &&
        mode == "subtract" && currentBest != -1) {
      FinalText <- sprintf(
        "\nScenario to use = %s %s %s\n",
        scenarios[[currentBest]]$scenarioName,
        scenarios[[currentBest]]$scenarioDescription,
        scenarios[[currentBest]]$key
      )
      cat(FinalText,
          file = stepwiseFilename,
          sep = "\n",
          append = TRUE)
      .output_progress(FinalText,
                       output_newline = FALSE)
    }

    assign("scenarios", scenarios, envir = nlmeEnv)
    return(bestListIndex)
  }
