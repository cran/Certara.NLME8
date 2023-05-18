# This method copies all files from grid/remote directory and summerizes the
# bootstrap run result
generateBootstrapResults <- function(localDir, jobList) {
  collectJobResults(jobList)

  results_file_list <- c(
    "out.bin.txt",
    "out.txt",
    "BootSubj.csv",
    "out.csv",
    "doses.csv",
    "iniest.csv",
    "err2.txt",
    "err.txt",
    "progress.txt",
    "dmp.txt",
    "test_new.mdl",
    "progress.xml",
    "out_initialEstimates.txt",
    "nlme7engine.log"
  )

  copyResults(dirToCopyTo = localDir)

  summarizeBootstrap(localDir)
}

summarizeBootstrap <-
  function(localWorkingDir) {
    separator <- "9875"
    bootSecondaryFilename <-
      file.path(localWorkingDir, "BootSecondary.csv")
    bootThetaFilename <- file.path(localWorkingDir, "BootTheta.csv")
    bootOverallFilename <-
      file.path(localWorkingDir, "BootOverall.csv")

    # it is not a stack of worksheet “BootTheta” but rather a stack of Theta
    # obtained in all the bootstrapping samples
    bootThetaStackedFilename <-
      file.path(localWorkingDir, "BootThetaStacked.csv")
    bootOmegaFilename <- file.path(localWorkingDir, "BootOmega.csv")

    # it is not a stack of worksheet “BootOmega” but rather a stack of Omega
    # obtained in all the bootstrapping samples
    bootOmegaStackedFilename <-
      file.path(localWorkingDir, "BootOmegaStacked.csv")
    bootOmegaStderrFilename <-
      file.path(localWorkingDir, "BootOmegaStderr.csv")
    bootVarCoVarFilename <-
      file.path(localWorkingDir, "BootVarCoVar.csv")

    GlobalSummaryLine1 <- "\nSummarizing bootstrap results\n"
    assign("GlobalSummaryLine1", GlobalSummaryLine1, envir = nlmeEnv)
    progress <- get("ProgressStatus", envir = nlmeEnv)
    numSamples <- numSuccessful <- progress$NumOfSamples
    UpdateProgress(numSamples, numSuccessful, 0, 0)
    UpdateProgressMessages()
    confidence_level <- get("confidence_level", envir = nlmeEnv)

    tryCatch({
      dmpLines <- shrinkDmpDotTxt(file.path(localWorkingDir, "dmp.txt"))
      source(textConnection(dmpLines), local = TRUE)

      if (file.exists(file.path(localWorkingDir, "out.bin.txt"))) {
        out.bin.txt <- readLines(file.path(localWorkingDir, "out.bin.txt"))
        outputData <-
          read.csv(file.path(localWorkingDir, "out.csv"))
        numSamples <- length(which(out.bin.txt == separator))

        fixedEffectNames <- names(dmp.txt$coefficients$fixed)
        secondaryEffectNames <-
          names(dmp.txt$coefficients$secondary)
        randomEffectNames <- colnames(dmp.txt$omega)
        numFixedEffects <- length(fixedEffectNames)
        numOmegas <- length(randomEffectNames)
        numParams <- dmp.txt$nParm
        numObservations <- dmp.txt$nObs
        numSubjects <- dmp.txt$nSubj
        numSecondaryEffects <- length(secondaryEffectNames)
        numRandomEffects <- length(randomEffectNames)

        # separate out.bin.txt by the separator 9875; each sample info goes to the new list
        out.bin.txtWOsep <- out.bin.txt[out.bin.txt != separator]
        n <- length(out.bin.txtWOsep)
        r <- rep(1:numSamples, each = ceiling(n / numSamples))[1:n]
        stuff <- split(out.bin.txtWOsep, r)

        frac <- (100.0 - confidence_level) / 200
        bootColnames <- c(
          "Scenario",
          "Parameter",
          "Mean",
          "Stderr",
          "CV%",
          "Median",
          sprintf("%3.1f%%", frac * 100.0),
          sprintf("%4.1f%%", 100.0 - (frac * 100))
        )

        if (numSecondaryEffects > 0) {
          secondaryStart <- 6 + numFixedEffects + 1
          SecondaryEnd <- 6 + numFixedEffects + numSecondaryEffects
          secondaryMatrix <-
            sapply(stuff, function(x, y) {
              as.double(x[y])
            }, secondaryStart:SecondaryEnd)
          if (numSecondaryEffects == 1) {
            secondaryMatrix <- t(as.matrix(secondaryMatrix))
          }

          SecondaryMeanValues <- apply(secondaryMatrix, 1, mean)
          stdError <-
            apply(secondaryMatrix, 1, .StdErrorComputation)

          bootSecondary <-
            cbind.data.frame(
              Scenario = "(B)",
              Parameter = secondaryEffectNames,
              Mean = SecondaryMeanValues,
              Stderr = stdError,
              CV = 100 * stdError / SecondaryMeanValues,
              Median = apply(secondaryMatrix, 1, median),
              q1 = apply(secondaryMatrix, 1, quantile, frac),
              q2 = apply(secondaryMatrix, 1, quantile, 1 - frac)
            )

          colnames(bootSecondary) <- bootColnames
          write.csv(bootSecondary, file = bootSecondaryFilename, row.names = FALSE)
        }

        # Generate BootTheta.txt
        thetaMatrix <- sapply(stuff, function(x, y) {
          as.double(x[y])
        }, 6:(6 + numFixedEffects - 1))
        if (numFixedEffects == 1) {
          thetaMatrix <- t(as.matrix(thetaMatrix))
        }
        fixedEffectsMeanValues <- apply(thetaMatrix, 1, mean)
        stdError <- apply(thetaMatrix, 1, .StdErrorComputation)
        cvPercent <- 100 * stdError / fixedEffectsMeanValues
        Median <- apply(thetaMatrix, 1, median)
        q1 <- apply(thetaMatrix, 1, quantile, frac)
        q2 <- apply(thetaMatrix, 1, quantile, 1 - frac)

        bootTheta <- cbind.data.frame(
          Scenario = "(B)",
          Parameter = fixedEffectNames,
          Mean = fixedEffectsMeanValues,
          Stderr = stdError,
          CV = cvPercent,
          Median = Median,
          q1 = q1,
          q2 = q2
        )

        colnames(bootTheta) <- bootColnames
        write.csv(bootTheta, file = bootThetaFilename, row.names = FALSE)

        # Write out BootVarCoVar.csv
        BootVarCoVar <-
          matrix(nrow = numFixedEffects, ncol = numFixedEffects)
        for (ThetaRow in 1:numFixedEffects) {
          for (ThetaCol in 1:numFixedEffects) {
            if (ThetaRow > ThetaCol) {
              # fill low triangle with upper triangle
              BootVarCoVar[ThetaRow, ThetaCol] <-
                BootVarCoVar[ThetaCol, ThetaRow]
            } else if (ThetaRow == ThetaCol) {
              RepDiffMean <-
                thetaMatrix[ThetaRow, ] - fixedEffectsMeanValues[ThetaRow]
              BootVarCoVar[ThetaRow, ThetaCol] <-
                sum(RepDiffMean * RepDiffMean) / numSamples
            } else {
              SumRepDiffMean <-
                sum((thetaMatrix[ThetaRow, ] - fixedEffectsMeanValues[ThetaRow]) *
                      (thetaMatrix[ThetaCol, ] - fixedEffectsMeanValues[ThetaCol])
                )
              BootVarCoVar[ThetaRow, ThetaCol] <-
                SumRepDiffMean / numSamples
            }
          }
        }

        BootVarCoVarDF <-
          cbind.data.frame(
            Scenario = "(B)",
            "Var Name" = fixedEffectNames,
            setNames(data.frame(BootVarCoVar), fixedEffectNames)
          )
        write.csv(BootVarCoVarDF, bootVarCoVarFilename, row.names = FALSE)

        # Generate BootThetaStacked.txt
        Replicate <- 1:numSamples
        Theta <- fixedEffectNames
        Value <- c(thetaMatrix)
        bootThetaStackedDF <- cbind.data.frame(
          Scenario = "(B)",
          Replicate = rep(Replicate, each = numFixedEffects),
          Theta = Theta,
          Value = Value
        )
        write.csv(bootThetaStackedDF,
                  bootThetaStackedFilename,
                  row.names = FALSE)


        # Generate BootOverall.txt
        BootOverallDF <- cbind.data.frame(
          Scenario = "(B)",
          Replicate = Replicate,
          ReturnCode = outputData$ReturnCode,
          LL = outputData$LL
        )

        write.csv(BootOverallDF, bootOverallFilename, row.names = FALSE)

        if (numRandomEffects > 0) {
          # Generate BootOmegaStacked file
          BootOmegaCast <- outputData
          BootOmegaCast$Replicate <- Replicate
          outNames <- names(outputData)
          numOmegas <-
            length(outNames) - 2 - numFixedEffects - numSecondaryEffects
          BootOmegaStacked <-
            reshape::melt(
              BootOmegaCast,
              id.vars = "Replicate",
              variable_name = "Omega",
              measure.vars = outNames[(length(outNames) - numOmegas + 1):length(outNames)]
            )

          BootOmegaStacked$Omega <-
            unlist(regmatches(
              BootOmegaStacked$Omega,
              gregexpr(
                "(?<=^omega\\.).*(?=\\.$)",
                BootOmegaStacked$Omega,
                perl = TRUE
              )
            ))
          BootOmegaStacked$Omega <-
            gsub(".", "_", BootOmegaStacked$Omega, fixed = TRUE)
          names(BootOmegaStacked)[names(BootOmegaStacked) == "value"] <-
            "Value"
          write.csv(BootOmegaStacked,
                    bootOmegaStackedFilename,
                    row.names = FALSE)

          # Grab mean for all omega values and Write out BootOmega file
          BootOmegaStacked$Omega <- factor(BootOmegaStacked$Omega,
                                           levels = unique(BootOmegaStacked$Omega))
          meanOmegaDF <-
            aggregate(BootOmegaStacked$Value,
                      list(BootOmegaStacked$Omega),
                      mean)

          bootOmegaMatrix <-
            matrix(
              nrow = numRandomEffects,
              ncol = numRandomEffects,
              byrow = TRUE,
              dimnames = list(randomEffectNames, randomEffectNames)
            )
          bootOmegaMatrix[upper.tri(bootOmegaMatrix, diag = TRUE)] <-
            meanOmegaDF$x
          bootOmegaMatrix[lower.tri(bootOmegaMatrix)] <-
            t(bootOmegaMatrix)[lower.tri(bootOmegaMatrix)]

          bootOmega <- cbind.data.frame(Scenario = "(B)",
                                        Label = randomEffectNames,
                                        bootOmegaMatrix)
          OmegaFirstLine <- cbind.data.frame(Scenario = "(B)",
                                             Label = "Omega")
          # add blank columns to rbind
          OmegaFirstLine[setdiff(names(bootOmega), names(OmegaFirstLine))] <-
            ""

          # add correlation to BootOmega
          OmegaCorrFirstLine <- cbind.data.frame(Scenario = "(B)",
                                                 Label = "Correlation")
          OmegaCorrFirstLine[setdiff(names(bootOmega), names(OmegaCorrFirstLine))] <-
            ""
          bootOmegaCorr <- cbind.data.frame(Scenario = "(B)",
                                            Label = randomEffectNames,
                                            cov2cor(bootOmegaMatrix))
          bootOmega <- rbind.data.frame(OmegaFirstLine,
                                        bootOmega,
                                        OmegaCorrFirstLine,
                                        bootOmegaCorr)

          write.csv(bootOmega, bootOmegaFilename, row.names = FALSE)

          # Write out BootOmegaStderr file
          stderrOmegaDF <-
            aggregate(
              BootOmegaStacked$Value,
              list(BootOmegaStacked$Omega),
              .StdErrorComputation
            )
          bootOmegaSEMatrix <-
            matrix(
              nrow = numRandomEffects,
              ncol = numRandomEffects,
              byrow = TRUE,
              dimnames = list(randomEffectNames, randomEffectNames)
            )
          bootOmegaSEMatrix[upper.tri(bootOmegaSEMatrix, diag = TRUE)] <-
            stderrOmegaDF$x
          bootOmegaSEMatrix[lower.tri(bootOmegaSEMatrix)] <-
            t(bootOmegaSEMatrix)[lower.tri(bootOmegaSEMatrix)]
          bootOmegaSE <- cbind.data.frame(Scenario = "(B)",
                                          Label = randomEffectNames,
                                          bootOmegaSEMatrix)
          write.csv(bootOmegaSE, bootOmegaStderrFilename, row.names = FALSE)
        }
      }
    },
    error = function(ex) {
      warning(
        "Failed to summarize bootstrap, ERROR is: ",
        ex,
        immediate. = TRUE,
        call. = FALSE
      )
    })
  }
