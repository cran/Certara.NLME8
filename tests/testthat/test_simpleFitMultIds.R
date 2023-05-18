test_that("performParallelNLMERun works with multiple IDs", {
  skip_on_cran()

  action <- "performParallelNLMERun"

  resultsPattern <- c(
    "EtaEta", "EtaCov", "EtaCovariate", "EtaCovariateCat",
    "StrCovariate", "StrCovariateCat", "nonParEtaResult"
  )

  INSTALLDIR <- Sys.getenv("INSTALLDIR")
  if (INSTALLDIR == "") {
    # the variable is not set
    stop("cannot start the test, INSTALLDIR variable is not specified.")
  }

  NLME_ROOT_DIRECTORY <- file.path(tempdir(TRUE), paste(action, "multipleIDs"))
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  DataLocation <- system.file(file.path("extdata", action, "multipleIds"),
    package = "Certara.NLME8",
    mustWork = TRUE
  )

  DataFiles <- list.files(DataLocation, pattern = "*")
  file.copy(file.path(DataLocation, DataFiles),
    NLME_ROOT_DIRECTORY,
    overwrite = TRUE
  )

  args <- list(
    args = c(
      "GENERIC",
      "none",
      INSTALLDIR,
      NLME_ROOT_DIRECTORY,
      NLME_ROOT_DIRECTORY,
      "jobControlFile.txt",
      1,
      "multipleIDs",
      "c(\"tvKa\", \"tvV\", \"tvCl\")"
    ),
    reportProgress = TRUE
  )

  do.call(action, args)

  for (epxectedFile in resultsPattern) {
    result <- read.csv(file.path(
      NLME_ROOT_DIRECTORY,
      paste0(epxectedFile, ".csv")
    ))
    expectedResult <-
      read.csv(file.path(
        NLME_ROOT_DIRECTORY,
        paste0(epxectedFile, ".expected")
      ))
    testthat::expect_equal(result, expectedResult)
  }
})
