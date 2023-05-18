test_that("running performStepwiseCovarSearch ", {
  skip_on_cran()

  action <- "performStepwiseCovarSearch"

  resultsPattern <- "Overall"
  resultsName <- paste0(resultsPattern, ".expected")

  INSTALLDIR <- Sys.getenv("INSTALLDIR")
  if (INSTALLDIR == "") {
    # the variable is not set
    stop("cannot start the test, INSTALLDIR variable is not specified.")
  }

  NLME_ROOT_DIRECTORY <- file.path(tempdir(TRUE), action)
  dir.create(NLME_ROOT_DIRECTORY)
  Sys.setenv("NLME_ROOT_DIRECTORY" = NLME_ROOT_DIRECTORY)

  DataLocation <- system.file(file.path("extdata", action),
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
      "multicore",
      INSTALLDIR,
      NLME_ROOT_DIRECTORY,
      NLME_ROOT_DIRECTORY,
      "test.mdl",
      "jobArgsCombined.txt",
      "jobArgsCombined.txt data1.txt cols1.txt test.mdl",
      4,
      "V-BodyWeight Cl-BodyWeight V2-Gender Cl2-Gender",
      "-2LL:1,1,1,1",
      0.01,
      0.001,
      2,
      action
    ),
    reportProgress = TRUE
  )

  results <- do.call(action, args)

  oldResults <- read.csv(file.path(
    NLME_ROOT_DIRECTORY,
    resultsName
  ), check.names = FALSE)
  testthat::expect_equal(results, oldResults)
})
