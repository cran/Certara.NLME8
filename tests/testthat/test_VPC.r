test_that("running performVPC ", {
  skip_on_cran()

  action <- "performVPC"

  resultsPattern <- "predout"
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
  file.copy(from = file.path(DataLocation, DataFiles),
    to = NLME_ROOT_DIRECTORY,
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
      action
    ),
    reportProgress = TRUE
  )

  do.call("performParallelNLMERun", args)
  results <- read.csv(file.path(
    NLME_ROOT_DIRECTORY,
    paste0(resultsPattern, ".csv")
  ))

  oldResults <- read.csv(file.path(
    NLME_ROOT_DIRECTORY,
    resultsName
  ))
  testthat::expect_equal(results, oldResults)
})
