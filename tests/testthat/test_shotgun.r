test_that("running performShotgunCovarSearch ", {
  skip_on_cran()

  action <- "performShotgunCovarSearch"

  resultsPattern <- "Overall"
  resultsName <- paste0(resultsPattern, ".expected")

  INSTALLDIR <- Sys.getenv("INSTALLDIR")
  if (INSTALLDIR == "") {
    # the variable is not set
    stop("cannot start the test, INSTALLDIR variable is not specified.")
  }

  NLME_ROOT_DIRECTORY <- file.path(tempdir(TRUE), paste(action, "dd"))
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
      "COVAR_SEARCH",
      "multicore",
      INSTALLDIR,
      NLME_ROOT_DIRECTORY,
      NLME_ROOT_DIRECTORY,
      "newNlmeControlFile.txt",
      2,
      action
    ),
    reportProgress = FALSE
  )

  do.call(action, args)
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
