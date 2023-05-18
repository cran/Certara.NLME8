test_that("running simulation with expected integration warnings ", {
  skip_on_cran()

  action <- "simIntegrationErrors"

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
      "GENERIC",
      "none",
      INSTALLDIR,
      NLME_ROOT_DIRECTORY,
      NLME_ROOT_DIRECTORY,
      "jobControlFile.txt",
      1,
      action
    ),
    reportProgress = FALSE
  )

  testthat::expect_warning(do.call("performParallelNLMERun", args))
})
