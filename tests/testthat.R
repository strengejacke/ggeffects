if (require("testthat")) {
  library(ggeffects)

  if (length(strsplit(packageDescription("ggeffects")$Version, "\\.")[[1]]) > 3) {
    Sys.setenv("RunAllggeffectsTests" = "yes")
  } else {
    Sys.setenv("RunAllggeffectsTests" = "no")
  }


  si <- Sys.info()

  osx <- tryCatch(
    {
      if (!is.null(si["sysname"])) {
        si["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )


  solaris <- tryCatch(
    {
      if (!is.null(si["sysname"])) {
        grepl("SunOS", si["sysname"], ignore.case = TRUE)
      } else {
        FALSE
      }
    },
    error = function(e) {
      FALSE
    }
  )

  if (!osx && !solaris) {
    test_check("ggeffects")
  }
}
