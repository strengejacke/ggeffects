if (require("testthat")) {
  library(ggeffects)

  if (length(strsplit(packageDescription("ggeffects")$Version, "\\.")[[1]]) > 3) {
    Sys.setenv("RunAllggeffectsTests" = "yes")
  } else {
    Sys.setenv("RunAllggeffectsTests" = "no")
  }

  test_check("ggeffects")
}
