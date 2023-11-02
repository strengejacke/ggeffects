skip_on_os(c("mac", "solaris"))
skip_if_not_installed("tibble")
test_that("ggeffect, tibble", {
  mtcars_tbl <- tibble::as_tibble(mtcars)
  fm2 <- lm(mpg ~ cyl + disp, mtcars_tbl)
  out <- ggpredict(fm2, terms = "cyl")
  expect_s3_class(out, "data.frame")
})
