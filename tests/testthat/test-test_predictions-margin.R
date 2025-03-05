skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")

test_that("test_predictions, margin", {
  set.seed(123)
  d <- rbind(
    data.frame(
      childcare = sample(c(0, 1), 50, replace = TRUE, prob = c(0.8, 0.2)),
      parfam = factor(sample(c("agr", "con", "cd", "lib", "sd", "green", "left", "rr"), 50, replace = TRUE)),
      year = sample(1970:2022, 50, replace = TRUE),
      countryname = factor(sample(c("Austria", "Belgium", "Finland", "France", "Germany", "UK"), 50, replace = TRUE))
    ),
    data.frame(
      childcare = sample(c(0, 1), 50, replace = TRUE, prob = c(0.9, 0.1)),
      parfam = factor(sample(c("agr", "con", "cd", "rr"), 50, replace = TRUE)),
      year = sample(2000:2022, 50, replace = TRUE),
      countryname = factor(sample(c("France", "Germany", "UK"), 50, replace = TRUE))
    ),
    data.frame(
      childcare = sample(c(0, 1), 50, replace = TRUE, prob = c(0.5, 0.5)),
      parfam = factor(sample(c("lib", "sd", "green", "left"), 50, replace = TRUE)),
      year = sample(2000:2022, 50, replace = TRUE),
      countryname = factor(sample(c("Austria", "Belgium", "Finland"), 50, replace = TRUE))
    )
  )

  m <- glm(childcare ~ parfam * year + countryname,
    data = d,
    family = binomial
  )

  expect_snapshot(print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]"))))
  expect_snapshot(print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]"), margin = "marginalmeans")))

  expect_snapshot(print(test_predictions(m, c("parfam [green, lib]", "year [1980]"))))
  expect_snapshot(print(test_predictions(m, c("parfam [green, lib]", "year [1980]"), margin = "marginalmeans")))

  expect_snapshot(print(test_predictions(m, c("parfam [green, lib]", "year"))))
  expect_snapshot(print(test_predictions(m, c("parfam [green, lib]", "year"), margin = "marginalmeans")))
})
