skip_on_cran()
skip_on_os(c("mac", "solaris"))
skip_if_not_installed("marginaleffects")
skip_if_not_installed("glmmTMB")
skip_if_not_installed("lme4")
skip_if_not_installed("datawizard")

test_that("ggpredict", {
  # sample data set
  data(efc, package = "ggeffects")

  d <- datawizard::to_factor(efc, select = c("c161sex", "c172code", "c175empl"))
  d <- datawizard::recode_values(
    d,
    select = "c160age",
    recode = list(`1` = "min:40", `2` = 41:64, `3` = "65:max")
  )
  d <- datawizard::data_rename(
    d,
    select = c("c161sex", "c160age", "quol_5", "c175empl"),
    replacement = c("gender", "age", "qol", "employed")
  )
  d <- datawizard::data_modify(d, age = factor(age, labels = c("-40", "41-64", "65+")))

  m_null1 <- glmmTMB::glmmTMB(qol ~ e42dep + (1 | gender:employed:age), data = d)
  m_null2 <- lme4::lmer(qol ~ e42dep + (1 | gender:employed:age), data = d)

  # alert when margin is not empirical
  expect_message(
    predict_response(m_null1, c("gender", "employed", "age")),
    regex = "All focal terms"
  )
  expect_message(
    predict_response(m_null2, c("gender", "employed", "age")),
    regex = "All focal terms"
  )
  expect_silent(
    predict_response(m_null1, c("gender", "employed", "age"), type = "random")
  )
  expect_silent(
    predict_response(m_null1, c("gender", "employed", "age"), margin = "empirical")
  )
  expect_silent(
    predict_response(m_null2, c("gender", "employed", "age"), type = "random")
  )
  expect_silent(
    predict_response(m_null2, c("gender", "employed", "age"), margin = "empirical")
  )

  predictions <- predict_response(m_null1, c("gender", "employed", "age"), margin = "empirical")
  expect_silent(test_predictions(predictions))
  expect_message(
    test_predictions(m_null1, c("gender", "employed", "age")),
    regex = "All focal terms are included"
  )
})
