if (suppressWarnings(requiet("testthat") && requiet("ggeffects"))) {
  test_that("check vars", {
    data(iris)
    m_check <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
    expect_error(
      ggpredict(m_check, terms = "Specis"),
      regex = "\"Species\""
    )
    m_check <- lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
    expect_error(
      ggpredict(m_check, terms = c("Sepal.Lenght", "Specis")),
      regex = "\"Sepal.Length\", \"Species\""
    )
  })
}
