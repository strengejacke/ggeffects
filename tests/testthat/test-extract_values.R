if (requiet("testthat") && requiet("ggeffects")) {
  test_that("values_at / pretty_range", {
    x <- 1:1000
    expect_equal(pretty_range(n = 5)(x),
                 pretty_range(x, n = 5))

    expect_equal(values_at(values = "meansd")(x),
                 values_at(x, values = "meansd"))

    expect_equal(values_at(values = "minmax")(x),
                 values_at(x, values = "minmax"))
  })
}
