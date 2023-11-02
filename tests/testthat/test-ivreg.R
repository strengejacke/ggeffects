skip_on_cran()
skip_if_not_installed("AER")

test_that("ggpredict", {
  data(CigarettesSW, package = "AER")
  CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
  CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
  CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

  m1 <- AER::ivreg(
    log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
    data = CigarettesSW,
    subset = year == "1995"
  )
  p <- ggpredict(m1, "rprice")
  expect_equal(p$predicted[1], 125.4697, tolerance = 1e-1)
})
