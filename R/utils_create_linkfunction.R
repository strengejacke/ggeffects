create_link_function <- function(fam, link) {
  # for generalized linear models, we need the family and link function first
  if (fam == "binomial")
    ff <- stats::binomial(link = link)
  else if (fam == "poisson")
    ff <- stats::poisson(link = link)
  else if (fam == "gaussian")
    ff <- stats::gaussian(link = link)
  else if (fam == "Gamma")
    ff <- stats::Gamma(link = link)
  else if (fam == "inverse.gaussian")
    ff <- stats::inverse.gaussian(link = link)
  else if (fam == "quasi")
    ff <- stats::quasi(link = link)
  else if (fam == "quasibinomial")
    ff <- stats::quasibinomial(link = link)
  else if (fam == "quasipoisson")
    ff <- stats::quasipoisson(link = link)

  ff
}
