#' @importFrom sjmisc str_contains
#' @importFrom stats family
get_glm_family <- function(fit) {
  # get model class
  mc <- get_predict_function(fit)

  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  if (any(mc %in% c("lme", "plm", "gls"))) {
    fitfam <- "gaussian"
    logit_link <- FALSE
    link.fun <- "identity"
  } else if (any(mc %in% c("vgam", "vglm"))) {
    faminfo <- fit@family
    fitfam <- faminfo@vfamily
    logit_link <- sjmisc::str_contains(faminfo@blurb, "logit")
    link.fun <- faminfo@blurb[3]
  } else {
    # "lrm"-object from pkg "rms" have no family method
    # so we construct a logistic-regression-family-object
    if (mc == "lrm")
      faminfo <- stats::binomial(link = "logit")
    else
      # get family info
      faminfo <- stats::family(fit)

    fitfam <- faminfo$family
    logit_link <- faminfo$link == "logit"
    link.fun <- faminfo$link
  }

  # create logical for family
  binom_fam <- fitfam %in% c("binomial", "quasibinomial", "binomialff")
  poisson_fam <- fitfam %in% c("poisson", "quasipoisson")
  neg_bin_fam <- sjmisc::str_contains(fitfam, "negative binomial", ignore.case = T)

  return(
    list(
      is_bin = binom_fam,
      is_pois = poisson_fam | neg_bin_fam,
      is_negbin = neg_bin_fam,
      is_logit = logit_link,
      link.fun = link.fun,
      family = fitfam
    )
  )
}

