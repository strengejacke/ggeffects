get_link_inverse <- function(fun, model) {
  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  if (any(fun %in% c("lme", "plm", "gls", "lm"))) {
    il <- NULL
  } else {
    # "lrm"-object from pkg "rms" have no family method
    # so we construct a logistic-regression-family-object
    if (fun == "lrm")
      il <- stats::binomial(link = "logit")$linkinv
    else
      # get family info
      il <- stats::family(model)$linkinv
  }

  il
}
