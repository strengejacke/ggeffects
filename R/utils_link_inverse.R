#' @importFrom stats family
get_link_inverse <- function(fun, model) {

  # handle glmmTMB models
  if (inherits(model, "glmmTMB")) {
    ff <- stats::family(model)

    if ("linkinv" %in% names(ff))
      return(ff$linkinv)
    else
      return(match.fun("exp"))
  }

  # do we have glm? if so, get link family. make exceptions
  # for specific models that don't have family function
  if (any(fun %in% c("lme", "plm", "gls", "lm", "truncreg", "zeroinfl", "hurdle", "coxph"))) {
    il <- NULL
  } else if (fun %in% c("betareg")) {
    il <- model$link$mean$linkinv
  } else if (fun %in% c("lrm", "polr")) {
    # "lrm"-object from pkg "rms" have no family method
    # so we construct a logistic-regression-family-object
    il <- stats::binomial(link = "logit")$linkinv
  } else {
    # get family info
    il <- stats::family(model)$linkinv
  }

  il
}


get_link_fun <- function(model) {
  # get model family
  ff <- stats::family(model)

  # return link function, if exists
  if ("linkfun" %in% names(ff)) return(ff$linkfun)

  # else, create link function from link-string
  if ("link" %in% names(ff)) return(match.fun(ff$link))

  NULL
}
