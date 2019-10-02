.set_attributes_and_class <- function(data, model, t.title, x.title, y.title, l.title, legend.labels, x.axis.labels, faminfo, constant.values = NULL, terms = NULL, ori.terms = NULL, at.list = NULL, n.trials = NULL, prediction.interval = NULL) {
  # check correct labels
  if (!is.null(x.axis.labels) && length(x.axis.labels) != length(stats::na.omit(unique(data$x))))
    x.axis.labels <- as.vector(sort(stats::na.omit(unique(data$x))))

  rownames(data) <- NULL

  if (!is.null(at.list) && !is.null(terms))
    at.list <- at.list[names(at.list) %in% terms]

  # add attributes
  attr(data, "title") <- t.title
  attr(data, "x.title") <- x.title
  attr(data, "y.title") <- y.title
  attr(data, "legend.title") <- l.title
  attr(data, "legend.labels") <- legend.labels
  attr(data, "x.axis.labels") <- x.axis.labels
  attr(data, "constant.values") <- constant.values
  attr(data, "terms") <- terms
  attr(data, "ori.terms") <- ori.terms
  attr(data, "at.list") <- at.list
  attr(data, "prediction.interval") <- prediction.interval

  # remember fit family
  attr(data, "family") <- faminfo$family
  attr(data, "link") <- faminfo$link_function
  attr(data, "logistic") <- ifelse(faminfo$is_binomial || faminfo$is_ordinal, "1", "0")
  attr(data, "is.trial") <- ifelse(faminfo$is_trial && inherits(model, "brmsfit"), "1", "0")
  attr(data, "n.trials") <- n.trials

  # and model-function
  attr(data, "fitfun") <- .get_model_function(model)

  # add class attribute
  class(data) <- c("ggeffects", class(data))

  data
}
