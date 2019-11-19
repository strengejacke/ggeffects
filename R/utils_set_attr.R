.set_attributes_and_class <- function(data, model, t.title, x.title, y.title, l.title, legend.labels, x.axis.labels, model_info, constant.values = NULL, terms = NULL, original_terms = NULL, at.list = NULL, n.trials = NULL, prediction.interval = NULL, condition = NULL) {
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
  attr(data, "original.terms") <- original_terms
  attr(data, "at.list") <- at.list
  attr(data, "prediction.interval") <- prediction.interval
  attr(data, "condition") <- condition

  # remember fit family
  attr(data, "family") <- model_info$family
  attr(data, "link") <- model_info$link_function
  attr(data, "logistic") <- ifelse(model_info$is_binomial || model_info$is_ordinal, "1", "0")
  attr(data, "is.trial") <- ifelse(model_info$is_trial && inherits(model, "brmsfit"), "1", "0")
  attr(data, "n.trials") <- n.trials

  # and model-function
  attr(data, "fitfun") <- .get_model_function(model)

  # add class attribute
  class(data) <- c("ggeffects", class(data))

  data
}
