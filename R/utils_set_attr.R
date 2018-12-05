set_attributes_and_class <- function(data, model, t.title, x.title, y.title, l.title, legend.labels, x.axis.labels, faminfo, x.is.factor, full.data, constant.values = NULL, terms = NULL, n.trials = NULL) {
  # check correct labels
  if (!is.null(x.axis.labels) && length(x.axis.labels) != length(stats::na.omit(unique(data$x))))
    x.axis.labels <- as.vector(sort(stats::na.omit(unique(data$x))))

  rownames(data) <- NULL

  # add attributes
  attr(data, "title") <- t.title
  attr(data, "x.title") <- x.title
  attr(data, "y.title") <- y.title
  attr(data, "legend.title") <- l.title
  attr(data, "legend.labels") <- legend.labels
  attr(data, "x.axis.labels") <- x.axis.labels
  attr(data, "x.is.factor") <- x.is.factor
  attr(data, "full.data") <- full.data
  attr(data, "constant.values") <- constant.values
  attr(data, "terms") <- terms

  # remember fit family
  attr(data, "family") <- faminfo$family
  attr(data, "link") <- faminfo$link.fun
  attr(data, "logistic") <- ifelse(faminfo$is_bin, "1", "0")
  attr(data, "is.trial") <- ifelse(faminfo$is_trial && inherits(model, "brmsfit"), "1", "0")
  attr(data, "n.trials") <- n.trials

  # and model-function
  attr(data, "fitfun") <- get_model_function(model)

  # add class attribute
  class(data) <- c("ggeffects", class(data))

  data
}
