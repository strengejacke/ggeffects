.post_processing_labels <- function(model, mydf, original.model.frame, expanded_frame, cleaned.terms, original.terms, faminfo, type, prediction.interval, at.list, condition = NULL) {
  # get axis titles and labels
  all.labels <- .get_axis_titles_and_labels(
    fitfram = original.model.frame,
    terms = cleaned.terms,
    fun = .get_model_function(model),
    faminfo = faminfo,
    no.transform = FALSE,
    type = type
  )

  # set attributes with necessary information
  .set_attributes_and_class(
    data = mydf,
    model = model,
    t.title = all.labels$t.title,
    x.title = all.labels$x.title,
    y.title = all.labels$y.title,
    l.title = all.labels$l.title,
    legend.labels = attr(mydf, "legend.labels"),
    x.axis.labels = all.labels$axis.labels,
    faminfo = faminfo,
    constant.values = attr(expanded_frame, "constant.values", exact = TRUE),
    terms = cleaned.terms,
    ori.terms = original.terms,
    at.list = at.list,
    n.trials = attr(expanded_frame, "n.trials", exact = TRUE),
    prediction.interval = prediction.interval,
    condition = condition
  )
}
