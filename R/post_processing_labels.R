.post_processing_labels <- function(model, mydf, original_model_frame, data_grid, cleaned_terms, original_terms, faminfo, type, prediction.interval, at.list, condition = NULL) {
  # get axis titles and labels
  all.labels <- .get_axis_titles_and_labels(
    original_model_frame = original_model_frame,
    terms = cleaned_terms,
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
    constant.values = attr(data_grid, "constant.values", exact = TRUE),
    terms = cleaned_terms,
    ori.terms = original_terms,
    at.list = at.list,
    n.trials = attr(data_grid, "n.trials", exact = TRUE),
    prediction.interval = prediction.interval,
    condition = condition
  )
}
