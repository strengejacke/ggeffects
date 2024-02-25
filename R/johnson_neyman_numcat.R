johnson_neyman_numcat <- function(x,
                                  model_data = NULL,
                                  focal_terms = NULL,
                                  original_terms = NULL,
                                  numeric_focal = NULL,
                                  dot_args = NULL,
                                  precision = 500) {
  out <- do.call(rbind, lapply(sort(unique(efc$c12hour)), function(i) {
    test_predictions(m, terms = c("c172code", paste0("c12hour [", i, "]")))
  }))
  out$x <- as.numeric(gsub("(\\d.*)-(\\d.*)", "\\1", out$c12hour))
  out$color <- ifelse(out$p.value < 0.05, "sig", "non-sig")

}


# library(ggeffects)
#   library(ggplot2)
#   ggplot(out, aes(x = x, y = Contrast, ymin = conf.low, ymax = conf.high, colour = color, fill = color)) +
#     geom_ribbon(alpha = 0.2, colour = NA) +
#     geom_line() +
#     facet_wrap(~c172code)

# data(efc, package = "ggeffects")
# efc$c172code <- as.factor(efc$c172code)
# m <- lm(neg_c_7 ~ c12hour * c172code, data = efc)
# ggpredict(m, terms = c("c12hour", "c172code")) |> plot()
# test_predictions(m, terms = c("c172code", "c12hour"))
# test_predictions(m, terms = c("c12hour [105]", "c172code")) |> as.data.frame()

# head(unique(sort(efc$c12hour)))
# library(marginaleffects)
# nd <- data_grid(m, c("c172code"))
# marginaleffects::predictions(m, newdata = nd, variables = "c12hour", hypothesis = "pairwise")


# test_predictions(m, terms = c("c172code", "c12hour [5]"))


#         model,
#         by = by_arg,
#         newdata = datagrid,
#         hypothesis = test,
#         df = df,
#         conf_level = ci_level
#       )


# m2 <- lm(neg_c_7 ~ c12hour * barthtot, data = efc)
# pr <- predict_response(m2, c("c12hour", "barthtot"))
# out2 <- johnson_neyman(pr)
# head(as.data.frame(out2))
# head(as.data.frame(out))
