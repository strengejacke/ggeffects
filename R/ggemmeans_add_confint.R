#' @importFrom stats confint
#' @importFrom sjmisc var_rename
.ggemmeans_add_confint <- function(tmp, ci.lvl) {
  suppressWarnings(
    tmp %>%
      stats::confint(level = ci.lvl) %>%
      as.data.frame() %>%
      sjmisc::var_rename(
        SE = "std.error",
        emmean = "predicted",
        lower.CL = "conf.low",
        upper.CL = "conf.high",
        prob = "predicted",
        asymp.LCL = "conf.low",
        asymp.UCL = "conf.high",
        lower.HPD = "conf.low",
        upper.HPD = "conf.high"
      )
  )
}
