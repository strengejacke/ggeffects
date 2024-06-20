skip_on_os(c("mac", "solaris"))
skip_if_not_installed("betareg")
skip_if_not_installed("datawizard")

test_that("predictions and SE with character focal terms", {
  n_size <- 2000
  set.seed(1)
  ex <- data.frame(
    x = rnorm(n_size),
    group = sample(letters[1:4], size = n_size, replace = TRUE),
    stringsAsFactors = FALSE
  )
  ex <- datawizard::data_modify(
    ex,
    group_value = datawizard::recode_into(
      group == "a" ~ 1,
      group == "b" ~ 2,
      group == "c" ~ 0,
      group == "d" ~ -1
    ),
    y_latent = x + rnorm(n_size) + group_value,
    y = pnorm(y_latent, sd = 3)
  )

  beta_fit <- betareg(y ~ x + group, data = ex)
  expect_warning(
    ggpredict(beta_fit, terms = c("x", "group [a, b]")),
    regex = "Some of the focal terms are of type `character`"
  )
})


test_that("predictions and SE work with factor focal terms", {
  n_size <- 2000
  set.seed(1)
  ex <- data.frame(
    x = rnorm(n_size),
    group = sample(letters[1:4], size = n_size, replace = TRUE),
    stringsAsFactors = FALSE
  )
  ex <- datawizard::data_modify(
    ex,
    group_value = datawizard::recode_into(
      group == "a" ~ 1,
      group == "b" ~ 2,
      group == "c" ~ 0,
      group == "d" ~ -1
    ),
    y_latent = x + rnorm(n_size) + group_value,
    y = pnorm(y_latent, sd = 3),
    group = as.factor(group)
  )

  beta_fit <- betareg(y ~ x + group, data = ex)
  out <- ggpredict(beta_fit, terms = c("x", "group [a, b]"))
  expect_false(anyNA(out$conf.low))
})
