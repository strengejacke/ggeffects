m <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)

nd <- ggeffects::new_data(m, "Species", condition = c(Sepal.Width = 50))


m <- lm(Sepal.Length ~ Species + Sepal.Width, data = iris)
Terms <- delete.response(terms(m))
mf <- model.frame(Terms, data = nd)
X <- model.matrix(Terms, mf)

# predictions
beta <- coef(m)
result <- as.numeric(crossprod(beta, t(X)))

# SE
sqrt(diag(X %*% vcov(m) %*% t(X)))

# Pool se?
# like: sqrt(mean(SEs^2)), see parameters::pool_parameters()

ggeffects:::.safe_se_from_vcov(
  m,
  nd,
  value_adjustment = "mean",
  vcov.fun = NULL,
  terms = "Species",
  condition = c(Sepal.Width = 50),
  type = "fixed",
  vcov.args = NULL,
  vcov.type = NULL,
  interval = NULL,
  model_class = "lm"
)
