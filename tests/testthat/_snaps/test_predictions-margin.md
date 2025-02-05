# test_predictions, margin

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]")))
    Output
      Model-based Contrasts Analysis
      
      Level1      | Level2      | Difference |   SE |         95% CI |     z |     p
      ------------------------------------------------------------------------------
      green, 2020 | green, 1980 |       0.16 | 0.42 | [-0.66,  0.97] |  0.38 | 0.704
      lib, 1980   | green, 1980 |      -0.18 | 0.43 | [-1.02,  0.66] | -0.41 | 0.680
      lib, 2020   | green, 1980 |      -0.26 | 0.34 | [-0.93,  0.41] | -0.77 | 0.444
      lib, 1980   | green, 2020 |      -0.33 | 0.34 | [-1.00,  0.33] | -0.98 | 0.325
      lib, 2020   | green, 2020 |      -0.42 | 0.21 | [-0.83,  0.00] | -1.97 | 0.049
      lib, 2020   | lib, 1980   |      -0.08 | 0.33 | [-0.73,  0.56] | -0.25 | 0.799
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib'), year=c(1980,2020)
      Predictors averaged: countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]"),
      margin = "marginalmeans"))
    Output
      Marginal Contrasts Analysis
      
      Level1      | Level2      | Difference |   SE |         95% CI |     z |     p
      ------------------------------------------------------------------------------
      green, 2020 | green, 1980 |       0.12 | 0.32 | [-0.51,  0.74] |  0.37 | 0.708
      lib, 1980   | green, 1980 |      -0.14 | 0.35 | [-0.82,  0.54] | -0.41 | 0.682
      lib, 2020   | green, 1980 |      -0.22 | 0.26 | [-0.72,  0.28] | -0.85 | 0.394
      lib, 1980   | green, 2020 |      -0.26 | 0.29 | [-0.83,  0.30] | -0.91 | 0.365
      lib, 2020   | green, 2020 |      -0.34 | 0.16 | [-0.65, -0.02] | -2.09 | 0.036
      lib, 2020   | lib, 1980   |      -0.08 | 0.29 | [-0.63,  0.48] | -0.27 | 0.791
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib'), year=c(1980,2020)
      Predictors averaged: countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]"),
      margin = "empirical"))
    Output
      Marginal Contrasts Analysis
      
      Level1      | Level2      | Difference |   SE |         95% CI |     z |     p
      ------------------------------------------------------------------------------
      green, 2020 | green, 1980 |       0.12 | 0.31 | [-0.50,  0.73] |  0.37 | 0.709
      lib, 1980   | green, 1980 |      -0.14 | 0.34 | [-0.81,  0.53] | -0.41 | 0.682
      lib, 2020   | green, 1980 |      -0.21 | 0.25 | [-0.71,  0.28] | -0.86 | 0.392
      lib, 1980   | green, 2020 |      -0.26 | 0.28 | [-0.81,  0.30] | -0.90 | 0.367
      lib, 2020   | green, 2020 |      -0.33 | 0.16 | [-0.64, -0.02] | -2.09 | 0.037
      lib, 2020   | lib, 1980   |      -0.07 | 0.28 | [-0.63,  0.48] | -0.27 | 0.791
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib'), year=c(1980,2020)
      Predictors averaged: countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]")))
    Output
      Model-based Contrasts Analysis
      
      Parameter   | Difference |   SE |        95% CI |     z |     p
      ---------------------------------------------------------------
      lib - green |      -0.18 | 0.43 | [-1.02, 0.66] | -0.41 | 0.680
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib'), year=c(1980)
      Predictors averaged: countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]"), margin = "marginalmeans"))
    Output
      Marginal Contrasts Analysis
      
      Parameter   | Difference |   SE |        95% CI |     z |     p
      ---------------------------------------------------------------
      lib - green |      -0.14 | 0.35 | [-0.82, 0.54] | -0.41 | 0.682
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib'), year=c(1980)
      Predictors averaged: countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]"), margin = "empirical"))
    Output
      Marginal Contrasts Analysis
      
      Parameter   | Difference |   SE |        95% CI |     z |     p
      ---------------------------------------------------------------
      lib - green |      -0.14 | 0.34 | [-0.81, 0.53] | -0.41 | 0.682
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib'), year=c(1980)
      Predictors averaged: countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year")))
    Output
      Model-based Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |         95% CI |     z |     p
      --------------------------------------------------------------------
      lib    | green  |      -0.34 | 0.17 | [-0.68, -0.01] | -2.04 | 0.041
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib')
      Predictors averaged: year (2e+03), countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year"), margin = "marginalmeans"))
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |         95% CI |     z |     p
      --------------------------------------------------------------------
      lib    | green  |      -0.28 | 0.13 | [-0.52, -0.03] | -2.21 | 0.027
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib')
      Predictors averaged: year (2e+03), countryname
      Contrasts are on the response-scale.

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year"), margin = "empirical"))
    Output
      Marginal Contrasts Analysis
      
      Level1 | Level2 | Difference |   SE |         95% CI |     z |     p
      --------------------------------------------------------------------
      lib    | green  |      -0.28 | 0.12 | [-0.51, -0.04] | -2.28 | 0.023
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','lib')
      Predictors averaged: year (2e+03), countryname
      Contrasts are on the response-scale.

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam", "countryname"), margin = "marginalmeans",
      test = "b19 = b26")))
    Output
      Marginal Contrasts Analysis
      
      Parameter | Difference |   SE |        95% CI |    z |     p
      ------------------------------------------------------------
      b19=b26   |       0.34 | 0.28 | [-0.20, 0.89] | 1.24 | 0.216
      
      Variable predicted: childcare
      Predictors contrasted: parfam, countryname
      Predictors averaged: year (2e+03)
      Contrasts are on the response-scale.
      Parameters:
      b19 = parfam [con], countryname [Finland]
      b26 = parfam [cd], countryname [France]

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam [green, left]",
        "countryname [Austria, Belgium]"), margin = "marginalmeans")))
    Output
      Marginal Contrasts Analysis
      
      Level1         | Level2         | Difference |   SE |        95% CI |     z |     p
      -----------------------------------------------------------------------------------
      green, Belgium | green, Austria |       0.45 | 0.33 | [-0.20, 1.10] |  1.35 | 0.176
      left, Austria  | green, Austria |       0.17 | 0.38 | [-0.57, 0.91] |  0.46 | 0.648
      left, Belgium  | green, Austria |       0.37 | 0.33 | [-0.28, 1.01] |  1.11 | 0.266
      left, Austria  | green, Belgium |      -0.28 | 0.31 | [-0.87, 0.32] | -0.91 | 0.364
      left, Belgium  | green, Belgium |      -0.08 | 0.24 | [-0.56, 0.39] | -0.35 | 0.730
      left, Belgium  | left, Austria  |       0.19 | 0.30 | [-0.40, 0.78] |  0.64 | 0.521
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','left'), countryname=c('Austria','Belgium')
      Predictors averaged: year (2e+03)
      Contrasts are on the response-scale.

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam", "countryname"), margin = "empirical",
      test = "b19 = b26")))
    Output
      Marginal Contrasts Analysis
      
      Parameter | Difference |   SE |        95% CI |    z |     p
      ------------------------------------------------------------
      b19=b26   |       0.35 | 0.27 | [-0.19, 0.88] | 1.26 | 0.208
      
      Variable predicted: childcare
      Predictors contrasted: parfam, countryname
      Predictors averaged: year (2e+03)
      Contrasts are on the response-scale.
      Parameters:
      b19 = parfam [con], countryname [Finland]
      b26 = parfam [cd], countryname [France]

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam [green, left]",
        "countryname [Austria, Belgium]"), margin = "empirical")))
    Output
      Marginal Contrasts Analysis
      
      Level1         | Level2         | Difference |   SE |        95% CI |     z |     p
      -----------------------------------------------------------------------------------
      green, Belgium | green, Austria |       0.44 | 0.33 | [-0.20, 1.09] |  1.35 | 0.178
      left, Austria  | green, Austria |       0.17 | 0.37 | [-0.56, 0.90] |  0.46 | 0.648
      left, Belgium  | green, Austria |       0.36 | 0.33 | [-0.28, 1.00] |  1.11 | 0.268
      left, Austria  | green, Belgium |      -0.27 | 0.30 | [-0.87, 0.32] | -0.91 | 0.364
      left, Belgium  | green, Belgium |      -0.08 | 0.24 | [-0.56, 0.39] | -0.35 | 0.730
      left, Belgium  | left, Austria  |       0.19 | 0.30 | [-0.39, 0.78] |  0.64 | 0.521
      
      Variable predicted: childcare
      Predictors contrasted: parfam=c('green','left'), countryname=c('Austria','Belgium')
      Predictors averaged: year (2e+03)
      Contrasts are on the response-scale.

