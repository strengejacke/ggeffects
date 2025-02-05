# test_predictions, mixed models, print with conditioned values

    Code
      print(test_predictions(fit, terms = c("e16sex", "c172code")))
    Output
      Model-based Contrasts Analysis
      
      Level1         | Level2         | Difference |   SE |         95% CI |     z |      p
      -------------------------------------------------------------------------------------
      male, medium   | male, low      |       0.25 | 0.18 | [-0.11,  0.61] |  1.37 |  0.169
      male, high     | male, low      |       0.45 | 0.28 | [-0.10,  0.99] |  1.61 |  0.107
      female, low    | male, low      |      -0.13 | 0.20 | [-0.51,  0.26] | -0.65 |  0.513
      female, medium | male, low      |       0.46 | 0.18 | [ 0.10,  0.81] |  2.51 |  0.012
      female, high   | male, low      |       0.98 | 0.26 | [ 0.47,  1.48] |  3.79 | < .001
      male, high     | male, medium   |       0.19 | 0.26 | [-0.32,  0.71] |  0.74 |  0.457
      female, low    | male, medium   |      -0.38 | 0.18 | [-0.74, -0.02] | -2.06 |  0.039
      female, medium | male, medium   |       0.20 | 0.16 | [-0.11,  0.52] |  1.28 |  0.199
      female, high   | male, medium   |       0.73 | 0.24 | [ 0.26,  1.19] |  3.06 |  0.002
      female, low    | male, high     |      -0.57 | 0.28 | [-1.12, -0.03] | -2.07 |  0.038
      female, medium | male, high     |       0.01 | 0.26 | [-0.49,  0.51] |  0.04 |  0.969
      female, high   | male, high     |       0.53 | 0.31 | [-0.07,  1.13] |  1.73 |  0.083
      female, medium | female, low    |       0.58 | 0.17 | [ 0.25,  0.92] |  3.41 | < .001
      female, high   | female, low    |       1.11 | 0.25 | [ 0.61,  1.60] |  4.35 | < .001
      female, high   | female, medium |       0.52 | 0.21 | [ 0.11,  0.93] |  2.48 |  0.013
      
      Variable predicted: tot_sc_e
      Predictors contrasted: e16sex, c172code
      Predictors averaged: e17age (79), e15relat (1)
      Contrasts are on the response-scale.

