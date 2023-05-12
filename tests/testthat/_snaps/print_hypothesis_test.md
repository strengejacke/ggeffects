# print hypothesis_test simple contrast link scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |       95% CI |     p
      -------------------------------------------
      0-1       |     0.48 | [0.23, 0.74] | 0.901
    Message <simpleMessage>
      
      Contrasts are presented as log-odds. Use `type = "response"` to return
        contrasts on the response-scale or `transform_post = "exp"` to return
        exponentiated contrasts.

# print hypothesis_test simple predictions link scale

    Code
      print(out)
    Output
      var_binom | Predicted |       95% CI |     p
      --------------------------------------------
      0         |      0.24 | [0.11, 0.45] | 0.015
      1         |      0.26 | [0.10, 0.53] | 0.076
    Message <simpleMessage>
      
      Predictions are presented as log-odds. Use `type = "response"` to
        return predictions on the response-scale or `transform_post = "exp"` to
        return exponentiated predictions.

# print hypothesis_test simple contrast exp scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |       95% CI |     p
      -------------------------------------------
      0-1       |     0.99 | [0.80, 1.22] | 0.902
    Message <simpleMessage>
      
      Contrasts are presented as odds ratios.

# print hypothesis_test simple contrast response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |        95% CI |     p
      --------------------------------------------
      0-1       |    -0.01 | [-0.22, 0.20] | 0.902
    Message <simpleMessage>
      
      Contrasts are presented as probabilities.

# print hypothesis_test simple predictions exp scale

    Code
      print(out)
    Output
      var_binom | Predicted |       95% CI |     p
      --------------------------------------------
      0         |      1.27 | [1.08, 1.51] | 0.005
      1         |      1.29 | [1.03, 1.62] | 0.025
    Message <simpleMessage>
      
      Predictions are presented as odds ratios.

# print hypothesis_test contrasts link scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont | Contrast |       95% CI |     p
      ---------------------------------------------------------
      0-1       |   2.48-2.48 |     0.41 | [0.13, 0.77] | 0.644
      0-0       |   2.48-9.25 |     0.47 | [0.36, 0.59] | 0.610
      0-1       |   2.48-9.25 |     0.45 | [0.19, 0.74] | 0.758
      0-0       |  2.48-16.02 |     0.44 | [0.23, 0.67] | 0.610
      0-1       |  2.48-16.02 |     0.49 | [0.13, 0.86] | 0.980
      1-0       |   2.48-9.25 |     0.56 | [0.23, 0.85] | 0.745
      1-1       |   2.48-9.25 |     0.54 | [0.27, 0.79] | 0.774
      1-0       |  2.48-16.02 |     0.53 | [0.20, 0.84] | 0.878
      1-1       |  2.48-16.02 |     0.58 | [0.12, 0.94] | 0.774
      0-1       |   9.25-9.25 |     0.48 | [0.23, 0.74] | 0.901
      0-0       |  9.25-16.02 |     0.47 | [0.36, 0.59] | 0.610
      0-1       |  9.25-16.02 |     0.52 | [0.16, 0.87] | 0.912
      1-0       |  9.25-16.02 |     0.49 | [0.22, 0.76] | 0.932
      1-1       |  9.25-16.02 |     0.54 | [0.27, 0.79] | 0.774
      0-1       | 16.02-16.02 |     0.56 | [0.17, 0.88] | 0.810
    Message <simpleMessage>
      
      Contrasts are presented as log-odds. Use `type = "response"` to return
        contrasts on the response-scale or `transform_post = "exp"` to return
        exponentiated contrasts.

# print hypothesis_test predictions link scale

    Code
      print(out)
    Output
      var_binom | var_cont | Predicted |       95% CI |     p
      -------------------------------------------------------
      0         |     2.48 |      0.22 | [0.09, 0.45] | 0.021
      1         |     2.48 |      0.29 | [0.08, 0.65] | 0.240
      0         |     9.25 |      0.24 | [0.11, 0.45] | 0.015
      1         |     9.25 |      0.26 | [0.10, 0.53] | 0.076
      0         |    16.02 |      0.27 | [0.12, 0.50] | 0.046
      1         |    16.02 |      0.22 | [0.05, 0.64] | 0.179
    Message <simpleMessage>
      
      Predictions are presented as log-odds. Use `type = "response"` to
        return predictions on the response-scale or `transform_post = "exp"` to
        return exponentiated predictions.

# print hypothesis_test contrasts exp scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont | Contrast |       95% CI |     p
      ---------------------------------------------------------
      0-1       |   2.48-2.48 |     0.93 | [0.69, 1.27] | 0.658
      0-0       |   2.48-9.25 |     0.98 | [0.90, 1.06] | 0.596
      0-1       |   2.48-9.25 |     0.97 | [0.77, 1.21] | 0.760
      0-0       |  2.48-16.02 |     0.96 | [0.80, 1.14] | 0.608
      0-1       |  2.48-16.02 |     1.00 | [0.72, 1.37] | 0.980
      1-0       |   2.48-9.25 |     1.05 | [0.78, 1.41] | 0.754
      1-1       |   2.48-9.25 |     1.03 | [0.82, 1.31] | 0.780
      1-0       |  2.48-16.02 |     1.02 | [0.75, 1.39] | 0.880
      1-1       |  2.48-16.02 |     1.07 | [0.69, 1.65] | 0.771
      0-1       |   9.25-9.25 |     0.99 | [0.80, 1.22] | 0.902
      0-0       |  9.25-16.02 |     0.98 | [0.89, 1.07] | 0.619
      0-1       |  9.25-16.02 |     1.02 | [0.75, 1.39] | 0.910
      1-0       |  9.25-16.02 |     0.99 | [0.79, 1.25] | 0.932
      1-1       |  9.25-16.02 |     1.03 | [0.84, 1.26] | 0.761
      0-1       | 16.02-16.02 |     1.04 | [0.75, 1.44] | 0.803
    Message <simpleMessage>
      
      Contrasts are presented as odds ratios.

# print hypothesis_test contrasts response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont |  Contrast |        95% CI |     p
      -----------------------------------------------------------
      0-1       |   2.48-2.48 |     -0.07 | [-0.38, 0.24] | 0.658
      0-0       |   2.48-9.25 |     -0.02 | [-0.10, 0.06] | 0.596
      0-1       |   2.48-9.25 |     -0.04 | [-0.26, 0.19] | 0.760
      0-0       |  2.48-16.02 |     -0.05 | [-0.22, 0.13] | 0.608
      0-1       |  2.48-16.02 | -4.06e-03 | [-0.32, 0.32] | 0.980
      1-0       |   2.48-9.25 |      0.05 | [-0.25, 0.34] | 0.754
      1-1       |   2.48-9.25 |      0.03 | [-0.20, 0.27] | 0.780
      1-0       |  2.48-16.02 |      0.02 | [-0.28, 0.33] | 0.880
      1-1       |  2.48-16.02 |      0.06 | [-0.37, 0.50] | 0.771
      0-1       |   9.25-9.25 |     -0.01 | [-0.22, 0.20] | 0.902
      0-0       |  9.25-16.02 |     -0.02 | [-0.12, 0.07] | 0.619
      0-1       |  9.25-16.02 |      0.02 | [-0.29, 0.33] | 0.910
      1-0       |  9.25-16.02 |     -0.01 | [-0.24, 0.22] | 0.932
      1-1       |  9.25-16.02 |      0.03 | [-0.17, 0.23] | 0.761
      0-1       | 16.02-16.02 |      0.04 | [-0.28, 0.36] | 0.803
    Message <simpleMessage>
      
      Contrasts are presented as probabilities.

# print hypothesis_test predictions exp scale

    Code
      print(out)
    Output
      var_binom | var_cont | Predicted |       95% CI |     p
      -------------------------------------------------------
      0         |     2.48 |      1.25 | [1.04, 1.50] | 0.019
      1         |     2.48 |      1.34 | [0.98, 1.82] | 0.065
      0         |     9.25 |      1.27 | [1.08, 1.51] | 0.005
      1         |     9.25 |      1.29 | [1.03, 1.62] | 0.025
      0         |    16.02 |      1.30 | [1.07, 1.59] | 0.007
      1         |    16.02 |      1.25 | [0.91, 1.71] | 0.161
    Message <simpleMessage>
      
      Predictions are presented as odds ratios.

# print hypothesis_test comma and dash levels

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                                      |  f2 | Contrast |        95% CI |     p
      --------------------------------------------------------------------------------
      and, another, comma-and, another, comma | a-b |     0.02 | [-0.12, 0.16] | 0.796
      and, another, comma-no comma            | a-a |    -0.15 | [-0.33, 0.03] | 0.099
      and, another, comma-no comma            | a-b |    -0.13 | [-0.36, 0.10] | 0.255
      and, another, comma-with, comma         | a-a |    -0.07 | [-0.24, 0.09] | 0.380
      and, another, comma-with, comma         | a-b |    -0.06 | [-0.27, 0.16] | 0.610
      and, another, comma-no comma            | b-a |    -0.17 | [-0.40, 0.06] | 0.147
      and, another, comma-no comma            | b-b |    -0.15 | [-0.33, 0.03] | 0.099
      and, another, comma-with, comma         | b-a |    -0.09 | [-0.32, 0.13] | 0.412
      and, another, comma-with, comma         | b-b |    -0.07 | [-0.24, 0.09] | 0.380
      no comma-no comma                       | a-b |     0.02 | [-0.12, 0.16] | 0.796
      no comma-with, comma                    | a-a |     0.08 | [-0.10, 0.26] | 0.402
      no comma-with, comma                    | a-b |     0.10 | [-0.13, 0.32] | 0.407
      no comma-with, comma                    | b-a |     0.06 | [-0.18, 0.29] | 0.625
      no comma-with, comma                    | b-b |     0.08 | [-0.10, 0.26] | 0.402
      with, comma-with, comma                 | a-b |     0.02 | [-0.12, 0.16] | 0.796
    Message <simpleMessage>
      
      Contrasts are presented on the response-scale.

---

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                                      |                      f2 | Contrast |        95% CI |     p
      ----------------------------------------------------------------------------------------------------
      and, another, comma-and, another, comma |     comma, here-nothere |     0.02 | [-0.12, 0.16] | 0.796
      and, another, comma-no comma            | comma, here-comma, here |    -0.15 | [-0.33, 0.03] | 0.099
      and, another, comma-no comma            |     comma, here-nothere |    -0.13 | [-0.36, 0.10] | 0.255
      and, another, comma-with, comma         | comma, here-comma, here |    -0.07 | [-0.24, 0.09] | 0.380
      and, another, comma-with, comma         |     comma, here-nothere |    -0.06 | [-0.27, 0.16] | 0.610
      and, another, comma-no comma            |     nothere-comma, here |    -0.17 | [-0.40, 0.06] | 0.147
      and, another, comma-no comma            |         nothere-nothere |    -0.15 | [-0.33, 0.03] | 0.099
      and, another, comma-with, comma         |     nothere-comma, here |    -0.09 | [-0.32, 0.13] | 0.412
      and, another, comma-with, comma         |         nothere-nothere |    -0.07 | [-0.24, 0.09] | 0.380
      no comma-no comma                       |     comma, here-nothere |     0.02 | [-0.12, 0.16] | 0.796
      no comma-with, comma                    | comma, here-comma, here |     0.08 | [-0.10, 0.26] | 0.402
      no comma-with, comma                    |     comma, here-nothere |     0.10 | [-0.13, 0.32] | 0.407
      no comma-with, comma                    |     nothere-comma, here |     0.06 | [-0.18, 0.29] | 0.625
      no comma-with, comma                    |         nothere-nothere |     0.08 | [-0.10, 0.26] | 0.402
      with, comma-with, comma                 |     comma, here-nothere |     0.02 | [-0.12, 0.16] | 0.796
    Message <simpleMessage>
      
      Contrasts are presented on the response-scale.

---

    Code
      print(ht)
    Output
      # Linear trend for Sepal.Width
      
      f1                                      |  f2 |  Contrast |        95% CI |      p
      ----------------------------------------------------------------------------------
      and, another, comma-and, another, comma | a-b |      0.00 | [ 0.00, 0.00] | > .999
      and, another, comma-no comma            | a-a |      0.00 | [ 0.00, 0.00] | > .999
      and, another, comma-no comma            | a-b |      0.00 | [ 0.00, 0.00] | > .999
      and, another, comma-with, comma         | a-a | -1.90e-13 | [ 0.00, 0.00] | > .999
      and, another, comma-with, comma         | a-b | -1.90e-13 | [ 0.00, 0.00] | > .999
      and, another, comma-no comma            | b-b |      0.00 | [ 0.00, 0.00] | > .999
      and, another, comma-with, comma         | b-b | -1.90e-13 | [ 0.00, 0.00] | > .999
      no comma-and, another, comma            | a-b |      0.00 | [ 0.00, 0.00] | > .999
      no comma-no comma                       | a-b |      0.00 | [ 0.00, 0.00] | > .999
      no comma-with, comma                    | a-a | -1.90e-13 | [ 0.00, 0.00] | > .999
      no comma-with, comma                    | a-b | -1.90e-13 | [ 0.00, 0.00] | > .999
      no comma-with, comma                    | b-b | -1.90e-13 | [ 0.00, 0.00] | > .999
      with, comma-and, another, comma         | a-b |  1.90e-13 | [ 0.00, 0.00] | > .999
      with, comma-no comma                    | a-b |  1.90e-13 | [ 0.00, 0.00] | > .999
      with, comma-with, comma                 | a-b |      0.00 | [ 0.00, 0.00] | > .999
    Message <simpleMessage>
      
      Contrasts are presented on the response-scale.

---

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                   |                     f2 | Contrast |        95% CI |     p
      --------------------------------------------------------------------------------
      and-dash             | comma, here-dash-there |     0.02 | [-0.12, 0.16] | 0.796
      and-dash-no dash     |            comma, here |    -0.15 | [-0.33, 0.03] | 0.099
      and-dash-no dash     | comma, here-dash-there |    -0.13 | [-0.36, 0.10] | 0.255
      and-dash-with, comma |            comma, here |    -0.07 | [-0.24, 0.09] | 0.380
      and-dash-with, comma | comma, here-dash-there |    -0.06 | [-0.27, 0.16] | 0.610
      and-dash-no dash     | dash-there-comma, here |    -0.17 | [-0.40, 0.06] | 0.147
      and-dash-no dash     |             dash-there |    -0.15 | [-0.33, 0.03] | 0.099
      and-dash-with, comma | dash-there-comma, here |    -0.09 | [-0.32, 0.13] | 0.412
      and-dash-with, comma |             dash-there |    -0.07 | [-0.24, 0.09] | 0.380
      no dash              | comma, here-dash-there |     0.02 | [-0.12, 0.16] | 0.796
      no dash-with, comma  |            comma, here |     0.08 | [-0.10, 0.26] | 0.402
      no dash-with, comma  | comma, here-dash-there |     0.10 | [-0.13, 0.32] | 0.407
      no dash-with, comma  | dash-there-comma, here |     0.06 | [-0.18, 0.29] | 0.625
      no dash-with, comma  |             dash-there |     0.08 | [-0.10, 0.26] | 0.402
      with, comma          | comma, here-dash-there |     0.02 | [-0.12, 0.16] | 0.796
    Message <simpleMessage>
      
      Contrasts are presented on the response-scale.

# print hypothesis_test collapse levels

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      c172code |     c161sex | Contrast |          95% CI |     p
      -----------------------------------------------------------
      1-2      |        male |    -3.89 | [-12.55,  4.77] | 0.378
      1-3      |        male |     0.86 | [ -9.28, 10.99] | 0.868
      1        | male-female |    -0.95 | [ -9.39,  7.50] | 0.825
      1-2      | male-female |    -3.56 | [-11.32,  4.20] | 0.368
      1-3      | male-female |    -3.12 | [-11.85,  5.62] | 0.484
      2-3      |        male |     4.75 | [ -3.56, 13.06] | 0.262
      2-1      | male-female |     2.94 | [ -3.21,  9.10] | 0.348
      2        | male-female |     0.33 | [ -4.83,  5.49] | 0.899
      2-3      | male-female |     0.78 | [ -5.74,  7.29] | 0.815
      3-1      | male-female |    -1.81 | [ -9.89,  6.28] | 0.661
      3-2      | male-female |    -4.42 | [-11.78,  2.95] | 0.239
      3        | male-female |    -3.97 | [-12.34,  4.40] | 0.352
      1-2      |      female |    -2.61 | [ -7.37,  2.15] | 0.282
      1-3      |      female |    -2.17 | [ -8.39,  4.05] | 0.494
      2-3      |      female |     0.44 | [ -4.80,  5.69] | 0.869
    Message <simpleMessage>
      
      Contrasts are presented on the response-scale.

