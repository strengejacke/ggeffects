# print hypothesis_test simple contrast link scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |      95% CI |     p
      ------------------------------------------
      0-1       |    -0.07 | -1.19, 1.05 | 0.901
    Message
      
      Contrasts are presented as log-odds.

# print hypothesis_test simple predictions link scale

    Code
      print(out)
    Output
      var_binom | Predicted |       95% CI |     p
      --------------------------------------------
      0         |     -1.14 | -2.06, -0.22 | 0.015
      1         |     -1.07 | -2.24,  0.11 | 0.076
    Message
      
      Predictions are presented as log-odds.

# print hypothesis_test simple contrast exp scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |     95% CI |     p
      -----------------------------------------
      0-1       |     0.99 | 0.80, 1.22 | 0.902
    Message
      
      Contrasts are presented on the exponentiated scale.

# print hypothesis_test simple contrast odds ratio scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |     95% CI |     p
      -----------------------------------------
      0-1       |     0.93 | 0.30, 2.86 | 0.901
    Message
      
      Contrasts are presented as odds ratios.

# print hypothesis_test simple contrast response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |      95% CI |     p
      ------------------------------------------
      0-1       |    -0.01 | -0.22, 0.20 | 0.902
    Message
      
      Contrasts are presented as probabilities (in %-points).

# print hypothesis_test simple predictions exp scale

    Code
      print(out)
    Output
      var_binom | Predicted |     95% CI |     p
      ------------------------------------------
      0         |      1.27 | 1.08, 1.51 | 0.005
      1         |      1.29 | 1.03, 1.62 | 0.025
    Message
      
      Predictions are presented on the exponentiated scale.

# print hypothesis_test simple predictions odds ratio scale

    Code
      print(out)
    Output
      var_binom | Predicted |     95% CI |     p
      ------------------------------------------
      0         |      0.32 | 0.13, 0.80 | 0.015
      1         |      0.34 | 0.11, 1.12 | 0.076
    Message
      
      Predictions are presented as odds ratios.

# print hypothesis_test contrasts link scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont | Contrast |      95% CI |     p
      --------------------------------------------------------
      0-1       |   2.48-2.48 |    -0.37 | -1.91, 1.18 | 0.644
      0-0       |   2.48-9.25 |    -0.12 | -0.60, 0.35 | 0.610
      0-1       |   2.48-9.25 |    -0.19 | -1.43, 1.04 | 0.758
      0-0       |  2.48-16.02 |    -0.25 | -1.19, 0.70 | 0.610
      0-1       |  2.48-16.02 |    -0.02 | -1.86, 1.82 | 0.980
      1-0       |   2.48-9.25 |     0.24 | -1.21, 1.70 | 0.745
      1-1       |   2.48-9.25 |     0.17 | -1.00, 1.34 | 0.774
      1-0       |  2.48-16.02 |     0.12 | -1.39, 1.63 | 0.878
      1-1       |  2.48-16.02 |     0.34 | -1.99, 2.68 | 0.774
      0-1       |   9.25-9.25 |    -0.07 | -1.19, 1.05 | 0.901
      0-0       |  9.25-16.02 |    -0.12 | -0.60, 0.35 | 0.610
      0-1       |  9.25-16.02 |     0.10 | -1.67, 1.87 | 0.912
      1-0       |  9.25-16.02 |    -0.05 | -1.25, 1.14 | 0.932
      1-1       |  9.25-16.02 |     0.17 | -1.00, 1.34 | 0.774
      0-1       | 16.02-16.02 |     0.22 | -1.59, 2.04 | 0.810
    Message
      
      Contrasts are presented as log-odds.

# print hypothesis_test predictions link scale

    Code
      print(out)
    Output
      var_binom | var_cont | Predicted |       95% CI |     p
      -------------------------------------------------------
      0         |     2.48 |     -1.26 | -2.33, -0.19 | 0.021
      1         |     2.48 |     -0.90 | -2.39,  0.60 | 0.240
      0         |     9.25 |     -1.14 | -2.06, -0.22 | 0.015
      1         |     9.25 |     -1.07 | -2.25,  0.11 | 0.076
      0         |    16.02 |     -1.02 | -2.01, -0.02 | 0.046
      1         |    16.02 |     -1.24 | -3.04,  0.57 | 0.179
    Message
      
      Predictions are presented as log-odds.

# print hypothesis_test contrasts exp scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont | Contrast |     95% CI |     p
      -------------------------------------------------------
      0-1       |   2.48-2.48 |     0.93 | 0.69, 1.27 | 0.658
      0-0       |   2.48-9.25 |     0.98 | 0.90, 1.06 | 0.596
      0-1       |   2.48-9.25 |     0.97 | 0.77, 1.21 | 0.760
      0-0       |  2.48-16.02 |     0.96 | 0.80, 1.14 | 0.608
      0-1       |  2.48-16.02 |     1.00 | 0.72, 1.37 | 0.980
      1-0       |   2.48-9.25 |     1.05 | 0.78, 1.41 | 0.754
      1-1       |   2.48-9.25 |     1.03 | 0.82, 1.31 | 0.780
      1-0       |  2.48-16.02 |     1.02 | 0.75, 1.39 | 0.880
      1-1       |  2.48-16.02 |     1.07 | 0.69, 1.65 | 0.771
      0-1       |   9.25-9.25 |     0.99 | 0.80, 1.22 | 0.902
      0-0       |  9.25-16.02 |     0.98 | 0.89, 1.07 | 0.619
      0-1       |  9.25-16.02 |     1.02 | 0.75, 1.39 | 0.910
      1-0       |  9.25-16.02 |     0.99 | 0.79, 1.25 | 0.932
      1-1       |  9.25-16.02 |     1.03 | 0.84, 1.26 | 0.761
      0-1       | 16.02-16.02 |     1.04 | 0.75, 1.44 | 0.803
    Message
      
      Contrasts are presented on the exponentiated scale.

# print hypothesis_test contrasts response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont | Contrast |      95% CI |     p
      --------------------------------------------------------
      0-1       |   2.48-2.48 |    -0.07 | -0.38, 0.24 | 0.658
      0-0       |   2.48-9.25 |    -0.02 | -0.10, 0.06 | 0.596
      0-1       |   2.48-9.25 |    -0.04 | -0.26, 0.19 | 0.760
      0-0       |  2.48-16.02 |    -0.05 | -0.22, 0.13 | 0.608
      0-1       |  2.48-16.02 |     0.00 | -0.32, 0.32 | 0.980
      1-0       |   2.48-9.25 |     0.05 | -0.25, 0.34 | 0.754
      1-1       |   2.48-9.25 |     0.03 | -0.20, 0.27 | 0.780
      1-0       |  2.48-16.02 |     0.02 | -0.28, 0.33 | 0.880
      1-1       |  2.48-16.02 |     0.06 | -0.37, 0.50 | 0.771
      0-1       |   9.25-9.25 |    -0.01 | -0.22, 0.20 | 0.902
      0-0       |  9.25-16.02 |    -0.02 | -0.12, 0.07 | 0.619
      0-1       |  9.25-16.02 |     0.02 | -0.29, 0.33 | 0.910
      1-0       |  9.25-16.02 |    -0.01 | -0.24, 0.22 | 0.932
      1-1       |  9.25-16.02 |     0.03 | -0.17, 0.23 | 0.761
      0-1       | 16.02-16.02 |     0.04 | -0.28, 0.36 | 0.803
    Message
      
      Contrasts are presented as probabilities (in %-points).

# print hypothesis_test predictions exp scale

    Code
      print(out)
    Output
      var_binom | var_cont | Predicted |     95% CI |     p
      -----------------------------------------------------
      0         |     2.48 |      1.25 | 1.04, 1.50 | 0.019
      1         |     2.48 |      1.34 | 0.98, 1.82 | 0.065
      0         |     9.25 |      1.27 | 1.08, 1.51 | 0.005
      1         |     9.25 |      1.29 | 1.03, 1.62 | 0.025
      0         |    16.02 |      1.30 | 1.07, 1.59 | 0.007
      1         |    16.02 |      1.25 | 0.91, 1.71 | 0.161
    Message
      
      Predictions are presented on the exponentiated scale.

# print hypothesis_test comma and dash levels

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                                      |  f2 | Contrast |      95% CI |     p
      ------------------------------------------------------------------------------
      and, another, comma-and, another, comma | a-b |     0.02 | -0.12, 0.16 | 0.796
      and, another, comma-no comma            | a-a |    -0.15 | -0.33, 0.03 | 0.099
      and, another, comma-no comma            | a-b |    -0.13 | -0.36, 0.10 | 0.255
      and, another, comma-with, comma         | a-a |    -0.07 | -0.24, 0.09 | 0.380
      and, another, comma-with, comma         | a-b |    -0.06 | -0.27, 0.16 | 0.610
      and, another, comma-no comma            | b-a |    -0.17 | -0.40, 0.06 | 0.147
      and, another, comma-no comma            | b-b |    -0.15 | -0.33, 0.03 | 0.099
      and, another, comma-with, comma         | b-a |    -0.09 | -0.32, 0.13 | 0.412
      and, another, comma-with, comma         | b-b |    -0.07 | -0.24, 0.09 | 0.380
      no comma-no comma                       | a-b |     0.02 | -0.12, 0.16 | 0.796
      no comma-with, comma                    | a-a |     0.08 | -0.10, 0.26 | 0.402
      no comma-with, comma                    | a-b |     0.10 | -0.13, 0.32 | 0.407
      no comma-with, comma                    | b-a |     0.06 | -0.18, 0.29 | 0.625
      no comma-with, comma                    | b-b |     0.08 | -0.10, 0.26 | 0.402
      with, comma-with, comma                 | a-b |     0.02 | -0.12, 0.16 | 0.796

---

    Code
      print(ht, table_width = Inf)
    Output
      # Pairwise comparisons
      
      f1                                      |                      f2 | Contrast |      95% CI |     p
      --------------------------------------------------------------------------------------------------
      and, another, comma-and, another, comma |     comma, here-nothere |     0.02 | -0.12, 0.16 | 0.796
      and, another, comma-no comma            | comma, here-comma, here |    -0.15 | -0.33, 0.03 | 0.099
      and, another, comma-no comma            |     comma, here-nothere |    -0.13 | -0.36, 0.10 | 0.255
      and, another, comma-with, comma         | comma, here-comma, here |    -0.07 | -0.24, 0.09 | 0.380
      and, another, comma-with, comma         |     comma, here-nothere |    -0.06 | -0.27, 0.16 | 0.610
      and, another, comma-no comma            |     nothere-comma, here |    -0.17 | -0.40, 0.06 | 0.147
      and, another, comma-no comma            |         nothere-nothere |    -0.15 | -0.33, 0.03 | 0.099
      and, another, comma-with, comma         |     nothere-comma, here |    -0.09 | -0.32, 0.13 | 0.412
      and, another, comma-with, comma         |         nothere-nothere |    -0.07 | -0.24, 0.09 | 0.380
      no comma-no comma                       |     comma, here-nothere |     0.02 | -0.12, 0.16 | 0.796
      no comma-with, comma                    | comma, here-comma, here |     0.08 | -0.10, 0.26 | 0.402
      no comma-with, comma                    |     comma, here-nothere |     0.10 | -0.13, 0.32 | 0.407
      no comma-with, comma                    |     nothere-comma, here |     0.06 | -0.18, 0.29 | 0.625
      no comma-with, comma                    |         nothere-nothere |     0.08 | -0.10, 0.26 | 0.402
      with, comma-with, comma                 |     comma, here-nothere |     0.02 | -0.12, 0.16 | 0.796

---

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                   |                     f2 | Contrast |      95% CI |     p
      ------------------------------------------------------------------------------
      and-dash             | comma, here-dash-there |     0.02 | -0.12, 0.16 | 0.796
      and-dash-no dash     |            comma, here |    -0.15 | -0.33, 0.03 | 0.099
      and-dash-no dash     | comma, here-dash-there |    -0.13 | -0.36, 0.10 | 0.255
      and-dash-with, comma |            comma, here |    -0.07 | -0.24, 0.09 | 0.380
      and-dash-with, comma | comma, here-dash-there |    -0.06 | -0.27, 0.16 | 0.610
      and-dash-no dash     | dash-there-comma, here |    -0.17 | -0.40, 0.06 | 0.147
      and-dash-no dash     |             dash-there |    -0.15 | -0.33, 0.03 | 0.099
      and-dash-with, comma | dash-there-comma, here |    -0.09 | -0.32, 0.13 | 0.412
      and-dash-with, comma |             dash-there |    -0.07 | -0.24, 0.09 | 0.380
      no dash              | comma, here-dash-there |     0.02 | -0.12, 0.16 | 0.796
      no dash-with, comma  |            comma, here |     0.08 | -0.10, 0.26 | 0.402
      no dash-with, comma  | comma, here-dash-there |     0.10 | -0.13, 0.32 | 0.407
      no dash-with, comma  | dash-there-comma, here |     0.06 | -0.18, 0.29 | 0.625
      no dash-with, comma  |             dash-there |     0.08 | -0.10, 0.26 | 0.402
      with, comma          | comma, here-dash-there |     0.02 | -0.12, 0.16 | 0.796

# print hypothesis_test collapse levels

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      c172code |     c161sex | Contrast |        95% CI |     p
      ---------------------------------------------------------
      1-2      |        male |    -3.89 | -12.55,  4.77 | 0.378
      1-3      |        male |     0.86 |  -9.28, 10.99 | 0.868
      1        | male-female |    -0.95 |  -9.39,  7.50 | 0.825
      1-2      | male-female |    -3.56 | -11.32,  4.20 | 0.368
      1-3      | male-female |    -3.12 | -11.85,  5.62 | 0.484
      2-3      |        male |     4.75 |  -3.56, 13.06 | 0.262
      2-1      | male-female |     2.94 |  -3.21,  9.10 | 0.348
      2        | male-female |     0.33 |  -4.83,  5.49 | 0.899
      2-3      | male-female |     0.78 |  -5.74,  7.29 | 0.815
      3-1      | male-female |    -1.81 |  -9.89,  6.28 | 0.661
      3-2      | male-female |    -4.42 | -11.78,  2.95 | 0.239
      3        | male-female |    -3.97 | -12.34,  4.40 | 0.352
      1-2      |      female |    -2.61 |  -7.37,  2.15 | 0.282
      1-3      |      female |    -2.17 |  -8.39,  4.05 | 0.494
      2-3      |      female |     0.44 |  -4.80,  5.69 | 0.869

# hypothesis_test, ci-level

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      Species              | Contrast |       95% CI |      p
      -------------------------------------------------------
      setosa-versicolor    |    -0.93 | -1.13, -0.73 | < .001
      setosa-virginica     |    -1.58 | -1.79, -1.38 | < .001
      versicolor-virginica |    -0.65 | -0.86, -0.45 | < .001

---

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      Species              | Contrast |       80% CI |      p
      -------------------------------------------------------
      setosa-versicolor    |    -0.93 | -1.06, -0.80 | < .001
      setosa-virginica     |    -1.58 | -1.71, -1.45 | < .001
      versicolor-virginica |    -0.65 | -0.78, -0.52 | < .001

# glmmTMB, orderedbeta

    Code
      print(test_predictions(out2))
    Output
      # Pairwise comparisons
      
      gear | Contrast |      95% CI |     p
      -------------------------------------
      3-4  |    -0.03 | -0.12, 0.06 | 0.471
      3-5  |    -0.05 | -0.18, 0.09 | 0.489
      4-5  |    -0.02 | -0.15, 0.12 | 0.820
    Message
      
      Contrasts are presented as proportions (in %-points).

# print hypothesis_test collapse CI

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      e42dep | Contrast |       95% CI |      p
      -----------------------------------------
      1-2    |     8.67 |  3.72, 13.61 | < .001
      1-3    |    21.19 | 16.38, 25.99 | < .001
      1-4    |    61.19 | 56.34, 66.05 | < .001
      2-3    |    12.52 |  9.40, 15.64 | < .001
      2-4    |    52.53 | 49.32, 55.73 | < .001
      3-4    |    40.01 | 37.10, 42.92 | < .001

---

    Code
      print(out, collapse_ci = TRUE)
    Output
      # Pairwise comparisons
      
      e42dep |    Contrast (95% CI) |      p
      --------------------------------------
      1-2    |  8.67  (3.72, 13.61) | < .001
      1-3    | 21.19 (16.38, 25.99) | < .001
      1-4    | 61.19 (56.34, 66.05) | < .001
      2-3    | 12.52  (9.40, 15.64) | < .001
      2-4    | 52.53 (49.32, 55.73) | < .001
      3-4    | 40.01 (37.10, 42.92) | < .001

