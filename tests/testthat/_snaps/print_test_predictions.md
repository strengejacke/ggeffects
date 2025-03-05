# print hypothesis_test simple contrast response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |      95% CI |     p
      ------------------------------------------
      0-1       |    -0.02 | -0.26, 0.23 | 0.901
    Message
      
      Contrasts are presented as probabilities (in %-points).

# print hypothesis_test contrasts response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom |    var_cont | Contrast |      95% CI |     p
      --------------------------------------------------------
      0-0       |  2.48-16.02 |    -0.05 | -0.26, 0.15 | 0.607
      0-0       |   2.48-9.25 |    -0.03 | -0.13, 0.07 | 0.603
      0-0       |  9.25-16.02 |    -0.03 | -0.13, 0.08 | 0.612
      0-1       | 16.02-16.02 |     0.05 | -0.34, 0.44 | 0.806
      0-1       |  2.48-16.02 |     0.00 | -0.40, 0.39 | 0.980
      0-1       |   2.48-2.48 |    -0.08 | -0.43, 0.27 | 0.648
      0-1       |   2.48-9.25 |    -0.04 | -0.31, 0.23 | 0.759
      0-1       |  9.25-16.02 |     0.02 | -0.36, 0.40 | 0.911
      0-1       |   9.25-9.25 |    -0.02 | -0.26, 0.23 | 0.902
      1-0       |  2.48-16.02 |     0.03 | -0.32, 0.37 | 0.878
      1-0       |   2.48-9.25 |     0.05 | -0.27, 0.38 | 0.748
      1-0       |  9.25-16.02 |    -0.01 | -0.28, 0.25 | 0.932
      1-1       |  2.48-16.02 |     0.08 | -0.43, 0.59 | 0.772
      1-1       |   2.48-9.25 |     0.04 | -0.23, 0.30 | 0.776
      1-1       |  9.25-16.02 |     0.04 | -0.21, 0.28 | 0.768
    Message
      
      Contrasts are presented as probabilities (in %-points).

# print hypothesis_test comma and dash levels

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                                      |  f2 | Contrast |      95% CI |     p
      ------------------------------------------------------------------------------
      and, another, comma-and, another, comma | a-b |     0.02 | -0.29, 0.33 | 0.819
      and, another, comma-no comma            | a-a |    -0.15 | -0.54, 0.23 | 0.236
      and, another, comma-no comma            | a-b |    -0.13 | -0.62, 0.36 | 0.370
      and, another, comma-no comma            | b-b |    -0.15 | -0.54, 0.23 | 0.236
      and, another, comma-with, comma         | a-a |    -0.07 | -0.43, 0.28 | 0.470
      and, another, comma-with, comma         | a-b |    -0.06 | -0.52, 0.41 | 0.659
      and, another, comma-with, comma         | b-b |    -0.07 | -0.43, 0.28 | 0.470
      no comma-and, another, comma            | a-b |     0.17 | -0.32, 0.66 | 0.280
      no comma-no comma                       | a-b |     0.02 | -0.29, 0.32 | 0.819
      no comma-with, comma                    | a-a |     0.08 | -0.31, 0.46 | 0.488
      no comma-with, comma                    | a-b |     0.10 | -0.39, 0.58 | 0.492
      no comma-with, comma                    | b-b |     0.08 | -0.31, 0.46 | 0.488
      with, comma-and, another, comma         | a-b |     0.09 | -0.39, 0.57 | 0.496
      with, comma-no comma                    | a-b |    -0.06 | -0.56, 0.44 | 0.672
      with, comma-with, comma                 | a-b |     0.02 | -0.29, 0.33 | 0.819

---

    Code
      print(ht, table_width = Inf)
    Output
      # Pairwise comparisons
      
      f1                                      |                      f2 | Contrast |      95% CI |     p
      --------------------------------------------------------------------------------------------------
      and, another, comma-and, another, comma |     comma, here-nothere |     0.02 | -0.29, 0.33 | 0.819
      and, another, comma-no comma            | comma, here-comma, here |    -0.15 | -0.54, 0.23 | 0.236
      and, another, comma-no comma            |     comma, here-nothere |    -0.13 | -0.62, 0.36 | 0.370
      and, another, comma-no comma            |         nothere-nothere |    -0.15 | -0.54, 0.23 | 0.236
      and, another, comma-with, comma         | comma, here-comma, here |    -0.07 | -0.43, 0.28 | 0.470
      and, another, comma-with, comma         |     comma, here-nothere |    -0.06 | -0.52, 0.41 | 0.659
      and, another, comma-with, comma         |         nothere-nothere |    -0.07 | -0.43, 0.28 | 0.470
      no comma-and, another, comma            |     comma, here-nothere |     0.17 | -0.32, 0.66 | 0.280
      no comma-no comma                       |     comma, here-nothere |     0.02 | -0.29, 0.32 | 0.819
      no comma-with, comma                    | comma, here-comma, here |     0.08 | -0.31, 0.46 | 0.488
      no comma-with, comma                    |     comma, here-nothere |     0.10 | -0.39, 0.58 | 0.492
      no comma-with, comma                    |         nothere-nothere |     0.08 | -0.31, 0.46 | 0.488
      with, comma-and, another, comma         |     comma, here-nothere |     0.09 | -0.39, 0.57 | 0.496
      with, comma-no comma                    |     comma, here-nothere |    -0.06 | -0.56, 0.44 | 0.672
      with, comma-with, comma                 |     comma, here-nothere |     0.02 | -0.29, 0.33 | 0.819

---

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                   |                     f2 | Contrast |      95% CI |     p
      ------------------------------------------------------------------------------
      and-dash             | comma, here-dash-there |     0.02 | -0.29, 0.33 | 0.819
      and-dash-no dash     |            comma, here |    -0.15 | -0.54, 0.23 | 0.236
      and-dash-no dash     | comma, here-dash-there |    -0.13 | -0.62, 0.36 | 0.370
      and-dash-no dash     |             dash-there |    -0.15 | -0.54, 0.23 | 0.236
      and-dash-with, comma |            comma, here |    -0.07 | -0.43, 0.28 | 0.470
      and-dash-with, comma | comma, here-dash-there |    -0.06 | -0.52, 0.41 | 0.659
      and-dash-with, comma |             dash-there |    -0.07 | -0.43, 0.28 | 0.470
      no dash              | comma, here-dash-there |     0.02 | -0.29, 0.32 | 0.819
      no dash-and-dash     | comma, here-dash-there |     0.17 | -0.32, 0.66 | 0.280
      no dash-with, comma  |            comma, here |     0.08 | -0.31, 0.46 | 0.488
      no dash-with, comma  | comma, here-dash-there |     0.10 | -0.39, 0.58 | 0.492
      no dash-with, comma  |             dash-there |     0.08 | -0.31, 0.46 | 0.488
      with, comma          | comma, here-dash-there |     0.02 | -0.29, 0.33 | 0.819
      with, comma-and-dash | comma, here-dash-there |     0.09 | -0.39, 0.57 | 0.496
      with, comma-no dash  | comma, here-dash-there |    -0.06 | -0.56, 0.44 | 0.672

# print hypothesis_test collapse levels

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      c172code |     c161sex | Contrast |        95% CI |     p
      ---------------------------------------------------------
      1        | male-female |    -0.95 |  -9.39,  7.50 | 0.825
      1-2      |      female |    -2.61 |  -7.37,  2.15 | 0.282
      1-2      |        male |    -3.89 | -12.55,  4.77 | 0.378
      1-2      | male-female |    -3.56 | -11.32,  4.20 | 0.368
      1-3      |      female |    -2.17 |  -8.39,  4.05 | 0.494
      1-3      |        male |     0.86 |  -9.28, 10.99 | 0.868
      1-3      | male-female |    -3.12 | -11.85,  5.62 | 0.484
      2        | male-female |     0.33 |  -4.83,  5.49 | 0.899
      2-1      | male-female |     2.94 |  -3.21,  9.10 | 0.348
      2-3      |      female |     0.44 |  -4.80,  5.69 | 0.869
      2-3      |        male |     4.75 |  -3.56, 13.06 | 0.262
      2-3      | male-female |     0.78 |  -5.74,  7.29 | 0.815
      3        | male-female |    -3.97 | -12.34,  4.40 | 0.352
      3-1      | male-female |    -1.81 |  -9.89,  6.28 | 0.661
      3-2      | male-female |    -4.42 | -11.78,  2.95 | 0.239

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
      print(test_predictions(m, "gear"))
    Output
      # Pairwise comparisons
      
      gear | Contrast |      95% CI |     p
      -------------------------------------
      3-4  |    -0.04 | -0.14, 0.06 | 0.464
      3-5  |    -0.06 | -0.22, 0.11 | 0.488
      4-5  |    -0.02 | -0.18, 0.15 | 0.821
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

