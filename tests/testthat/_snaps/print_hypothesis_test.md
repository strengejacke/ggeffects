# print hypothesis_test simple contrast link scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |       95% CI |     p
      -------------------------------------------
      0-1       |     0.48 | [0.23, 0.74] | 0.901
    Message <simpleMessage>
      
      Contrasts are presented on the link-scale.

# print hypothesis_test simple predictions link scale

    Code
      print(out)
    Output
      var_binom | Predicted |       95% CI |     p
      --------------------------------------------
      0         |      0.24 | [0.11, 0.45] | 0.015
      1         |      0.26 | [0.10, 0.53] | 0.076
    Message <simpleMessage>
      
      Predictions are presented on the link-scale.

# print hypothesis_test simple contrast response scale

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      var_binom | Contrast |       95% CI |     p
      -------------------------------------------
      0-1       |     0.99 | [0.80, 1.22] | 0.902
    Message <simpleMessage>
      
      Contrasts are presented on the response-scale.

# print hypothesis_test simple predictions response scale

    Code
      print(out)
    Output
      var_binom | Predicted |       95% CI |     p
      --------------------------------------------
      0         |      1.27 | [1.08, 1.51] | 0.005
      1         |      1.29 | [1.03, 1.62] | 0.025
    Message <simpleMessage>
      
      Predictions are presented on the response-scale.

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
      
      Contrasts are presented on the link-scale.

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
      
      Predictions are presented on the link-scale.

# print hypothesis_test contrasts response scale

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
      
      Contrasts are presented on the response-scale.

# print hypothesis_test predictions response scale

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
      
      Predictions are presented on the response-scale.

# print hypothesis_test comma levels

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                                  |  f2 | Contrast |        95% CI |     p
      ----------------------------------------------------------------------------
      and another comma-and another comma | a-b |     0.02 | [-0.12, 0.16] | 0.796
      and another comma-no comma          | a-a |    -0.15 | [-0.33, 0.03] | 0.099
      and another comma-no comma          | a-b |    -0.13 | [-0.36, 0.10] | 0.255
      and another comma-with comma        | a-a |    -0.07 | [-0.24, 0.09] | 0.380
      and another comma-with comma        | a-b |    -0.06 | [-0.27, 0.16] | 0.610
      and another comma-no comma          | b-a |    -0.17 | [-0.40, 0.06] | 0.147
      and another comma-no comma          | b-b |    -0.15 | [-0.33, 0.03] | 0.099
      and another comma-with comma        | b-a |    -0.09 | [-0.32, 0.13] | 0.412
      and another comma-with comma        | b-b |    -0.07 | [-0.24, 0.09] | 0.380
      no comma-no comma                   | a-b |     0.02 | [-0.12, 0.16] | 0.796
      no comma-with comma                 | a-a |     0.08 | [-0.10, 0.26] | 0.402
      no comma-with comma                 | a-b |     0.10 | [-0.13, 0.32] | 0.407
      no comma-with comma                 | b-a |     0.06 | [-0.18, 0.29] | 0.625
      no comma-with comma                 | b-b |     0.08 | [-0.10, 0.26] | 0.402
      with comma-with comma               | a-b |     0.02 | [-0.12, 0.16] | 0.796

---

    Code
      print(ht)
    Output
      # Pairwise comparisons
      
      f1                                  |                    f2 | Contrast |        95% CI |     p
      ----------------------------------------------------------------------------------------------
      and another comma-and another comma |    comma here-nothere |     0.02 | [-0.12, 0.16] | 0.796
      and another comma-no comma          | comma here-comma here |    -0.15 | [-0.33, 0.03] | 0.099
      and another comma-no comma          |    comma here-nothere |    -0.13 | [-0.36, 0.10] | 0.255
      and another comma-with comma        | comma here-comma here |    -0.07 | [-0.24, 0.09] | 0.380
      and another comma-with comma        |    comma here-nothere |    -0.06 | [-0.27, 0.16] | 0.610
      and another comma-no comma          |    nothere-comma here |    -0.17 | [-0.40, 0.06] | 0.147
      and another comma-no comma          |       nothere-nothere |    -0.15 | [-0.33, 0.03] | 0.099
      and another comma-with comma        |    nothere-comma here |    -0.09 | [-0.32, 0.13] | 0.412
      and another comma-with comma        |       nothere-nothere |    -0.07 | [-0.24, 0.09] | 0.380
      no comma-no comma                   |    comma here-nothere |     0.02 | [-0.12, 0.16] | 0.796
      no comma-with comma                 | comma here-comma here |     0.08 | [-0.10, 0.26] | 0.402
      no comma-with comma                 |    comma here-nothere |     0.10 | [-0.13, 0.32] | 0.407
      no comma-with comma                 |    nothere-comma here |     0.06 | [-0.18, 0.29] | 0.625
      no comma-with comma                 |       nothere-nothere |     0.08 | [-0.10, 0.26] | 0.402
      with comma-with comma               |    comma here-nothere |     0.02 | [-0.12, 0.16] | 0.796

