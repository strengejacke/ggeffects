# test_predictions, engine ggeffects, by-arg and printing levels with dots

    Code
      print(out1)
    Output
      # Pairwise comparisons
      
      x1                          |  x2 | Contrast |      95% CI |     p
      ------------------------------------------------------------------
      1. Generation-1. Generation | 1-2 |     0.81 |  0.26, 1.36 | 0.005
      1. Generation-2nd Gen       | 1-1 |     0.37 | -0.15, 0.89 | 0.164
      1. Generation-2nd Gen       | 1-2 |     0.84 |  0.29, 1.39 | 0.003
      1. Generation-2nd Gen       | 2-2 |     0.03 | -0.60, 0.67 | 0.921
      1. Generation-Gen. 3.       | 1-1 |     0.19 | -0.35, 0.73 | 0.481
      1. Generation-Gen. 3.       | 1-2 |     0.39 | -0.34, 1.12 | 0.293
      1. Generation-Gen. 3.       | 2-2 |    -0.42 | -1.21, 0.38 | 0.298
      2nd Gen-1. Generation       | 1-2 |     0.44 | -0.17, 1.05 | 0.153
      2nd Gen-2nd Gen             | 1-2 |     0.47 | -0.13, 1.08 | 0.126
      2nd Gen-Gen. 3.             | 1-1 |    -0.17 | -0.77, 0.42 | 0.562
      2nd Gen-Gen. 3.             | 1-2 |     0.02 | -0.75, 0.79 | 0.956
      2nd Gen-Gen. 3.             | 2-2 |    -0.45 | -1.25, 0.34 | 0.263
      Gen. 3.-1. Generation       | 1-2 |     0.62 | -0.01, 1.24 | 0.053
      Gen. 3.-2nd Gen             | 1-2 |     0.65 |  0.02, 1.27 | 0.042
      Gen. 3.-Gen. 3.             | 1-2 |     0.20 | -0.59, 0.98 | 0.621

---

    Code
      print(out1)
    Output
      # Pairwise comparisons
      
      x1                    | x2 | Contrast |      95% CI |     p
      -----------------------------------------------------------
      1. Generation-2nd Gen |  1 |     0.37 | -0.15, 0.89 | 0.164
      1. Generation-Gen. 3. |  1 |     0.19 | -0.35, 0.73 | 0.481
      2nd Gen-Gen. 3.       |  1 |    -0.17 | -0.77, 0.42 | 0.562
      1. Generation-2nd Gen |  2 |     0.03 | -0.60, 0.67 | 0.921
      1. Generation-Gen. 3. |  2 |    -0.42 | -1.21, 0.38 | 0.298
      2nd Gen-Gen. 3.       |  2 |    -0.45 | -1.25, 0.34 | 0.263

---

    Code
      print(out1)
    Output
      # Pairwise comparisons
      
      x1                          |  x2 |  x3 | Contrast |       95% CI |     p
      -------------------------------------------------------------------------
      1. Generation-1. Generation | 1-1 | a-b |     0.04 | -0.65,  0.74 | 0.903
      1. Generation-1. Generation | 1-2 | a-a |     0.78 | -0.27,  1.83 | 0.144
      1. Generation-1. Generation | 1-2 | a-b |     0.86 |  0.08,  1.63 | 0.031
      1. Generation-1. Generation | 1-2 | b-b |     0.81 |  0.14,  1.49 | 0.019
      1. Generation-1. Generation | 2-1 | a-b |    -0.74 | -1.72,  0.24 | 0.137
      1. Generation-1. Generation | 2-2 | a-b |     0.07 | -0.96,  1.11 | 0.887
      1. Generation-2nd Gen       | 1-1 | a-a |     0.37 | -0.81,  1.54 | 0.537
      1. Generation-2nd Gen       | 1-1 | a-b |     0.40 | -0.32,  1.13 | 0.275
      1. Generation-2nd Gen       | 1-1 | b-b |     0.36 | -0.26,  0.97 | 0.249
      1. Generation-2nd Gen       | 1-2 | a-a |     0.90 | -0.02,  1.82 | 0.055
      1. Generation-2nd Gen       | 1-2 | a-b |     0.85 |  0.03,  1.66 | 0.042
      1. Generation-2nd Gen       | 1-2 | b-b |     0.80 |  0.08,  1.52 | 0.029
      1. Generation-2nd Gen       | 2-1 | a-b |    -0.38 | -1.38,  0.62 | 0.453
      1. Generation-2nd Gen       | 2-2 | a-a |     0.12 | -1.03,  1.27 | 0.840
      1. Generation-2nd Gen       | 2-2 | a-b |     0.07 | -1.00,  1.13 | 0.904
      1. Generation-2nd Gen       | 2-2 | b-b |    -0.01 | -0.81,  0.79 | 0.982
      1. Generation-Gen. 3.       | 1-1 | a-a |     0.63 | -0.34,  1.61 | 0.202
      1. Generation-Gen. 3.       | 1-1 | a-b |     0.03 | -0.74,  0.81 | 0.930
      1. Generation-Gen. 3.       | 1-1 | b-b |    -0.01 | -0.68,  0.67 | 0.981
      1. Generation-Gen. 3.       | 1-2 | a-a |     0.36 | -1.51,  2.23 | 0.702
      1. Generation-Gen. 3.       | 1-2 | a-b |     0.43 | -0.49,  1.35 | 0.359
      1. Generation-Gen. 3.       | 1-2 | b-b |     0.38 | -0.45,  1.22 | 0.363
      1. Generation-Gen. 3.       | 2-1 | a-b |    -0.75 | -1.79,  0.29 | 0.156
      1. Generation-Gen. 3.       | 2-2 | a-a |    -0.42 | -2.41,  1.57 | 0.675
      1. Generation-Gen. 3.       | 2-2 | a-b |    -0.36 | -1.50,  0.79 | 0.540
      1. Generation-Gen. 3.       | 2-2 | b-b |    -0.43 | -1.33,  0.47 | 0.347
      2nd Gen-1. Generation       | 1-1 | a-b |    -0.32 | -1.43,  0.78 | 0.563
      2nd Gen-1. Generation       | 1-2 | a-a |     0.42 | -0.94,  1.78 | 0.545
      2nd Gen-1. Generation       | 1-2 | a-b |     0.49 | -0.67,  1.65 | 0.403
      2nd Gen-1. Generation       | 1-2 | b-b |     0.45 | -0.25,  1.16 | 0.204
      2nd Gen-1. Generation       | 2-1 | a-b |    -0.86 | -1.69, -0.02 | 0.044
      2nd Gen-1. Generation       | 2-2 | a-b |    -0.04 | -0.95,  0.86 | 0.925
      2nd Gen-2nd Gen             | 1-1 | a-b |     0.04 | -1.09,  1.16 | 0.950
      2nd Gen-2nd Gen             | 1-2 | a-a |     0.53 | -0.73,  1.79 | 0.402
      2nd Gen-2nd Gen             | 1-2 | a-b |     0.48 | -0.71,  1.67 | 0.422
      2nd Gen-2nd Gen             | 1-2 | b-b |     0.45 | -0.30,  1.20 | 0.241
      2nd Gen-2nd Gen             | 2-1 | a-b |    -0.50 | -1.36,  0.36 | 0.253
      2nd Gen-2nd Gen             | 2-2 | a-b |    -0.05 | -0.99,  0.89 | 0.913
      2nd Gen-Gen. 3.             | 1-1 | a-a |     0.27 | -1.03,  1.56 | 0.686
      2nd Gen-Gen. 3.             | 1-1 | a-b |    -0.33 | -1.49,  0.83 | 0.572
      2nd Gen-Gen. 3.             | 1-1 | b-b |    -0.37 | -1.07,  0.34 | 0.305
      2nd Gen-Gen. 3.             | 1-2 | a-a |    -0.01 | -2.06,  2.05 | 0.996
      2nd Gen-Gen. 3.             | 1-2 | a-b |     0.06 | -1.20,  1.32 | 0.924
      2nd Gen-Gen. 3.             | 1-2 | b-b |     0.03 | -0.83,  0.88 | 0.954
      2nd Gen-Gen. 3.             | 2-1 | a-b |    -0.86 | -1.77,  0.04 | 0.060
      2nd Gen-Gen. 3.             | 2-2 | a-a |    -0.54 | -2.46,  1.38 | 0.579
      2nd Gen-Gen. 3.             | 2-2 | a-b |    -0.47 | -1.50,  0.56 | 0.364
      2nd Gen-Gen. 3.             | 2-2 | b-b |    -0.42 | -1.36,  0.52 | 0.375
      Gen. 3.-1. Generation       | 1-1 | a-b |    -0.59 | -1.48,  0.31 | 0.195
      Gen. 3.-1. Generation       | 1-2 | a-a |     0.15 | -1.04,  1.34 | 0.802
      Gen. 3.-1. Generation       | 1-2 | a-b |     0.23 | -0.73,  1.18 | 0.642
      Gen. 3.-1. Generation       | 1-2 | b-b |     0.82 |  0.06,  1.58 | 0.034
      Gen. 3.-1. Generation       | 2-1 | a-b |    -0.32 | -2.14,  1.51 | 0.730
      Gen. 3.-1. Generation       | 2-2 | a-b |     0.50 | -1.36,  2.35 | 0.598
      Gen. 3.-2nd Gen             | 1-1 | a-b |    -0.23 | -1.15,  0.69 | 0.621
      Gen. 3.-2nd Gen             | 1-2 | a-a |     0.27 | -0.81,  1.35 | 0.623
      Gen. 3.-2nd Gen             | 1-2 | a-b |     0.22 | -0.78,  1.21 | 0.666
      Gen. 3.-2nd Gen             | 1-2 | b-b |     0.81 |  0.01,  1.61 | 0.047
      Gen. 3.-2nd Gen             | 2-1 | a-b |     0.04 | -1.80,  1.88 | 0.965
      Gen. 3.-2nd Gen             | 2-2 | a-b |     0.49 | -1.39,  2.36 | 0.608
      Gen. 3.-Gen. 3.             | 1-1 | a-b |    -0.60 | -1.56,  0.36 | 0.220
      Gen. 3.-Gen. 3.             | 1-2 | a-a |    -0.27 | -2.22,  1.68 | 0.784
      Gen. 3.-Gen. 3.             | 1-2 | a-b |    -0.20 | -1.28,  0.87 | 0.707
      Gen. 3.-Gen. 3.             | 1-2 | b-b |     0.39 | -0.51,  1.30 | 0.391
      Gen. 3.-Gen. 3.             | 2-1 | a-b |    -0.33 | -2.18,  1.53 | 0.728
      Gen. 3.-Gen. 3.             | 2-2 | a-b |     0.07 | -1.86,  1.99 | 0.946

---

    Code
      print(out1)
    Output
      # Pairwise comparisons
      
      x1                    | x2 | x3 | Contrast |      95% CI |     p
      ----------------------------------------------------------------
      1. Generation-2nd Gen |  1 |  a |     0.37 | -0.81, 1.54 | 0.537
      1. Generation-Gen. 3. |  1 |  a |     0.63 | -0.34, 1.61 | 0.202
      2nd Gen-Gen. 3.       |  1 |  a |     0.27 | -1.03, 1.56 | 0.686
      1. Generation-2nd Gen |  1 |  b |     0.36 | -0.26, 0.97 | 0.249
      1. Generation-Gen. 3. |  1 |  b |    -0.01 | -0.68, 0.67 | 0.981
      2nd Gen-Gen. 3.       |  1 |  b |    -0.37 | -1.07, 0.34 | 0.305
      1. Generation-2nd Gen |  2 |  a |     0.12 | -1.03, 1.27 | 0.840
      1. Generation-Gen. 3. |  2 |  a |    -0.42 | -2.41, 1.57 | 0.675
      2nd Gen-Gen. 3.       |  2 |  a |    -0.54 | -2.46, 1.38 | 0.579
      1. Generation-2nd Gen |  2 |  b |    -0.01 | -0.81, 0.79 | 0.982
      1. Generation-Gen. 3. |  2 |  b |    -0.43 | -1.33, 0.47 | 0.347
      2nd Gen-Gen. 3.       |  2 |  b |    -0.42 | -1.36, 0.52 | 0.375

---

    Code
      print(out)
    Output
      # Pairwise comparisons
      
      x2  |            x1 | Contrast |      95% CI |     p
      ----------------------------------------------------
      1-2 | 1. Generation |     0.81 | -1.64, 3.26 | 0.514
      1-2 |       2nd Gen |     0.46 | -1.99, 2.92 | 0.708
      1-2 |       Gen. 3. |     0.20 | -2.27, 2.67 | 0.875

# test_predictions, engine ggeffects, by-arg and column order

    Code
      print(out1)
    Output
      # Pairwise comparisons
      
      treatment      |      time | Contrast |       95% CI |     p
      ------------------------------------------------------------
      coffee-control |   morning |     0.25 | -0.04,  0.54 | 0.091
      coffee-control |      noon |    -0.30 | -0.60,  0.00 | 0.047
      coffee-control | afternoon |     0.25 | -0.04,  0.54 | 0.091
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(out2)
    Output
      # Pairwise comparisons
      
      treatment      |      time | Contrast |       95% CI |     p
      ------------------------------------------------------------
      coffee-control |   morning |     0.25 | -0.04,  0.54 | 0.091
      coffee-control |      noon |    -0.30 | -0.60,  0.00 | 0.047
      coffee-control | afternoon |     0.25 | -0.04,  0.54 | 0.091
    Message
      
      Contrasts are presented as probabilities (in %-points).

