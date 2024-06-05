# test_predictions, margin

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]")))
    Output
      # Pairwise comparisons
      
      parfam      |      year | Contrast |      95% CI |     p
      --------------------------------------------------------
      green-lib   | 1980-1980 |     0.18 | -0.66, 1.02 | 0.680
      green-green | 1980-2020 |    -0.16 | -0.97, 0.66 | 0.704
      green-lib   | 1980-2020 |     0.26 | -0.41, 0.93 | 0.444
      lib-green   | 1980-2020 |    -0.33 | -1.00, 0.33 | 0.325
      lib-lib     | 1980-2020 |     0.08 | -0.56, 0.73 | 0.799
      green-lib   | 2020-2020 |     0.42 |  0.00, 0.83 | 0.049
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]"),
      margin = "marginalmeans"))
    Output
      # Pairwise comparisons
      
      parfam      |      year | Contrast |      95% CI |     p
      --------------------------------------------------------
      green-green | 1980-2020 |    -0.12 | -0.74, 0.51 | 0.708
      green-lib   | 1980-1980 |     0.14 | -0.54, 0.82 | 0.682
      green-lib   | 1980-2020 |     0.22 | -0.28, 0.72 | 0.394
      green-lib   | 2020-1980 |     0.26 | -0.30, 0.83 | 0.365
      green-lib   | 2020-2020 |     0.34 |  0.02, 0.65 | 0.036
      lib-lib     | 1980-2020 |     0.08 | -0.48, 0.63 | 0.791
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]"),
      margin = "empirical"))
    Output
      # Pairwise comparisons
      
      parfam      |      year | Contrast |      95% CI |     p
      --------------------------------------------------------
      green-green | 1980-2020 |    -0.12 | -0.73, 0.50 | 0.709
      green-lib   | 1980-1980 |     0.14 | -0.53, 0.81 | 0.682
      green-lib   | 1980-2020 |     0.21 | -0.28, 0.71 | 0.392
      green-lib   | 2020-1980 |     0.26 | -0.30, 0.81 | 0.367
      green-lib   | 2020-2020 |     0.33 |  0.02, 0.64 | 0.037
      lib-lib     | 1980-2020 |     0.07 | -0.48, 0.63 | 0.791
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]")))
    Output
      # Pairwise comparisons
      
      parfam    |      year | Contrast |      95% CI |     p
      ------------------------------------------------------
      green-lib | 1980-1980 |     0.18 | -0.66, 1.02 | 0.680
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]"), margin = "marginalmeans"))
    Output
      # Pairwise comparisons
      
      parfam    | Contrast |      95% CI |     p
      ------------------------------------------
      green-lib |     0.14 | -0.54, 0.82 | 0.682
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]"), margin = "empirical"))
    Output
      # Pairwise comparisons
      
      parfam    | Contrast |      95% CI |     p
      ------------------------------------------
      green-lib |     0.14 | -0.53, 0.81 | 0.682
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year")))
    Output
      # Pairwise comparisons
      
      parfam      |          year | Contrast |       95% CI |     p
      -------------------------------------------------------------
      green-lib   | 1996.1-1996.1 |     0.28 | -0.19,  0.74 | 0.244
      green-green | 1996.1-2007.2 |    -0.04 | -0.28,  0.19 | 0.710
      green-lib   | 1996.1-2007.2 |     0.30 | -0.13,  0.73 | 0.167
      green-green | 1996.1-2018.3 |    -0.09 | -0.56,  0.39 | 0.713
      green-lib   | 1996.1-2018.3 |     0.32 | -0.12,  0.76 | 0.151
      lib-green   | 1996.1-2007.2 |    -0.32 | -0.70,  0.06 | 0.099
      lib-lib     | 1996.1-2007.2 |     0.02 | -0.15,  0.19 | 0.793
      lib-green   | 1996.1-2018.3 |    -0.37 | -0.80,  0.06 | 0.094
      lib-lib     | 1996.1-2018.3 |     0.04 | -0.25,  0.33 | 0.780
      green-lib   | 2007.2-2007.2 |     0.34 |  0.01,  0.68 | 0.041
      green-green | 2007.2-2018.3 |    -0.04 | -0.29,  0.20 | 0.715
      green-lib   | 2007.2-2018.3 |     0.36 |  0.02,  0.71 | 0.038
      lib-green   | 2007.2-2018.3 |    -0.39 | -0.77, -0.01 | 0.046
      lib-lib     | 2007.2-2018.3 |     0.02 | -0.10,  0.14 | 0.761
      green-lib   | 2018.3-2018.3 |     0.41 |  0.02,  0.80 | 0.041
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year"), margin = "marginalmeans"))
    Output
      # Pairwise comparisons
      
      parfam      |          year | Contrast |      95% CI |     p
      ------------------------------------------------------------
      green-green | 1996.1-2007.2 |    -0.03 | -0.21, 0.14 | 0.712
      green-green | 1996.1-2018.3 |    -0.07 | -0.43, 0.29 | 0.715
      green-lib   | 1996.1-1996.1 |     0.22 | -0.14, 0.59 | 0.231
      green-lib   | 1996.1-2007.2 |     0.24 | -0.07, 0.56 | 0.128
      green-lib   | 1996.1-2018.3 |     0.26 | -0.06, 0.58 | 0.106
      green-green | 2007.2-2018.3 |    -0.03 | -0.22, 0.15 | 0.718
      green-lib   | 2007.2-1996.1 |     0.26 | -0.05, 0.57 | 0.105
      green-lib   | 2007.2-2007.2 |     0.28 |  0.03, 0.52 | 0.027
      green-lib   | 2007.2-2018.3 |     0.30 |  0.05, 0.54 | 0.020
      green-lib   | 2018.3-1996.1 |     0.29 | -0.06, 0.64 | 0.104
      green-lib   | 2018.3-2007.2 |     0.31 |  0.02, 0.60 | 0.038
      green-lib   | 2018.3-2018.3 |     0.33 |  0.03, 0.62 | 0.029
      lib-lib     | 1996.1-2007.2 |     0.02 | -0.13, 0.17 | 0.787
      lib-lib     | 1996.1-2018.3 |     0.04 | -0.23, 0.30 | 0.775
      lib-lib     | 2007.2-2018.3 |     0.02 | -0.10, 0.13 | 0.759
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year"), margin = "empirical"))
    Output
      # Pairwise comparisons
      
      parfam      |          year | Contrast |      95% CI |     p
      ------------------------------------------------------------
      green-green | 1996.1-2007.2 |    -0.03 | -0.21, 0.14 | 0.712
      green-green | 1996.1-2018.3 |    -0.07 | -0.42, 0.29 | 0.715
      green-lib   | 1996.1-1996.1 |     0.22 | -0.14, 0.58 | 0.231
      green-lib   | 1996.1-2007.2 |     0.24 | -0.07, 0.55 | 0.127
      green-lib   | 1996.1-2018.3 |     0.26 | -0.05, 0.57 | 0.105
      green-green | 2007.2-2018.3 |    -0.03 | -0.21, 0.15 | 0.718
      green-lib   | 2007.2-1996.1 |     0.25 | -0.05, 0.56 | 0.106
      green-lib   | 2007.2-2007.2 |     0.27 |  0.03, 0.51 | 0.027
      green-lib   | 2007.2-2018.3 |     0.29 |  0.05, 0.54 | 0.020
      green-lib   | 2018.3-1996.1 |     0.29 | -0.06, 0.63 | 0.105
      green-lib   | 2018.3-2007.2 |     0.31 |  0.02, 0.60 | 0.038
      green-lib   | 2018.3-2018.3 |     0.32 |  0.03, 0.62 | 0.029
      lib-lib     | 1996.1-2007.2 |     0.02 | -0.13, 0.17 | 0.787
      lib-lib     | 1996.1-2018.3 |     0.04 | -0.22, 0.30 | 0.775
      lib-lib     | 2007.2-2018.3 |     0.02 | -0.10, 0.13 | 0.759
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam", "countryname"), margin = "marginalmeans",
      test = "b4 = b13")))
    Output
      Hypothesis | Contrast |      95% CI |     p
      -------------------------------------------
      b4=b13     |    -0.37 | -1.01, 0.27 | 0.262
      
      Tested hypothesis: parfam[green],countryname[Austria] =
        parfam[left],countryname[Belgium]
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam [green, left]",
        "countryname [Austria, Belgium]"), margin = "marginalmeans")))
    Output
      # Pairwise comparisons
      
      parfam      |     countryname | Contrast |      95% CI |     p
      --------------------------------------------------------------
      green-left  | Austria-Austria |    -0.17 | -0.91, 0.56 | 0.648
      green-green | Austria-Belgium |    -0.45 | -1.10, 0.20 | 0.173
      green-left  | Austria-Belgium |    -0.37 | -1.01, 0.27 | 0.262
      left-green  | Austria-Belgium |    -0.28 | -0.88, 0.32 | 0.362
      left-left   | Austria-Belgium |    -0.20 | -0.79, 0.40 | 0.519
      green-left  | Belgium-Belgium |     0.09 | -0.40, 0.57 | 0.730
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam", "countryname"), margin = "empirical",
      test = "b4 = b13")))
    Output
      Hypothesis | Contrast |      95% CI |     p
      -------------------------------------------
      b4=b13     |    -0.37 | -1.01, 0.27 | 0.262
      
      Tested hypothesis: parfam[green],countryname[Austria] =
        parfam[left],countryname[Belgium]
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      suppressWarnings(print(test_predictions(m, c("parfam [green, left]",
        "countryname [Austria, Belgium]"), margin = "empirical")))
    Output
      # Pairwise comparisons
      
      parfam      |     countryname | Contrast |      95% CI |     p
      --------------------------------------------------------------
      green-left  | Austria-Austria |    -0.17 | -0.91, 0.56 | 0.648
      green-green | Austria-Belgium |    -0.45 | -1.10, 0.20 | 0.173
      green-left  | Austria-Belgium |    -0.37 | -1.01, 0.27 | 0.262
      left-green  | Austria-Belgium |    -0.28 | -0.88, 0.32 | 0.362
      left-left   | Austria-Belgium |    -0.20 | -0.79, 0.40 | 0.519
      green-left  | Belgium-Belgium |     0.09 | -0.40, 0.57 | 0.730
    Message
      
      Contrasts are presented as probabilities (in %-points).

