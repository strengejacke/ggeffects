# test_predictions, margin

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980, 2020]")))
    Output
      # Pairwise comparisons
      
      parfam      |      year | Contrast |      95% CI |     p
      --------------------------------------------------------
      green-green | 1980-2020 |    -0.12 | -0.74, 0.51 | 0.708
      green-lib   | 1980-1980 |     0.14 | -0.54, 0.82 | 0.682
      green-lib   | 1980-2020 |     0.22 | -0.28, 0.72 | 0.394
      green-lib   | 2020-2020 |     0.34 |  0.02, 0.65 | 0.036
      lib-green   | 1980-2020 |    -0.26 | -0.83, 0.30 | 0.365
      lib-lib     | 1980-2020 |     0.08 | -0.48, 0.63 | 0.791
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
      green-lib   | 2020-2020 |     0.34 |  0.02, 0.65 | 0.036
      lib-green   | 1980-2020 |    -0.26 | -0.83, 0.30 | 0.365
      lib-lib     | 1980-2020 |     0.08 | -0.48, 0.63 | 0.791
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]")))
    Output
      # Pairwise comparisons
      
      parfam    |      year | Contrast |      95% CI |     p
      ------------------------------------------------------
      green-lib | 1980-1980 |     0.14 | -0.54, 0.82 | 0.682
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year [1980]"), margin = "marginalmeans"))
    Output
      # Pairwise comparisons
      
      parfam    |      year | Contrast |      95% CI |     p
      ------------------------------------------------------
      green-lib | 1980-1980 |     0.14 | -0.54, 0.82 | 0.682
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year")))
    Output
      # Pairwise comparisons
      
      parfam      |          year | Contrast |       95% CI |     p
      -------------------------------------------------------------
      green-green | 1996.1-2007.2 |    -0.03 | -0.21,  0.14 | 0.712
      green-green | 1996.1-2018.3 |    -0.07 | -0.43,  0.29 | 0.715
      green-green | 2007.2-2018.3 |    -0.03 | -0.22,  0.15 | 0.718
      green-lib   | 1996.1-1996.1 |     0.22 | -0.14,  0.59 | 0.231
      green-lib   | 1996.1-2007.2 |     0.24 | -0.07,  0.56 | 0.128
      green-lib   | 1996.1-2018.3 |     0.26 | -0.06,  0.58 | 0.106
      green-lib   | 2007.2-2007.2 |     0.28 |  0.03,  0.52 | 0.027
      green-lib   | 2007.2-2018.3 |     0.30 |  0.05,  0.54 | 0.020
      green-lib   | 2018.3-2018.3 |     0.33 |  0.03,  0.62 | 0.029
      lib-green   | 1996.1-2007.2 |    -0.26 | -0.57,  0.05 | 0.105
      lib-green   | 1996.1-2018.3 |    -0.29 | -0.64,  0.06 | 0.104
      lib-green   | 2007.2-2018.3 |    -0.31 | -0.60, -0.02 | 0.038
      lib-lib     | 1996.1-2007.2 |     0.02 | -0.13,  0.17 | 0.787
      lib-lib     | 1996.1-2018.3 |     0.04 | -0.23,  0.30 | 0.775
      lib-lib     | 2007.2-2018.3 |     0.02 | -0.10,  0.13 | 0.759
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(test_predictions(m, c("parfam [green, lib]", "year"), margin = "marginalmeans"))
    Output
      # Pairwise comparisons
      
      parfam      |          year | Contrast |       95% CI |     p
      -------------------------------------------------------------
      green-green | 1996.1-2007.2 |    -0.03 | -0.21,  0.14 | 0.712
      green-green | 1996.1-2018.3 |    -0.07 | -0.43,  0.29 | 0.715
      green-green | 2007.2-2018.3 |    -0.03 | -0.22,  0.15 | 0.718
      green-lib   | 1996.1-1996.1 |     0.22 | -0.14,  0.59 | 0.231
      green-lib   | 1996.1-2007.2 |     0.24 | -0.07,  0.56 | 0.128
      green-lib   | 1996.1-2018.3 |     0.26 | -0.06,  0.58 | 0.106
      green-lib   | 2007.2-2007.2 |     0.28 |  0.03,  0.52 | 0.027
      green-lib   | 2007.2-2018.3 |     0.30 |  0.05,  0.54 | 0.020
      green-lib   | 2018.3-2018.3 |     0.33 |  0.03,  0.62 | 0.029
      lib-green   | 1996.1-2007.2 |    -0.26 | -0.57,  0.05 | 0.105
      lib-green   | 1996.1-2018.3 |    -0.29 | -0.64,  0.06 | 0.104
      lib-green   | 2007.2-2018.3 |    -0.31 | -0.60, -0.02 | 0.038
      lib-lib     | 1996.1-2007.2 |     0.02 | -0.13,  0.17 | 0.787
      lib-lib     | 1996.1-2018.3 |     0.04 | -0.23,  0.30 | 0.775
      lib-lib     | 2007.2-2018.3 |     0.02 | -0.10,  0.13 | 0.759
    Message
      
      Contrasts are presented as probabilities (in %-points).

