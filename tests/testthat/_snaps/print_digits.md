# ggpredict, print digits

    Code
      print(ggpredict(m, "Petal.Length"), digits = 5)
    Output
      # Predicted values of Sepal.Length
      
      Petal.Length | Predicted |             95% CI
      ---------------------------------------------
           1.00000 |   4.71553 | [4.59340, 4.83765]
           1.50000 |   4.91999 | [4.81312, 5.02685]
           2.50000 |   5.32891 | [5.24817, 5.40965]
           3.50000 |   5.73783 | [5.67145, 5.80422]
           4.00000 |   5.94229 | [5.87599, 6.00859]
           4.50000 |   6.14675 | [6.07547, 6.21804]
           5.50000 |   6.55568 | [6.46325, 6.64811]
           7.00000 |   7.16906 | [7.03136, 7.30676]
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

---

    Code
      print(ggpredict(m, "Petal.Length"), digits = 4, n = 3)
    Output
      # Predicted values of Sepal.Length
      
      Petal.Length | Predicted |           95% CI
      -------------------------------------------
                 1 |    4.7155 | [4.5934, 4.8377]
                 3 |    5.5334 | [5.4619, 5.6049]
                 7 |    7.1691 | [7.0314, 7.3068]
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

# ggpredict, print digits and labels

    Code
      print(ggpredict(fit, "e42dep"), value_labels = FALSE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      e42dep | Predicted |           95% CI
      -------------------------------------
           1 |    104.42 | [101.21, 107.64]
           2 |     83.81 | [ 81.91,  85.72]
           3 |     63.20 | [ 61.94,  64.47]
           4 |     42.59 | [ 40.54,  44.65]
      
      Adjusted for:
      * c12hour = 42.25

---

    Code
      print(ggpredict(fit, "e42dep"), value_labels = TRUE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      e42dep                   | Predicted |           95% CI
      -------------------------------------------------------
      [1]          independent |    104.42 | [101.21, 107.64]
      [2]   slightly dependent |     83.81 | [ 81.91,  85.72]
      [3] moderately dependent |     63.20 | [ 61.94,  64.47]
      [4]   severely dependent |     42.59 | [ 40.54,  44.65]
      
      Adjusted for:
      * c12hour = 42.25

---

    Code
      print(ggpredict(fit, "e42dep"), value_labels = TRUE, digits = 4)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      e42dep                   | Predicted |               95% CI
      -----------------------------------------------------------
      [1]          independent |  104.4234 | [101.2108, 107.6360]
      [2]   slightly dependent |   83.8133 | [ 81.9073,  85.7193]
      [3] moderately dependent |   63.2032 | [ 61.9366,  64.4698]
      [4]   severely dependent |   42.5931 | [ 40.5352,  44.6511]
      
      Adjusted for:
      * c12hour = 42.25

