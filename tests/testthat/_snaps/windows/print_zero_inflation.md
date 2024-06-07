# ggpredict, print zero-inflated, glmmTMB

    Code
      print(out)
    Output
      # Predicted (conditional) counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      1.09 | 0.69, 1.72
      no    |      3.42 | 2.86, 4.09
      
      Adjusted for:
      * site = NA (population-level)

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -2.39 | -3.18, -1.60 | < .001
    Message
      
      Contrasts are presented as conditional means.

---

    Code
      print(out)
    Output
      # Average predicted (conditional) counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      1.12 | 0.61, 1.63
      no    |      3.51 | 2.89, 4.14
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -2.39 | -3.18, -1.60 | < .001
    Message
      
      Contrasts are presented as conditional means.

---

    Code
      print(out)
    Output
      # Expected counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.26 | 0.11, 0.42
      no    |      2.21 | 1.76, 2.65
      
      Adjusted for:
      * site = NA (population-level)

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -1.99 | -2.41, -1.58 | < .001
    Message
      
      Contrasts are presented as counts.

---

    Code
      print(out)
    Output
      # Average expected counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.27 | 0.15, 0.40
      no    |      2.27 | 1.86, 2.67
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -1.99 | -2.41, -1.58 | < .001
    Message
      
      Contrasts are presented as counts.

---

    Code
      print(out)
    Output
      # Predicted zero-inflation probabilities of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.76 | 0.66, 0.83
      no    |      0.36 | 0.30, 0.41
      
      Adjusted for:
      * site = NA (population-level)

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |     95% CI |      p
      ---------------------------------------
      yes-no |     0.40 | 0.30, 0.50 | < .001
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(out)
    Output
      # Average predicted zero-inflation probabilities of count
      
      mined | Predicted
      -----------------
      yes   |      0.76
      no    |      0.36
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |     95% CI |      p
      ---------------------------------------
      yes-no |     0.40 | 0.30, 0.50 | < .001
    Message
      
      Contrasts are presented as probabilities (in %-points).

# ggpredict, print zero-inflated, pscl

    Code
      print(out)
    Output
      # Predicted (conditional) counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      1.40 | 1.06, 1.85
      no    |      3.55 | 3.30, 3.83
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -2.16 | -2.63, -1.68 | < .001
    Message
      
      Contrasts are presented as conditional means.

---

    Code
      print(out)
    Output
      # Average predicted (conditional) counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      1.40 | 1.01, 1.79
      no    |      3.55 | 3.29, 3.82
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -2.16 | -2.63, -1.68 | < .001
    Message
      
      Contrasts are presented as conditional means.

---

    Code
      print(out)
    Output
      # Expected counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.30 | 0.17, 0.42
      no    |      2.26 | 2.02, 2.51
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -1.97 | -2.23, -1.71 | < .001
    Message
      
      Contrasts are presented as counts.

---

    Code
      print(out)
    Output
      # Average expected counts of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.30 | 0.21, 0.38
      no    |      2.26 | 2.02, 2.51
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |       95% CI |      p
      -----------------------------------------
      yes-no |    -1.97 | -2.23, -1.71 | < .001
    Message
      
      Contrasts are presented as counts.

---

    Code
      print(out)
    Output
      # Predicted zero-inflation probabilities of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.79 | 0.74, 0.83
      no    |      0.36 | 0.35, 0.38
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |     95% CI |      p
      ---------------------------------------
      yes-no |     0.43 | 0.35, 0.51 | < .001
    Message
      
      Contrasts are presented as probabilities (in %-points).

---

    Code
      print(out)
    Output
      # Average predicted zero-inflation probabilities of count
      
      mined | Predicted |     95% CI
      ------------------------------
      yes   |      0.79 | 0.73, 0.85
      no    |      0.36 | 0.31, 0.42
      

---

    Code
      print(test_predictions(out))
    Output
      # Pairwise comparisons
      
      mined  | Contrast |     95% CI |      p
      ---------------------------------------
      yes-no |     0.43 | 0.35, 0.51 | < .001
    Message
      
      Contrasts are presented as probabilities (in %-points).

