# ggpredict, polr

    Code
      print(pr)
    Output
      # Predicted probabilities of Sat
      
      Sat: Low
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.38 | 0.38, 0.38
      Medium |      0.26 | 0.22, 0.30
      High   |      0.14 | 0.12, 0.18
      
      Sat: Medium
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.29 | 0.29, 0.29
      Medium |      0.27 | 0.24, 0.32
      High   |      0.21 | 0.17, 0.26
      
      Sat: High
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.33 | 0.33, 0.33
      Medium |      0.47 | 0.42, 0.52
      High   |      0.65 | 0.59, 0.70
      
      Adjusted for:
      * Type = Tower
      * Cont =   Low

# ggaverage, polr, weights

    Code
      print(pr)
    Output
      # Average predicted probabilities of Sat
      
      Sat: Low
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.46 | 0.42, 0.50
      Medium |      0.33 | 0.30, 0.36
      High   |      0.20 | 0.16, 0.23
      
      Sat: Medium
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.27 | 0.24, 0.29
      Medium |      0.28 | 0.25, 0.30
      High   |      0.24 | 0.21, 0.26
      
      Sat: High
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.27 | 0.24, 0.31
      Medium |      0.39 | 0.36, 0.43
      High   |      0.56 | 0.52, 0.61
      

---

    Code
      format(pr)
    Output
          Infl Predicted     95% CI groups
      1    Low      0.46 0.42, 0.50    Low
      2 Medium      0.33 0.30, 0.36    Low
      3   High      0.20 0.16, 0.23    Low
      4    Low      0.27 0.24, 0.29 Medium
      5 Medium      0.28 0.25, 0.30 Medium
      6   High      0.24 0.21, 0.26 Medium
      7    Low      0.27 0.24, 0.31   High
      8 Medium      0.39 0.36, 0.43   High
      9   High      0.56 0.52, 0.61   High

---

    Code
      print(pr, collapse_tables = TRUE)
    Output
      # Average predicted probabilities of Sat
      
      Infl   |    Sat |      Type | Predicted |     95% CI
      ----------------------------------------------------
      Low    |    Low |     Tower |      0.33 | 0.28, 0.38
      Medium |        |           |      0.22 | 0.18, 0.26
      High   |        |           |      0.12 | 0.09, 0.15
      Low    |        | Apartment |      0.47 | 0.42, 0.51
      Medium |        |           |      0.33 | 0.29, 0.37
      High   |        |           |      0.20 | 0.16, 0.23
      Low    |        |    Atrium |      0.42 | 0.35, 0.48
      Medium |        |           |      0.29 | 0.23, 0.34
      High   |        |           |      0.17 | 0.12, 0.21
      Low    |        |   Terrace |      0.59 | 0.54, 0.65
      Medium |        |           |      0.46 | 0.39, 0.52
      High   |        |           |      0.29 | 0.23, 0.35
      Low    | Medium |     Tower |      0.29 | 0.26, 0.31
      Medium |        |           |      0.26 | 0.23, 0.28
      High   |        |           |      0.19 | 0.16, 0.22
      Low    |        | Apartment |      0.27 | 0.25, 0.30
      Medium |        |           |      0.29 | 0.26, 0.31
      High   |        |           |      0.25 | 0.22, 0.27
      Low    |        |    Atrium |      0.28 | 0.26, 0.31
      Medium |        |           |      0.28 | 0.26, 0.30
      High   |        |           |      0.23 | 0.19, 0.26
      Low    |        |   Terrace |      0.23 | 0.20, 0.26
      Medium |        |           |      0.28 | 0.25, 0.30
      High   |        |           |      0.28 | 0.26, 0.30
      Low    |   High |     Tower |      0.38 | 0.33, 0.44
      Medium |        |           |      0.52 | 0.47, 0.57
      High   |        |           |      0.69 | 0.63, 0.75
      Low    |        | Apartment |      0.26 | 0.22, 0.30
      Medium |        |           |      0.38 | 0.34, 0.42
      High   |        |           |      0.56 | 0.50, 0.61
      Low    |        |    Atrium |      0.30 | 0.25, 0.36
      Medium |        |           |      0.43 | 0.37, 0.49
      High   |        |           |      0.61 | 0.54, 0.68
      Low    |        |   Terrace |      0.17 | 0.14, 0.21
      Medium |        |           |      0.27 | 0.22, 0.32
      High   |        |           |      0.43 | 0.36, 0.50
      

