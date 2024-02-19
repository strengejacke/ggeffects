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

