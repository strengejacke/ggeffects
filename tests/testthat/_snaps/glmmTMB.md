# glmmTMB, orderedbeta

    Code
      print(out1)
    Output
      # Predicted proportions of ord
      
       hp | Predicted |     95% CI
      ----------------------------
       50 |      0.51 | 0.38, 0.64
       80 |      0.46 | 0.36, 0.56
      120 |      0.40 | 0.33, 0.48
      150 |      0.36 | 0.30, 0.43
      250 |      0.24 | 0.15, 0.36
      330 |      0.16 | 0.07, 0.33
      
      Adjusted for:
      *   wt = 3.22
      * gear =    3
      *  cyl = NA (population-level)

---

    Code
      print(out2)
    Output
      # Average predicted proportions of ord
      
       hp | Predicted |     95% CI
      ----------------------------
       50 |      0.53 | 0.42, 0.64
       80 |      0.49 | 0.41, 0.57
      120 |      0.44 | 0.40, 0.48
      150 |      0.40 | 0.36, 0.44
      250 |      0.28 | 0.17, 0.39
      330 |      0.20 | 0.05, 0.35
      

