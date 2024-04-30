# ggeffect, polr, latent = FALSE

    Code
      print(out)
    Output
      # Predicted probabilities of Sat
      
      Sat: Low
      Type: Tower
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.34 | 0.29, 0.39
      Medium |      0.22 | 0.19, 0.27
      High   |      0.12 | 0.10, 0.16
      
      Sat: Low
      Type: Apartment
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.47 | 0.43, 0.52
      Medium |      0.34 | 0.30, 0.38
      High   |      0.20 | 0.17, 0.24
      
      Sat: Low
      Type: Atrium
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.42 | 0.36, 0.49
      Medium |      0.29 | 0.24, 0.35
      High   |      0.17 | 0.13, 0.21
      
      Sat: Low
      Type: Terrace
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.60 | 0.54, 0.66
      Medium |      0.46 | 0.40, 0.53
      High   |      0.29 | 0.24, 0.36
      
      Sat: Medium
      Type: Tower
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.29 | 0.27, 0.31
      Medium |      0.26 | 0.24, 0.29
      High   |      0.19 | 0.16, 0.22
      
      Sat: Medium
      Type: Apartment
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.27 | 0.25, 0.30
      Medium |      0.29 | 0.27, 0.31
      High   |      0.25 | 0.22, 0.28
      
      Sat: Medium
      Type: Atrium
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.28 | 0.26, 0.31
      Medium |      0.28 | 0.26, 0.31
      High   |      0.23 | 0.20, 0.27
      
      Sat: Medium
      Type: Terrace
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.23 | 0.20, 0.26
      Medium |      0.28 | 0.25, 0.30
      High   |      0.28 | 0.26, 0.31
      
      Sat: High
      Type: Tower
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.38 | 0.32, 0.43
      Medium |      0.51 | 0.46, 0.57
      High   |      0.69 | 0.63, 0.74
      
      Sat: High
      Type: Apartment
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.25 | 0.22, 0.29
      Medium |      0.37 | 0.33, 0.42
      High   |      0.55 | 0.50, 0.60
      
      Sat: High
      Type: Atrium
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.29 | 0.24, 0.35
      Medium |      0.42 | 0.36, 0.49
      High   |      0.60 | 0.53, 0.67
      
      Sat: High
      Type: Terrace
      
      Infl   | Predicted |     95% CI
      -------------------------------
      Low    |      0.17 | 0.13, 0.21
      Medium |      0.26 | 0.22, 0.32
      High   |      0.42 | 0.35, 0.49
      

---

    Code
      print(out)
    Output
      # Predicted probabilities of rating
      
      rating: X1
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.20 | 0.08, 0.42
      warm |      0.02 | 0.01, 0.09
      
      rating: X1
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.06 | 0.02, 0.18
      warm |      0.00 | 0.00, 0.02
      
      rating: X2
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.56 | 0.39, 0.72
      warm |      0.21 | 0.10, 0.41
      
      rating: X2
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.39 | 0.23, 0.58
      warm |      0.05 | 0.02, 0.14
      
      rating: X3
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.21 | 0.09, 0.40
      warm |      0.51 | 0.36, 0.65
      
      rating: X3
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.43 | 0.28, 0.60
      warm |      0.29 | 0.15, 0.48
      
      rating: X4
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.03 | 0.01, 0.09
      warm |      0.19 | 0.09, 0.37
      
      rating: X4
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.09 | 0.03, 0.22
      warm |      0.37 | 0.22, 0.56
      
      rating: X5
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.01 | 0.00, 0.03
      warm |      0.07 | 0.02, 0.20
      
      rating: X5
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.03 | 0.01, 0.09
      warm |      0.29 | 0.13, 0.51
      

# ggeffect, polr, latent = TRUE

    Code
      print(out)
    Output
      # Predicted log-odds of Sat
      
      Type: Tower
      
      Infl   | Predicted |       95% CI
      ---------------------------------
      Low    |      0.18 |  0.09,  0.27
      Medium |      0.75 |  0.52,  0.98
      High   |      1.47 |  1.19,  1.75
      
      Type: Apartment
      
      Infl   | Predicted |       95% CI
      ---------------------------------
      Low    |     -0.39 | -0.63, -0.15
      Medium |      0.17 | -0.14,  0.49
      High   |      0.90 |  0.55,  1.24
      
      Type: Atrium
      
      Infl   | Predicted |       95% CI
      ---------------------------------
      Low    |     -0.19 | -0.49,  0.12
      Medium |      0.38 |  0.01,  0.76
      High   |      1.10 |  0.71,  1.50
      
      Type: Terrace
      
      Infl   | Predicted |       95% CI
      ---------------------------------
      Low    |     -0.91 | -1.21, -0.61
      Medium |     -0.34 | -0.71,  0.02
      High   |      0.38 | -0.02,  0.77
      

---

    Code
      print(out)
    Output
      # Predicted log-odds of rating
      
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.00 | 0.00, 0.00
      warm |      2.32 | 0.95, 3.70
      
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      1.35 | 0.05, 2.64
      warm |      4.03 | 2.51, 5.55
      

