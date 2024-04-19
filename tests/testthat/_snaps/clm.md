# ggpredict, ordinal

    Code
      print(p)
    Output
      # Predicted probabilities of rating
      
      rating: 1
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.20 | 0.08, 0.42
      warm |      0.02 | 0.01, 0.09
      
      rating: 2
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.56 | 0.39, 0.72
      warm |      0.21 | 0.10, 0.41
      
      rating: 3
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.21 | 0.09, 0.40
      warm |      0.51 | 0.36, 0.65
      
      rating: 4
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.03 | 0.01, 0.09
      warm |      0.19 | 0.09, 0.37
      
      rating: 5
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.01 | 0.00, 0.03
      warm |      0.07 | 0.02, 0.20
      
      Adjusted for:
      * contact = no

---

    Code
      print(p2)
    Output
      # Predicted probabilities of rating
      
      rating: 1
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.20 | 0.08, 0.42
      warm |      0.02 | 0.01, 0.09
      
      rating: 1
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.06 | 0.02, 0.18
      warm |      0.00 | 0.00, 0.02
      
      rating: 2
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.56 | 0.39, 0.72
      warm |      0.21 | 0.10, 0.41
      
      rating: 2
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.39 | 0.23, 0.58
      warm |      0.05 | 0.02, 0.14
      
      rating: 3
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.21 | 0.09, 0.40
      warm |      0.51 | 0.36, 0.65
      
      rating: 3
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.43 | 0.28, 0.60
      warm |      0.29 | 0.15, 0.48
      
      rating: 4
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.03 | 0.01, 0.09
      warm |      0.19 | 0.09, 0.37
      
      rating: 4
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.09 | 0.03, 0.22
      warm |      0.37 | 0.22, 0.56
      
      rating: 5
      contact: no
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.01 | 0.00, 0.03
      warm |      0.07 | 0.02, 0.20
      
      rating: 5
      contact: yes
      
      temp | Predicted |     95% CI
      -----------------------------
      cold |      0.03 | 0.01, 0.09
      warm |      0.29 | 0.13, 0.51
      

