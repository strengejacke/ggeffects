# print subsetting 1

    Code
      print(gge[c(1:2, 4:6)])
    Output
      # Predicted values of mpg
      
      cyl: 4
      
      gear | Predicted |       95% CI
      -------------------------------
         3 |     23.57 | 18.82, 28.33
         4 |     26.41 | 24.35, 28.46
      
      cyl: 6
      
      gear | Predicted |       95% CI
      -------------------------------
         3 |     19.76 | 15.51, 24.01
         4 |     19.74 | 17.14, 22.34
      
      cyl: 8
      
      gear | Predicted |       95% CI
      -------------------------------
         4 |     15.22 | 12.66, 17.79
      

---

    Code
      print(gge[c(1:2, 4:6)])
    Output
      # Predicted values of mpg
      
      : 1
      
      cyl | Predicted |       95% CI
      ------------------------------
        4 |     26.66 | 24.68, 28.65
        6 |     19.74 | 17.25, 22.23
      
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

