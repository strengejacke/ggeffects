# ggpredict, condition

    Code
      print(out1)
    Output
      # Average predicted values of Negative impact with 7 items
      
      c172code: low level of education
      
      c12hour | Predicted |       95% CI
      ----------------------------------
           20 |     11.49 | 10.78, 12.20
           30 |     11.55 | 10.85, 12.26
           40 |     11.62 | 10.92, 12.31
      
      c172code: intermediate level of education
      
      c12hour | Predicted |       95% CI
      ----------------------------------
           20 |     11.65 | 11.09, 12.21
           30 |     11.71 | 11.17, 12.26
           40 |     11.78 | 11.23, 12.32
      
      c172code: high level of education
      
      c12hour | Predicted |       95% CI
      ----------------------------------
           20 |     12.22 | 11.48, 12.95
           30 |     12.28 | 11.55, 13.01
           40 |     12.34 | 11.61, 13.07
      

# ggaverage, glm

    Code
      print(out1)
    Output
      # Average predicted probabilities of neg_c_7d
      
      c172code: low level of education
      
      c12hour | Predicted |     95% CI
      --------------------------------
           20 |      0.39 | 0.32, 0.48
           30 |      0.40 | 0.32, 0.48
           40 |      0.41 | 0.33, 0.49
      
      c172code: intermediate level of education
      
      c12hour | Predicted |     95% CI
      --------------------------------
           20 |      0.40 | 0.36, 0.46
           30 |      0.41 | 0.36, 0.46
           40 |      0.42 | 0.37, 0.47
      
      c172code: high level of education
      
      c12hour | Predicted |     95% CI
      --------------------------------
           20 |      0.47 | 0.38, 0.55
           30 |      0.48 | 0.39, 0.56
           40 |      0.48 | 0.40, 0.57
      

