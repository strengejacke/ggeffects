# ggpredict, terms = NULL

    Code
      print(out)
    Output
      $c12hour
      # Predicted values of Total score BARTHEL INDEX
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     72.81 | 67.90, 77.72
           20 |     67.75 | 63.01, 72.48
           45 |     61.42 | 56.77, 66.07
           65 |     56.35 | 51.66, 61.05
           85 |     51.29 | 46.45, 56.13
          105 |     46.22 | 41.16, 51.29
          125 |     41.16 | 35.79, 46.53
          170 |     29.76 | 23.48, 36.05
      
      Adjusted for:
      *  neg_c_7 =                  11.84
      *  c161sex =                   Male
      * c172code = low level of education
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.
    Output
      
      $neg_c_7
      # Predicted values of Total score BARTHEL INDEX
      
      neg_c_7 | Predicted |       95% CI
      ----------------------------------
            6 |     75.45 | 70.26, 80.63
            8 |     70.88 | 66.03, 75.74
           12 |     61.76 | 57.10, 66.42
           14 |     57.19 | 52.39, 62.00
           16 |     52.63 | 47.52, 57.74
           20 |     43.51 | 37.45, 49.57
           22 |     38.94 | 32.28, 45.61
           28 |     25.26 | 16.49, 34.02
      
      Adjusted for:
      *  c12hour =                  42.20
      *  c161sex =                   Male
      * c172code = low level of education
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.
    Output
      
      $c161sex
      # Predicted values of Total score BARTHEL INDEX
      
      c161sex | Predicted |       95% CI
      ----------------------------------
      Male    |     62.13 | 57.47, 66.78
      Female  |     63.00 | 59.30, 66.71
      
      Adjusted for:
      *  c12hour =                  42.20
      *  neg_c_7 =                  11.84
      * c172code = low level of education
      
      $c172code
      # Predicted values of Total score BARTHEL INDEX
      
      c172code                        | Predicted |       95% CI
      ----------------------------------------------------------
      low level of education          |     62.13 | 57.47, 66.78
      intermediate level of education |     65.01 | 61.29, 68.73
      high level of education         |     63.34 | 58.63, 68.06
      
      Adjusted for:
      * c12hour = 42.20
      * neg_c_7 = 11.84
      * c161sex =  Male
      
      attr(,"class")
      [1] "ggalleffects" "list"        
      attr(,"model.name")
      [1] "fit"

