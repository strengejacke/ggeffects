# ggpredict, print

    Code
      print(ggpredict(fit, terms = c("c12hour", "neg_c_7", "c161sex")))
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      neg_c_7: 8
      c161sex: [1] Male
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     94.41 | 89.04, 99.77
           45 |     91.36 | 85.91, 96.81
           85 |     88.65 | 82.90, 94.40
          170 |     82.89 | 75.94, 89.85
      
      neg_c_7: 8
      c161sex: [2] Female
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     94.91 | 89.88, 99.95
           45 |     91.86 | 86.78, 96.95
           85 |     89.16 | 83.77, 94.54
          170 |     83.40 | 76.79, 90.01
      
      neg_c_7: 11.8
      c161sex: [1] Male
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     90.60 | 85.18, 96.02
           45 |     87.55 | 82.08, 93.03
           85 |     84.85 | 79.09, 90.60
          170 |     79.09 | 72.17, 86.00
      
      neg_c_7: 11.8
      c161sex: [2] Female
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     91.11 | 86.06, 96.15
           45 |     88.06 | 82.99, 93.13
           85 |     85.35 | 80.01, 90.69
          170 |     79.59 | 73.06, 86.12
      
      neg_c_7: 15.7
      c161sex: [1] Male
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     86.70 | 80.89, 92.50
           45 |     83.65 | 77.82, 89.48
           85 |     80.94 | 74.87, 87.01
          170 |     75.18 | 68.05, 82.32
      
      neg_c_7: 15.7
      c161sex: [2] Female
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     87.20 | 81.78, 92.62
           45 |     84.15 | 78.74, 89.57
           85 |     81.44 | 75.80, 87.09
          170 |     75.69 | 68.96, 82.42
      
      Adjusted for:
      *  c82cop1 =                   3.13
      *   e42dep =            independent
      * c172code = low level of education
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

---

    Code
      print(ggpredict(fit, terms = c("c12hour", "neg_c_7", "c161sex")), n = Inf)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      neg_c_7: 8
      c161sex: [1] Male
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     94.41 | 89.04, 99.77
            5 |     94.07 | 88.71, 99.43
           10 |     93.73 | 88.37, 99.09
           15 |     93.39 | 88.03, 98.75
           20 |     93.05 | 87.69, 98.42
           25 |     92.71 | 87.34, 98.09
           30 |     92.38 | 86.99, 97.76
           35 |     92.04 | 86.63, 97.44
           40 |     91.70 | 86.27, 97.12
           45 |     91.36 | 85.91, 96.81
           50 |     91.02 | 85.55, 96.50
           55 |     90.68 | 85.18, 96.19
           60 |     90.34 | 84.80, 95.88
           65 |     90.01 | 84.43, 95.58
           70 |     89.67 | 84.05, 95.28
           75 |     89.33 | 83.67, 94.99
           80 |     88.99 | 83.29, 94.69
           85 |     88.65 | 82.90, 94.40
           90 |     88.31 | 82.51, 94.12
           95 |     87.97 | 82.11, 93.83
          100 |     87.64 | 81.72, 93.55
          105 |     87.30 | 81.32, 93.27
          110 |     86.96 | 80.92, 93.00
          115 |     86.62 | 80.52, 92.72
          120 |     86.28 | 80.11, 92.45
          125 |     85.94 | 79.70, 92.18
          130 |     85.60 | 79.29, 91.92
          135 |     85.26 | 78.88, 91.65
          140 |     84.93 | 78.46, 91.39
          145 |     84.59 | 78.05, 91.13
          150 |     84.25 | 77.63, 90.87
          155 |     83.91 | 77.21, 90.61
          160 |     83.57 | 76.79, 90.36
          165 |     83.23 | 76.36, 90.10
          170 |     82.89 | 75.94, 89.85
      
      neg_c_7: 8
      c161sex: [2] Female
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     94.91 | 89.88, 99.95
            5 |     94.57 | 89.55, 99.60
           10 |     94.23 | 89.22, 99.25
           15 |     93.90 | 88.88, 98.91
           20 |     93.56 | 88.54, 98.58
           25 |     93.22 | 88.19, 98.25
           30 |     92.88 | 87.84, 97.92
           35 |     92.54 | 87.49, 97.59
           40 |     92.20 | 87.13, 97.27
           45 |     91.86 | 86.78, 96.95
           50 |     91.53 | 86.41, 96.64
           55 |     91.19 | 86.04, 96.33
           60 |     90.85 | 85.67, 96.02
           65 |     90.51 | 85.30, 95.72
           70 |     90.17 | 84.92, 95.42
           75 |     89.83 | 84.54, 95.12
           80 |     89.49 | 84.16, 94.83
           85 |     89.16 | 83.77, 94.54
           90 |     88.82 | 83.38, 94.25
           95 |     88.48 | 82.99, 93.97
          100 |     88.14 | 82.59, 93.69
          105 |     87.80 | 82.19, 93.41
          110 |     87.46 | 81.79, 93.14
          115 |     87.12 | 81.39, 92.86
          120 |     86.79 | 80.98, 92.59
          125 |     86.45 | 80.57, 92.32
          130 |     86.11 | 80.16, 92.06
          135 |     85.77 | 79.74, 91.80
          140 |     85.43 | 79.33, 91.53
          145 |     85.09 | 78.91, 91.27
          150 |     84.75 | 78.49, 91.02
          155 |     84.42 | 78.07, 90.76
          160 |     84.08 | 77.64, 90.51
          165 |     83.74 | 77.22, 90.26
          170 |     83.40 | 76.79, 90.01
      
      neg_c_7: 11.8
      c161sex: [1] Male
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     90.60 | 85.18, 96.02
            5 |     90.26 | 84.85, 95.67
           10 |     89.92 | 84.52, 95.33
           15 |     89.59 | 84.18, 94.99
           20 |     89.25 | 83.84, 94.65
           25 |     88.91 | 83.49, 94.32
           30 |     88.57 | 83.15, 93.99
           35 |     88.23 | 82.79, 93.67
           40 |     87.89 | 82.44, 93.35
           45 |     87.55 | 82.08, 93.03
           50 |     87.22 | 81.72, 92.71
           55 |     86.88 | 81.35, 92.40
           60 |     86.54 | 80.98, 92.09
           65 |     86.20 | 80.61, 91.79
           70 |     85.86 | 80.24, 91.48
           75 |     85.52 | 79.86, 91.19
           80 |     85.18 | 79.48, 90.89
           85 |     84.85 | 79.09, 90.60
           90 |     84.51 | 78.71, 90.31
           95 |     84.17 | 78.32, 90.02
          100 |     83.83 | 77.92, 89.74
          105 |     83.49 | 77.53, 89.45
          110 |     83.15 | 77.13, 89.18
          115 |     82.81 | 76.73, 88.90
          120 |     82.48 | 76.33, 88.63
          125 |     82.14 | 75.92, 88.35
          130 |     81.80 | 75.51, 88.08
          135 |     81.46 | 75.10, 87.82
          140 |     81.12 | 74.69, 87.55
          145 |     80.78 | 74.27, 87.29
          150 |     80.44 | 73.86, 87.03
          155 |     80.10 | 73.44, 86.77
          160 |     79.77 | 73.02, 86.51
          165 |     79.43 | 72.60, 86.26
          170 |     79.09 | 72.17, 86.00
      
      neg_c_7: 11.8
      c161sex: [2] Female
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     91.11 | 86.06, 96.15
            5 |     90.77 | 85.73, 95.80
           10 |     90.43 | 85.40, 95.46
           15 |     90.09 | 85.07, 95.11
           20 |     89.75 | 84.73, 94.77
           25 |     89.41 | 84.39, 94.44
           30 |     89.07 | 84.04, 94.11
           35 |     88.74 | 83.70, 93.78
           40 |     88.40 | 83.34, 93.45
           45 |     88.06 | 82.99, 93.13
           50 |     87.72 | 82.63, 92.82
           55 |     87.38 | 82.26, 92.50
           60 |     87.04 | 81.89, 92.19
           65 |     86.70 | 81.52, 91.89
           70 |     86.37 | 81.15, 91.58
           75 |     86.03 | 80.77, 91.28
           80 |     85.69 | 80.39, 90.99
           85 |     85.35 | 80.01, 90.69
           90 |     85.01 | 79.62, 90.40
           95 |     84.67 | 79.23, 90.12
          100 |     84.33 | 78.84, 89.83
          105 |     84.00 | 78.44, 89.55
          110 |     83.66 | 78.04, 89.27
          115 |     83.32 | 77.64, 89.00
          120 |     82.98 | 77.23, 88.73
          125 |     82.64 | 76.83, 88.46
          130 |     82.30 | 76.42, 88.19
          135 |     81.96 | 76.01, 87.92
          140 |     81.63 | 75.59, 87.66
          145 |     81.29 | 75.18, 87.40
          150 |     80.95 | 74.76, 87.14
          155 |     80.61 | 74.34, 86.88
          160 |     80.27 | 73.91, 86.63
          165 |     79.93 | 73.49, 86.38
          170 |     79.59 | 73.06, 86.12
      
      neg_c_7: 15.7
      c161sex: [1] Male
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     86.70 | 80.89, 92.50
            5 |     86.36 | 80.56, 92.15
           10 |     86.02 | 80.23, 91.81
           15 |     85.68 | 79.90, 91.46
           20 |     85.34 | 79.56, 91.12
           25 |     85.00 | 79.22, 90.79
           30 |     84.66 | 78.87, 90.45
           35 |     84.33 | 78.53, 90.13
           40 |     83.99 | 78.17, 89.80
           45 |     83.65 | 77.82, 89.48
           50 |     83.31 | 77.46, 89.16
           55 |     82.97 | 77.10, 88.84
           60 |     82.63 | 76.74, 88.53
           65 |     82.29 | 76.37, 88.22
           70 |     81.96 | 76.00, 87.91
           75 |     81.62 | 75.63, 87.61
           80 |     81.28 | 75.25, 87.30
           85 |     80.94 | 74.87, 87.01
           90 |     80.60 | 74.49, 86.71
           95 |     80.26 | 74.11, 86.42
          100 |     79.92 | 73.72, 86.13
          105 |     79.59 | 73.33, 85.84
          110 |     79.25 | 72.94, 85.56
          115 |     78.91 | 72.54, 85.28
          120 |     78.57 | 72.14, 85.00
          125 |     78.23 | 71.74, 84.72
          130 |     77.89 | 71.34, 84.44
          135 |     77.55 | 70.94, 84.17
          140 |     77.21 | 70.53, 83.90
          145 |     76.88 | 70.12, 83.63
          150 |     76.54 | 69.71, 83.37
          155 |     76.20 | 69.30, 83.10
          160 |     75.86 | 68.88, 82.84
          165 |     75.52 | 68.46, 82.58
          170 |     75.18 | 68.05, 82.32
      
      neg_c_7: 15.7
      c161sex: [2] Female
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     87.20 | 81.78, 92.62
            5 |     86.86 | 81.46, 92.27
           10 |     86.52 | 81.13, 91.92
           15 |     86.18 | 80.80, 91.57
           20 |     85.85 | 80.47, 91.23
           25 |     85.51 | 80.13, 90.89
           30 |     85.17 | 79.79, 90.55
           35 |     84.83 | 79.44, 90.22
           40 |     84.49 | 79.09, 89.89
           45 |     84.15 | 78.74, 89.57
           50 |     83.81 | 78.38, 89.24
           55 |     83.48 | 78.03, 88.93
           60 |     83.14 | 77.66, 88.61
           65 |     82.80 | 77.30, 88.30
           70 |     82.46 | 76.93, 87.99
           75 |     82.12 | 76.56, 87.69
           80 |     81.78 | 76.18, 87.39
           85 |     81.44 | 75.80, 87.09
           90 |     81.11 | 75.42, 86.79
           95 |     80.77 | 75.04, 86.50
          100 |     80.43 | 74.65, 86.21
          105 |     80.09 | 74.26, 85.92
          110 |     79.75 | 73.86, 85.64
          115 |     79.41 | 73.47, 85.36
          120 |     79.07 | 73.07, 85.08
          125 |     78.74 | 72.67, 84.80
          130 |     78.40 | 72.27, 84.53
          135 |     78.06 | 71.86, 84.26
          140 |     77.72 | 71.45, 83.99
          145 |     77.38 | 71.04, 83.72
          150 |     77.04 | 70.63, 83.46
          155 |     76.70 | 70.21, 83.20
          160 |     76.37 | 69.80, 82.93
          165 |     76.03 | 69.38, 82.68
          170 |     75.69 | 68.96, 82.42
      
      Adjusted for:
      *  c82cop1 =                   3.13
      *   e42dep =            independent
      * c172code = low level of education

---

    Code
      print(out, group_name = TRUE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      c172code: low level of education
      e42dep: independent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     87.73 | 82.27, 93.19
            2 |     88.24 | 83.17, 93.30
      
      c172code: low level of education
      e42dep: slightly dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     81.03 | 77.03, 85.04
            2 |     81.54 | 78.12, 84.95
      
      c172code: low level of education
      e42dep: moderately dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     70.50 | 66.82, 74.18
            2 |     71.00 | 67.89, 74.12
      
      c172code: low level of education
      e42dep: severely dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     34.02 | 30.14, 37.89
            2 |     34.52 | 31.20, 37.85
      
      c172code: intermediate level of education
      e42dep: independent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     89.98 | 84.95, 95.01
            2 |     90.49 | 85.93, 95.05
      
      c172code: intermediate level of education
      e42dep: slightly dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     83.28 | 79.85, 86.72
            2 |     83.79 | 81.12, 86.46
      
      c172code: intermediate level of education
      e42dep: moderately dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     72.75 | 69.63, 75.87
            2 |     73.25 | 70.89, 75.62
      
      c172code: intermediate level of education
      e42dep: severely dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     36.27 | 32.92, 39.61
            2 |     36.77 | 34.14, 39.40
      
      c172code: high level of education
      e42dep: independent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     89.39 | 83.75, 95.02
            2 |     89.89 | 84.54, 95.24
      
      c172code: high level of education
      e42dep: slightly dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     82.69 | 78.65, 86.73
            2 |     83.19 | 79.59, 86.80
      
      c172code: high level of education
      e42dep: moderately dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     72.15 | 68.47, 75.83
            2 |     72.66 | 69.38, 75.94
      
      c172code: high level of education
      e42dep: severely dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     35.67 | 31.75, 39.59
            2 |     36.18 | 32.65, 39.70
      
      Adjusted for:
      * c12hour = 42.04
      * neg_c_7 = 11.82
      * c82cop1 =  3.13

---

    Code
      print(out, group_name = FALSE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      low level of education, independent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     87.73 | 82.27, 93.19
            2 |     88.24 | 83.17, 93.30
      
      low level of education, slightly dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     81.03 | 77.03, 85.04
            2 |     81.54 | 78.12, 84.95
      
      low level of education, moderately dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     70.50 | 66.82, 74.18
            2 |     71.00 | 67.89, 74.12
      
      low level of education, severely dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     34.02 | 30.14, 37.89
            2 |     34.52 | 31.20, 37.85
      
      intermediate level of education, independent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     89.98 | 84.95, 95.01
            2 |     90.49 | 85.93, 95.05
      
      intermediate level of education, slightly dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     83.28 | 79.85, 86.72
            2 |     83.79 | 81.12, 86.46
      
      intermediate level of education, moderately dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     72.75 | 69.63, 75.87
            2 |     73.25 | 70.89, 75.62
      
      intermediate level of education, severely dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     36.27 | 32.92, 39.61
            2 |     36.77 | 34.14, 39.40
      
      high level of education, independent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     89.39 | 83.75, 95.02
            2 |     89.89 | 84.54, 95.24
      
      high level of education, slightly dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     82.69 | 78.65, 86.73
            2 |     83.19 | 79.59, 86.80
      
      high level of education, moderately dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     72.15 | 68.47, 75.83
            2 |     72.66 | 69.38, 75.94
      
      high level of education, severely dependent
      
      c161sex | Predicted |       95% CI
      ----------------------------------
            1 |     35.67 | 31.75, 39.59
            2 |     36.18 | 32.65, 39.70
      
      Adjusted for:
      * c12hour = 42.04
      * neg_c_7 = 11.82
      * c82cop1 =  3.13

# ggpredict, print factors

    Code
      print(pr)
    Output
      # Predicted probabilities of bin_choice
      
      Wshort     | Predicted |     95% CI
      -----------------------------------
      climate    |      0.30 | 0.10, 0.62
      cutwelfare |      0.67 | 0.33, 0.89
      discipline |      0.79 | 0.51, 0.93
      freedom    |      0.75 | 0.24, 0.97
      ineqincOK  |      0.50 | 0.17, 0.83
      leader     |      0.43 | 0.14, 0.77
      police     |      0.75 | 0.38, 0.94
      politduty  |      0.29 | 0.07, 0.67
      refugees   |      0.50 | 0.17, 0.83
      Russia     |      0.36 | 0.14, 0.66
      taxesdown  |      0.29 | 0.07, 0.67
      worse-off  |      0.45 | 0.20, 0.73
      

---

    Code
      print(pr)
    Output
      # Predicted probabilities of bin_choice
      
      Wshort     | Predicted |     95% CI
      -----------------------------------
      climate    |      0.30 | 0.10, 0.62
      cutwelfare |      0.67 | 0.33, 0.89
      discipline |      0.79 | 0.51, 0.93
      freedom    |      0.75 | 0.24, 0.97
      ineqincOK  |      0.50 | 0.17, 0.83
      leader     |      0.43 | 0.14, 0.77
      police     |      0.75 | 0.38, 0.94
      politduty  |      0.29 | 0.07, 0.67
      refugees   |      0.50 | 0.17, 0.83
      Russia     |      0.36 | 0.14, 0.66
      taxesdown  |      0.29 | 0.07, 0.67
      worse-off  |      0.45 | 0.20, 0.73
      

# ggpredict, collapse CI

    Code
      print(pr)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      c172code: low level of education
      e42dep: independent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     96.07 | 75.80, 116.35
      Female  |     88.02 | 76.31,  99.73
      
      c172code: low level of education
      e42dep: slightly dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     84.39 | 73.29,  95.50
      Female  |     84.14 | 77.73,  90.55
      
      c172code: low level of education
      e42dep: moderately dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     68.85 | 60.08,  77.62
      Female  |     73.20 | 67.82,  78.59
      
      c172code: low level of education
      e42dep: severely dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     34.83 | 24.63,  45.03
      Female  |     31.39 | 26.44,  36.33
      
      c172code: intermediate level of education
      e42dep: independent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     94.31 | 84.54, 104.07
      Female  |     96.49 | 90.18, 102.80
      
      c172code: intermediate level of education
      e42dep: slightly dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     88.03 | 81.77,  94.28
      Female  |     86.30 | 82.77,  89.83
      
      c172code: intermediate level of education
      e42dep: moderately dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     74.79 | 68.93,  80.65
      Female  |     73.02 | 69.88,  76.16
      
      c172code: intermediate level of education
      e42dep: severely dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     30.32 | 23.66,  36.99
      Female  |     32.20 | 29.09,  35.31
      
      c172code: high level of education
      e42dep: independent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     89.59 | 57.44, 121.74
      Female  |     89.23 | 73.53, 104.93
      
      c172code: high level of education
      e42dep: slightly dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     85.19 | 74.09,  96.29
      Female  |     83.19 | 76.67,  89.71
      
      c172code: high level of education
      e42dep: moderately dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     73.19 | 65.14,  81.24
      Female  |     71.76 | 66.21,  77.31
      
      c172code: high level of education
      e42dep: severely dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     32.85 | 24.05,  41.66
      Female  |     37.10 | 30.99,  43.21
      
      Adjusted for:
      * c160age = 53.00

---

    Code
      print(pr, group_name = FALSE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      low level of education, independent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     96.07 | 75.80, 116.35
      Female  |     88.02 | 76.31,  99.73
      
      low level of education, slightly dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     84.39 | 73.29,  95.50
      Female  |     84.14 | 77.73,  90.55
      
      low level of education, moderately dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     68.85 | 60.08,  77.62
      Female  |     73.20 | 67.82,  78.59
      
      low level of education, severely dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     34.83 | 24.63,  45.03
      Female  |     31.39 | 26.44,  36.33
      
      intermediate level of education, independent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     94.31 | 84.54, 104.07
      Female  |     96.49 | 90.18, 102.80
      
      intermediate level of education, slightly dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     88.03 | 81.77,  94.28
      Female  |     86.30 | 82.77,  89.83
      
      intermediate level of education, moderately dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     74.79 | 68.93,  80.65
      Female  |     73.02 | 69.88,  76.16
      
      intermediate level of education, severely dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     30.32 | 23.66,  36.99
      Female  |     32.20 | 29.09,  35.31
      
      high level of education, independent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     89.59 | 57.44, 121.74
      Female  |     89.23 | 73.53, 104.93
      
      high level of education, slightly dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     85.19 | 74.09,  96.29
      Female  |     83.19 | 76.67,  89.71
      
      high level of education, moderately dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     73.19 | 65.14,  81.24
      Female  |     71.76 | 66.21,  77.31
      
      high level of education, severely dependent
      
      c161sex | Predicted |        95% CI
      -----------------------------------
      Male    |     32.85 | 24.05,  41.66
      Female  |     37.10 | 30.99,  43.21
      
      Adjusted for:
      * c160age = 53.00

---

    Code
      print(pr, group_name = FALSE, collapse_ci = TRUE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      low level of education, independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 96.07 (75.80, 116.35)
      Female  | 88.02 (76.31,  99.73)
      
      low level of education, slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 84.39 (73.29,  95.50)
      Female  | 84.14 (77.73,  90.55)
      
      low level of education, moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 68.85 (60.08,  77.62)
      Female  | 73.20 (67.82,  78.59)
      
      low level of education, severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 34.83 (24.63,  45.03)
      Female  | 31.39 (26.44,  36.33)
      
      intermediate level of education, independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 94.31 (84.54, 104.07)
      Female  | 96.49 (90.18, 102.80)
      
      intermediate level of education, slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 88.03 (81.77,  94.28)
      Female  | 86.30 (82.77,  89.83)
      
      intermediate level of education, moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 74.79 (68.93,  80.65)
      Female  | 73.02 (69.88,  76.16)
      
      intermediate level of education, severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 30.32 (23.66,  36.99)
      Female  | 32.20 (29.09,  35.31)
      
      high level of education, independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 89.59 (57.44, 121.74)
      Female  | 89.23 (73.53, 104.93)
      
      high level of education, slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 85.19 (74.09,  96.29)
      Female  | 83.19 (76.67,  89.71)
      
      high level of education, moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 73.19 (65.14,  81.24)
      Female  | 71.76 (66.21,  77.31)
      
      high level of education, severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 32.85 (24.05,  41.66)
      Female  | 37.10 (30.99,  43.21)
      
      Adjusted for:
      * c160age = 53.00

---

    Code
      print(pr, group_name = FALSE, collapse_ci = TRUE, ci_brackets = c("[", "]"))
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      low level of education, independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 96.07 [75.80, 116.35]
      Female  | 88.02 [76.31,  99.73]
      
      low level of education, slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 84.39 [73.29,  95.50]
      Female  | 84.14 [77.73,  90.55]
      
      low level of education, moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 68.85 [60.08,  77.62]
      Female  | 73.20 [67.82,  78.59]
      
      low level of education, severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 34.83 [24.63,  45.03]
      Female  | 31.39 [26.44,  36.33]
      
      intermediate level of education, independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 94.31 [84.54, 104.07]
      Female  | 96.49 [90.18, 102.80]
      
      intermediate level of education, slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 88.03 [81.77,  94.28]
      Female  | 86.30 [82.77,  89.83]
      
      intermediate level of education, moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 74.79 [68.93,  80.65]
      Female  | 73.02 [69.88,  76.16]
      
      intermediate level of education, severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 30.32 [23.66,  36.99]
      Female  | 32.20 [29.09,  35.31]
      
      high level of education, independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 89.59 [57.44, 121.74]
      Female  | 89.23 [73.53, 104.93]
      
      high level of education, slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 85.19 [74.09,  96.29]
      Female  | 83.19 [76.67,  89.71]
      
      high level of education, moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 73.19 [65.14,  81.24]
      Female  | 71.76 [66.21,  77.31]
      
      high level of education, severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 32.85 [24.05,  41.66]
      Female  | 37.10 [30.99,  43.21]
      
      Adjusted for:
      * c160age = 53.00

---

    Code
      print(pr, group_name = TRUE, collapse_ci = TRUE, ci_brackets = c("[", "]"))
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      c172code: low level of education
      e42dep: independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 96.07 [75.80, 116.35]
      Female  | 88.02 [76.31,  99.73]
      
      c172code: low level of education
      e42dep: slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 84.39 [73.29,  95.50]
      Female  | 84.14 [77.73,  90.55]
      
      c172code: low level of education
      e42dep: moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 68.85 [60.08,  77.62]
      Female  | 73.20 [67.82,  78.59]
      
      c172code: low level of education
      e42dep: severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 34.83 [24.63,  45.03]
      Female  | 31.39 [26.44,  36.33]
      
      c172code: intermediate level of education
      e42dep: independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 94.31 [84.54, 104.07]
      Female  | 96.49 [90.18, 102.80]
      
      c172code: intermediate level of education
      e42dep: slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 88.03 [81.77,  94.28]
      Female  | 86.30 [82.77,  89.83]
      
      c172code: intermediate level of education
      e42dep: moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 74.79 [68.93,  80.65]
      Female  | 73.02 [69.88,  76.16]
      
      c172code: intermediate level of education
      e42dep: severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 30.32 [23.66,  36.99]
      Female  | 32.20 [29.09,  35.31]
      
      c172code: high level of education
      e42dep: independent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 89.59 [57.44, 121.74]
      Female  | 89.23 [73.53, 104.93]
      
      c172code: high level of education
      e42dep: slightly dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 85.19 [74.09,  96.29]
      Female  | 83.19 [76.67,  89.71]
      
      c172code: high level of education
      e42dep: moderately dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 73.19 [65.14,  81.24]
      Female  | 71.76 [66.21,  77.31]
      
      c172code: high level of education
      e42dep: severely dependent
      
      c161sex |    Predicted (95% CI)
      -------------------------------
      Male    | 32.85 [24.05,  41.66]
      Female  | 37.10 [30.99,  43.21]
      
      Adjusted for:
      * c160age = 53.00

---

    Code
      print(pr, group_name = FALSE, collapse_ci = TRUE)
    Output
      # Predicted values of Total score BARTHEL INDEX
      
      e42dep               |   Predicted (95% CI)
      -------------------------------------------
      independent          | 94.36 (90.00, 98.72)
      slightly dependent   | 85.69 (83.31, 88.07)
      moderately dependent | 73.17 (71.15, 75.20)
      severely dependent   | 33.17 (31.09, 35.24)
      
      Adjusted for:
      * c160age = 53.36

# ggpredict, collapse tables

    Code
      print(ggpredict(m, c("Petal.Length", "Species")), collapse_tables = TRUE, n = 3)
    Output
      # Predicted values of Sepal.Length
      
      Petal.Length |    Species | Predicted |      95% CI
      ---------------------------------------------------
              1.00 |     setosa |      4.76 | 4.49,  5.03
              3.00 |            |      5.84 | 4.99,  6.69
              7.00 |            |      8.01 | 4.98, 11.04
              1.00 | versicolor |      3.24 | 2.57,  3.90
              3.00 |            |      4.89 | 4.62,  5.16
              7.00 |            |      8.21 | 7.64,  8.77
              1.00 |  virginica |      2.06 | 1.27,  2.84
              3.00 |            |      4.05 | 3.60,  4.50
              7.00 |            |      8.03 | 7.76,  8.30
      
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

# ggpredict, ci-level

    Code
      print(out)
    Output
      # Predicted values of Sepal.Length
      
      Species    | Predicted |     95% CI
      -----------------------------------
      setosa     |      5.01 | 4.86, 5.15
      versicolor |      5.94 | 5.79, 6.08
      virginica  |      6.59 | 6.44, 6.73
      

---

    Code
      print(out)
    Output
      # Predicted values of Sepal.Length
      
      Species    | Predicted |     80% CI
      -----------------------------------
      setosa     |      5.01 | 4.91, 5.10
      versicolor |      5.94 | 5.84, 6.03
      virginica  |      6.59 | 6.49, 6.68
      

# ggpredict, weights

    Code
      print(ggaverage(m, c("Infl", "Type", "Sat")), collapse_tables = TRUE)
    Output
      # Average predicted values of Freq
      
      Infl   |      Type |    Sat | Predicted |       95% CI
      ------------------------------------------------------
      Low    |     Tower |    Low |     17.50 |  2.69, 32.31
      Medium |           |        |     25.50 | 10.69, 40.31
      High   |           |        |      6.50 | -8.31, 21.31
      Low    |           | Medium |     20.00 |  5.19, 34.81
      Medium |           |        |     22.50 |  7.69, 37.31
      High   |           |        |      8.00 | -6.81, 22.81
      Low    |           |   High |     32.50 | 17.69, 47.31
      Medium |           |        |     38.00 | 23.19, 52.81
      High   |           |        |     29.50 | 14.69, 44.31
      Low    | Apartment |    Low |     69.50 | 54.69, 84.31
      Medium |           |        |     45.50 | 30.69, 60.31
      High   |           |        |     20.50 |  5.69, 35.31
      Low    |           | Medium |     34.50 | 19.69, 49.31
      Medium |           |        |     40.00 | 25.19, 54.81
      High   |           |        |     21.50 |  6.69, 36.31
      Low    |           |   High |     30.00 | 15.19, 44.81
      Medium |           |        |     63.00 | 48.19, 77.81
      High   |           |        |     58.00 | 43.19, 72.81
      Low    |    Atrium |    Low |     16.50 |  1.69, 31.31
      Medium |           |        |      9.00 | -5.81, 23.81
      High   |           |        |      6.50 | -8.31, 21.31
      Low    |           | Medium |     16.00 |  1.19, 30.81
      Medium |           |        |     15.00 |  0.19, 29.81
      High   |           |        |      8.50 | -6.31, 23.31
      Low    |           |   High |     15.00 |  0.19, 29.81
      Medium |           |        |     18.00 |  3.19, 32.81
      High   |           |        |     15.00 |  0.19, 29.81
      Low    |   Terrace |    Low |     37.50 | 22.69, 52.31
      Medium |           |        |     23.00 |  8.19, 37.81
      High   |           |        |      6.00 | -8.81, 20.81
      Low    |           | Medium |     14.50 | -0.31, 29.31
      Medium |           |        |     17.00 |  2.19, 31.81
      High   |           |        |      5.50 | -9.31, 20.31
      Low    |           |   High |     10.00 | -4.81, 24.81
      Medium |           |        |     13.00 | -1.81, 27.81
      High   |           |        |     12.00 | -2.81, 26.81
      

# print, 5-way-interaction

    Code
      print(pr)
    Output
      # Predicted values of Negative impact with 7 items
      
      barthtot: 35.2
      c161sex: Male
      c172code: 1
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     15.14 | 12.68, 17.61
           45 |     14.15 | 12.49, 15.82
           85 |     13.28 | 11.70, 14.86
          170 |     11.41 |  8.19, 14.63
      
      barthtot: 35.2
      c161sex: Male
      c172code: 2
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     13.26 | 12.02, 14.50
           45 |     13.13 | 12.27, 14.00
           85 |     13.02 | 12.16, 13.88
          170 |     12.77 | 11.07, 14.48
      
      barthtot: 35.2
      c161sex: Male
      c172code: 3
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.38 |  9.55, 13.21
           45 |     12.11 | 10.76, 13.46
           85 |     12.76 | 11.43, 14.09
          170 |     14.14 | 11.73, 16.54
      
      barthtot: 35.2
      c161sex: Female
      c172code: 1
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     14.01 | 12.87, 15.15
           45 |     13.66 | 12.88, 14.44
           85 |     13.35 | 12.65, 14.05
          170 |     12.69 | 11.34, 14.04
      
      barthtot: 35.2
      c161sex: Female
      c172code: 2
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     13.50 | 12.85, 14.14
           45 |     13.72 | 13.27, 14.17
           85 |     13.92 | 13.50, 14.33
          170 |     14.34 | 13.56, 15.12
      
      barthtot: 35.2
      c161sex: Female
      c172code: 3
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     12.98 | 11.72, 14.24
           45 |     13.78 | 12.90, 14.65
           85 |     14.48 | 13.61, 15.36
          170 |     15.99 | 14.25, 17.73
      
      barthtot: 64.8
      c161sex: Male
      c172code: 1
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.70 | 10.21, 13.19
           45 |     11.61 | 10.35, 12.88
           85 |     11.54 |  8.96, 14.12
          170 |     11.37 |  5.47, 17.28
      
      barthtot: 64.8
      c161sex: Male
      c172code: 2
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.38 | 10.62, 12.14
           45 |     11.78 | 11.06, 12.49
           85 |     12.13 | 10.74, 13.52
          170 |     12.88 |  9.77, 15.98
      
      barthtot: 64.8
      c161sex: Male
      c172code: 3
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.06 |  9.90, 12.22
           45 |     11.94 | 10.83, 13.04
           85 |     12.72 | 10.81, 14.62
          170 |     14.38 | 10.31, 18.45
      
      barthtot: 64.8
      c161sex: Female
      c172code: 1
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.71 | 11.00, 12.42
           45 |     11.75 | 11.20, 12.29
           85 |     11.78 | 11.05, 12.52
          170 |     11.85 | 10.31, 13.39
      
      barthtot: 64.8
      c161sex: Female
      c172code: 2
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.70 | 11.31, 12.09
           45 |     12.19 | 11.88, 12.51
           85 |     12.63 | 12.17, 13.10
          170 |     13.57 | 12.57, 14.58
      
      barthtot: 64.8
      c161sex: Female
      c172code: 3
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     11.68 | 10.93, 12.44
           45 |     12.64 | 12.03, 13.25
           85 |     13.49 | 12.51, 14.46
          170 |     15.29 | 13.16, 17.42
      
      barthtot: 94.4
      c161sex: Male
      c172code: 1
      
      c12hour | Predicted |      95% CI
      ---------------------------------
            0 |      8.26 | 6.39, 10.13
           45 |      9.07 | 6.98, 11.17
           85 |      9.80 | 5.55, 14.05
          170 |     11.34 | 1.99, 20.68
      
      barthtot: 94.4
      c161sex: Male
      c172code: 2
      
      c12hour | Predicted |      95% CI
      ---------------------------------
            0 |      9.50 | 8.60, 10.39
           45 |     10.42 | 9.29, 11.55
           85 |     11.24 | 8.95, 13.52
          170 |     12.98 | 8.03, 17.93
      
      barthtot: 94.4
      c161sex: Male
      c172code: 3
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     10.73 |  9.19, 12.28
           45 |     11.76 | 10.05, 13.47
           85 |     12.68 |  9.54, 15.81
          170 |     14.62 |  7.94, 21.30
      
      barthtot: 94.4
      c161sex: Female
      c172code: 1
      
      c12hour | Predicted |      95% CI
      ---------------------------------
            0 |      9.41 | 8.47, 10.36
           45 |      9.83 | 9.04, 10.63
           85 |     10.21 | 9.05, 11.37
          170 |     11.01 | 8.60, 13.42
      
      barthtot: 94.4
      c161sex: Female
      c172code: 2
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |      9.90 |  9.41, 10.39
           45 |     10.67 | 10.22, 11.11
           85 |     11.35 | 10.62, 12.08
          170 |     12.80 | 11.25, 14.36
      
      barthtot: 94.4
      c161sex: Female
      c172code: 3
      
      c12hour | Predicted |       95% CI
      ----------------------------------
            0 |     10.39 |  9.39, 11.39
           45 |     11.50 | 10.58, 12.43
           85 |     12.49 | 10.95, 14.03
          170 |     14.59 | 11.33, 17.86
      
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

---

    Code
      print(pr, collapse_tables = TRUE)
    Output
      # Predicted values of Negative impact with 7 items
      
      c12hour | barthtot | c161sex | c172code | Predicted |       95% CI
      ------------------------------------------------------------------
            0 |     35.2 |    Male |        1 |     15.14 | 12.68, 17.61
           45 |          |         |          |     14.15 | 12.49, 15.82
           85 |          |         |          |     13.28 | 11.70, 14.86
          170 |          |         |          |     11.41 |  8.19, 14.63
            0 |          |         |        2 |     13.26 | 12.02, 14.50
           45 |          |         |          |     13.13 | 12.27, 14.00
           85 |          |         |          |     13.02 | 12.16, 13.88
          170 |          |         |          |     12.77 | 11.07, 14.48
            0 |          |         |        3 |     11.38 |  9.55, 13.21
           45 |          |         |          |     12.11 | 10.76, 13.46
           85 |          |         |          |     12.76 | 11.43, 14.09
          170 |          |         |          |     14.14 | 11.73, 16.54
            0 |          |  Female |        1 |     14.01 | 12.87, 15.15
           45 |          |         |          |     13.66 | 12.88, 14.44
           85 |          |         |          |     13.35 | 12.65, 14.05
          170 |          |         |          |     12.69 | 11.34, 14.04
            0 |          |         |        2 |     13.50 | 12.85, 14.14
           45 |          |         |          |     13.72 | 13.27, 14.17
           85 |          |         |          |     13.92 | 13.50, 14.33
          170 |          |         |          |     14.34 | 13.56, 15.12
            0 |          |         |        3 |     12.98 | 11.72, 14.24
           45 |          |         |          |     13.78 | 12.90, 14.65
           85 |          |         |          |     14.48 | 13.61, 15.36
          170 |          |         |          |     15.99 | 14.25, 17.73
            0 |     64.8 |    Male |        1 |     11.70 | 10.21, 13.19
           45 |          |         |          |     11.61 | 10.35, 12.88
           85 |          |         |          |     11.54 |  8.96, 14.12
          170 |          |         |          |     11.37 |  5.47, 17.28
            0 |          |         |        2 |     11.38 | 10.62, 12.14
           45 |          |         |          |     11.78 | 11.06, 12.49
           85 |          |         |          |     12.13 | 10.74, 13.52
          170 |          |         |          |     12.88 |  9.77, 15.98
            0 |          |         |        3 |     11.06 |  9.90, 12.22
           45 |          |         |          |     11.94 | 10.83, 13.04
           85 |          |         |          |     12.72 | 10.81, 14.62
          170 |          |         |          |     14.38 | 10.31, 18.45
            0 |          |  Female |        1 |     11.71 | 11.00, 12.42
           45 |          |         |          |     11.75 | 11.20, 12.29
           85 |          |         |          |     11.78 | 11.05, 12.52
          170 |          |         |          |     11.85 | 10.31, 13.39
            0 |          |         |        2 |     11.70 | 11.31, 12.09
           45 |          |         |          |     12.19 | 11.88, 12.51
           85 |          |         |          |     12.63 | 12.17, 13.10
          170 |          |         |          |     13.57 | 12.57, 14.58
            0 |          |         |        3 |     11.68 | 10.93, 12.44
           45 |          |         |          |     12.64 | 12.03, 13.25
           85 |          |         |          |     13.49 | 12.51, 14.46
          170 |          |         |          |     15.29 | 13.16, 17.42
            0 |     94.4 |    Male |        1 |      8.26 |  6.39, 10.13
           45 |          |         |          |      9.07 |  6.98, 11.17
           85 |          |         |          |      9.80 |  5.55, 14.05
          170 |          |         |          |     11.34 |  1.99, 20.68
            0 |          |         |        2 |      9.50 |  8.60, 10.39
           45 |          |         |          |     10.42 |  9.29, 11.55
           85 |          |         |          |     11.24 |  8.95, 13.52
          170 |          |         |          |     12.98 |  8.03, 17.93
            0 |          |         |        3 |     10.73 |  9.19, 12.28
           45 |          |         |          |     11.76 | 10.05, 13.47
           85 |          |         |          |     12.68 |  9.54, 15.81
          170 |          |         |          |     14.62 |  7.94, 21.30
            0 |          |  Female |        1 |      9.41 |  8.47, 10.36
           45 |          |         |          |      9.83 |  9.04, 10.63
           85 |          |         |          |     10.21 |  9.05, 11.37
          170 |          |         |          |     11.01 |  8.60, 13.42
            0 |          |         |        2 |      9.90 |  9.41, 10.39
           45 |          |         |          |     10.67 | 10.22, 11.11
           85 |          |         |          |     11.35 | 10.62, 12.08
          170 |          |         |          |     12.80 | 11.25, 14.36
            0 |          |         |        3 |     10.39 |  9.39, 11.39
           45 |          |         |          |     11.50 | 10.58, 12.43
           85 |          |         |          |     12.49 | 10.95, 14.03
          170 |          |         |          |     14.59 | 11.33, 17.86
      
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

---

    Code
      print(pr)
    Output
      # Predicted values of Negative impact with 7 items
      
      barthtot: 35.2
      c161sex: Male
      c172code: 1
      e42dep: independent
      
      c12hour | Predicted |            95% CI
      ---------------------------------------
            0 |     71.62 |  -211.63,  354.87
           45 |   -141.61 |  -617.03,  333.81
           85 |   -331.15 | -1465.18,  802.88
          170 |   -733.93 | -3272.84, 1804.98
      
      barthtot: 35.2
      c161sex: Male
      c172code: 1
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      0.44 | -19.71,   20.60
           45 |     18.88 | -13.32,   51.08
           85 |     35.27 | -38.38,  108.92
          170 |     70.09 | -93.33,  233.52
      
      barthtot: 35.2
      c161sex: Male
      c172code: 1
      e42dep: moderately dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     20.06 |  12.38,   27.75
           45 |     14.25 |   9.92,   18.57
           85 |      9.07 |   0.55,   17.60
          170 |     -1.92 | -23.57,   19.74
      
      barthtot: 35.2
      c161sex: Male
      c172code: 1
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     15.58 | 12.61,   18.54
           45 |     14.33 | 12.33,   16.33
           85 |     13.22 | 11.28,   15.16
          170 |     10.87 |  6.89,   14.84
      
      barthtot: 35.2
      c161sex: Male
      c172code: 2
      e42dep: independent
      
      c12hour | Predicted |            95% CI
      ---------------------------------------
            0 |     44.42 |   -68.88,  157.72
           45 |   -148.19 |  -753.87,  457.50
           85 |   -319.39 | -1560.24,  921.46
          170 |   -683.19 | -3274.30, 1907.92
      
      barthtot: 35.2
      c161sex: Male
      c172code: 2
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     13.33 |   4.40,   22.27
           45 |      4.15 |  -5.58,   13.89
           85 |     -4.01 | -26.27,   18.25
          170 |    -21.35 | -71.93,   29.23
      
      barthtot: 35.2
      c161sex: Male
      c172code: 2
      e42dep: moderately dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     17.41 |  13.75,   21.08
           45 |     13.25 |  10.06,   16.45
           85 |      9.55 |   2.99,   16.11
          170 |      1.69 | -13.29,   16.67
      
      barthtot: 35.2
      c161sex: Male
      c172code: 2
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     13.10 | 11.60,   14.61
           45 |     12.87 | 11.81,   13.93
           85 |     12.67 | 11.60,   13.74
          170 |     12.23 | 10.14,   14.32
      
      barthtot: 35.2
      c161sex: Male
      c172code: 3
      e42dep: independent
      
      c12hour | Predicted |            95% CI
      ---------------------------------------
            0 |     17.21 |   -98.48,  132.90
           45 |   -154.76 |  -908.89,  599.37
           85 |   -307.62 | -1686.82, 1071.58
          170 |   -632.45 | -3345.46, 2080.56
      
      barthtot: 35.2
      c161sex: Male
      c172code: 3
      e42dep: slightly dependent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |     26.22 |    5.76,   46.69
           45 |    -10.58 |  -40.20,   19.05
           85 |    -43.29 | -114.67,   28.10
          170 |   -112.79 | -273.95,   48.36
      
      barthtot: 35.2
      c161sex: Male
      c172code: 3
      e42dep: moderately dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     14.76 |   9.97,   19.55
           45 |     12.26 |   6.48,   18.03
           85 |     10.03 |   0.19,   19.86
          170 |      5.30 | -14.72,   25.32
      
      barthtot: 35.2
      c161sex: Male
      c172code: 3
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     10.63 |  8.41,   12.85
           45 |     11.42 |  9.75,   13.09
           85 |     12.11 | 10.50,   13.73
          170 |     13.60 | 10.83,   16.36
      
      barthtot: 35.2
      c161sex: Female
      c172code: 1
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |     40.48 |    9.32,   71.65
           45 |    -15.03 |  -60.12,   30.06
           85 |    -64.38 | -175.65,   46.90
          170 |   -169.24 | -421.76,   83.28
      
      barthtot: 35.2
      c161sex: Female
      c172code: 1
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     13.30 |   5.57,   21.02
           45 |     19.18 |  10.97,   27.39
           85 |     24.41 |   4.55,   44.26
          170 |     35.51 | -10.18,   81.21
      
      barthtot: 35.2
      c161sex: Female
      c172code: 1
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     14.29 | 11.01,   17.56
           45 |     13.99 | 11.65,   16.34
           85 |     13.73 | 10.70,   16.76
          170 |     13.17 |  6.63,   19.72
      
      barthtot: 35.2
      c161sex: Female
      c172code: 1
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     14.41 | 12.91,   15.90
           45 |     13.82 | 12.79,   14.84
           85 |     13.29 | 12.43,   14.15
          170 |     12.17 | 10.59,   13.74
      
      barthtot: 35.2
      c161sex: Female
      c172code: 2
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |     23.12 |    4.32,   41.91
           45 |     -9.30 |  -67.00,   48.39
           85 |    -38.12 | -157.59,   81.34
          170 |    -99.36 | -351.28,  152.56
      
      barthtot: 35.2
      c161sex: Female
      c172code: 2
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     14.15 | 10.28,   18.02
           45 |     15.88 | 11.75,   20.01
           85 |     17.42 |  7.72,   27.11
          170 |     20.68 | -1.49,   42.86
      
      barthtot: 35.2
      c161sex: Female
      c172code: 2
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     15.15 | 13.43,   16.87
           45 |     14.02 | 12.73,   15.30
           85 |     13.01 | 10.91,   15.11
          170 |     10.87 |  6.13,   15.62
      
      barthtot: 35.2
      c161sex: Female
      c172code: 2
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     13.13 | 12.32,   13.93
           45 |     13.71 | 13.15,   14.27
           85 |     14.23 | 13.69,   14.77
          170 |     15.34 | 14.29,   16.38
      
      barthtot: 35.2
      c161sex: Female
      c172code: 3
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |      5.75 |  -36.05,   47.55
           45 |     -3.58 | -124.66,  117.50
           85 |    -11.87 | -265.52,  241.78
          170 |    -29.48 | -567.33,  508.36
      
      barthtot: 35.2
      c161sex: Female
      c172code: 3
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     15.00 |   6.35,   23.66
           45 |     12.58 |   5.29,   19.87
           85 |     10.43 |  -8.18,   29.04
          170 |      5.85 | -38.26,   49.97
      
      barthtot: 35.2
      c161sex: Female
      c172code: 3
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     16.01 | 12.79,   19.23
           45 |     14.04 | 11.54,   16.53
           85 |     12.29 |  7.75,   16.83
          170 |      8.57 | -1.82,   18.96
      
      barthtot: 35.2
      c161sex: Female
      c172code: 3
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     11.85 | 10.30,   13.39
           45 |     13.61 | 12.54,   14.68
           85 |     15.18 | 14.05,   16.31
          170 |     18.51 | 16.17,   20.85
      
      barthtot: 64.8
      c161sex: Male
      c172code: 1
      e42dep: independent
      
      c12hour | Predicted |            95% CI
      ---------------------------------------
            0 |     40.75 |  -110.75,  192.25
           45 |    -66.38 |  -310.11,  177.35
           85 |   -161.61 |  -748.81,  425.59
          170 |   -363.97 | -1683.77,  955.83
      
      barthtot: 64.8
      c161sex: Male
      c172code: 1
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      5.25 |  -4.17,   14.67
           45 |     13.71 |  -2.09,   29.52
           85 |     21.23 | -14.91,   57.37
          170 |     37.21 | -42.81,  117.24
      
      barthtot: 64.8
      c161sex: Male
      c172code: 1
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     13.76 | 10.58,   16.94
           45 |     11.14 |  8.98,   13.30
           85 |      8.81 |  3.61,   14.01
          170 |      3.87 | -8.76,   16.49
      
      barthtot: 64.8
      c161sex: Male
      c172code: 1
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     14.65 |  9.81,   19.49
           45 |     13.36 | 10.10,   16.62
           85 |     12.21 |  8.48,   15.94
          170 |      9.77 |  1.60,   17.94
      
      barthtot: 64.8
      c161sex: Male
      c172code: 2
      e42dep: independent
      
      c12hour | Predicted |            95% CI
      ---------------------------------------
            0 |     27.04 |   -30.74,   84.82
           45 |    -72.41 |  -380.46,  235.64
           85 |   -160.81 |  -792.02,  470.40
          170 |   -348.67 | -1666.87,  969.53
      
      barthtot: 64.8
      c161sex: Male
      c172code: 2
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     10.92 |   6.80,   15.05
           45 |      9.55 |   5.47,   13.63
           85 |      8.33 |  -1.03,   17.69
          170 |      5.74 | -15.74,   27.21
      
      barthtot: 64.8
      c161sex: Male
      c172code: 2
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.87 | 11.31,   14.43
           45 |     12.27 | 11.10,   13.44
           85 |     11.73 |  9.20,   14.27
          170 |     10.60 |  4.60,   16.60
      
      barthtot: 64.8
      c161sex: Male
      c172code: 2
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.83 | 10.31,   15.34
           45 |     12.40 | 10.60,   14.20
           85 |     12.02 |  9.94,   14.10
          170 |     11.21 |  6.91,   15.51
      
      barthtot: 64.8
      c161sex: Male
      c172code: 3
      e42dep: independent
      
      c12hour | Predicted |            95% CI
      ---------------------------------------
            0 |     13.34 |   -52.23,   78.91
           45 |    -78.44 |  -461.06,  304.18
           85 |   -160.02 |  -855.89,  535.85
          170 |   -333.37 | -1698.35, 1031.61
      
      barthtot: 64.8
      c161sex: Male
      c172code: 3
      e42dep: slightly dependent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |     16.59 |    6.38,   26.81
           45 |      5.39 |   -8.55,   19.32
           85 |     -4.57 |  -38.79,   29.64
          170 |    -25.74 | -103.50,   52.02
      
      barthtot: 64.8
      c161sex: Male
      c172code: 3
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     11.98 |  9.68,   14.28
           45 |     13.40 | 11.44,   15.35
           85 |     14.66 | 10.94,   18.38
          170 |     17.34 |  8.93,   25.74
      
      barthtot: 64.8
      c161sex: Male
      c172code: 3
      e42dep: severely dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |     11.00 | 7.75,   14.25
           45 |     11.44 | 8.84,   14.04
           85 |     11.83 | 8.96,   14.70
          170 |     12.66 | 7.50,   17.81
      
      barthtot: 64.8
      c161sex: Female
      c172code: 1
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |     25.01 |   10.19,   39.83
           45 |     -0.68 |  -19.73,   18.38
           85 |    -23.51 |  -71.77,   24.74
          170 |    -72.04 | -182.71,   38.63
      
      barthtot: 64.8
      c161sex: Female
      c172code: 1
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     11.34 |  7.72,   14.96
           45 |     12.93 |  9.30,   16.55
           85 |     14.34 |  5.46,   23.22
          170 |     17.33 | -3.25,   37.92
      
      barthtot: 64.8
      c161sex: Female
      c172code: 1
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.04 | 10.66,   13.43
           45 |     12.13 | 11.14,   13.12
           85 |     12.20 | 10.87,   13.53
          170 |     12.35 |  9.43,   15.28
      
      barthtot: 64.8
      c161sex: Female
      c172code: 1
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     13.66 | 11.06,   16.26
           45 |     12.80 | 10.98,   14.61
           85 |     12.03 | 10.52,   13.54
          170 |     10.40 |  7.86,   12.94
      
      barthtot: 64.8
      c161sex: Female
      c172code: 2
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |     16.12 |    6.88,   25.36
           45 |      1.19 |  -23.47,   25.85
           85 |    -12.09 |  -63.39,   39.22
          170 |    -40.30 | -148.96,   68.36
      
      barthtot: 64.8
      c161sex: Female
      c172code: 2
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     11.79 |  9.99,   13.58
           45 |     12.99 | 11.23,   14.76
           85 |     14.07 |  9.92,   18.21
          170 |     16.34 |  6.79,   25.89
      
      barthtot: 64.8
      c161sex: Female
      c172code: 2
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.62 | 11.88,   13.37
           45 |     12.31 | 11.75,   12.87
           85 |     12.03 | 11.12,   12.94
          170 |     11.43 |  9.38,   13.48
      
      barthtot: 64.8
      c161sex: Female
      c172code: 2
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.63 | 11.40,   13.85
           45 |     13.41 | 12.53,   14.28
           85 |     14.10 | 13.14,   15.06
          170 |     15.57 | 13.63,   17.52
      
      barthtot: 64.8
      c161sex: Female
      c172code: 3
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |      7.23 |  -12.28,   26.75
           45 |      3.05 |  -47.18,   53.29
           85 |     -0.66 | -106.24,  104.91
          170 |     -8.56 | -233.21,  216.09
      
      barthtot: 64.8
      c161sex: Female
      c172code: 3
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.24 |  8.13,   16.35
           45 |     13.06 |  9.80,   16.33
           85 |     13.80 |  5.25,   22.34
          170 |     15.35 | -5.09,   35.80
      
      barthtot: 64.8
      c161sex: Female
      c172code: 3
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     13.20 | 11.78,   14.62
           45 |     12.49 | 11.41,   13.57
           85 |     11.86 |  9.89,   13.83
          170 |     10.52 |  5.98,   15.05
      
      barthtot: 64.8
      c161sex: Female
      c172code: 3
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     11.59 |  9.20,   13.99
           45 |     14.01 | 12.30,   15.73
           85 |     16.17 | 14.11,   18.23
          170 |     20.74 | 16.41,   25.08
      
      barthtot: 94.4
      c161sex: Male
      c172code: 1
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |      9.88 |  -11.45,   31.20
           45 |      8.85 |   -9.51,   27.20
           85 |      7.93 |  -44.16,   60.03
          170 |      5.99 | -118.60,  130.57
      
      barthtot: 94.4
      c161sex: Male
      c172code: 1
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |     10.06 |   5.93,   14.19
           45 |      8.54 |   2.80,   14.29
           85 |      7.20 |  -5.75,   20.14
          170 |      4.34 | -24.46,   33.13
      
      barthtot: 94.4
      c161sex: Male
      c172code: 1
      e42dep: moderately dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      7.45 |   2.87,   12.03
           45 |      8.03 |   3.07,   12.99
           85 |      8.55 |  -2.07,   19.17
          170 |      9.65 | -14.14,   33.44
      
      barthtot: 94.4
      c161sex: Male
      c172code: 1
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     13.72 |  5.12,   22.32
           45 |     12.39 |  6.45,   18.32
           85 |     11.20 |  4.94,   17.46
          170 |      8.67 | -4.27,   21.61
      
      barthtot: 94.4
      c161sex: Male
      c172code: 2
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |      9.67 |    3.19,   16.15
           45 |      3.37 |  -28.38,   35.11
           85 |     -2.24 |  -67.61,   63.13
          170 |    -14.15 | -151.02,  122.73
      
      barthtot: 94.4
      c161sex: Male
      c172code: 2
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |      8.51 |  6.31,   10.71
           45 |     14.95 | 10.52,   19.37
           85 |     20.67 | 10.90,   30.44
          170 |     32.83 | 11.55,   54.10
      
      barthtot: 94.4
      c161sex: Male
      c172code: 2
      e42dep: moderately dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |      8.32 | 6.16,   10.49
           45 |     11.29 | 7.96,   14.61
           85 |     13.92 | 6.80,   21.04
          170 |     19.51 | 3.99,   35.04
      
      barthtot: 94.4
      c161sex: Male
      c172code: 2
      e42dep: severely dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |     12.55 | 8.17,   16.92
           45 |     11.92 | 8.76,   15.08
           85 |     11.37 | 7.93,   14.81
          170 |     10.20 | 3.35,   17.04
      
      barthtot: 94.4
      c161sex: Male
      c172code: 3
      e42dep: independent
      
      c12hour | Predicted |           95% CI
      --------------------------------------
            0 |      9.47 |  -13.00,   31.94
           45 |     -2.12 |  -65.06,   60.83
           85 |    -12.41 | -143.30,  118.47
          170 |    -34.29 | -311.17,  242.60
      
      barthtot: 94.4
      c161sex: Male
      c172code: 3
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |      6.96 |  3.03,   10.89
           45 |     21.35 | 11.87,   30.82
           85 |     34.14 | 13.71,   54.56
          170 |     61.32 | 17.38,  105.25
      
      barthtot: 94.4
      c161sex: Male
      c172code: 3
      e42dep: moderately dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |      9.20 |  5.85,   12.55
           45 |     14.54 |  7.67,   21.41
           85 |     19.29 |  4.75,   33.83
          170 |     29.37 | -1.84,   60.59
      
      barthtot: 94.4
      c161sex: Male
      c172code: 3
      e42dep: severely dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |     11.37 | 5.97,   16.77
           45 |     11.46 | 7.16,   15.77
           85 |     11.55 | 6.86,   16.23
          170 |     11.72 | 3.41,   20.03
      
      barthtot: 94.4
      c161sex: Female
      c172code: 1
      e42dep: independent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      9.54 |   5.25,   13.83
           45 |     13.68 |   2.32,   25.04
           85 |     17.35 |  -7.01,   41.71
          170 |     25.16 | -27.02,   77.33
      
      barthtot: 94.4
      c161sex: Female
      c172code: 1
      e42dep: slightly dependent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      9.38 |   7.11,   11.66
           45 |      6.67 |   2.26,   11.09
           85 |      4.27 |  -5.46,   14.00
          170 |     -0.85 | -22.05,   20.36
      
      barthtot: 94.4
      c161sex: Female
      c172code: 1
      e42dep: moderately dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |      9.80 | 7.82,   11.78
           45 |     10.26 | 8.71,   11.80
           85 |     10.67 | 8.80,   12.54
          170 |     11.53 | 7.85,   15.22
      
      barthtot: 94.4
      c161sex: Female
      c172code: 1
      e42dep: severely dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |     12.91 | 8.60,   17.22
           45 |     11.78 | 8.76,   14.80
           85 |     10.77 | 8.30,   13.24
          170 |      8.64 | 4.60,   12.68
      
      barthtot: 94.4
      c161sex: Female
      c172code: 2
      e42dep: independent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      9.13 |   6.74,   11.52
           45 |     11.68 |   1.71,   21.65
           85 |     13.94 |  -6.67,   34.56
          170 |     18.76 | -24.53,   62.05
      
      barthtot: 94.4
      c161sex: Female
      c172code: 2
      e42dep: slightly dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |      9.43 | 8.39,   10.46
           45 |     10.11 | 8.33,   11.89
           85 |     10.72 | 6.81,   14.62
          170 |     12.00 | 3.47,   20.54
      
      barthtot: 94.4
      c161sex: Female
      c172code: 2
      e42dep: moderately dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |     10.10 | 9.09,   11.12
           45 |     10.60 | 9.79,   11.42
           85 |     11.05 | 9.91,   12.19
          170 |     12.00 | 9.62,   14.37
      
      barthtot: 94.4
      c161sex: Female
      c172code: 2
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     12.12 | 10.06,   14.19
           45 |     13.10 | 11.62,   14.58
           85 |     13.97 | 12.41,   15.52
          170 |     15.81 | 12.74,   18.88
      
      barthtot: 94.4
      c161sex: Female
      c172code: 3
      e42dep: independent
      
      c12hour | Predicted |          95% CI
      -------------------------------------
            0 |      8.72 |   3.20,   14.24
           45 |      9.68 | -12.37,   31.74
           85 |     10.54 | -35.22,   56.30
          170 |     12.36 | -83.90,  108.62
      
      barthtot: 94.4
      c161sex: Female
      c172code: 3
      e42dep: slightly dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |      9.47 |  7.37,   11.57
           45 |     13.54 | 10.05,   17.04
           85 |     17.16 |  9.50,   24.83
          170 |     24.86 |  8.07,   41.64
      
      barthtot: 94.4
      c161sex: Female
      c172code: 3
      e42dep: moderately dependent
      
      c12hour | Predicted |        95% CI
      -----------------------------------
            0 |     10.40 | 8.52,   12.28
           45 |     10.95 | 9.36,   12.53
           85 |     11.43 | 9.14,   13.72
          170 |     12.46 | 7.73,   17.19
      
      barthtot: 94.4
      c161sex: Female
      c172code: 3
      e42dep: severely dependent
      
      c12hour | Predicted |         95% CI
      ------------------------------------
            0 |     11.34 |  7.14,   15.53
           45 |     14.42 | 11.39,   17.45
           85 |     17.16 | 13.78,   20.54
          170 |     22.98 | 16.17,   29.80
      
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

---

    Code
      print(pr, collapse_tables = TRUE)
    Output
      # Predicted values of Negative impact with 7 items
      
      c12hour | barthtot | c161sex | c172code |               e42dep | Predicted |            95% CI
      ----------------------------------------------------------------------------------------------
            0 |     35.2 |    Male |        1 |          independent |     71.62 |  -211.63,  354.87
           45 |          |         |          |                      |   -141.61 |  -617.03,  333.81
           85 |          |         |          |                      |   -331.15 | -1465.18,  802.88
          170 |          |         |          |                      |   -733.93 | -3272.84, 1804.98
            0 |          |         |          |   slightly dependent |      0.44 |   -19.71,   20.60
           45 |          |         |          |                      |     18.88 |   -13.32,   51.08
           85 |          |         |          |                      |     35.27 |   -38.38,  108.92
          170 |          |         |          |                      |     70.09 |   -93.33,  233.52
            0 |          |         |          | moderately dependent |     20.06 |    12.38,   27.75
           45 |          |         |          |                      |     14.25 |     9.92,   18.57
           85 |          |         |          |                      |      9.07 |     0.55,   17.60
          170 |          |         |          |                      |     -1.92 |   -23.57,   19.74
            0 |          |         |          |   severely dependent |     15.58 |    12.61,   18.54
           45 |          |         |          |                      |     14.33 |    12.33,   16.33
           85 |          |         |          |                      |     13.22 |    11.28,   15.16
          170 |          |         |          |                      |     10.87 |     6.89,   14.84
            0 |          |         |        2 |          independent |     44.42 |   -68.88,  157.72
           45 |          |         |          |                      |   -148.19 |  -753.87,  457.50
           85 |          |         |          |                      |   -319.39 | -1560.24,  921.46
          170 |          |         |          |                      |   -683.19 | -3274.30, 1907.92
            0 |          |         |          |   slightly dependent |     13.33 |     4.40,   22.27
           45 |          |         |          |                      |      4.15 |    -5.58,   13.89
           85 |          |         |          |                      |     -4.01 |   -26.27,   18.25
          170 |          |         |          |                      |    -21.35 |   -71.93,   29.23
            0 |          |         |          | moderately dependent |     17.41 |    13.75,   21.08
           45 |          |         |          |                      |     13.25 |    10.06,   16.45
           85 |          |         |          |                      |      9.55 |     2.99,   16.11
          170 |          |         |          |                      |      1.69 |   -13.29,   16.67
            0 |          |         |          |   severely dependent |     13.10 |    11.60,   14.61
           45 |          |         |          |                      |     12.87 |    11.81,   13.93
           85 |          |         |          |                      |     12.67 |    11.60,   13.74
          170 |          |         |          |                      |     12.23 |    10.14,   14.32
            0 |          |         |        3 |          independent |     17.21 |   -98.48,  132.90
           45 |          |         |          |                      |   -154.76 |  -908.89,  599.37
           85 |          |         |          |                      |   -307.62 | -1686.82, 1071.58
          170 |          |         |          |                      |   -632.45 | -3345.46, 2080.56
            0 |          |         |          |   slightly dependent |     26.22 |     5.76,   46.69
           45 |          |         |          |                      |    -10.58 |   -40.20,   19.05
           85 |          |         |          |                      |    -43.29 |  -114.67,   28.10
          170 |          |         |          |                      |   -112.79 |  -273.95,   48.36
            0 |          |         |          | moderately dependent |     14.76 |     9.97,   19.55
           45 |          |         |          |                      |     12.26 |     6.48,   18.03
           85 |          |         |          |                      |     10.03 |     0.19,   19.86
          170 |          |         |          |                      |      5.30 |   -14.72,   25.32
            0 |          |         |          |   severely dependent |     10.63 |     8.41,   12.85
           45 |          |         |          |                      |     11.42 |     9.75,   13.09
           85 |          |         |          |                      |     12.11 |    10.50,   13.73
          170 |          |         |          |                      |     13.60 |    10.83,   16.36
            0 |          |  Female |        1 |          independent |     40.48 |     9.32,   71.65
           45 |          |         |          |                      |    -15.03 |   -60.12,   30.06
           85 |          |         |          |                      |    -64.38 |  -175.65,   46.90
          170 |          |         |          |                      |   -169.24 |  -421.76,   83.28
            0 |          |         |          |   slightly dependent |     13.30 |     5.57,   21.02
           45 |          |         |          |                      |     19.18 |    10.97,   27.39
           85 |          |         |          |                      |     24.41 |     4.55,   44.26
          170 |          |         |          |                      |     35.51 |   -10.18,   81.21
            0 |          |         |          | moderately dependent |     14.29 |    11.01,   17.56
           45 |          |         |          |                      |     13.99 |    11.65,   16.34
           85 |          |         |          |                      |     13.73 |    10.70,   16.76
          170 |          |         |          |                      |     13.17 |     6.63,   19.72
            0 |          |         |          |   severely dependent |     14.41 |    12.91,   15.90
           45 |          |         |          |                      |     13.82 |    12.79,   14.84
           85 |          |         |          |                      |     13.29 |    12.43,   14.15
          170 |          |         |          |                      |     12.17 |    10.59,   13.74
            0 |          |         |        2 |          independent |     23.12 |     4.32,   41.91
           45 |          |         |          |                      |     -9.30 |   -67.00,   48.39
           85 |          |         |          |                      |    -38.12 |  -157.59,   81.34
          170 |          |         |          |                      |    -99.36 |  -351.28,  152.56
            0 |          |         |          |   slightly dependent |     14.15 |    10.28,   18.02
           45 |          |         |          |                      |     15.88 |    11.75,   20.01
           85 |          |         |          |                      |     17.42 |     7.72,   27.11
          170 |          |         |          |                      |     20.68 |    -1.49,   42.86
            0 |          |         |          | moderately dependent |     15.15 |    13.43,   16.87
           45 |          |         |          |                      |     14.02 |    12.73,   15.30
           85 |          |         |          |                      |     13.01 |    10.91,   15.11
          170 |          |         |          |                      |     10.87 |     6.13,   15.62
            0 |          |         |          |   severely dependent |     13.13 |    12.32,   13.93
           45 |          |         |          |                      |     13.71 |    13.15,   14.27
           85 |          |         |          |                      |     14.23 |    13.69,   14.77
          170 |          |         |          |                      |     15.34 |    14.29,   16.38
            0 |          |         |        3 |          independent |      5.75 |   -36.05,   47.55
           45 |          |         |          |                      |     -3.58 |  -124.66,  117.50
           85 |          |         |          |                      |    -11.87 |  -265.52,  241.78
          170 |          |         |          |                      |    -29.48 |  -567.33,  508.36
            0 |          |         |          |   slightly dependent |     15.00 |     6.35,   23.66
           45 |          |         |          |                      |     12.58 |     5.29,   19.87
           85 |          |         |          |                      |     10.43 |    -8.18,   29.04
          170 |          |         |          |                      |      5.85 |   -38.26,   49.97
            0 |          |         |          | moderately dependent |     16.01 |    12.79,   19.23
           45 |          |         |          |                      |     14.04 |    11.54,   16.53
           85 |          |         |          |                      |     12.29 |     7.75,   16.83
          170 |          |         |          |                      |      8.57 |    -1.82,   18.96
            0 |          |         |          |   severely dependent |     11.85 |    10.30,   13.39
           45 |          |         |          |                      |     13.61 |    12.54,   14.68
           85 |          |         |          |                      |     15.18 |    14.05,   16.31
          170 |          |         |          |                      |     18.51 |    16.17,   20.85
            0 |     64.8 |    Male |        1 |          independent |     40.75 |  -110.75,  192.25
           45 |          |         |          |                      |    -66.38 |  -310.11,  177.35
           85 |          |         |          |                      |   -161.61 |  -748.81,  425.59
          170 |          |         |          |                      |   -363.97 | -1683.77,  955.83
            0 |          |         |          |   slightly dependent |      5.25 |    -4.17,   14.67
           45 |          |         |          |                      |     13.71 |    -2.09,   29.52
           85 |          |         |          |                      |     21.23 |   -14.91,   57.37
          170 |          |         |          |                      |     37.21 |   -42.81,  117.24
            0 |          |         |          | moderately dependent |     13.76 |    10.58,   16.94
           45 |          |         |          |                      |     11.14 |     8.98,   13.30
           85 |          |         |          |                      |      8.81 |     3.61,   14.01
          170 |          |         |          |                      |      3.87 |    -8.76,   16.49
            0 |          |         |          |   severely dependent |     14.65 |     9.81,   19.49
           45 |          |         |          |                      |     13.36 |    10.10,   16.62
           85 |          |         |          |                      |     12.21 |     8.48,   15.94
          170 |          |         |          |                      |      9.77 |     1.60,   17.94
            0 |          |         |        2 |          independent |     27.04 |   -30.74,   84.82
           45 |          |         |          |                      |    -72.41 |  -380.46,  235.64
           85 |          |         |          |                      |   -160.81 |  -792.02,  470.40
          170 |          |         |          |                      |   -348.67 | -1666.87,  969.53
            0 |          |         |          |   slightly dependent |     10.92 |     6.80,   15.05
           45 |          |         |          |                      |      9.55 |     5.47,   13.63
           85 |          |         |          |                      |      8.33 |    -1.03,   17.69
          170 |          |         |          |                      |      5.74 |   -15.74,   27.21
            0 |          |         |          | moderately dependent |     12.87 |    11.31,   14.43
           45 |          |         |          |                      |     12.27 |    11.10,   13.44
           85 |          |         |          |                      |     11.73 |     9.20,   14.27
          170 |          |         |          |                      |     10.60 |     4.60,   16.60
            0 |          |         |          |   severely dependent |     12.83 |    10.31,   15.34
           45 |          |         |          |                      |     12.40 |    10.60,   14.20
           85 |          |         |          |                      |     12.02 |     9.94,   14.10
          170 |          |         |          |                      |     11.21 |     6.91,   15.51
            0 |          |         |        3 |          independent |     13.34 |   -52.23,   78.91
           45 |          |         |          |                      |    -78.44 |  -461.06,  304.18
           85 |          |         |          |                      |   -160.02 |  -855.89,  535.85
          170 |          |         |          |                      |   -333.37 | -1698.35, 1031.61
            0 |          |         |          |   slightly dependent |     16.59 |     6.38,   26.81
           45 |          |         |          |                      |      5.39 |    -8.55,   19.32
           85 |          |         |          |                      |     -4.57 |   -38.79,   29.64
          170 |          |         |          |                      |    -25.74 |  -103.50,   52.02
            0 |          |         |          | moderately dependent |     11.98 |     9.68,   14.28
           45 |          |         |          |                      |     13.40 |    11.44,   15.35
           85 |          |         |          |                      |     14.66 |    10.94,   18.38
          170 |          |         |          |                      |     17.34 |     8.93,   25.74
            0 |          |         |          |   severely dependent |     11.00 |     7.75,   14.25
           45 |          |         |          |                      |     11.44 |     8.84,   14.04
           85 |          |         |          |                      |     11.83 |     8.96,   14.70
          170 |          |         |          |                      |     12.66 |     7.50,   17.81
            0 |          |  Female |        1 |          independent |     25.01 |    10.19,   39.83
           45 |          |         |          |                      |     -0.68 |   -19.73,   18.38
           85 |          |         |          |                      |    -23.51 |   -71.77,   24.74
          170 |          |         |          |                      |    -72.04 |  -182.71,   38.63
            0 |          |         |          |   slightly dependent |     11.34 |     7.72,   14.96
           45 |          |         |          |                      |     12.93 |     9.30,   16.55
           85 |          |         |          |                      |     14.34 |     5.46,   23.22
          170 |          |         |          |                      |     17.33 |    -3.25,   37.92
            0 |          |         |          | moderately dependent |     12.04 |    10.66,   13.43
           45 |          |         |          |                      |     12.13 |    11.14,   13.12
           85 |          |         |          |                      |     12.20 |    10.87,   13.53
          170 |          |         |          |                      |     12.35 |     9.43,   15.28
            0 |          |         |          |   severely dependent |     13.66 |    11.06,   16.26
           45 |          |         |          |                      |     12.80 |    10.98,   14.61
           85 |          |         |          |                      |     12.03 |    10.52,   13.54
          170 |          |         |          |                      |     10.40 |     7.86,   12.94
            0 |          |         |        2 |          independent |     16.12 |     6.88,   25.36
           45 |          |         |          |                      |      1.19 |   -23.47,   25.85
           85 |          |         |          |                      |    -12.09 |   -63.39,   39.22
          170 |          |         |          |                      |    -40.30 |  -148.96,   68.36
            0 |          |         |          |   slightly dependent |     11.79 |     9.99,   13.58
           45 |          |         |          |                      |     12.99 |    11.23,   14.76
           85 |          |         |          |                      |     14.07 |     9.92,   18.21
          170 |          |         |          |                      |     16.34 |     6.79,   25.89
            0 |          |         |          | moderately dependent |     12.62 |    11.88,   13.37
           45 |          |         |          |                      |     12.31 |    11.75,   12.87
           85 |          |         |          |                      |     12.03 |    11.12,   12.94
          170 |          |         |          |                      |     11.43 |     9.38,   13.48
            0 |          |         |          |   severely dependent |     12.63 |    11.40,   13.85
           45 |          |         |          |                      |     13.41 |    12.53,   14.28
           85 |          |         |          |                      |     14.10 |    13.14,   15.06
          170 |          |         |          |                      |     15.57 |    13.63,   17.52
            0 |          |         |        3 |          independent |      7.23 |   -12.28,   26.75
           45 |          |         |          |                      |      3.05 |   -47.18,   53.29
           85 |          |         |          |                      |     -0.66 |  -106.24,  104.91
          170 |          |         |          |                      |     -8.56 |  -233.21,  216.09
            0 |          |         |          |   slightly dependent |     12.24 |     8.13,   16.35
           45 |          |         |          |                      |     13.06 |     9.80,   16.33
           85 |          |         |          |                      |     13.80 |     5.25,   22.34
          170 |          |         |          |                      |     15.35 |    -5.09,   35.80
            0 |          |         |          | moderately dependent |     13.20 |    11.78,   14.62
           45 |          |         |          |                      |     12.49 |    11.41,   13.57
           85 |          |         |          |                      |     11.86 |     9.89,   13.83
          170 |          |         |          |                      |     10.52 |     5.98,   15.05
            0 |          |         |          |   severely dependent |     11.59 |     9.20,   13.99
           45 |          |         |          |                      |     14.01 |    12.30,   15.73
           85 |          |         |          |                      |     16.17 |    14.11,   18.23
          170 |          |         |          |                      |     20.74 |    16.41,   25.08
            0 |     94.4 |    Male |        1 |          independent |      9.88 |   -11.45,   31.20
           45 |          |         |          |                      |      8.85 |    -9.51,   27.20
           85 |          |         |          |                      |      7.93 |   -44.16,   60.03
          170 |          |         |          |                      |      5.99 |  -118.60,  130.57
            0 |          |         |          |   slightly dependent |     10.06 |     5.93,   14.19
           45 |          |         |          |                      |      8.54 |     2.80,   14.29
           85 |          |         |          |                      |      7.20 |    -5.75,   20.14
          170 |          |         |          |                      |      4.34 |   -24.46,   33.13
            0 |          |         |          | moderately dependent |      7.45 |     2.87,   12.03
           45 |          |         |          |                      |      8.03 |     3.07,   12.99
           85 |          |         |          |                      |      8.55 |    -2.07,   19.17
          170 |          |         |          |                      |      9.65 |   -14.14,   33.44
            0 |          |         |          |   severely dependent |     13.72 |     5.12,   22.32
           45 |          |         |          |                      |     12.39 |     6.45,   18.32
           85 |          |         |          |                      |     11.20 |     4.94,   17.46
          170 |          |         |          |                      |      8.67 |    -4.27,   21.61
            0 |          |         |        2 |          independent |      9.67 |     3.19,   16.15
           45 |          |         |          |                      |      3.37 |   -28.38,   35.11
           85 |          |         |          |                      |     -2.24 |   -67.61,   63.13
          170 |          |         |          |                      |    -14.15 |  -151.02,  122.73
            0 |          |         |          |   slightly dependent |      8.51 |     6.31,   10.71
           45 |          |         |          |                      |     14.95 |    10.52,   19.37
           85 |          |         |          |                      |     20.67 |    10.90,   30.44
          170 |          |         |          |                      |     32.83 |    11.55,   54.10
            0 |          |         |          | moderately dependent |      8.32 |     6.16,   10.49
           45 |          |         |          |                      |     11.29 |     7.96,   14.61
           85 |          |         |          |                      |     13.92 |     6.80,   21.04
          170 |          |         |          |                      |     19.51 |     3.99,   35.04
            0 |          |         |          |   severely dependent |     12.55 |     8.17,   16.92
           45 |          |         |          |                      |     11.92 |     8.76,   15.08
           85 |          |         |          |                      |     11.37 |     7.93,   14.81
          170 |          |         |          |                      |     10.20 |     3.35,   17.04
            0 |          |         |        3 |          independent |      9.47 |   -13.00,   31.94
           45 |          |         |          |                      |     -2.12 |   -65.06,   60.83
           85 |          |         |          |                      |    -12.41 |  -143.30,  118.47
          170 |          |         |          |                      |    -34.29 |  -311.17,  242.60
            0 |          |         |          |   slightly dependent |      6.96 |     3.03,   10.89
           45 |          |         |          |                      |     21.35 |    11.87,   30.82
           85 |          |         |          |                      |     34.14 |    13.71,   54.56
          170 |          |         |          |                      |     61.32 |    17.38,  105.25
            0 |          |         |          | moderately dependent |      9.20 |     5.85,   12.55
           45 |          |         |          |                      |     14.54 |     7.67,   21.41
           85 |          |         |          |                      |     19.29 |     4.75,   33.83
          170 |          |         |          |                      |     29.37 |    -1.84,   60.59
            0 |          |         |          |   severely dependent |     11.37 |     5.97,   16.77
           45 |          |         |          |                      |     11.46 |     7.16,   15.77
           85 |          |         |          |                      |     11.55 |     6.86,   16.23
          170 |          |         |          |                      |     11.72 |     3.41,   20.03
            0 |          |  Female |        1 |          independent |      9.54 |     5.25,   13.83
           45 |          |         |          |                      |     13.68 |     2.32,   25.04
           85 |          |         |          |                      |     17.35 |    -7.01,   41.71
          170 |          |         |          |                      |     25.16 |   -27.02,   77.33
            0 |          |         |          |   slightly dependent |      9.38 |     7.11,   11.66
           45 |          |         |          |                      |      6.67 |     2.26,   11.09
           85 |          |         |          |                      |      4.27 |    -5.46,   14.00
          170 |          |         |          |                      |     -0.85 |   -22.05,   20.36
            0 |          |         |          | moderately dependent |      9.80 |     7.82,   11.78
           45 |          |         |          |                      |     10.26 |     8.71,   11.80
           85 |          |         |          |                      |     10.67 |     8.80,   12.54
          170 |          |         |          |                      |     11.53 |     7.85,   15.22
            0 |          |         |          |   severely dependent |     12.91 |     8.60,   17.22
           45 |          |         |          |                      |     11.78 |     8.76,   14.80
           85 |          |         |          |                      |     10.77 |     8.30,   13.24
          170 |          |         |          |                      |      8.64 |     4.60,   12.68
            0 |          |         |        2 |          independent |      9.13 |     6.74,   11.52
           45 |          |         |          |                      |     11.68 |     1.71,   21.65
           85 |          |         |          |                      |     13.94 |    -6.67,   34.56
          170 |          |         |          |                      |     18.76 |   -24.53,   62.05
            0 |          |         |          |   slightly dependent |      9.43 |     8.39,   10.46
           45 |          |         |          |                      |     10.11 |     8.33,   11.89
           85 |          |         |          |                      |     10.72 |     6.81,   14.62
          170 |          |         |          |                      |     12.00 |     3.47,   20.54
            0 |          |         |          | moderately dependent |     10.10 |     9.09,   11.12
           45 |          |         |          |                      |     10.60 |     9.79,   11.42
           85 |          |         |          |                      |     11.05 |     9.91,   12.19
          170 |          |         |          |                      |     12.00 |     9.62,   14.37
            0 |          |         |          |   severely dependent |     12.12 |    10.06,   14.19
           45 |          |         |          |                      |     13.10 |    11.62,   14.58
           85 |          |         |          |                      |     13.97 |    12.41,   15.52
          170 |          |         |          |                      |     15.81 |    12.74,   18.88
            0 |          |         |        3 |          independent |      8.72 |     3.20,   14.24
           45 |          |         |          |                      |      9.68 |   -12.37,   31.74
           85 |          |         |          |                      |     10.54 |   -35.22,   56.30
          170 |          |         |          |                      |     12.36 |   -83.90,  108.62
            0 |          |         |          |   slightly dependent |      9.47 |     7.37,   11.57
           45 |          |         |          |                      |     13.54 |    10.05,   17.04
           85 |          |         |          |                      |     17.16 |     9.50,   24.83
          170 |          |         |          |                      |     24.86 |     8.07,   41.64
            0 |          |         |          | moderately dependent |     10.40 |     8.52,   12.28
           45 |          |         |          |                      |     10.95 |     9.36,   12.53
           85 |          |         |          |                      |     11.43 |     9.14,   13.72
          170 |          |         |          |                      |     12.46 |     7.73,   17.19
            0 |          |         |          |   severely dependent |     11.34 |     7.14,   15.53
           45 |          |         |          |                      |     14.42 |    11.39,   17.45
           85 |          |         |          |                      |     17.16 |    13.78,   20.54
          170 |          |         |          |                      |     22.98 |    16.17,   29.80
      
    Message
      
      Not all rows are shown in the output. Use `print(..., n = Inf)` to show
        all rows.

