# ggpredict, print_md

    Code
      print(out)
    Output
      +----------+-----------+--------------+
      | barthtot | Predicted | 95% CI       |
      +==========+===========+==============+
      | c161sex: 1                          |
      +----------+-----------+--------------+
      | 0        | 13.48     | 12.10, 14.86 |
      +----------+-----------+--------------+
      | 17       | 12.96     | 11.88, 14.03 |
      +----------+-----------+--------------+
      | 33       | 12.46     | 11.66, 13.27 |
      +----------+-----------+--------------+
      | 50       | 11.94     | 11.37, 12.51 |
      +----------+-----------+--------------+
      | 67       | 11.42     | 10.94, 11.89 |
      +----------+-----------+--------------+
      | 100      | 10.40     | 9.59, 11.21  |
      +----------+-----------+--------------+
      | c161sex: 2                          |
      +----------+-----------+--------------+
      | 0        | 14.38     | 13.52, 15.25 |
      +----------+-----------+--------------+
      | 17       | 13.74     | 13.08, 14.40 |
      +----------+-----------+--------------+
      | 33       | 13.13     | 12.65, 13.62 |
      +----------+-----------+--------------+
      | 50       | 12.49     | 12.17, 12.81 |
      +----------+-----------+--------------+
      | 67       | 11.85     | 11.58, 12.11 |
      +----------+-----------+--------------+
      | 100      | 10.60     | 10.07, 11.12 |
      +==========+===========+==============+
      | Adjusted for: c12hour = 42.10,      |
      | e42dep = 2.93                       |
      +==========+===========+==============+
      Table: Predicted values of Negative impact with 7 items 

---

    Code
      print(out)
    Output
      +----------+----------------------+
      | barthtot | Predicted (95% CI)   |
      +==========+======================+
      | c161sex: 1                      |
      +----------+----------------------+
      | 0        | 13.48 (12.10, 14.86) |
      +----------+----------------------+
      | 17       | 12.96 (11.88, 14.03) |
      +----------+----------------------+
      | 33       | 12.46 (11.66, 13.27) |
      +----------+----------------------+
      | 50       | 11.94 (11.37, 12.51) |
      +----------+----------------------+
      | 67       | 11.42 (10.94, 11.89) |
      +----------+----------------------+
      | 100      | 10.40  (9.59, 11.21) |
      +----------+----------------------+
      | c161sex: 2                      |
      +----------+----------------------+
      | 0        | 14.38 (13.52, 15.25) |
      +----------+----------------------+
      | 17       | 13.74 (13.08, 14.40) |
      +----------+----------------------+
      | 33       | 13.13 (12.65, 13.62) |
      +----------+----------------------+
      | 50       | 12.49 (12.17, 12.81) |
      +----------+----------------------+
      | 67       | 11.85 (11.58, 12.11) |
      +----------+----------------------+
      | 100      | 10.60 (10.07, 11.12) |
      +==========+======================+
      | Adjusted for: c12hour = 42.10,  |
      | e42dep = 2.93                   |
      +==========+======================+
      Table: Predicted values of Negative impact with 7 items 

---

    Code
      print(out)
    Output
      
      +----------+---------+----------------------+
      | barthtot | c161sex | Predicted (95% CI)   |
      +==========+=========+======================+
      | 0        | 1       | 13.48 (12.10, 14.86) |
      +----------+---------+----------------------+
      | 17       |         | 12.96 (11.88, 14.03) |
      +----------+---------+----------------------+
      | 33       |         | 12.46 (11.66, 13.27) |
      +----------+---------+----------------------+
      | 50       |         | 11.94 (11.37, 12.51) |
      +----------+---------+----------------------+
      | 67       |         | 11.42 (10.94, 11.89) |
      +----------+---------+----------------------+
      | 100      |         | 10.40  (9.59, 11.21) |
      +----------+---------+----------------------+
      | 0        | 2       | 14.38 (13.52, 15.25) |
      +----------+---------+----------------------+
      | 17       |         | 13.74 (13.08, 14.40) |
      +----------+---------+----------------------+
      | 33       |         | 13.13 (12.65, 13.62) |
      +----------+---------+----------------------+
      | 50       |         | 12.49 (12.17, 12.81) |
      +----------+---------+----------------------+
      | 67       |         | 11.85 (11.58, 12.11) |
      +----------+---------+----------------------+
      | 100      |         | 10.60 (10.07, 11.12) |
      +==========+=========+======================+
      | Adjusted for: c12hour = 42.10, e42dep =   |
      | 2.93                                      |
      +==========+=========+======================+
      Table: Predicted values of Negative impact with 7 items 

---

    Code
      print(out)
    Output
      
      
      Table: Model-based Contrasts Analysis
      
      |Level1                                  |                                 Level2 | Difference|   SE|        95% CI| t(825)|     p|
      |:---------------------------------------|:---------------------------------------|----------:|----:|-------------:|------:|-----:|
      |low level of education, Female          |           low level of education, Male |       0.80| 0.64| (-0.45, 2.05)|   1.25| 0.210|
      |intermediate level of education, Male   |           low level of education, Male |       0.56| 0.65| (-0.73, 1.84)|   0.85| 0.396|
      |intermediate level of education, Female |           low level of education, Male |       0.78| 0.59| (-0.38, 1.93)|   1.32| 0.186|
      |high level of education, Male           |           low level of education, Male |       0.35| 0.77| (-1.16, 1.86)|   0.45| 0.651|
      |high level of education, Female         |           low level of education, Male |       1.64| 0.66| ( 0.35, 2.93)|   2.49| 0.013|
      |intermediate level of education, Male   |         low level of education, Female |      -0.25| 0.46| (-1.15, 0.66)|  -0.53| 0.593|
      |intermediate level of education, Female |         low level of education, Female |      -0.02| 0.36| (-0.72, 0.68)|  -0.06| 0.949|
      |high level of education, Male           |         low level of education, Female |      -0.45| 0.61| (-1.65, 0.75)|  -0.74| 0.460|
      |high level of education, Female         |         low level of education, Female |       0.84| 0.47| (-0.08, 1.75)|   1.80| 0.072|
      |intermediate level of education, Female |  intermediate level of education, Male |       0.22| 0.39| (-0.53, 0.98)|   0.58| 0.563|
      |high level of education, Male           |  intermediate level of education, Male |      -0.21| 0.63| (-1.44, 1.03)|  -0.33| 0.742|
      |high level of education, Female         |  intermediate level of education, Male |       1.09| 0.49| ( 0.13, 2.04)|   2.23| 0.026|
      |high level of education, Male           |intermediate level of education, Female |      -0.43| 0.56| (-1.53, 0.67)|  -0.77| 0.443|
      |high level of education, Female         |intermediate level of education, Female |       0.86| 0.39| ( 0.09, 1.63)|   2.19| 0.029|
      |high level of education, Female         |          high level of education, Male |       1.29| 0.63| ( 0.05, 2.53)|   2.04| 0.042|
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code, c161sex
      Predictors averaged: c12hour (42), e42dep (2.9)
      

---

    Code
      print(out)
    Output
      
      
      Table: Model-based Contrasts Analysis
      
      |Level1                                  |                                 Level2 | Difference|   SE|        95% CI| t(825)|     p|
      |:---------------------------------------|:---------------------------------------|----------:|----:|-------------:|------:|-----:|
      |low level of education, Female          |           low level of education, Male |       0.80| 0.64| (-0.45, 2.05)|   1.25| 0.210|
      |intermediate level of education, Male   |           low level of education, Male |       0.56| 0.65| (-0.73, 1.84)|   0.85| 0.396|
      |intermediate level of education, Female |           low level of education, Male |       0.78| 0.59| (-0.38, 1.93)|   1.32| 0.186|
      |high level of education, Male           |           low level of education, Male |       0.35| 0.77| (-1.16, 1.86)|   0.45| 0.651|
      |high level of education, Female         |           low level of education, Male |       1.64| 0.66| ( 0.35, 2.93)|   2.49| 0.013|
      |intermediate level of education, Male   |         low level of education, Female |      -0.25| 0.46| (-1.15, 0.66)|  -0.53| 0.593|
      |intermediate level of education, Female |         low level of education, Female |      -0.02| 0.36| (-0.72, 0.68)|  -0.06| 0.949|
      |high level of education, Male           |         low level of education, Female |      -0.45| 0.61| (-1.65, 0.75)|  -0.74| 0.460|
      |high level of education, Female         |         low level of education, Female |       0.84| 0.47| (-0.08, 1.75)|   1.80| 0.072|
      |intermediate level of education, Female |  intermediate level of education, Male |       0.22| 0.39| (-0.53, 0.98)|   0.58| 0.563|
      |high level of education, Male           |  intermediate level of education, Male |      -0.21| 0.63| (-1.44, 1.03)|  -0.33| 0.742|
      |high level of education, Female         |  intermediate level of education, Male |       1.09| 0.49| ( 0.13, 2.04)|   2.23| 0.026|
      |high level of education, Male           |intermediate level of education, Female |      -0.43| 0.56| (-1.53, 0.67)|  -0.77| 0.443|
      |high level of education, Female         |intermediate level of education, Female |       0.86| 0.39| ( 0.09, 1.63)|   2.19| 0.029|
      |high level of education, Female         |          high level of education, Male |       1.29| 0.63| ( 0.05, 2.53)|   2.04| 0.042|
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code, c161sex
      Predictors averaged: c12hour (42), e42dep (2.9)
      

---

    Code
      print(out)
    Output
      
      
      Table: Model-based Contrasts Analysis
      
      |Level1                                  |                                 Level2 | Difference|   SE|        95% CI| t(825)|     p|
      |:---------------------------------------|:---------------------------------------|----------:|----:|-------------:|------:|-----:|
      |low level of education, Female          |           low level of education, Male |       0.80| 0.64| (-0.45, 2.05)|   1.25| 0.210|
      |intermediate level of education, Male   |           low level of education, Male |       0.56| 0.65| (-0.73, 1.84)|   0.85| 0.396|
      |intermediate level of education, Female |           low level of education, Male |       0.78| 0.59| (-0.38, 1.93)|   1.32| 0.186|
      |high level of education, Male           |           low level of education, Male |       0.35| 0.77| (-1.16, 1.86)|   0.45| 0.651|
      |high level of education, Female         |           low level of education, Male |       1.64| 0.66| ( 0.35, 2.93)|   2.49| 0.013|
      |intermediate level of education, Male   |         low level of education, Female |      -0.25| 0.46| (-1.15, 0.66)|  -0.53| 0.593|
      |intermediate level of education, Female |         low level of education, Female |      -0.02| 0.36| (-0.72, 0.68)|  -0.06| 0.949|
      |high level of education, Male           |         low level of education, Female |      -0.45| 0.61| (-1.65, 0.75)|  -0.74| 0.460|
      |high level of education, Female         |         low level of education, Female |       0.84| 0.47| (-0.08, 1.75)|   1.80| 0.072|
      |intermediate level of education, Female |  intermediate level of education, Male |       0.22| 0.39| (-0.53, 0.98)|   0.58| 0.563|
      |high level of education, Male           |  intermediate level of education, Male |      -0.21| 0.63| (-1.44, 1.03)|  -0.33| 0.742|
      |high level of education, Female         |  intermediate level of education, Male |       1.09| 0.49| ( 0.13, 2.04)|   2.23| 0.026|
      |high level of education, Male           |intermediate level of education, Female |      -0.43| 0.56| (-1.53, 0.67)|  -0.77| 0.443|
      |high level of education, Female         |intermediate level of education, Female |       0.86| 0.39| ( 0.09, 1.63)|   2.19| 0.029|
      |high level of education, Female         |          high level of education, Male |       1.29| 0.63| ( 0.05, 2.53)|   2.04| 0.042|
      
      Variable predicted: neg_c_7
      Predictors contrasted: c172code, c161sex
      Predictors averaged: c12hour (42), e42dep (2.9)
      

