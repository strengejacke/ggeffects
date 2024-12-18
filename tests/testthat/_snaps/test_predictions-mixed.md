# test_predictions, mixed models, print with conditioned values

    Code
      print(test_predictions(fit, terms = c("e16sex", "c172code")))
    Output
      # Pairwise comparisons
      
      e16sex        |      c172code | Contrast |       95% CI |      p
      ----------------------------------------------------------------
      male-male     |    low-medium |    -0.16 | -0.38,  0.07 | 0.169 
      male-male     |      low-high |    -0.28 | -0.62,  0.06 | 0.107 
      male-female   |       low-low |     0.08 | -0.16,  0.32 | 0.513 
      male-female   |    low-medium |    -0.29 | -0.51, -0.06 | 0.012 
      male-female   |      low-high |    -0.61 | -0.93, -0.30 | < .001
      male-male     |   medium-high |    -0.12 | -0.44,  0.20 | 0.457 
      male-female   |    medium-low |     0.24 |  0.01,  0.46 | 0.039 
      male-female   | medium-medium |    -0.13 | -0.32,  0.07 | 0.199 
      male-female   |   medium-high |    -0.45 | -0.75, -0.16 | 0.002 
      male-female   |      high-low |     0.36 |  0.02,  0.70 | 0.038 
      male-female   |   high-medium |    -0.01 | -0.32,  0.31 | 0.969 
      male-female   |     high-high |    -0.33 | -0.71,  0.04 | 0.083 
      female-female |    low-medium |    -0.37 | -0.58, -0.16 | < .001
      female-female |      low-high |    -0.69 | -1.00, -0.38 | < .001
      female-female |   medium-high |    -0.33 | -0.58, -0.07 | 0.013 
    Message
      
      Contrasts are presented as counts.

---

    Code
      print(test_predictions(fit, terms = c("e16sex", "c172code [medium]")))
    Output
      # Pairwise comparisons
      
      e16sex      | Contrast |      95% CI |     p
      --------------------------------------------
      male-female |    -0.13 | -0.32, 0.07 | 0.199
    Message
      
      Contrasts are presented as counts.

