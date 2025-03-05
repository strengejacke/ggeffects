# test_predictions, mixed models, print with conditioned values

    Code
      print(test_predictions(fit, terms = c("e16sex", "c172code")))
    Output
      # Pairwise comparisons
      
      e16sex        |      c172code | Contrast |       95% CI |      p
      ----------------------------------------------------------------
      female-female |      low-high |    -0.67 | -0.97, -0.37 | < .001
      female-female |    low-medium |    -0.35 | -0.56, -0.15 | < .001
      female-female |   medium-high |    -0.32 | -0.56, -0.07 | 0.013 
      female-male   |      low-high |    -0.35 | -0.68, -0.02 | 0.038 
      female-male   |    low-medium |    -0.23 | -0.45, -0.01 | 0.039 
      female-male   |   medium-high |     0.01 | -0.30,  0.31 | 0.969 
      male-female   |     high-high |    -0.32 | -0.69,  0.04 | 0.083 
      male-female   |      low-high |    -0.59 | -0.90, -0.29 | < .001
      male-female   |       low-low |     0.08 | -0.16,  0.31 | 0.513 
      male-female   |    low-medium |    -0.28 | -0.49, -0.06 | 0.012 
      male-female   |   medium-high |    -0.44 | -0.72, -0.16 | 0.002 
      male-female   | medium-medium |    -0.12 | -0.31,  0.07 | 0.199 
      male-male     |      low-high |    -0.27 | -0.60,  0.06 | 0.107 
      male-male     |    low-medium |    -0.15 | -0.37,  0.06 | 0.169 
      male-male     |   medium-high |    -0.12 | -0.43,  0.19 | 0.457 
    Message
      
      Contrasts are presented as counts.

