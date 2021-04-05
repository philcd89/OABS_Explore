score_SF12 = function (sf12_data) {
  
  sf12_scored = numeric(nrow(sf12_data))
  
  for (i in 1:nrow(sf12_data)) {
    
    # Item 1
    if(!(is.na(sf12_data$V0SF1[i]) | sf12_data$V0SF1[i] == "")) {
      
      score1 = as.numeric(substr(as.character(sf12_data$V0SF1[i]), 1, 1))
      
      if (score1 == 5) {
        score1_wt = -8.37399
      } else if (score1 == 4) {
        score1_wt = -5.56461
      } else if (score1 == 3) {
        score1_wt = -3.02396
      } else if (score1 == 2) {
        score1_wt = -1.31872
      } else {
        score1_wt = 0
      }
    } else {
      score1_wt = NA
    }
    
    # Item 2
    if(!(is.na(sf12_data$V0SF2[i]) | sf12_data$V0SF2[i] == "")) {
      
      score2 = as.numeric(substr(as.character(sf12_data$V0SF2[i]), 1, 1))
      
      if (score2 == 1) {
        score2_wt = -7.23216
      } else if (score2 == 2) {
        score2_wt = -3.45555
      } else {
        score2_wt = 0
      }
    } else {
      score2_wt = NA
    }
    
    # Item 3
    if(!(is.na(sf12_data$V0SF3[i]) | sf12_data$V0SF3[i] == "")) {
      
      score3 = as.numeric(substr(as.character(sf12_data$V0SF3[i]), 1, 1))
      
      if (score3 == 1) {
        score3_wt = -6.24397
      } else if (score3 == 2) {
        score3_wt = -2.73557
      } else {
        score3_wt = 0
      }
    } else {
      score3_wt = NA
    }
    
    # Item 4
    if (!(is.na(sf12_data$V0SF4[i]) | sf12_data$V0SF4[i] == "")) {
      
      score4 = as.numeric(substr(as.character(sf12_data$V0SF4[i]), 1, 1))
      
      if (score4 == 1) {
        score4_wt = -4.61617
      } else {
        score4_wt = 0
      }
    } else {
      score4_wt = NA
    }
    
    # Item 5
    if(!(is.na(sf12_data$V0SF5[i]) | sf12_data$V0SF5[i] == "")) {
      
      score5 = as.numeric(substr(as.character(sf12_data$V0SF5[i]), 1, 1))
      
      if (score5 == 1) {
        score5_wt = -5.51747
      } else {
        score5_wt = 0
      }
    } else {
      score5_wt = NA
    }
    
    # Item 6
    if(!(is.na(sf12_data$V0SF6[i]) | sf12_data$V0SF6[i] == "")) {
      
      score6 = as.numeric(substr(as.character(sf12_data$V0SF6[i]), 1, 1))
      
      if (score6 == 1) {
        score6_wt = 3.04365
      } else {
        score6_wt = 0
      }
    } else {
      score6_wt = NA
    }
    
    # Item 7
    if(!(is.na(sf12_data$V0SF7[i]) | sf12_data$V0SF7[i] == "")) {
      
      score7 = as.numeric(substr(as.character(sf12_data$V0SF7[i]), 1, 1))
      
      if (score7 == 1) {
        score7_wt = 2.32091
      } else {
        score7_wt = 0
      }
    } else {
      score7_wt = NA
    }
    
    # Item 8
    if(!(is.na(sf12_data$V0SF8[i]) | sf12_data$V0SF8[i] == "")) {
      
      score8 = as.numeric(substr(as.character(sf12_data$V0SF8[i]), 1, 1))
      
      if (score8 == 4) {
        score8_wt = -11.25544
      } else if (score8 == 3) {
        score8_wt = -8.38063
      } else if (score8 == 2) {
        score8_wt = -6.50522
      } else if (score8 == 1) {
        score8_wt = -3.80130
      } else {
        score8_wt = 0
      }
    } else {
      score8_wt = NA
    }
    
    # Item 9
    if(!(is.na(sf12_data$V0SF9[i]) | sf12_data$V0SF9[i] == "")) {
      
      score9 = as.numeric(substr(as.character(sf12_data$V0SF9[i]), 1, 1))
      
      if (score9 == 0) {
        score9_wt = 3.46638
      } else if (score9 == 1) {
        score9_wt = 2.90426
      } else if (score9 == 2) {
        score9_wt = 2.37241
      } else if (score9 == 3) {
        score9_wt = 1.36689
      } else if (score9 == 4) {
        score9_wt = 0.66514
      } else {
        score9_wt = 0
      }
    } else {
      score9_wt = NA
    }
    
    # Item 10
    
    if(!(is.na(sf12_data$V0SF10[i]) | sf12_data$V0SF10[i] == "")) {
      
      score10 = as.numeric(substr(as.character(sf12_data$V0SF10[i]), 1, 1))
      
      if (score10 == 0) {
        score10_wt = -2.44706
      } else if (score10 == 1) {
        score10_wt = -2.02168
      } else if (score10 == 2) {
        score10_wt = -1.61850
      } else if (score10 == 3) {
        score10_wt = -1.14387
      } else if (score10 == 4) {
        score10_wt = -0.12251
      } else {
        score10_wt = 0
      }
    } else {
      score10_wt = NA
    }
    
    # Item 11
    if(!(is.na(sf12_data$V0SF11[i]) | sf12_data$V0SF11[i] == "")) {
      
      score11 = as.numeric(substr(as.character(sf12_data$V0SF11[i]), 1, 1))
      
      if (score11 == 0) {
        score11_wt = 0
      } else if (score11 == 1) {
        score11_wt = 0.41188
      } else if (score11 == 2) {
        score11_wt = 1.28044
      } else if (score11 == 3) {
        score11_wt = 2.34247
      } else if (score11 == 4) {
        score11_wt = 3.41593
      } else {
        score11_wt = 4.61446
      }
    } else {
      score11_wt = NA
    }
    
    # Item 12
    if(!(is.na(sf12_data$V0SF12[i]) | sf12_data$V0SF12[i] == "")) {
      
      score12 = as.numeric(substr(as.character(sf12_data$V0SF12[i]), 1, 1))
      
      if (score12 == 0) {
        score12_wt = 0
      } else if (score12 == 1) {
        score12_wt = 0.11038
      } else if (score12 == 2) {
        score12_wt = -0.18043
      } else if (score12 == 3) {
        score12_wt = -0.94342
      } else {
        score12_wt = -0.33682
      }
    } else {
      score12_wt = NA
    }
    
    sf12_subScore = sum(c(score1_wt, score2_wt, score3_wt, score4_wt, score5_wt, score6_wt, score7_wt, score8_wt, score9_wt, score10_wt, score11_wt, score12_wt, 56.57706))
    
    sf12_scored[i] = sf12_subScore
    
  }
  
  return(sf12_scored)
  
}








