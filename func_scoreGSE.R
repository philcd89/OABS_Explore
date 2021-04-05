score_GSE = function(ChangeActivity, SF12_1, SF12_2, SF12_5) {
  
  # Get actual score
  ChangeActivity_score = as.numeric(substr(as.character(ChangeActivity), 1, 1))
  SF12_1_score = as.numeric(substr(as.character(SF12_1), 1, 1))
  SF12_2_score = as.numeric(substr(as.character(SF12_2), 1, 1))
  SF12_5_score = as.numeric(substr(as.character(SF12_5), 1, 1))
  
  # Flip ChangeActivity and SF12_1 so that larger scores become indicative of higher efficacy
  
  if (!is.na(ChangeActivity_score)) {
    if (ChangeActivity_score == 1) {
      ChangeActivity_score = 0
    } else if (ChangeActivity_score == 0) {
      ChangeActivity_score = 1
    }
  }
  
  # modiifying so that that Excellent (1), Very Good (2), Good (3) are scores as 1, and Fair (4) and Poor (5) are scored as 0
  if (!is.na(SF12_1_score)) {
    if (SF12_1_score == 1) {
      SF12_1_score = 1
    } else if (SF12_1_score == 2) {
      SF12_1_score = 1
    } else if (SF12_1_score == 3) {
      SF12_1_score = 1
    } else if (SF12_1_score == 4) {
      SF12_1_score = 0
    } else if (SF12_1_score == 5) {
      SF12_1_score = 0
    }
  }
  
  # modiifying so that that Excellent (1), Very Good (2), Good (3) are scores as 1, and Fair (4) and Poor (5) are scored as 0
  if (!is.na(SF12_2_score)) {
    if (SF12_2_score == 1) {
      SF12_2_score = 0
    } else if (SF12_2_score == 2) {
      SF12_1_score = 0
    } else if (SF12_2_score == 3) {
      SF12_1_score = 1
    } 
  }
  
  #Flip score so that responses to "were you limited in your activities" YES (1) is scored as 0 and NO (0) scored as 1
  if (!is.na(SF12_5_score)) {
    if (SF12_5_score == 0) {
      SF12_5_score = 1
    } else if (SF12_5_score == 1) {
      SF12_5_score = 0
    }
  }
  
  # if (!is.na(ChangeActivity_score)) {
  #   if (ChangeActivity_score == 1) {
  #     ChangeActivity_score = 0
  #   } else if (ChangeActivity_score == 0) {
  #     ChangeActivity_score = 1
  #   }
  # }
  # 
  # if (!is.na(SF12_1_score)) {
  #   if (SF12_1_score == 1) {
  #     SF12_1_score = 5
  #   } else if (SF12_1_score == 2) {
  #     SF12_1_score = 4
  #   } else if (SF12_1_score == 4) {
  #     SF12_1_score = 2
  #   } else if (SF12_1_score == 5) {
  #     SF12_1_score = 1
  #   }
  # }
  # 
  # if (!is.na(SF12_5_score)) {
  #   if (SF12_5_score == 0) {
  #     SF12_5_score = 1
  #   } else if (SF12_5_score == 1) {
  #     SF12_5_score = 0
  #   }
  # }
  
  GSE_score = sum(c(ChangeActivity_score, SF12_1_score, SF12_2_score, SF12_5_score), na.rm = TRUE)
  
  return(GSE_score)
  
}