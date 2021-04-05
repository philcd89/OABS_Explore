score_PASE = function(actDays, actHours, workAct, colname) {
  
  if (colname %in% c("V0WALK", "V0LTE", "V0MOD", "V0STR", "V0WGT")) {
    if (is.na(actDays) | is.na(actHours)) {
      return(NA)
    }
    
    ConvTable = data.frame(DaysOfActivity = c("0", "1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3", "3"),
                           HoursPerDayActivity = c("0", "1", "2", "3", "4", "1", "2", "3", "4", "1", "2", "3", "4"),
                           HoursPerDay = c(0, 0.11, 0.32, 0.64, 1.07, 0.25, 0.75, 1.50, 2.50, 0.43, 1.29, 2.57, 4.29))
    
    HoursPerDay = ConvTable$HoursPerDay[which(ConvTable$DaysOfActivity == substr(as.character(actDays),1,1) & ConvTable$HoursPerDayActivity == substr(as.character(actHours), 1, 1))]
    
    if (colname == "V0WALK" | colname == "V0GARDN") {
      ActWeight = 20
    } else if (colname == "V0LTE") {
      ActWeight = 21
    } else if (colname == "V0MOD" | colname == "V0STR") {
      ActWeight = 23
    } else if (colname == "V0WGT" | colname == "V0HOME") {
      ActWeight = 30
    } else if (colname == "V0LHW" | colname == "V0HHW") {
      ActWeight = 25
    } else if (colname == "V0LAWN") {
      ActWeight = 36
    } else if (colname == "V0CARE") {
      ActWeight = 35
    } else if (colname == "V0WORK") {
      ActWeight = 21
    }
    
    itemScore = HoursPerDay * ActWeight
    
  } else if (colname == "V0WORK") {
    
    if (substr(as.character(workAct), 1, 1) == "1") {
      ActWeight = 0
    } else {
      ActWeight = 21
    }
    
    itemScore = (actHours/7) * ActWeight
    
  } else {
    
    if (colname == "V0WALK" | colname == "V0GARDN") {
      ActWeight = 20
    } else if (colname == "V0LTE") {
      ActWeight = 21
    } else if (colname == "V0MOD" | colname == "V0STR") {
      ActWeight = 23
    } else if (colname == "V0WGT" | colname == "V0HOME") {
      ActWeight = 30
    } else if (colname == "V0LHW" | colname == "V0HHW") {
      ActWeight = 25
    } else if (colname == "V0LAWN") {
      ActWeight = 36
    } else if (colname == "V0CARE") {
      ActWeight = 35
    } else if (colname == "V0WORK") {
      ActWeight = 21
    }
    
    itemScore = as.numeric(substr(as.character(actDays), 1, 1)) * ActWeight
    
  }
  return(itemScore)
}






#   
#   
#   if(substr(actType, 1, 1) == 0) {
#     HourPerDay = 0
#   } else if {
#     (substr(actType, 1, 1) ==1) {
#       if (actTime == 1) {
#         HoursPerDay = 0.11
#       } else if (actTime == 2) {
#         HoursPerDay = 
#       }
#     }
#   }
# }