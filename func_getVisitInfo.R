getVisitInfo = function(SubNum, visitNum, visitData = MDL_visit_data) {
  data = filter(visitData, Sub_ID_new == SubNum)
  if(visitNum %in% data$Visit) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}