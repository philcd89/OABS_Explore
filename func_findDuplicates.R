findDuplicates = function(SubNum, MDLdata = MDL_data, BMCdata = BMC_data){
  
  
  
  
  MDL_SubData = MDLdata %>%
    filter(Sub_ID == as.integer(SubNum))
  
  MDL_numAges = nlevels(as.factor(MDL_SubData$Age))
  MDL_Ages = levels(as.factor(MDL_SubData$Age))
  
  MDL_numHeights = nlevels(as.factor(MDL_SubData$Height))
  MDL_Heights = levels(as.factor(MDL_SubData$Height))
  
  
  
  
  BMC_SubData = BMCdata %>%
    filter(Sub_ID == as.integer(SubNum))
  
  BMC_numAges = nlevels(as.factor(BMC_SubData$Age))
  BMC_Ages = levels(as.factor(BMC_SubData$Age))
  
  BMC_numHeights = nlevels(as.factor(BMC_SubData$V0HTCM))
  BMC_Heights = levels(as.factor(BMC_SubData$V0HTCM))
  
  
  
  
  return(list(SubNum, MDL_numAges, MDL_Ages, MDL_numHeights, MDL_Heights, BMC_numAges, BMC_Ages, BMC_numHeights, BMC_Heights))
  
}