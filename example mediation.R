summary(lm(Gait_Velocity~Waist_Circ, data = corrData_IB))

BMI_KBuckle_Vel_IB_SpecMod = "
# Path c' (direct effect)
Gait_Velocity ~ cp*Waist_Circ

# Path a
Physical_Health ~ a*Waist_Circ

# Path b
Gait_Velocity ~ b*Physical_Health

# Indirect effect
ab := a*b
total := cp + ab"

set.seed(80085)

myMod = sem(BMI_KBuckle_Vel_IB_SpecMod, data = corrData_TO, se = "bootstrap", bootstrap = 5000) # Should bootstrap like 5000 times, but computationally intensive

parameterEstimates(myMod, ci = TRUE, level = 0.95, boot.ci.type = "perc")

summary(myMod, fit.measures = TRUE, rsquare = TRUE)