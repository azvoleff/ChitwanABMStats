#############################################################################
# Does a series of models to try to predict fuelwood usage based on household 
# size, etc.
###############################################################################

library(Hmisc)

load("t3ag.Rdata")

# Dependent variable - fuelwood usage:
#	t3e15.1 - "Since last year till now, approximately, how many bhari, carts, 
#	or quintal of firewood did your household use?"
# 	t3e15.1a - bhari of firewood
# 	t3e15.1b - carts of firewood
# 	t3e15.1c - quintal of firewood
fwusage.bhari<- t3ag$t3e15.1a
fwusage.bhari[is.na(fwusage.bhari)] <- 0
fwusage.cart <- t3ag$t3e15.1b
fwusage.cart[is.na(fwusage.cart)] <- 0
fwusage.quintal <- t3ag$t3e15.1c
fwusage.quintal[is.na(fwusage.quintal)] <- 0
fwusage.kg <- 30*fwusage.bhari + 30*10*fwusage.cart + 30*10*2*fwusage.quintal

# Potential predictor variables:
# 	t3a48 - orchard land? (make dichotomous)
# 	t3c31 - what heating sources do you use in your house for cooking?
# 	t3e15 - use firewood for any purpose
# 	t3e35 - bring fodder/firewood/thatch from community forest
# 	t3b5 - any livestock
# 	t3c45 - gas stove
# 	ethnic group
# 	HH size
# 	distance from national park
# 	distance from comm. forest
# 	distance from comm. forest
fwpred  <- glm(fwusage.total ~ hhsize + ethnic, family=binomial())
