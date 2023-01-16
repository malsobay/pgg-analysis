library(causalTree)
library(tidyverse)

#Interesting behavior when including rounds to end 
method_space = c('playerCount',
                  'numRounds',
                  'showNRounds',
                  'roi',
                  'punishmentCost',
                  'punishment_tech',
                  'showOtherSummaries',
                  'showPunishmentId')


# method_space = c('playerCount',
#                  'numRounds',
#                  'showNRounds',
#                  'roi',
#                  'showOtherSummaries')

df = read.csv("~/Dropbox (MIT)/research/pgg/pgg_roundlevel_wave4.csv") %>%
  mutate(punishmentExists = case_when(punishmentExists == "False" ~ 0, punishmentExists != "False" ~ 1))


fmla <- as.formula(paste("contribution ~ ", paste(method_space, collapse= "+")))

tree <- causalTree(fmla, data = df, 
                   treatment = df$punishmentExists,
                   split.Rule = "CT", cv.option = "CT", 
                   split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 2, propensity = 0.5)

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

opfit <- prune(tree, opcp)

rpart.plot(opfit)
rpart.plot(tree)
