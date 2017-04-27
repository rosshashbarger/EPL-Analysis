### Run data cleaning file
source("data cleaning.r")



library(rjags)
library(ggplot2)
library(plyr)
library(dplyr)
library(magrittr)
library(gridExtra)
library(reshape)



nTeams = nrow(epl)
scored = epl$total_scored
conceded = epl$total_conceded
age = epl$Age
wages = epl$Wages
points = epl$Points


dataList = list("y" = points, "nTeams" = nTeams, "scored" = scored, "conceded" = conceded)

parameters = c("alpha", "beta", "alpha_0", "beta_0", "sigma_2", "sigma_a2", "sigma_b2")

initsValues = list("alpha" = rep(1, nTeams), "beta" = rep(1, nTeams),  "alpha_0" = 0, "beta_0" = 0, "sigma_2" = 9,
                  "sigma_a2" = 1, "sigma_b2" = 1)

adaptSteps = 25000
burnInSteps = 25000
nChains = 2
numSavedSteps = 50000
thinSteps = 1
ITER = ceiling((numSavedSteps * thinSteps )/ nChains)

jagsModel <- jags.model("epl_model.txt", data = dataList, inits = initsValues, 
                        n.chains = nChains, n.adapt = adaptSteps)

update(jagsModel, n.iter = burnInSteps)

codaSamples <- coda.samples(jagsModel, variable.names = parameters, n.iter = ITER, thin = thinSteps)

mcmcChainDF <- data.frame(as.matrix(codaSamples, iters = T, chains = T))

varNames <- names(mcmcChainDF)[3:(dim(mcmcChainDF)[2])]

nVars <- length(varNames)

mcmcChainDF$CHAIN <- as.factor(mcmcChainDF$CHAIN)

for(i in 1:nVars){
  p = ggplot(mcmcChainDF, aes(x = ITER, y = mcmcChainDF[ ,varNames[i]])) +
    geom_line(aes(color = CHAIN)) + 
    labs(y = varNames[i])
  print(p)
}



mcmcChainDF.box.alpha = mcmcChainDF %>% select(c(3:7)) %>% melt()
alpha.box = ggplot(mcmcChainDF.box.alpha, aes(x=variable, y=value)) + geom_boxplot() +
  labs(x="", y="Change in PCV")

mcmcChainDF.box.beta = mcmcChainDF %>% select(c(8:12)) %>% melt()
beta.box = ggplot(mcmcChainDF.box.beta, aes(x=variable, y=value)) + geom_boxplot() +
  labs(x="", y="Change in PCV")

grid.arrange(alpha.box, beta.box)