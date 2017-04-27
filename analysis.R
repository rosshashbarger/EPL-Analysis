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


dataList = list("y" = points, "nTeams" = nTeams, "scored" = scored, "conceded" = conceded, "age" = age, 
                "wages" = wages)

parameters = c("alpha", "beta", "gamma", "lambda", "alpha_0", "beta_0", "gamma_0", "lambda_0", "sigma_2",
               "sigma_a2", "sigma_b2", "sigma_g2", "sigma_l2")

initsValues = list("alpha" = rep(1, nTeams), "beta" = rep(1, nTeams), "gamma" = rep(1, nTeams),
                   "lambda" = rep(1, nTeams),  "alpha_0" = 0, "beta_0" = 0, "gamma_0" = 0, "sigma_2" = 9,
                   "lambda_0" = 0, "sigma_a2" = 9, "sigma_b2" = 9, "sigma_g2" = 9, "sigma_l2" = 9)

adaptSteps = 5000
burnInSteps = 5000
nChains = 2
numSavedSteps = 10000
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

for(i in 1:5){
  p = ggplot(mcmcChainDF, aes(x = ITER, y = mcmcChainDF[ ,varNames[i]])) +
    geom_line(aes(color = CHAIN)) + 
    labs(y = varNames[i])
  print(p)
}







alpha1.plot = ggplot(mcmcChainDF, aes(x = ITER, y = mcmcChainDF[ ,varNames[1]])) +
  geom_line(aes(color = CHAIN)) + 
  labs(y = varNames[1])
beta1.plot = ggplot(mcmcChainDF, aes(x = ITER, y = mcmcChainDF[ ,varNames[6]])) +
  geom_line(aes(color = CHAIN)) + 
  labs(y = varNames[6])
sigma2.plot = ggplot(mcmcChainDF, aes(x = ITER, y = mcmcChainDF[ ,varNames[10]])) +
  geom_line(aes(color = CHAIN)) + 
  labs(y = varNames[10])

grid.arrange(alpha1.plot, beta1.plot, sigma2.plot)


mcmcChainDF.box.alpha = mcmcChainDF %>% select(c(3:7)) %>% melt()
alpha.box = ggplot(mcmcChainDF.box.alpha, aes(x=variable, y=value)) + geom_boxplot() +
  labs(x="", y="Change in PCV")

mcmcChainDF.box.beta = mcmcChainDF %>% select(c(8:12)) %>% melt()
beta.box = ggplot(mcmcChainDF.box.beta, aes(x=variable, y=value)) + geom_boxplot() +
  labs(x="", y="Change in PCV")

grid.arrange(alpha.box, beta.box)