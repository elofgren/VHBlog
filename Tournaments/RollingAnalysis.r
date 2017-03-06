### Rolling Warhammer 40K Tournament Results

# Load necessary libraries
library(MCMCpack)

# Mugu Games - June 7th #
Mugu <- read.csv("~/Documents/Code/VHBlog/Tournaments/Mugu.csv")
mugumc = MCMClogit(Top5 ~ EDE + Allied + SelfAllied + SM +
                     CD + CC + Ork + SoB + SW +
                     IK + GK + Nec + IG + Tau, data=Mugu,
                    burnin=10000, mcmc=100000, thin = 10, tune=0.35, verbose = 1000, b0=0, B0=0.20)

summary(mugumc)

postmugu <- c(-0.7278,-1.1104,-1.1817,-0.4450,0.0,0.0,0.0,-1.6120,0.0,-1.1673,2.2655,-1.1447,-0.7292,-1.1532,0.0,1.1739,-0.7739,2.2316,-0.7039,1.7233)
muguse <- c(0.8800,1.7659,0.9192,2.1139,1.6579,1.7887,1.6973,1.8290,1.8478,1.8170,1.4972,1.9054,1.7115,1.8884,1.7173)
muguvar <- sqrt(muguse)
muguprecision <- 1/mugu

logmcmc = MCMClogit(Top5 ~ EDE + Allied + SelfAllied 
                    + Inq + Eldar + DE + SM + CSM +
                      CD + CC + Nid + Ork + SoB + SW+
                      BA + IK + GK + Nec + IG + Tau, data=BAO,
                    burnin=10000, mcmc=100000, thin = 10,tune=0.35, verbose = 1000, b0=0, B0=0.20)
summary(logmcmc)




# Mugu Games - June 7th #
Mugu <- read.csv("~/Documents/Code/VHBlog/Tournaments/Mugu.csv")
mugumc <- MCMClogit(Mugu$Top)


library(MCMCpack)
n = glm(formula = BAO$Top5 ~ BAO$EDE, family = "binomial")
summary(n)
logmcmc = MCMClogit(BAO$Top5~BAO$EDE, burnin=10000, mcmc=100000, b0=0, B0=0.20)
summary(logmcmc)
#plot(logmcmc)

variance = 5
sd = sqrt(variance)
impliedci = exp(0 + (1.96*sd))
inpliedci2 = exp(0 - (1.96*sd))
