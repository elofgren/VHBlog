setwd("~/Documents/Code/VHBlog/LVO17")
library(readr)
library(Amelia)
library(snow)
library(sandwich)
library(lmtest)
library(reshape)

LVO <- read_csv("~/Documents/Code/VHBlog/LVO17/LVO_2017.csv", col_types = cols(ITC = col_double(),LVO16 = col_integer()))

#Remove Names
LVO$FN <- NULL
LVO$LN <- NULL

LVO$Faction <- as.factor(LVO$Faction)

counts <- table(LVO$Faction)
barplot(counts, main="Army Frequency", 
        xlab="Army",ylim=c(0,50),cex.names=0.50)

LVO$ITC10 <- LVO$ITC/10
LVO$Flip16 <- 291 - LVO$LVO16

itc <- lm(BP~ITC10 + I(ITC10^2),data=LVO)
summary(itc)

preditc <- predict(itc, data.frame(ITC10=c(seq(from=1,to=75,by=1))),interval='confidence')

plot(LVO$ITC10,LVO$BP,xlab="ITC Points/10",ylab="Battle Points")
lines(preditc[,2],col="grey",lwd=3)
lines(preditc[,3],col="grey",lwd=3)
lines(preditc[,1],col="blue",lwd=5)

legend("topleft",c("Quadratic Fit","95% CI"),lwd=c(3,2),col=c("Blue","Grey"),lty=1,bty='n', horiz=T)

mi.lvo <- amelia(LVO,m=100,noms=c("Faction"),parallel="snow",ncpus=4, p2s=2,empri = .01*400,incheck = FALSE)

b.out <- NULL
se.out <- NULL
for(i in 1:mi.lvo$m){
  uni.out <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[i]])
  b.out <- rbind(b.out,uni.out$coef)
  se.out <- rbind(se.out, coeftest(uni.out,vcov=sandwich)[,2])
}
LVO_main <- mi.meld(q = b.out, se = se.out)
print(LVO_main)

# Put in Data Frame
coefs <- as.data.frame(rbind(LVO_main$q.mi,LVO_main$se.mi))

mods <- as.data.frame(t(coefs))
colnames(mods) <- c("Est","SE")
mods$LCL <- mods$Est - (1.96*mods$SE)
mods$UCL <- mods$Est + (1.96*mods$SE)

mifit1 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[1]])
mifit2 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[2]])
mifit3 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[3]])
mifit4 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[4]])
mifit5 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[5]])
mifit6 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[6]])
mifit7 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[7]])
mifit8 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[8]])
mifit9 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[9]])
mifit10 <- lm(BP ~ Faction + ITC10 + I(ITC10^2) + LVO16, data=mi.lvo$imputations[[10]])

mipred1 <- predict(mifit1)
mipred2 <- predict(mifit2)
mipred2 <- predict(mifit3)
mipred2 <- predict(mifit4)
mipred2 <- predict(mifit5)

plot(LVO$BP,mipred1)
lines(LVO$BP,mipred2,type="p")
lines(LVO$BP,mipred3,type="p")
lines(LVO$BP,mipred4,type="p")
lines(LVO$BP,mipred5,type="p")
lines(LVO$BP,mipred6,type="p")
lines(LVO$BP,mipred7,type="p")
lines(LVO$BP,mipred8,type="p")
lines(LVO$BP,mipred9,type="p")
lines(LVO$BP,mipred10,type="p")

plot(mipred1,LVO$BP, xlab="Predicted Score", ylab="Actual Score",xlim=c(50,290),ylim=c(50,290))
abline(coef=c(0,1),col="Red3",lwd=2)
legend("topleft",c("Ideal Fit","Single Result"),lwd=c(2,NA),col=c("Red3","Black"),lty=c(1,1),pch=c(NA,1),bty='n')

