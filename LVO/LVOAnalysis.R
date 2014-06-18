### Statistical Analysis of Las Vegas Open Tournament Results, Feb. 2014

# Import packages
library(vioplot)

# Read in data
LVO <- read.csv("LVO.csv")

## Analysis of Frequency and Performance of "Allied" Lists
# Raw Percent Based Analysis
percent_allies <- mean(LVO$Allied)
percent_taudar <- mean(LVO$Taudar)
percent_doubletau <- mean(LVO$Dtau)
percent_potentialSeerStar <- mean(LVO$SeerStar)
percent_buffedchaos <- mean(LVO$BuffedChaos)
percent_CSMOnly <- mean(LVO$CSMOnly)
percent_DemonsOnly <- mean(LVO$DemonsOnly)
percent_OtherEldar <- mean(LVO$OtherEldar)

# Comparisons between Lists
Allied <- subset(LVO, Allied==1)$Total
NonAlly <- subset(LVO, Allied==0)$Total
vioplot(Allied,NonAlly,names=c("Allied Armies","Solo Armies"),col="Grey",drawRect=FALSE)
title(xlab="Army Type",ylab="Total Score")
allied_mean <- mean(Allied)
allied_median <- median(Allied)
nonallied_mean <- mean(NonAlly)
nonallied_median <- median(NonAlly)
AlliedTest <- kruskal.test(LVO$Total,LVO$Allied)

## Specific List Performance Breakdown
# Make Data Sets
BA <- subset(LVO, Army=="BA")
CombiChaos <- subset(LVO, Army=="CombiChaos")
CSM <- subset(LVO, Army=="CSM")
DA <- subset(LVO, Army=="DA")
Dem <- subset(LVO, Army=="Daemons")
DE <- subset(LVO, Army=="Dark Eldar")
DT <- subset(LVO, Army=="DoubleTau")
E <- subset(LVO, Army=="Eldar")
EDE <- subset(LVO, Army=="Eldar/DE")
GK <- subset(LVO, Army=="GK")
IG <- subset(LVO, Army=="IG")
N <- subset(LVO, Army=="Necrons")
O <- subset(LVO, Army=="Orks")
SM <- subset(LVO, Army=="SM")
SoB <- subset(LVO, Army=="SoB")
SW <- subset(LVO, Army=="SW")
Tau <- subset(LVO, Army=="Tau")
Taudar <- subset(LVO, Army=="Taudar")
Nids <- subset(LVO, Army=="Tyranids")

# Inquisitorial Armies
Inq <- subset(LVO, Inquisition==1)$Total
NonInq <- subset(LVO, Inquisition==0)$Total
InqMedian <- median(Inq)
NonInqMedian <- median(NonInq)
InqTest <- kruskal.test(LVO$Total,LVO$Inquisition)
vioplot(Inq,NonInq,names=c("Yes","No"),col="Grey",drawRect=FALSE)
title(xlab="Inquisitorial Allies",ylab="Total Score")

# Eldar Lists
vioplot(E$Total,EDE$Total,DE$Total,names=c("Eldar","Eldar/Dark Eldar","Dark Eldar"),col="Grey",drawRect=FALSE)
title(xlab="Type of Eldar",ylab="Total Score")
AllEldar <- rbind(E,EDE,DE)
EldarMedian <- median(E$Total)
DEMedian <- median(DE$Total)
EDEMedian <- median(EDE$Total)
EldarTest <- kruskal.test(AllEldar$Total,AllEldar$Army)

# Tau Lists
vioplot(Tau$Total,Taudar$Total,DT$Total,names=c("Tau","Taudar","Tau/Tau"),col="Grey",drawRect=FALSE)
title(xlab="Type of Tau",ylab="Total Score")
AllTau <- rbind(Tau,Taudar,DT)
TauMedian <- median(Tau$Total)
TaudarMedian <- median(Taudar$Total)
DTauMedian <- median(DT$Total)
TauTest <- kruskal.test(AllTau$Total,AllTau$Army)

#Chaos
vioplot(CSM$Total,CombiChaos$Total,Dem$Total,names=c("CSM","Combined","Daemons"),col="Grey",drawRect=FALSE)
title(xlab="Type of Chaos",ylab="Total Score")
AllChaos <- rbind(CSM,CombiChaos,Dem)
CSMMedian <- median(CSM$Total)
CombieChaosMedian <- median(CombiChaos$Total)
DaemonsMedian <- median(Dem$Total)
ChaosTest <- kruskal.test(AllChaos$Total,AllChaos$Army)

#5h Edition
Old <- subset(LVO, OldCodex==1)
New <- subset(LVO, OldCodex==0)
OldMedian <- median(Old$Total)
NewMedian <- median(New$Total)
vioplot(Old$Total,New$Total,names=c("Fifth Edition","Sixth Edition"),col="Grey",drawRect=FALSE)
title(xlab="Codex Vintage",ylab="Total Score")
AgeTest <- kruskal.test(LVO$Total,LVO$OldCodex)


#Overall
vioplot(BA$Total,CombiChaos$Total,CSM$Total,DA$Total,Dem$Total,
        DE$Total,DT$Total,E$Total,EDE$Total,GK$Total,IG$Total,
        N$Total,O$Total,SM$Total,SoB$Total,SW$Total,Tau$Total,Taudar$Total,Nids$Total,
        col="Grey",drawRect="False",names=c(
          "BA","CC","CSM","DA","CD","DE","DT","E","E/DE","GK",
          "IG","NE","ORK","SM","SoB","SW","Tau","Tau/E","NID"))
title(ylab="Army",xlab="Total Score")
abline(h=2516,col="red3",lwd=3,lty=2)
legend("topright",c("Overall Median"),lwd=3,col=c("Red3"),lty=c(2),bty='n')

OverallTest <- kruskal.test(LVO$Total,LVO$Army)

BAMedian <- median(BA$Total)
DAMedian <- median(DA$Total)
GKMedian <- median(GK$Total)
IGMedian <- median(IG$Total)
NEMEdian <- median(N$Total)
ORKMedian <- median(O$Total)
SMMedian <- median(SM$Total)
SOBMedian <- median(SoB$Total)
SWMedian <- median(SW$Total)
NIDMedian <- median(Nids$Total)
TotalMedian <- median(LVO$Total)

## Clubs
percent_clubs <- mean(LVO$ClubMember)
ClubNo <- subset(LVO, ClubMember==0)$Total
ClubYes <- subset(LVO, ClubMember==1)$Total
ClubNoMedian <- median(ClubNo)
ClubYesMedian <- median(ClubYes)
ClubTest <- kruskal.test(LVO$Total,LVO$ClubMember)
vioplot(ClubYes,ClubNo,names=c("Club Players","Unaffiliated Players"),col="Grey",drawRect=FALSE)
title(xlab="Club Membership",ylab="Total Score")

## Hobby
#Overall
vioplot(BA$HS,CombiChaos$HS,CSM$HS,DA$HS,Dem$HS,
        DE$HS,DT$HS,E$HS,EDE$HS,GK$HS,IG$HS,
        N$HS,O$HS,SM$HS,SoB$HS,SW$HS,Tau$HS,Taudar$HS,Nids$HS,
        col="Grey",drawRect="False",names=c(
          "BA","CC","CSM","DA","CD","DE","DT","E","E/DE","GK",
          "IG","NE","ORK","SM","SoB","SW","Tau","Tau/E","NID"))
title(ylab="Army",xlab="Hobby Score")
abline(h=21,col="red3",lwd=3,lty=2)
legend("topright",c("Overall Median"),lwd=3,col=c("Red3"),lty=c(2),bty='n')

HOverallTest <- kruskal.test(LVO$HS,LVO$Army)
HCCMedian <- median(CombiChaos$HS)
HCSMMedian <- median(CSM$HS)
HDaemMedian <- median(Dem$HS)
HDEMedian <- median(DE$HS)
HDTMedian <- median(DT$HS)
HEldarMedian <- median(E$HS)
HEDEMedian <- median(EDE$HS)
HTauMedian <- median(Tau$HS)
HTaudarMedian <- median(Taudar$HS)
HBAMedian <- median(BA$HS)
HDAMedian <- median(DA$HS)
HGKMedian <- median(GK$HS)
HIGMedian <- median(IG$HS)
HNEMEdian <- median(N$HS)
HORKMedian <- median(O$HS)
HSMMedian <- median(SM$HS)
HSOBMedian <- median(SoB$HS)
HSWMedian <- median(SW$HS)
HNIDMedian <- median(Nids$HS)
HSMedian <- median(LVO$HS)