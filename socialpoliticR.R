rm(list=c())

library(forecast)
library(zoo)
library(reshape2)
library(lme4)

setwd("C:/Users/jakob/Dropbox/Jakob/Stats")

projEcon <- read.csv("./1_ProjektionSocialPol.txt", sep="")

#projEcon <- scale(projEcon[,2:15])
projEcon <- as.data.frame(projEcon )

projEcon$date <- as.Date(projEcon$date, '%d.%m.%Y')
projEcon$d_diff <- as.numeric(projEcon$date-projEcon$date[1])

head(projEcon)

pE.SG <- projEcon[,c(1:6,16:22)]
head(pE.SG)
data_long <- melt(pE.SG,
        # ID variables - all the variables to keep but not split apart on
    id.vars=c("d_diff", "CDU.CSU", "SPD"),
        # The source columns
    measure.vars=c("CDU.CSU.S", "SPD.S", "keine.S" ),
        # Name of the destination column that will identify the original
        # column that the measurement came from
    variable.name="Partei",
    value.name="Zustimmung.SG"
)
str(data_long)

## first stats
fit.SPD <- lm(SPD ~ CDU.S + SPD.S + Grüne_S + Linke_S + keine_S + d_diff, data = projEcon)
summary(fit.SPD)

fit.CDU <- lm(CDU.CSU ~ CDU.S + SPD.S + Grüne_S + Linke_S + keine_S + d_diff, data = projEcon)
summary(fit.CDU)

fit.Linke <- lm(Linke ~ CDU.S + SPD.S + Grüne_S + Linke_S + keine_S + d_diff, data = projEcon)
summary(fit.Linke)

fit.Grüne <- lm(Grüne ~ CDU.S + SPD.S + Grüne_S + Linke_S + keine_S + d_diff, data = projEcon)
summary(fit.Grüne)

fit.FDP <- lm(FDP ~ CDU.S + SPD.S + Grüne_S + Linke_S + keine_S + d_diff, data = projEcon)
summary(fit.FDP)

fit.AfD <- lm(AfD ~ CDU.S + SPD.S + Grüne_S + Linke_S + keine_S + d_diff, data = projEcon)
summary(fit.AfD)

### new analysis AFD ~ SPD.S || AFD ~ CDU.CSU.S

plot.ts(projEcon$AfD, col="blue", ylim=c(0,65), xlab = "Weeks", ylab = "")
points(projEcon$SPD.S, type = "l", col="red")
points(projEcon$CDU.CSU.S, type = "l", col="black")
legend("topleft", c("AfD", "SPD.SG", "CDU/CSU.SG"), lty=c(1,1,1), col= c("blue", "red","black"))
# dev.off()

Polls <- zoo(projEcon$AfD)
GN.S <- zoo(projEcon$SPD.S)
GN.C <- zoo(projEcon$SPD.C)

ndiffs(Polls, alpha=0.05, test=c("kpss")) ## [1] 1
ndiffs(GN.S, alpha=0.05, test=c("kpss")) ## [1] 1
ndiffs(GN.C, alpha=0.05, test=c("kpss")) ## [1] 0 warning
# differenced time series
GNDiff.S <- diff(GN.S)
GNDiff.C <- diff(GN.C)
PollsDiff <- diff(Polls)

# png("Differenz.png", type = "cairo", width = 16, height = 10, res = 600, units = "cm")
plot.ts(GNDiff.S, col="red", xlab = "Weeks", ylab = "", ylim = c(-30,30))
points(PollsDiff, type = "l", col="blue")
legend("topleft", c("SPD.SG", "AfD"), lty=c(1,1), col= c("red", "blue"))
# dev.off()

# png("DifferenzScaled.png", type = "cairo", width = 16, height = 10, res = 600, units = "cm")
plot.ts(scale(GNDiff.S), col="red", xlab = "Weeks", ylab = "", ylim = c(-4,6))
points(scale(PollsDiff), type = "l", col="blue")
legend("topleft", c("SPD.SG", "AfD"), lty=c(1,1), col= c("red", "blue"))
#dev.off()

library(lmtest)

#AfD~SPD.S
grangertest(GNDiff.S ~ PollsDiff, order=1)
grangertest(GNDiff.S ~ PollsDiff, order=2)
grangertest(GNDiff.S ~ PollsDiff, order=3)
grangertest(GNDiff.S ~ PollsDiff, order=4) #p=.05(.)
grangertest(GNDiff.S ~ PollsDiff, order=5) #p=.09(.)
grangertest(GNDiff.S ~ PollsDiff, order=6)

grangertest(PollsDiff  ~ GNDiff.S, order=1)
grangertest(PollsDiff  ~ GNDiff.S, order=2)
grangertest(PollsDiff  ~ GNDiff.S, order=3)
grangertest(PollsDiff  ~ GNDiff.S, order=4)
grangertest(PollsDiff  ~ GNDiff.S, order=5)
grangertest(PollsDiff  ~ GNDiff.S, order=6)

# png("DifferenzScaledLag5.png", type = "cairo", width = 16, height = 10, res = 600, units = "cm")
plot.ts(scale(GNDiff.S), type="n", ylim=c(-4,6), xlab = "Weeks", ylab = "")
points(scale(lag(GNDiff.S,5)), type= "l", col ="red")
points(scale(PollsDiff), type = "l", col ="blue")
legend("topleft", c("AfD", "SPD.SG"), lty=c(1,1), col= c("darkblue", "red"))
# dev.off()

dfLaged <- cbind.data.frame(PollsDiff[6:25], PollsDiff[5:24], PollsDiff[4:23],
PollsDiff[3:22], PollsDiff[2:21], PollsDiff[1:20],
GNDiff.S[6:25], GNDiff.S[5:24], GNDiff.S[4:23],GNDiff.S[3:22], GNDiff.S[2:21], GNDiff.S[1:20])
colnames(dfLaged) <- c("Polls", "P1","P2", "P3", "P4","P5", "N", "N1", "N2", "N3", "N4","N5")
dfLaged <- dfLaged[-20,]
fit <- lm(Polls ~ P1+P2+P3+P4+P5+N1+N2+N3+N4+N5, data = dfLaged)
fit2 <- lm(Polls ~P1+P2+P3+P4+P5, data = dfLaged)
summary(fit)
summary(fit2)
waldtest(fit, fit2)

GNnew <- GN
GNnew[26:17] <- 0
GNDiffN <- diff(GNnew)
dfLagedN <- cbind.data.frame(PollsDiff[6:25], PollsDiff[5:24], PollsDiff[4:23],
PollsDiff[3:22], PollsDiff[2:21], PollsDiff[1:20],
GNDiff.S[6:25], GNDiff.S[5:24], GNDiff.S[4:23],GNDiff.S[3:22], GNDiff.S[2:21], GNDiff.S[1:20])
colnames(dfLagedN) <- c("Polls", "P1","P2", "P3", "P4","P5", "N", "N1", "N2", "N3", "N4","N5")

reDiff <- function(x,s){
  a <- c(as.numeric(s[1]))
  for (i in 2:length(x)){
    a <- c(a, a[i-1]+as.numeric(x[i-1]))
  }
  return(a)
}
sim <- predict(fit, newdata = dfLagedN, interval = "confidence")

# png("DifferenzScaledConf.png", type = "cairo", width = 16, height = 10, res = 600, units = "cm")
plot(as.numeric(PollsDiff[-c(1:5)]), type="l", col = "red", xlab = "Weeks", 
	ylab = "", ylim=c(-60,60))
points(sim[,1], type = "l", col="blue")
points(sim[,2], type = "l", col="black")
points(sim[,3], type = "l", col="black")
# dev.off()

simM <- reDiff(sim[,1], Polls[1])

# png("Sim.png", type = "cairo", width = 16, height = 10, res = 600, units = "cm")
par(mfrow=c(1,1))
plot(as.numeric(Polls[-c(1:5)]), type="l", col = "red", xlab = "Weeks", ylab = "", ylim=c(0,20))
points(simM, type = "l", col="blue")
legend("topleft", c("Simulation", "Mean Polls"), lty=c(1,1), col= c("blue", "red"))
abline(v=20) #ab wann werte auf 0 gesetzt
# dev.off()

par(mfrow=c(2,2))
plot(fit)





