rm(list=c())

library(forecast)
library(zoo)
library(reshape2)
library(lme4)
library(Hmisc)
library(lmtest)

##############################################################################################

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

rcorr(as.matrix(projEcon[,2:13]))

##############################################################################################

### new analysis Grüne ~ Grüne.S || AFD ~ CDU.CSU.S

plot.ts(projEcon$Grüne, col="green", ylim=c(0,65), xlab = "Weeks", ylab = "")
points(projEcon$Grüne_S, type = "l", col="blue")
legend("topleft", c("Grüne", "Grüne.social"), lty=c(1,1,1), col= c("green", "blue"))
# dev.off()
Polls.c <- zoo(projEcon$CDU.CSU)
GN.c <- zoo(projEcon$CDU.S)

Polls <- zoo(projEcon$Grüne)
GN.S <- zoo(projEcon$Grüne_S)

Polls.s <- zoo(projEcon$SPD)
GN.sS <- zoo(projEcon$SPD.S)

Polls.l <- zoo(projEcon$Linke)
GN.sl <- zoo(projEcon$Linke_S)

Polls.a <- zoo(projEcon$AfD)

ndiffs(Polls, alpha=0.05, test=c("kpss")) ## [1] 1
ndiffs(GN.S, alpha=0.05, test=c("kpss")) ## [1] 1
# differenced time series
GNDiff.c <- diff(GN.c)
PollsDiff.c <- diff(Polls.c)

GNDiff.S <- diff(GN.S)
PollsDiff <- diff(Polls)

GNDiff.sS <- diff(GN.sS)
PollsDiff.s <- diff(Polls.s)

GNDiff.sl <- diff(GN.sl)
PollsDiff.l <- diff(Polls.l)

PollsDiff.a <- diff(Polls.a)

##############################################################################################

# CDU/CSU
jpeg("a_csuparty.jpg", res=500, width = 20, height = 18, units="cm")
plot.ts(scale(GNDiff.c), col="blue", xlab = "Messzeitpunkte", ylab = "Prognose", 
	ylim = c(-4,6),main="Zusammenhang zwischen Prognose und Sozialpolitik CDU/CSU",lwd=3, lty=2)
points(scale(PollsDiff.c), type = "l", col="black", lwd=3, lty=1)
legend(3,6, c("CDU/CSU Sozialpolitik (SP, skaliert)", "CDU/CSU Sonntagsfrage (skaliert)"), 
	lty=c(2,1), col= c("blue", "black"), lwd=c(3,3))

a1 <- grangertest(GNDiff.c ~ PollsDiff.c, order=1) # p = 0.049*
a2 <- grangertest(GNDiff.c ~ PollsDiff.c, order=2)
a3 <- grangertest(GNDiff.c ~ PollsDiff.c, order=3)
a4 <- grangertest(GNDiff.c ~ PollsDiff.c, order=4) 
a5 <- grangertest(GNDiff.c ~ PollsDiff.c, order=5) 

b1 <- grangertest(PollsDiff.c  ~ GNDiff.c, order=1)
b2 <- grangertest(PollsDiff.c  ~ GNDiff.c, order=2)
b3 <- grangertest(PollsDiff.c  ~ GNDiff.c, order=3)
b4 <- grangertest(PollsDiff.c  ~ GNDiff.c, order=4)
b5 <- grangertest(PollsDiff.c  ~ GNDiff.c, order=5)

text(12,-3, paste("GrangerTest, 1st order: SP~Prognose: F(", a1[[1]][2], ",", a1[[2]][2],") = ", 
	round(a1[[3]][2],digits=3),", p = ", round(a1[[4]][2],digits=3), sep=""))
text(11.8,-3.5, paste("GrangerTest, 1nd order: Prognose~SP: F(", b1[[1]][2], ",", b1[[2]][2],") = ", 
	round(b1[[3]][2],digits=3),", p = ", round(b1[[4]][2],digits=3), sep=""))
text(16,4,"Daten: Forschungsgruppe Wahlen 19.09.2013 - 15.09.2017")
text(20,3.5,"Grafik: Jakob Fink @jaemaf")
dev.off()

# B90/DIE GRÜNEN
jpeg("a_greenparty.jpg", res=500, width = 20, height = 18, units="cm")
plot.ts(scale(GNDiff.S), col="blue", xlab = "Messzeitpunkte", ylab = "Prognose", 
	ylim = c(-4,6),main="Zusammenhang zwischen Prognose und Sozialpolitik GRÜNE",lwd=3, lty=2)
points(scale(PollsDiff), type = "l", col="green", lwd=3, lty=1)
legend(3,6, c("B90/Grüne Sozialpolitik (SP, skaliert)", "B90/Grüne Sonntagsfrage (skaliert)"), 
	lty=c(2,1), col= c("blue", "green"), lwd=c(3,3))

a1 <- grangertest(GNDiff.S ~ PollsDiff, order=1) # p = 0.03*
a2 <- grangertest(GNDiff.S ~ PollsDiff, order=2) # p = 0.008**
a3 <- grangertest(GNDiff.S ~ PollsDiff, order=3) # p = 0.04*
a4 <- grangertest(GNDiff.S ~ PollsDiff, order=4) 
a5 <- grangertest(GNDiff.S ~ PollsDiff, order=5) 

b1 <- grangertest(PollsDiff  ~ GNDiff.S, order=1)
b2 <- grangertest(PollsDiff  ~ GNDiff.S, order=2) # p = 0.01*
b3 <- grangertest(PollsDiff  ~ GNDiff.S, order=3) # p = 0.01*
b4 <- grangertest(PollsDiff  ~ GNDiff.S, order=4)
b5 <- grangertest(PollsDiff  ~ GNDiff.S, order=5)

text(12,-3, paste("GrangerTest, 2nd order: SP~Prognose: F(", a2[[1]][2], ",", a2[[2]][2],") = ", 
	round(a2[[3]][2],digits=3),", p = ", round(a2[[4]][2],digits=3), sep=""))
text(11.8,-3.5, paste("GrangerTest, 2nd order: Prognose~SP: F(", b2[[1]][2], ",", b2[[2]][2],") = ", 
	round(b2[[3]][2],digits=3),", p = ", round(b2[[4]][2],digits=3), sep=""))
text(16,4,"Daten: Forschungsgruppe Wahlen 19.09.2013 - 15.09.2017")
text(20,3.5,"Grafik: Jakob Fink @jaemaf")
dev.off()

# SPD
jpeg("a_spdparty.jpg", res=500, width = 20, height = 18, units="cm")
plot.ts(scale(GNDiff.sS), col="blue", xlab = "Messzeitpunkte", ylab = "Prognose", ylim = c(-4,6),
	main="Zusammenhang zwischen Prognose und Sozialpolitik SPD", lwd=3,lty=2)
points(scale(PollsDiff.s), type = "l", col="red", lwd=3)
legend(3,6, c("SPD Sozialpolitik (SP, skaliert)", "SPD Sonntagsfrage (skaliert)"), lty=c(2,1), 
	col= c("blue", "red"), lwd=c(3,3))

a1 <- grangertest(GNDiff.sS ~ PollsDiff.s, order=1) # p < 0.001***
a2 <- grangertest(GNDiff.sS ~ PollsDiff.s, order=2) # p < 0.001***
a3 <- grangertest(GNDiff.sS ~ PollsDiff.s, order=3) # p < 0.001***
a4 <- grangertest(GNDiff.sS ~ PollsDiff.s, order=4) # p = 0.002** 
a5 <- grangertest(GNDiff.sS ~ PollsDiff.s, order=5) # p = 0.02* 
a6 <- grangertest(GNDiff.sS ~ PollsDiff.s, order=6) 

b1 <- grangertest(PollsDiff.s  ~ GNDiff.sS, order=1) # p < 0.001***
b2 <- grangertest(PollsDiff.s  ~ GNDiff.sS, order=2) # p < 0.001***
b3 <- grangertest(PollsDiff.s  ~ GNDiff.sS, order=3) # p < 0.001***
b4 <- grangertest(PollsDiff.s  ~ GNDiff.sS, order=4) # p = 0.003**
b5 <- grangertest(PollsDiff.s  ~ GNDiff.sS, order=5) # p = 0.04*

text(12,-3, paste("GrangerTest, 1st order: SP~Prognose: F(", a1[[1]][2], ",", a1[[2]][2],") = ", 
	round(a1[[3]][2],digits=3),", p = ", round(a1[[4]][2],digits=3), sep=""))
text(12,-3.5, paste("GrangerTest, 1st order: Prognose~SP: F(", b1[[1]][2], ",", b1[[2]][2],") = ", 
	round(b1[[3]][2],digits=3),", p = ", round(b1[[4]][2],digits=3), sep=""))
text(16,4,"Daten: Forschungsgruppe Wahlen 19.09.2013 - 15.09.2017")
text(20,3.5,"Grafik: Jakob Fink @jaemaf")
dev.off()

# DIE LINKE
jpeg("a_leftparty.jpg", res=500, width = 20, height = 18, units="cm")
plot.ts(scale(GNDiff.sl), col="blue", xlab = "Messzeitpunkte", ylab = "Prognose", ylim = c(-4,6),
	main="Zusammenhang zwischen Prognose und Sozialpolitik LINKE", lty=2, lwd=3)
points(scale(PollsDiff.l), type = "l", col="darkred",lwd=3)
legend(3,6, c("DIE LINKE Sozialpolitik (skaliert)", "DIE LINKE Sonntagsfrage (skaliert)"), 
	lty=c(2,1), col= c("blue", "darkred"), lwd=c(3,3))

a1 <- grangertest(GNDiff.sl ~ PollsDiff.l, order=1) # p = 0.03*
a2 <- grangertest(GNDiff.sl ~ PollsDiff.l, order=2)
a3 <- grangertest(GNDiff.sl ~ PollsDiff.l, order=3)
a4 <- grangertest(GNDiff.sl ~ PollsDiff.l, order=4) 
a5 <- grangertest(GNDiff.sl ~ PollsDiff.l, order=5)
a6 <- grangertest(GNDiff.sl ~ PollsDiff.l, order=6) 

b1 <- grangertest(PollsDiff.l  ~ GNDiff.sl, order=1) # p = 0.02*
b2 <- grangertest(PollsDiff.l  ~ GNDiff.sl, order=2)
b3 <- grangertest(PollsDiff.l  ~ GNDiff.sl, order=3)
b4 <- grangertest(PollsDiff.l  ~ GNDiff.sl, order=4)
b5 <- grangertest(PollsDiff.l  ~ GNDiff.sl, order=5)

text(12,-3, paste("GrangerTest, 1st order: SP~Prognose: F(", a1[[1]][2], ",", a1[[2]][2],") = ", 
	round(a1[[3]][2],digits=3),", p = ", round(a1[[4]][2],digits=3), sep=""))
text(12,-3.5, paste("GrangerTest, 1st order: Prognose~SP: F(", b1[[1]][2], ",", b1[[2]][2],") = ", 
	round(b1[[3]][2],digits=3),", p = ", round(b1[[4]][2],digits=3), sep=""))
text(16,4,"Daten: Forschungsgruppe Wahlen 19.09.2013 - 15.09.2017")
text(20,3.5,"Grafik: Jakob Fink @jaemaf")
dev.off()

# SPD~AfD
jpeg("a_afdparty.jpg", res=500, width = 20, height = 18, units="cm")
plot.ts(scale(GNDiff.sS), col="red", xlab = "Messzeitpunkte", ylab = "Prognose", ylim = c(-4,6),
	main="Zusammenhang zwischen Prognose AfD und Sozialpolitik SPD", lwd=3,lty=2)
points(scale(PollsDiff.a), type = "l", col="blue", lwd=3)
legend(3,6, c("SPD Sozialpolitik (SP, skaliert)", "AfD Sonntagsfrage (skaliert)"), lty=c(2,1), 
	col= c("red", "blue"), lwd=c(3,3))

a1 <- grangertest(GNDiff.sS ~ PollsDiff.a, order=1) # p = 0.019*
a2 <- grangertest(GNDiff.sS ~ PollsDiff.a, order=2) # p = 0.02*
a3 <- grangertest(GNDiff.sS ~ PollsDiff.a, order=3) # p = 0.02*
a4 <- grangertest(GNDiff.sS ~ PollsDiff.a, order=4)
a5 <- grangertest(GNDiff.sS ~ PollsDiff.a, order=5)
a6 <- grangertest(GNDiff.sS ~ PollsDiff.a, order=6) 

b1 <- grangertest(PollsDiff.a  ~ GNDiff.sS, order=1) # p = 0.04*
b2 <- grangertest(PollsDiff.a  ~ GNDiff.sS, order=2) # p = 0.03*
b3 <- grangertest(PollsDiff.a  ~ GNDiff.sS, order=3)
b4 <- grangertest(PollsDiff.a  ~ GNDiff.sS, order=4)
b5 <- grangertest(PollsDiff.a  ~ GNDiff.sS, order=5)

text(12,-3, paste("GrangerTest, 1st order: SP~Prognose: F(", a1[[1]][2], ",", a1[[2]][2],") = ", 
	round(a1[[3]][2],digits=3),", p = ", round(a1[[4]][2],digits=3), sep=""))
text(12,-3.5, paste("GrangerTest, 2nd order: Prognose~SP: F(", b2[[1]][2], ",", b2[[2]][2],") = ", 
	round(b2[[3]][2],digits=3),", p = ", round(b2[[4]][2],digits=3), sep=""))
text(16,4,"Daten: Forschungsgruppe Wahlen 19.09.2013 - 15.09.2017")
text(20,3.5,"Grafik: Jakob Fink @jaemaf")
dev.off()

##############################################################################################

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





