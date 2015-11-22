nfldata = read.csv(file.choose(), header=TRUE)
attach(nfldata)
head(nfldata)
Pick = nfldata$Overall
Salary = nfldata$Year1
P2 = Pick^2
P3 = Pick^3
summary(lm(Salary ~ Pick + P2 + P3 + CBA + CBA:Pick + CBA:P2+CBA:P3))

#Test interactions
plot(Pick, lm(Salary ~ Pick + P2 + P3 + CBA + CBA:Pick + CBA:P2+CBA:P3)$fitted)
moduno = lm(Salary ~ Pick + P2 + CBA + CBA:Pick + CBA:P2)
summary(moduno)
#Test which variables have an impact
step(moduno)
anova(moduno)

#Test moduno on only Pre lockout
modelpre =  lm(Salary ~ Pick + P2 + CBA + CBA:Pick + CBA:P2,subset=1:(253*2)) 

#Test moduno on only Post lockout
modelpost = lm(Salary ~ Pick + P2 + CBA + CBA:Pick + CBA:P2,subset=((253*2)+1):(253*4)) 

#Plot fitted variables
plot(1:253,(modelpre$fitted[1:253]+modelpre$fitted[254:506])/2,col="green")
points(1:253,(modelpost$fitted[1:253]+modelpost$fitted[254:506])/2,col="red")
summary(modelpre)
summary(modelpost)

#####Model with positional dummies
modpositional = lm(Salary ~ Pick + P2 + CBA + CBA:Pick + CBA:P2 + QB + RB + WR + OL + DE + LB+ CB)
summary(modpositional)
step(modpositional)

###Plot pre and post on one graph
plot(Pick[1:(253*2)], Salary[1:(253*2)], col="blue",xlab="Salary", ylab="Pick", main="Salary Comparison",
     ylim=c(0,5000000))
points(Pick[507:1012], Salary[507:1012], col="gold")
legend(x=100,y=5000000,paste("Pre-lockout in blue"))
legend(x=100,y=4300000,paste("Post-lockout in gold"))

head(nfldata[,12])
nfldata[254,]
dim(PostLock)

###Define all periods and variables
PreLock = nfldata[1:(253*2),]
PostLock = nfldata[507:1012,]
LockDiffYear1 = -nfldata$Year1[Year==2009]-nfldata$Year1[Year==2010]+nfldata$Year1[Year==2012]+nfldata$Year1[Year==2013]
LockDiffAvg = -nfldata$Average[Year==2009]-nfldata$Average[Year==2010]+nfldata$Average[Year==2012]+nfldata$Average[Year==2013]
OldYearOne=(nfldata$Year1[Year==2009]+nfldata$Year1[Year==2010])/2
NewYearOne=(nfldata$Year1[Year==2012]+nfldata$Year1[Year==2013])/2
OldAverage=(nfldata$Average[Year==2009]+nfldata$Average[Year==2010])/2
NewAverage=(nfldata$Average[Year==2012]+nfldata$Average[Year==2013])/2

#Start plotting differences
plot(1:253,LockDiffYear1, col="black", xlab="Pick Number",ylab="Difference in Salary",ylim=c(-1e+06,1e+06))
plot(1:253,LockDiffYear1-LockDiffAvg,col="black", xlab="Pick Number",ylab="Difference in Salary",ylim=c(-1e+06,1e+06))
plot(1:253,LockDiffYear1-LockDiffAvg,col="black", xlab="Pick Number",ylab="Difference in Salary",ylim=c(-1e+06,1e+06))

#Awesome plot
plot(1:253,(NewYearOne-OldYearOne)/OldYearOne,col="black", xlab="Pick Number")
summary((NewYearOne-OldYearOne)/OldYearOne)

###Great plot that change is percent change is negative at top of draft and positive everywhere else
plot(1:253,(NewAverage-OldAverage)/OldAverage,col="black", xlab="Pick Number")
abline(h=0)

#Plot the year one salaries for each period then do KS Test
par(mar = c(4, 4, 1, 1))
par(mfrow=c(2,1))
plot(1:253,OldYearOne, col="orange", ylab="Salary in Year One", cex=0.5, xlab="Pick Number",ylim=c(0,4000000))
points(1:253,NewYearOne,col="green",cex=0.5)
legend(locator(1),"Pre-lockout in orange")

plot(1:253,OldAverage, col="blue", ylab="Average Salary", cex=0.5, xlab="Pick Number",ylim=c(0,4000000))
points(1:253,NewAverage,col="red",cex=0.5)
legend(locator(1),"Pre-lockout in blue")

#kS test for for Year One salaries
ks.test(jitter(NewYearOne,.0001),jitter(OldYearOne,.0001))

#Plot the average salaries for each period then do KS Test
plot(33:253,OldAverage[33:253], col="orange")
points(33:253,NewAverage[33:253],col="green")
ks.test(NewAverage[33:253],OldAverage[33:253])

#Figure out influencial point
nfldata$Player[Year==2009 & Overall==44]
nfldata$Player[Year==2010 & Overall==44] #Sergio Kindle is the issue

#Compare average salary pre and post-lockout
plot(1:64,OldAverage[1:64],col="red",xlab="Pick",ylab="Average Salary",
     main="Average Pre-Lockout (Red) vs. Post-Lockout (Green)")
points(1:64,NewAverage[1:64],col="green")
abline(v=32.5,col="blue")

#Use a loop to test Year One salary for each round
lapply(1:7,function(i) {
  ks.test(jitter(NewYearOne[Round==i],.00001),jitter(OldYearOne[Round==i],.00001),alternative="two.sided")
})
#Use a loop to test Year One salary for each pair of rounds
lapply(1:6,function(i) {
  ks.test(jitter(NewYearOne[Round==i|Round==(i+1)],.00001),jitter(OldYearOne[Round==i|Round==(i+1)],.00001),
          alternative="two.sided")
})
#Use a loop to test Average salary for each round
lapply(1:7,function(i) {
  ks.test(jitter(NewAverage[Round==i],.00001),jitter(OldAverage[Round==i],.00001),alternative="two.sided")
})
#Use a loop to test Average salary for each pair round
lapply(1:6,function(i) {
  ks.test(jitter(NewAverage[Round==i|Round==(i+1)],.00001),jitter(OldAverage[Round==i|Round==(i+1)],.00001),
          alternative="two.sided")
})

####Dual Axis Prelockout
options(scipen=5)
par(mar = c(3, 4, 4, 4))
plot(c(1:96), (PreLock$AV1[1:96]+PreLock$AV1[254:(254+95)])/2, 
     xlab="Overall Pick", ylab="First Year Production in AV", 
     main="Production and Salary Pre-Lockout", cex=1, cex.axis=0.8,col="blue",col.lab="blue")
par(new = TRUE)
plot(c(1:96),(PreLock$Year1[1:96]+PreLock$Year1[254:(254+95)])/2, type = "p", axes = FALSE, 
     bty = "n", xlab = "", ylab = "",col="red",ylim=c(0,4000000))
axis(side=4, ylim=c(0,4000000)) #at = pretty(range(PreLock$Year1[1:96])))
mtext("First Year Salary", side=4, line=3, col="red")

####Dual Axis Post Lockout
options(scipen=5)
par(mar = c(4.5, 4, 4, 4))
plot(c(1:96), (PostLock$AV1[1:96]+PostLock$AV1[254:(254+95)])/2, xlab="Overall Pick", 
     ylab="First Year Production in AV", 
     main="Production and Salary Post Lockout", cex=1, cex.axis=1,col="blue",col.lab="blue")
par(new = TRUE)
plot(c(1:96),(PostLock$Year1[1:96]+PostLock$Year1[254:(254+95)])/2, type = "p", axes = FALSE, 
     bty = "n", xlab = "", ylab = "",col="red",ylim=c(0,5000000),cex.axis=0.1)
axis(side=4, cex.axis=0.8, ylim=c(0,5000000)) #at = pretty(range(PreLock$Year1[1:96])))
mtext("First Year Salary", side=4, line=3, col="red")


#Pure production comparison
plot(c(1:96), (PostLock$AV1[1:96]+PostLock$AV1[254:(254+95)])/2, xlab="Overall Pick", 
     ylab="First Year Production in AV", 
     main="Production Comparison Pre and Post Lockout", cex=1, cex.axis=1,col="red")
points(c(1:96),(PreLock$AV1[1:96]+PreLock$AV1[254:(254+95)])/2,col="blue")
prodpost <- loess((PostLock$AV1[1:96]+PostLock$AV1[254:(254+95)])/2~c(1:96))
prodpre <- loess((PreLock$AV1[1:96]+PreLock$AV1[254:(254+95)])/2~c(1:96))
lines(predict(prodpost), col="red", lwd=0.5)
lines(predict(prodpre), col="blue", lwd=0.5)
legend(locator(1),"Red is Post-lockout",cex=0.8)
legend(locator(1),"Blue is Pre-lockout",cex=0.5)


#Efficiency Comparison
par(mar = c(5, 4, 4, 4))
plot(c(1:96),1000000*((PreLock$AV1[1:96]+PreLock$AV1[254:(254+95)])/2)/((PreLock$Year1[1:96]+PreLock$Year1[254:(254+95)])/2),
     xlab="Pick",ylab="Efficiency",col="blue",main="Pick Efficiency Pre and Post Lockout")
points(c(1:96),1000000*((PostLock$AV1[1:96]+PostLock$AV1[254:(254+95)])/2)/((PostLock$Year1[1:96]+PostLock$Year1[254:(254+95)])/2),
       xlab="Pick",ylab="Efficiency",col="red")

#Make polyfit models
polypre <- loess(1000000*((PreLock$AV1[1:96]+PreLock$AV1[254:(254+95)])/2)/((PreLock$Year1[1:96]+PreLock$Year1[254:(254+95)])/2)~c(1:96))
polypost <- loess(1000000*((PostLock$AV1[1:96]+PostLock$AV1[254:(254+95)])/2)/((PostLock$Year1[1:96]+PostLock$Year1[254:(254+95)])/2)~c(1:96))
lines(predict(polypre), col="blue", lwd=0.5)
lines(predict(polypost), col="red", lwd=0.5)

#####Looking at escalator clauses

#Year 1 to 2 Escalator
mean(Year2[CBA==0]/(Year1[CBA==0]+.000001))
mean(Year2[CBA==1]/(Year1[CBA==1]+.000001))
#Year 2 to 3 Escalator
mean(Year3[CBA==0]/(Year2[CBA==0]+.000001))
mean(Year3[CBA==1]/(Year2[CBA==1]+.000001))
#Year 3 to 4 Escalator
mean(Year4[CBA==0]/(Year3[CBA==0]+.000001))
mean(Year4[CBA==1]/(Year3[CBA==1]+.000001))
