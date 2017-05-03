require(VGAM)
require(MASS)
#load data
yrdta <- dtfr_p

#fit model and get a summary

model <- glm(Goals ~ Home + Team + Opponent, family="poisson", data=yrdta)
model <- glm(Goals ~ Home + Team + Opponent, family="poisson", data=yrdta)

summary(model)
#aston villa
predict(model, data.frame(Home=1, Team="Бенфика (Португалия)", Opponent="Боруссия Д (Германия)"), type="response")
# 0.9453705 

#for sunderland. note that Home=0.
predict(model, data.frame(Home=0, Team="Боруссия Д (Германия)", Opponent="Бенфика (Португалия)"), type="response")
# 0.999 

#expected number of goals for both teams
predictHome <- predict(model, data.frame(Home=1, Team="Бенфика (Португалия)", Opponent="Боруссия Д (Германия)"), type="response")
predictAway <- predict(model, data.frame(Home=0, Team="Боруссия Д (Германия)", Opponent="Бенфика (Португалия)"), type="response")

#plot the poisson distributions
plotrange <- 0:6
hp <- dpois(plotrange, predictHome)
ap <- dpois(plotrange, predictAway)
plot(plotrange, hp, type="b", ylim=range(hp, ap), main="Goals, Aston Villa vs Sunderland", xlab="Number of goals", ylab="Probability")
points(plotrange, ap, type="b", pch=24)
legend(x=4, y=0.4, legend=c("AstonVilla", "Sunderland"), pch=c(21, 24))

#Goal difference, skellam distribution
goalDiffRange <- -7:7
plot(goalDiffRange, dskellam(goalDiffRange, predictHome, predictAway), type="b", main="Goal difference, Aston Villa vs. Sunderland", ylab="Probability", xlab="Goal difference") 
     
#Away
sum(dskellam(-100:-1, predictHome, predictAway))
#Home
sum(dskellam(1:100, predictHome, predictAway))
#Draw
sum(dskellam(0, predictHome, predictAway))
set.seed(915706074)
nsim <- 10000
homeGoalsSim <- rpois(nsim, predictHome) 
awayGoalsSim <- rpois(nsim, predictAway)
goalDiffSim <- homeGoalsSim - awayGoalsSim
#Home
sum(goalDiffSim > 0) / nsim
#Draw
sum(goalDiffSim == 0) / nsim
#Away
sum(goalDiffSim < 0) / nsim
