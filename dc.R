tau <- Vectorize(function(xx, yy, lambda, mu, rho){
  if (xx == 0 & yy == 0){return(1 - (lambda*mu*rho))
  } else if (xx == 0 & yy == 1){return(1 + (lambda*rho))
  } else if (xx == 1 & yy == 0){return(1 + (mu*rho))
  } else if (xx == 1 & yy == 1){return(1 - rho)
  } else {return(1)}
})
DClogLik <- function(y1, y2, lambda, mu, rho=0){
  #rho=0, independence
  #y1: home goals
  #y2: away goals
  sum(log(tau(y1, y2, lambda, mu, rho)) + log(dpois(y1, lambda)) + log(dpois(y2, mu)))
}
DCmodelData <- function(df){
  
  hm <- model.matrix(~ HomeTeam - 1, data=df, contrasts.arg=list(HomeTeam='contr.treatment'))
  am <- model.matrix(~ AwayTeam -1, data=df)
  
  team.names <- unique(c(levels(df$HomeTeam), levels(df$AwayTeam)))
  return(list(
    homeTeamDM=hm,
    awayTeamDM=am,
    homeGoals=df$FTHG,
    awayGoals=df$FTAG,
    teams=team.names
  )) 
}
DCoptimFn <- function(params, DCm){
  
  home.p <- params[1]
  rho.p <- params[2]
  
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  defence.p <- matrix(params[(nteams+3):length(params)], ncol=1)
  
  lambda <- exp(DCm$homeTeamDM %*% attack.p + DCm$awayTeamDM %*% defence.p + home.p)
  mu <- exp(DCm$awayTeamDM %*% attack.p + DCm$homeTeamDM %*% defence.p)
  
  return(
    DClogLik(y1=DCm$homeGoals, y2=DCm$awayGoals, lambda, mu, rho.p) * -1
  )
}
DCattackConstr <- function(params, DCm, ...){
  nteams <- length(DCm$teams)
  attack.p <- matrix(params[3:(nteams+2)], ncol=1)
  return((sum(attack.p) / nteams) - 1)
}

dta <- as.data.frame(dtfr)
dcm <- DCmodelData(dta)
#initial parameter estimates
attack.params <- rep(.01, times=nlevels(dta$HomeTeam))
defence.params <- rep(-0.08, times=nlevels(dta$HomeTeam))
home.param <- 0.06
rho.init <- 0.03
par.inits <- c(home.param, rho.init, attack.params, defence.params)
#it is also usefull to give the parameters some informative names
names(par.inits) <- c('HOME', 'RHO', paste('Attack', dcm$teams, sep='.'), paste('Defence', dcm$teams, sep='.'))
library(alabama)
res <- auglag(par=par.inits, fn=DCoptimFn, heq=DCattackConstr, DCm=dcm, control.outer = list(method="nlminb"))
# Expected goals home
res$par

lambda <- exp(res$par['HOME'] + res$par['Attack.Бенфика (Португалия)'] + res$par['Defence.Боруссия Д (Германия)'])
# Expected goals away
mu <- exp(res$par['Attack.Боруссия Д (Германия)'] + res$par['Defence.Бенфика (Португалия)'])

maxgoal <- 9 # will be useful later
probability_matrix <- dpois(0:maxgoal, lambda) %*% t(dpois(0:maxgoal, mu))                                         

scaling_matrix <- matrix(tau(c(0,1,0,1), c(0,0,1,1), lambda, mu, res$par['RHO']), nrow=2)
probability_matrix[1:2, 1:2] <- probability_matrix[1:2, 1:2] * scaling_matrix

HomeWinProbability <- sum(probability_matrix[lower.tri(probability_matrix)])
DrawProbability <- sum(diag(probability_matrix))
AwayWinProbability <- sum(probability_matrix[upper.tri(probability_matrix)])

awayG <- numeric(maxgoal)
for (gg in 2:maxgoal){
  awayG[gg-1] <- sum(diag(probability_matrix[,gg:(maxgoal+1)]))
}
awayG[maxgoal] <- probability_matrix[1,(maxgoal+1)]

homeG <- numeric(maxgoal)
for (gg in 2:maxgoal){
  homeG[gg-1] <- sum(diag(probability_matrix[gg:(maxgoal+1),]))
}
homeG[maxgoal] <- probability_matrix[(maxgoal+1),1]

goaldiffs <- c(rev(awayG), sum(diag(probability_matrix)), homeG)
names(goaldiffs) <- -maxgoal:maxgoal
plot(goaldiffs)

