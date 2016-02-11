# function to compute LR statistic
LR = function(pi.0, y, N, alpha) { 
  pi.hat = y/N; L0 = 0; L1 = 0 
  if (pi.0 < 1) L0 = L0 + (N-y)*log(1-pi.0)
  if (pi.0 > 0) L0 = L0 + y*log(pi.0)
  if (pi.hat > 0) L1 = L1 + y*log(pi.hat)
  if (pi.hat < 1) L1 = L1 + (N-y)*log(1-pi.hat)
  LR = 2*(L1-L0)
  return(LR)
}

# function used in uniroot to find lower and upper bounds 
# of confidence interval
LRCI = function(pi.0, y, N, alpha) {  
  pi.hat = y/N; L0 = 0; L1 = 0 
  if (pi.0 < 1) L0 = L0 + (N-y)*log(1-pi.0)
  if (pi.0 > 0) L0 = L0 + y*log(pi.0)
  if (pi.hat > 0) L1 = L1 + y*log(pi.hat)
  if (pi.hat < 1) L1 = L1 + (N-y)*log(1-pi.hat)
  LR = 2*(L1-L0)
  LR-qchisq(1-alpha, df=1)
}

# function used in uniroot to compute lower bound CI of mid-P 
# version of clopper-pearson exact test
midpL = function(pi.0, y, N, alpha) {  
  lowerbound = sum(dbinom(y:N, N, pi.0)) - 0.5*dbinom(y, N, pi.0)- alpha/2
  return(lowerbound)
}

# function used in uniroot to compute upper bound CI of mid-P 
# version of clopper-pearson exact test
midpU = function(pi.0, y, N, alpha) { 
  upperbound = sum(dbinom(0:y, N, pi.0)) - 0.5*dbinom(y, N, pi.0)- alpha/2
  return(upperbound)
}

n = 35
y = 0:n
pi.hat = y/n
pi.wig = (y+2)/(n+4)

tmp = matrix(0, length(seq(0.001, 0.999, by=0.001)), 8)
# (n+1) by 2 matrices to hold confindence bounds 
#    (first col--lower bound, second col -- upper bound)
#  W -wald, S - score, E - exact, L - LR, BJ =bayes, jeffrey's prior, 
#  BU - bayes uniform prior, MP - mid p, Wil = Wilson 
W = matrix(0, n+1, 2)   
S = W    
E = W
L = W
BJ = W
BU = W
MP = W
Wil = W

tmps = lapply(y, prop.test, n, correct=F) #compute confidence bounds for score 
tmpe = lapply(y, binom.test, n)          #compute confidence bounds for exact

# compute lower/upper confidence bounds for wald
W[, 1] = pi.hat  - qnorm(0.975)*sqrt(pi.hat*(1-pi.hat)/n)  
W[, 2] = pi.hat  + qnorm(0.975)*sqrt(pi.hat*(1-pi.hat)/n)

# compute lower/upper confidence bounds for Wilson
Wil[, 1] = pi.wig  - qnorm(0.975)*sqrt(pi.wig*(1-pi.wig)/(n+4))  
Wil[, 2] = pi.wig  + qnorm(0.975)*sqrt(pi.wig*(1-pi.wig)/(n+4))

for (i in 1:(n+1)) {
   S[i, ] = tmps[[i]]$conf.int   #extract confidence interval for score
   E[i, ] = tmpe[[i]]$conf.int   # extract conf. int. for exact
   if (y[i] == 0){
     L[i, 1] = 0
     MP[i, 1] = 0
   }
   else { 
     L[i, 1] =  uniroot(f=LR, interval=c(0.000001, y[i]/n), N=n, y=y[i],
                       alpha=0.05)$root            # lower bound for LR
       MP[i, 1] = uniroot(f=midpL, interval=c(0.000001, 0.999999),
                       N=n, y=y[i], alpha=0.05)$root # lower bound for mid-P
  }
  if (y[i] == n) {
     L[i, 2] = 1
     MP[i, 2] = 1
  }
  else { 
     L[i, 2] = uniroot(f=LR, interval=c(y[i]/n, 0.999999), N=n, y=y[i],
                      alpha=0.05)$root             # upper bound for LR
     MP[i, 2] = uniroot(f=midpU, interval=c(0.000001, 0.999999),
                       N=n, y=y[i], alpha=0.05)$root #upper bound for mid-P
  }
}

BJ[, 1] = qbeta(0.025, 0.5+y, n+0.5-y)  # lower bounds, bayes, jeffrey's prior
BJ[, 2] = qbeta(0.975, 0.5+y, n+0.5-y)  # upper bounds
BU[, 1] = qbeta(0.025, 1+y, n+1-y)    # lower bounds bayes, uniform prior
BU[, 2] = qbeta(0.975, 1+y, n+1-y)    # upper bounds

cnt = 1
# probabilities from the binomial y = (0, 1, 2, ..., 25)
#probs = dbinom(y, n, pi.0) 

pi.values <- seq(0.001, 0.999, by=0.001)
for (pi.0 in pi.values ) {
# calculate coverage rates, put into matrix tmp
 probs = dbinom(y, n, pi.0)
 tmp[cnt, 1] = (S[, 1] < pi.0 & pi.0 < S[, 2]) %*% probs
 tmp[cnt, 2] = (W[, 1] < pi.0 & pi.0 < W[, 2]) %*% probs
 tmp[cnt, 3] = (E[, 1] < pi.0 & pi.0 < E[, 2]) %*% probs
 tmp[cnt, 4] = (L[, 1] < pi.0 & pi.0 < L[, 2]) %*% probs
 tmp[cnt, 5] = (BJ[, 1] < pi.0 & pi.0 < BJ[, 2]) %*% probs
 tmp[cnt, 6] = (BU[, 1] < pi.0 & pi.0 < BU[, 2]) %*% probs
 tmp[cnt, 7] = (MP[, 1] < pi.0 & pi.0 < MP[, 2]) %*% probs
 tmp[cnt, 8] = (Wil[, 1] < pi.0 & pi.0 < Wil[, 2]) %*% probs
 cnt = cnt + 1
}

nn <- length(pi.values)
coverage <- data.frame(
	pi = rep(pi.values, times=3),
	coverage = c(tmp[, 1], tmp[, 2], tmp[, 3] ),
	method = factor(rep(c('Score', 'Wald', 'Clopper-Pearson'), each=nn),
				levels=c('Wald', 'Clopper-Pearson', 'Score'))
	)

# below, opens a pdf file creates various plots shown in lecture  
# and closes the PDF device 

trellis.par.set(theme=col.fastR(bw=T));
if(FALSE) {
matplot(seq(0.001, 0.999, by=0.001), tmp[, 1:3], type="l",
    lty=1,
    col=trellis.par.get('superpose.line')$col[1:3],
    main=paste("Coverage rates (n=", n, "; 95% CI)", sep=""),
    xlab=expression(pi),
    ylab = "Coverage Rate",
    lwd=2,
    ylim=c(0.8, 1));
    abline(h=0.95);
    legend(0.35, 0.875, c("Score", "Wald", "Clopper-Pearson")[c(3, 1, 2)], 
        col=trellis.par.get('superpose.line')$col[c(3, 1, 2)],
        lwd=2,
        lty=1,
        cex=1);

trellis.par.set(theme=col.fastR(bw=T));
matplot(seq(0.001, 0.999, by=0.001), tmp[, c(1, 8)], type="l",
    lty=1, col=trellis.par.get('superpose.line')$col[1:4],
    main=paste("Coverage rates (n=", n, "; 95% CI)", sep=""),
    xlab=expression(pi),
    ylab = "Coverage Rate",
    lwd=2,
    ylim=c(0.8, 1));
    abline(h=0.95);
    legend(0.40, 0.875, c("Score", "Wilson"), col=trellis.par.get('superpose.line')$col[1:2], lty=1, cex=1);
}


xyplot( coverage ~ pi, coverage, groups=method,
	lty=1,
	type="l", cex=.25,
    main=paste("Coverage rates (n=", n, "; 95% CI)", sep=""),
    xlab=expression(pi),
    ylab = "Coverage Rate",
    ylim=c(0.8, 1),
	col=c('gray50', 'gray80', 'gray20'),
	#col=c('blue', 'red', 'green', 'purple'),
	auto.key=T,
	legend=list(
		inside= list(x=.5, y=.1, corner=c(.5, 0), 
			fun=draw.key,
			args=list(
				key=list(
					lines=list(lty=1, lwd=2,
						col=c('gray70', 'gray20', 'gray50')
					),
					text=list(
						lab=c('Clopper-Pearson', 'Score', 'Wald'),
						cex=.8)
				)
			)
		)
	),
	panel = function(x, y, ...){
		panel.abline(h=0.95)
		panel.xyplot(x, y, ...)
		}
	);
#write.csv(coverage, file="CIcoverage.csv", row.names=FALSE)

