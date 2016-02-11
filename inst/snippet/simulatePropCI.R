easyci <- function(x, n, conf.level=0.95) { 
    alpha = 1 - conf.level
    p = x/n
    zstar <- - qnorm(alpha/2)
    interval <- c( p + c(-1, 1) * zstar * sqrt(p * (1-p) / n), conf.level)
	names(interval) <- c("lower", "upper", "conf.level")
	interval
    }
Wilsonci <- function(x, n=100, conf.level=0.95) { 
    alpha = 1 - conf.level
    p = (x+2)/(n+4)
    zstar <- - qnorm(alpha/2)
    interval <- c(p + c(-1, 1) * zstar * sqrt(p * (1-p) / n), conf.level)
	names(interval) <- c("lower", "upper", "conf.level")
	interval
    }

# sample size = 35
do(1) * easyci(rbinom(1, 35, 0.3), n=35)    
results <- do(10000) * easyci(rbinom(1, 35, 0.3), n=35) # simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data=results)    # coverage rate

results <- do(10000) * Wilsonci(rbinom(1,35,0.3), n=35) # simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data=results)    # coverage rate

# sample size =100 
results <- do(10000) * easyci(rbinom(1,100,0.3), n=100) # simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data=results)    # coverage rate

results <- do(10000) * Wilsonci(rbinom(1,100,0.3),n=100)# simulate 10000x 
prop( ~ (lower <= 0.3 & upper >= 0.3), data=results)    # coverage rate

