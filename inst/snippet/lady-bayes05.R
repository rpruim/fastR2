# interval formed by removing equal tail probabilities
cdata( ~ posterior_sample20, 0.90)
cdata( ~ posterior_sample10, 0.90)
# HPDI
coda::HPDinterval(coda::as.mcmc(posterior_sample20), 0.90)
rethinking::HPDI(posterior_sample20, 0.90)
coda::HPDinterval(coda::as.mcmc(posterior_sample10), 0.90)
rethinking::HPDI(posterior_sample10, 0.90)

