gf_dhistogram( ~ duration, data = geyser, binwidth = 0.20, alpha = 0.2,
               fill = "navy") %>%
  gf_function(fun = dmix, 
              args = list(
                alpha =  mle[1],
                mu1 =    mle[2], mu2 =    mle[3],
                sigma1 = mle[4], sigma2 = mle[5]),
                color = "gray30"
  ) %>%
  gf_function(fun = dmix, 
              args = list(
                alpha =  0.5,
                mu1 =    mle0[1], mu2 =    mle0[2],
                sigma1 = mle0[3], sigma2 = mle0[4]),
                linetype = "dashed"
  )

