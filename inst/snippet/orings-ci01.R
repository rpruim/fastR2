st.err <- sqrt(diag(vcov(orings.model))); st.err
coef(orings.model)[2] + c(-1, 1) * st.err[2] * qnorm(0.975)
exp(coef(orings.model)[2] + c(-1, 1) * st.err[2] * qnorm(0.975))

