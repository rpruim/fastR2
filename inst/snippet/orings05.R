orings.model2 <-                  # link = logit is default, so unnecessary
    glm(cbind(damage, 6 - damage) ~ temp, data = orings, 
        family = binomial(link = logit))
msummary(orings.model2)
p1 <- predict(orings.model, newdata = data.frame(temp = 31), type = "response"); p1
p2 <- predict(orings.model2, newdata = data.frame(temp = 31), type = "response"); p2
dbinom(0, 6, prob = p2)               # 0 damaged O-rings
pred_damage1 <- makeFun(orings.model)
pred_damage2 <- makeFun(orings.model2)
Pred_data <- 
  tibble(
    temp = seq(30, 100, by = 2),
    pred1 = pred_damage1(temp),
    pred2 = pred_damage2(temp)
    )
gf_point(damage / 6 ~ temp, data = orings, alpha = 0.7) %>%
  gf_line(pred1 ~ temp, data = Pred_data, color = "gray50") %>%
  gf_line(pred2 ~ temp, data = Pred_data, color = "blue") %>%
  gf_labs(y = "proportion of O-rings damaged") 

