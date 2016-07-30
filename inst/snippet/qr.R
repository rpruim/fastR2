QRdata <- data.frame(x = c(1, 1, 5, 5), y = c(1, 2, 4, 6))
glimpse(QRdata)
qr.model <- lm(y ~ x, data = QRdata)
Q <- qr.model %>% qr() %>% qr.Q(); Q
R <- qr.model %>% qr() %>% qr.R(); R

