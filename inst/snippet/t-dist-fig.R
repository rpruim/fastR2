x <- seq(-5, 5, by = 0.05)
l <- length(x)
Plot_data <- 
  tibble(
    pdf = c(dnorm(x), dt(x, df = 1), dt(x, df = 2), dt(x, df = 4), dt(x, df = 10)),
    distribution = rep(c(1000, 1, 2, 4, 10), each = l),
    x = rep(x, times = 5)
    )

Plot_data$distribution <- 
  factor(Plot_data$distribution,
         labels = c("df=1", "df=2", "df=4", "df=10", "normal")
  )

line.list <- list(
  lty = c(1, 1, 1, 1, 1), # lty = c(1, 2, 3, 4, 1), 
  lwd = c(2, 2, 2, 2, 2),
  col = paste("gray", c(80, 60, 40, 20, 5), sep = "")  
)

gf_line( pdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_refine(scale_color_manual(values = paste("gray", c(80, 60, 40, 20, 10), sep = "")))

