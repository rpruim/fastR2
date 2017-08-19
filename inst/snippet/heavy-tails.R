x <- seq(-7, 10, by = 0.10)
l <- length(x)
Plot_data <- data.frame(
    pdf = c(dnorm(x, 0, sqrt(3)), dt(x, df = 3)),
    cdf = c(pnorm(x, 0, sqrt(3)), pt(x, df = 3)),
    distribution = rep(c(1000, 3), each = l),
    x = rep(x, times = 2)
    )

Plot_data$distribution <- factor(Plot_data$distribution,
    labels = c("T(3)", "Norm")
    )

# line.list <- list(
#             lty = c(1, 1),
#             lwd = c(1.5, 1.5),
#             col = trellis.par.get("superpose.line")$col[1:2]
#             )

gf_line(pdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "PDFs") %>% 
  gf_lims(x = c(0, 7), y = c(0, 0.40)) %>%
  gf_theme(legend.position = "top")

gf_line(cdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "CDFs") %>% 
  gf_lims(x = c(-2, 7), y = c(0, 1)) %>%
  gf_theme(legend.position = "top")

gf_line(pdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "PDFs (zoomed)") %>% 
  gf_lims(x = c(3, 9), y = c(0, 0.06)) %>%
  gf_theme(legend.position = "top")

gf_line(cdf ~ x, data = Plot_data, color = ~ distribution) %>%
  gf_labs(title = "CDFs (zoomed)") %>% 
  gf_lims(x = c(2.8, 9), y = c(0.97, 1)) %>%
  gf_theme(legend.position = "top")

# xyplot(pdf ~ x, Plot_data,
#     groups = distribution,
#     main = "PDFs",
#     type = "l",
#     xlim = c(0, 7),
#     ylim = c(-0.005, 0.40),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )
# 
# xyplot(cdf ~ x, Plot_data,
#     groups = distribution,
#     main = "CDFs",
#     type = "l",
#     xlim = c(-3, 7),
#     ylim = c(0, 1),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )
# 
# xyplot(pdf ~ x, Plot_data,
#     groups = distribution,
#     main = "PDFs",
#     type = "l",
#     xlim = c(3, 9),
#     ylim = c(-0.005, 0.06),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )
# 
# xyplot(cdf ~ x, Plot_data,
#     groups = distribution,
#     main = "CDFs",
#     type = "l",
#     xlim = c(3, 9),
#     ylim = c(0.98, 1),
#     lattice.options = list( 
#         superpose.line = line.list
#         ),
#     lwd = line.list$lwd,
#     lty = line.list$lty,
#     col = line.list$col,
#     key = list(
#         lines = line.list,
#         text = list(
#             lab = c(expression(T(3)), expression(Norm(0, sqrt(3))))
#             ),
#         columns = 2
#         )
#     )

