firstDigit <- function(x) {
    trunc(x / 10^(floor(log10(abs(x)))))
}
# rivers contains lengths (mi) of 141 major North American rivers
# Rivers has first digits of lengths in miles and km
Rivers <-
  data_frame(
    digit = 1:9,
    model = log10(digit + 1) - log10(digit),
    miles = as.numeric(tally( ~ firstDigit(rivers), format = "prop")),
    km = as.numeric(tally( ~ firstDigit(1.61 * rivers), format = "prop"))
  ) %>%
  tidyr::gather(source, proportion, model:km)
    
gf_point(proportion ~ digit, color = ~ source, data = Rivers) %>%
  gf_line(proportion ~ digit, color = ~ source, data = Rivers) %>%
  gf_refine(scale_x_continuous(breaks = 1:10))

