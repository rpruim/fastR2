# this will be wrong for values not among 0, 1, 2, 3, or 4
f <- function(x) {
  factorial(4) / (16 * factorial(x) * factorial(4 - x))
}
f(0:4)   
sum(f(0:4))    # check to be sure the probabilities add to 1
my_data <- data.frame(probability = f(0:4), x = 0:4)
gf_point(probability ~ x, data = my_data)
gf_point(probability ~ x, data = my_data) %>%
  gf_segment(0 + probability ~ x + x, data = my_data) 
gf_point(probability ~ x, data = my_data) %>%
  gf_line(probability ~ x, data = my_data)

