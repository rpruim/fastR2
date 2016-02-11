Dice <- expand.grid(red=1:6, blue=1:6)
Dice %>% sample(4)
# part b 
Dice <-
  Dice %>% 
  mutate(
    A = red + blue >= 9,
    B = blue > red,
    C = blue == 5)

