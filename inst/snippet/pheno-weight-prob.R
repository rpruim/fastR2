# testing beta_1 = 2
t <- ( 1.0754 - 2.0) / 0.0121; t
2 * pt(-abs(t) , df=2237)
# testing beta_1 = 1
t <- ( 1.0754- 1.0) / 0.0121; t
2 * pt(-abs(t) , df=2237)
# testing beta_2 = 1
t <- ( 0.8942 - 1.0) / 0.0302; t
2 * pt(-abs(t) , df=2237)

