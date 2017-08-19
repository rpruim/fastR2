GoosePermits <-
  data_frame(
    bid = c(1,5,10,20,30,40,50,75,100,150,200),
    n = c(31,29,27,25,23,21,19,17,15,15,15),
    sell = c(0,3,6,7,9,13,17,12,11,14,13),
    keep = n - sell,
    prop_sell = sell / n
  )