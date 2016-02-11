data <- c(18.0, 6.3, 7.5, 8.1, 3.1, 0.8, 2.4, 3.5, 9.5, 39.7,
          3.4, 14.6, 5.1, 6.8, 2.6, 8.0, 8.5, 3.7, 21.2, 3.1,
          10.2, 8.3, 6.4, 3.0, 5.7, 5.6, 7.4, 3.9, 9.1, 4.0)
GOF(data, cutpts = c(0,3,5,9,13,Inf), iterlim = 1000, start = c(5,5))$table
GOF(data, cutpts = c(0,3,5,9,13,Inf), iterlim = 1000, start = c(5,5))
GOF(data, cutpts = c(0,3,5,9,13,Inf), iterlim = 1000, start = c(5,5), pearson = TRUE)

