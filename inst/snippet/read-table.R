# need header = TRUE because there is a header line.
# could also use read.file() without header = TRUE
Traffic <- 
    read.table("http://www.calvin.edu/~rpruim/fastR/trafficTufte.txt", 
    header = TRUE)
Traffic

