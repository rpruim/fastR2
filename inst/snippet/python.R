python <- rbind( c(27-16, 16), c(56-38, 38), c(104-75, 75))
rownames(python) <- c("cold", "neutral", "hot")
colnames(python) <- c("unhatched", "hatched")
python
chisq.test(python)

