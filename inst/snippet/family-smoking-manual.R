rowTotal <- rowSums(smokeTab); rowTotal
colTotal <- colSums(smokeTab); colTotal
grandTotal <- sum(smokeTab); grandTotal
e <- outer(rowTotal, colTotal) / grandTotal; e
o <- smokeTab
stat <- sum ((e - o)^2 / e); stat
pval <- 1 - pchisq(stat, df = 2); pval

