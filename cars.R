cars <- na.omit(read.csv("cars.csv", stringsAsFactors = TRUE))
hist(log10(cars$Price))
set.seed(12345)

agg_low_counts <- function(x, min_count = 20, common = "Other") {
  f2 <- reorder(x, x, length, decreasing = TRUE)
  lev <- levels(f2)
  cnt <- table(f2)
  keep <- cnt >= min_count
  
  lab <- c(lev[keep], rep(common, length(lev) - sum(keep)))
  f3 <- factor(f2, levels = lev, labels = lab)
  
  return(f3)
}


