load("ppcs_1996.RData")
load("ppcs_1999.RData")
load("ppcs_2002.Rdata")
load("ppcs_2005.Rdata")
load("ppcs_2008.RData")
load("ppcs_2011.RData")

nrow(ppcs_1996_cleaned) # 6467
nrow(ppcs_1999_cleaned) # 94717
nrow(ppcs2002_polished) # 93410
nrow(ppcs2005_polished) # 53492
nrow(ppcs_2008_cleaned) # 72566
nrow(ppcs_2011)         # 62280

y <- c(nrow(ppcs_1996_cleaned), # 6467
  nrow(ppcs_1999_cleaned), # 94717
  nrow(ppcs2002_polished), # 93410
  nrow(ppcs2005_polished), # 80238
  nrow(ppcs_2008_cleaned), # 72566
  nrow(ppcs_2011))         # 62280

y2 <- c(6467,94717,93410,80238,72566, 6)
x <- c(1996, 1999, 2002, 2005, 2008, 2011)

dfr <- data.frame(x,y)

dfr %>%
  ggplot(aes(x, y)) + 
  geom_point() + 
  geom_line()
