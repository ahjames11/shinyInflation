
# Scratch work

indexData <- getIndex("OPN")

attr(indexData, "metadata")$LongTitle


d <- indexData %>%
  ungroup() %>%
  select(FYStart, Raw, Weighted) %>%
  rename(Date = FYStart) %>%
  tbl_xts()

p1 <- dygraph(d, main = attr(indexData, "metadata")$LongTitle) %>%
  dySeries("Raw", label = "Raw Index") %>%
  dySeries("Weighted", label = "Weighted Index")

p1
