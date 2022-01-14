for (i in unique(a$disease)) {
  pdf(file = paste0(i, "coverage_start.pdf"),   # The directory you want to save the file in
      width = 4, # The width of the plot in inches
      height = 4) # The height of the plot in inches
  a[year == minyr & disease == i]$coverage %>%
    hist(50, main = paste0("Coverage at the start year for ", i))
  dev.off()
}






