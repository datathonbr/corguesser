library(magrittr)
library(tidyverse)

bd <- generate_n_sets(2e5)

saveRDS(bd %>% select(id, data, corr = corr_observada), file = "correlations.rds", compress = FALSE)

salva_plot <- function(dados, filename, path) {
  png(filename = sprintf("%s/%s.png", path, filename), width = 150, height = 150)
  par(mar = c(1.4,1.4,.1,.1), cex.axis=0.6, mgp=c(3, .5, 0))
  plot(dados, asp = 1, xlab = "", ylab = "", pch = 16)
  dev.off()
}

bd %$% walk2(data, id, salva_plot, path = "img/")

