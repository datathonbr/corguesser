library(magrittr)
library(tidyverse)

bd <- generate_n_sets(5e4)

saveRDS(bd %>% select(id, data, corr = corr_observada), file = "correlations.rds", compress = FALSE)

salva_plot <- function(dados, filename, path) {
  cat(".")
  png(filename = sprintf("%s/%s.png", path, filename), width = 150, height = 150)
  par(mar = c(1.4,1.4,.1,.1), cex.axis=0.6, mgp=c(3, .5, 0))
  plot(dados, asp = 1, xlab = "", ylab = "", pch = 16)
  dev.off()
}

train_full <- readRDS("train_full.rds")

imgs_ok <- dir("train_imgs/")
hashid_setting <- hashids::hashid_settings(as.character(runif(1)), min_length = 5 + sample(5, 1), alphabet = paste0(c(letters, 0:9), collapse = ""))

ids <- matrix(sample(letters, 210000*8, replace = TRUE), ncol = 8) %>% apply(1, paste0, collapse = "")
train_full <- train_full %>% mutate(id = unique(ids)[1:n()])

train_full %$% walk2(data, id, salva_plot, path = "train_imgs/")


set.seed(1)
train_full <- train_full %>% mutate(Usage = case_when (rank(id) <= 150000 ~ "train",
                                                       rank(id) <= 175000 ~ "Public",
                                                       TRUE ~ "Private"))
saveRDS(train_full, file = "train_full.rds", compress = FALSE)

train_full %>% filter(!Usage %in% "train") %$%
  walk(id, ~{
    fs::file_move(
      sprintf("train_imgs/%s.png", .x),
      sprintf("test_imgs/%s.png", .x)
    )
  })

# train
train <- train_full %>%
  filter(Usage %in% "train") %>%
  select(id, corr) %>%
  write_csv(path = "train.csv")

# test
test <- train_full %>%
  filter(!Usage %in% "train") %>%
  select(id, corr, Usage) %>%
  write_csv(path = "test.csv")

# example_submition
set.seed(1)
example_submition <- train_full %>%
  filter(!Usage %in% "train") %>%
  select(id, corr) %>%
  mutate(corr = 2*runif(n()) - 1) %>%
  write_csv(path = "example_submition.csv")

example_submition %>%
  head %>%
  write_csv(path = "stdout")
