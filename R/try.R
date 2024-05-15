# Finding distance matrix
# distance_mat <- dist(mtcars, method = 'euclidean')
# distance_mat

library(tidyverse)
library(beautiful.minds)

data_detoxed <- detox(py_20_sample, remove_strictly_na = TRUE)

data_use <- data_detoxed |>
  select(-contains("date") &
           -contains("eligibility") &
           -contains("ext") &
           -contains("compdisenrollmsg"))

distance_mat <- dist(data_use, method = 'euclidean')
distance_mat

# Fitting Hierarchical clustering Model
# to training dataset
set.seed(240) # Setting seed
Hierar_cl <- hclust(distance_mat, method = "average")
Hierar_cl

# Plotting dendrogram
plot(Hierar_cl)

# Choosing no. of clusters
# Cutting tree by height
abline(h = 110, col = "green")

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )
fit

table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")
