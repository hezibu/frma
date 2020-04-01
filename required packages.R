#####libraries needed for functional response meta-analysis:
is_inst <- function(pkg) {
  nzchar(system.file(package = pkg))
}

if (!is_inst("pacman")) {install.packages("pacman")}

library("pacman")

p_load(tidyverse
       ,rfishbase
       ,lme4
       ,MuMIn
       ,metafor
       ,plotly
       ,magrittr
       ,gridExtra,
       here,
       randomForest,
       randomForestExplainer,
       sjPlot)
