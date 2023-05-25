## TJ Sipin, 05/15/23
## install packages
# remotes::install_github("cran/LowRankQP")
# remotes::install_github("ssdavenport/microsynth")

library(microsynth)
library(dplyr)
options(scipen = 999)
data("seattledmi", package = "microsynth")

seattledmi %>% tibble()

seattledmi %>% names()

match.out <- c("i_felony", "i_misdemea", "i_drugs", "any_crime")

cov.var <- c("TotalPop", "BLACK", "HISPANIC", "Males_1521", "HOUSEHOLDS",
             "FAMILYHOUS", "FEMALE_HOU", "RENTER_HOU", "VACANT_HOU")

sea1 <- microsynth(
  seattledmi, idvar = "ID", timevar = "time",
  intvar = "Intervention", start.pre = 1, end.pre = 12,
  end.post = 16, match.out = match.out, match.covar = cov.var,
  result.var = match.out, jack = T, perm = 250, test = "lower", n.cores = 1
)

sea1 %>% summary()

plot_microsynth(sea1)


#### ex 2

match.out <- c("i_robbery", "i_aggassau", "i_burglary", "i_larceny",
               "i_felony", "i_misdemea", "i_drugsale", "i_drugposs", "any_crime")

sea2 <- microsynth(
  seattledmi, idvar = "ID", timevar = "time",
  intvar = "Intervention", start.pre = 1, end.pre = 12, end.post = 16,
  match.out = match.out, match.covar = cov.var, result.var = match.out,
  jack = TRUE, perm = 250, check.feas = TRUE, use.backup = TRUE,
  test = "lower", n.cores = 1
)

sea2 %>% summary()
