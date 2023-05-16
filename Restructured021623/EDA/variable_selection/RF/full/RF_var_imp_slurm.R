library(randomForestSRC)
library(tidyr)
library(rsample)
# library(doParallel)
library(dplyr)



## Data

## Read in most of data and create indicator variables
data <- readRDS("~/peregrine_amazon/data/annual/aad_2021_forests.rds") %>%
  mutate(across(contains('pland'), ~ case_when(.x > 0 ~ 1,
                                               .x == 0 ~ 0)),
         CL = case_when(CL == 0 ~ 0,
                        CL > 0 ~ 1) %>% as.factor())

# Data partitioning

set.seed(777)

full.split <- initial_split(data, strata=CL)
full.training <- training(full.split) %>%
  as.data.frame()
full.testing <- testing(full.split) %>%
  as.data.frame()


#### Get all predictors and make formula


# remove variables to make formula (e.g. don't want Name or Code)
full.predictors <- data %>%
  select(-c(Name, Code, CL, OptTemp_Obs:Malaria_OptTemp, fold, StableLights, AvgRad)) %>%
  names()

# output length of predictors
full.predictors %>% length() # 487

full.formula <- paste("CL ~ ", paste0(full.predictors, collapse=" + ")) %>%
  as.formula()



#### The model


t0 <- Sys.time()
full.o <- tune(full.formula, data = full.training, rfq = TRUE) # uses (minimizes?) out of bag error
Sys.time() - t0 # 38 seconds

saveRDS(full.o, "~/peregrine_amazon/var_selection/RF/full/full.o.rds")

t0 <- Sys.time()
# train model
full.rf0_imb_rfq=imbalanced(
  full.formula,
  ntree=3000, # increase ntree to 3000
  data=full.training,
  mtry = as.numeric(full.o$optimal[2]),
  nodesize = as.numeric(full.o$optimal[1]),
  method = "rfq",
  do.trace=T,
  importance="random",
  statistics = T,
  forest=T
)

Sys.time() - t0 # 90 seconds

saveRDS(full.rf0_imb_rfq, "~/peregrine_amazon/var_selection/RF/full/full.rf0_imb_rfq.rds")