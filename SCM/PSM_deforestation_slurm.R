# PSM deforestation for Slurm

library(MatchIt)
library(lmtest)
library(sandwich)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(kableExtra)

# use forest_fragmentation or edge_loss or a mix?

psmForestFragmentationFunc <- function(lagYear){
  full_data <- readRDS("~/peregrine_amazon/data/annual/aad_2021_forests.rds") %>%
    group_by(Code) %>%
    mutate(intervention.st = lag(forest_fragmentation, n = lagYear) - forest_fragmentation > 0) %>%
    ungroup()


  intervention_data <- full_data %>%
    filter(intervention.st == T) %>%
    group_by(Code) %>%
    # filter(row_number() == 1) %>%
    complete(Year = Year:2020) %>% # get all years after the initial mining presence
    ungroup() %>%
    select(Code, Year) %>%
    unique() %>%
    # use this key_pair column to access later
    mutate(key_pair = as.character(paste0(as.character(Code), "_", as.character(Year))))

  data <- full_data %>%
    filter(!is.na(Pfal.Malaria)) %>%  # remove observations with missing Malaria
    select(# ID vars
      Code, Name, Country, Year,
      Pfal.Malaria, intervention.st, forest_density,
      # time-variant vars
      land_use_change, forest_fragmentation, edge_loss,
      NDVI, LST_Day, min_LST_Day, max_LST_Day,
      HNTL, Precip, min_Precip, max_Precip, min_Precip_Month, max_Precip_Month,
      # time-invariant vars
      mean_Elevation, var_Elevation, SWChange_abs, SWRecurrence, SWSeasonality, SWOccurrence) %>%
    mutate(key_pair = as.character(paste0(as.character(Code), "_", as.character(Year))))

  # saveRDS(data, "~/peregrine_amazon/SCM/deforestation/data/data.rds")

  # get year of first treatment called intervention
  first_intervention_data <- intervention_data %>%
    # group_by(Code) %>%
    # filter(row_number() == 1) %>%
    # ungroup() %>%
    rename(intervention = Year) %>%
    select(-key_pair)


  data.use <- data %>%
    mutate(key_pair = as.character(paste0(as.character(Code), "_", as.character(Year)))) %>%
    group_by(Code) %>%
    # filter out Peru since there's no data for years before 2016
    # filter(Country != "Peru") %>%
    # mark the observations of deforestation in the municipality
    mutate(deforested = key_pair %in% intervention_data$key_pair) %>%
    # filter out municipalities that have incomplete data (missing for some years)
    filter(n() == 2020 - 2007 + 1) %>%
    ungroup() %>%
    # balance the data set by code across all years
    complete(nesting(Code), Year = full_seq(Year, period = 1)) %>%
    as.data.frame() %>%
    # merge data with first intervention years
    full_join(first_intervention_data, by=c("Code")) %>%
    mutate(intervention = ifelse(is.na(intervention),
                                 Inf, intervention),
           deforested = 1* (Year >= intervention)) %>%
    # rearrange variables
    select(Code, Name, Country, Year, Pfal.Malaria, intervention, everything(), -intervention.st, -key_pair) %>%
    # multisynth needs a few years of pre-intervention in order to work
    # need as many complete cases as possible, so only filter out missing data for covariates
    filter(across(forest_density:SWOccurrence, ~ !is.na(.x)))

  # saveRDS(data.use, "~/peregrine_amazon/SCM/deforestation/data/data.use.rds")

  t0 <- Sys.time()

  # https://statsnotebook.io/blog/analysis/matching/#r-codes-for-outcome-analysis-step-2
  match_obj <- matchit(
    deforested ~
      # forest variables
      forest_density + # land_use_change + forest_fragmentation + edge_loss +
      # environmental variables
      NDVI + LST_Day + min_LST_Day + max_LST_Day +
      HNTL + Precip + min_Precip + max_Precip +
      min_Precip_Month + max_Precip_Month +
      mean_Elevation + var_Elevation +
      SWChange_abs + SWRecurrence + SWSeasonality + SWOccurrence,
    data.use, method = "nearest", distance = "glm", link = 'logit'
  )

  Sys.time() - t0

  saveRDS(match_obj, paste0("~/peregrine_amazon/SCM/data/match_obj_forestfragmentation_", lagYear, ".rds"))

  summary(match_obj)

  matched_data <- match.data(match_obj)

  # Run regression model with psychological distress as the outcome, and smoker as the only predictor
  # We need to specify the weights - Matched participants have a weight of 1, unmatched participants
  res <- glm(Pfal.Malaria ~ deforested, data = matched_data, weights = weights, family = "quasipoisson")

  summary(res)

  # Test the coefficient using cluster robust standard error
  coeftest(res, vcov. = vcovCL, cluster = ~subclass)
  # Calculate confidence intervals based on cluster robust standard error
  coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)
}

psmEdgeLossFunc <- function(lagYear){
  full_data <- readRDS("~/peregrine_amazon/data/annual/aad_2021_forests.rds") %>%
    group_by(Code) %>%
    mutate(intervention.st = lag(edge_loss, n = lagYear) - edge_loss > 0) %>%
    ungroup()


  intervention_data <- full_data %>%
    filter(intervention.st == T) %>%
    group_by(Code) %>%
    # filter(row_number() == 1) %>%
    complete(Year = Year:2020) %>% # get all years after the initial mining presence
    ungroup() %>%
    select(Code, Year) %>%
    unique() %>%
    # use this key_pair column to access later
    mutate(key_pair = as.character(paste0(as.character(Code), "_", as.character(Year))))

  data <- full_data %>%
    filter(!is.na(Pfal.Malaria)) %>%  # remove observations with missing Malaria
    select(# ID vars
      Code, Name, Country, Year,
      Pfal.Malaria, intervention.st, forest_density,
      # time-variant vars
      land_use_change, forest_fragmentation, edge_loss,
      NDVI, LST_Day, min_LST_Day, max_LST_Day,
      HNTL, Precip, min_Precip, max_Precip, min_Precip_Month, max_Precip_Month,
      # time-invariant vars
      mean_Elevation, var_Elevation, SWChange_abs, SWRecurrence, SWSeasonality, SWOccurrence) %>%
    mutate(key_pair = as.character(paste0(as.character(Code), "_", as.character(Year))))

  # saveRDS(data, "~/peregrine_amazon/SCM/deforestation/data/data.rds")

  # get year of first treatment called intervention
  first_intervention_data <- intervention_data %>%
    # group_by(Code) %>%
    # filter(row_number() == 1) %>%
    # ungroup() %>%
    rename(intervention = Year) %>%
    select(-key_pair)


  data.use <- data %>%
    mutate(key_pair = as.character(paste0(as.character(Code), "_", as.character(Year)))) %>%
    group_by(Code) %>%
    # filter out Peru since there's no data for years before 2016
    # filter(Country != "Peru") %>%
    # mark the observations of deforestation in the municipality
    mutate(deforested = key_pair %in% intervention_data$key_pair) %>%
    # filter out municipalities that have incomplete data (missing for some years)
    filter(n() == 2020 - 2007 + 1) %>%
    ungroup() %>%
    # balance the data set by code across all years
    complete(nesting(Code), Year = full_seq(Year, period = 1)) %>%
    as.data.frame() %>%
    # merge data with first intervention years
    full_join(first_intervention_data, by=c("Code")) %>%
    mutate(intervention = ifelse(is.na(intervention),
                                 Inf, intervention),
           deforested = 1* (Year >= intervention)) %>%
    # rearrange variables
    select(Code, Name, Country, Year, Pfal.Malaria, intervention, everything(), -intervention.st, -key_pair) %>%
    # multisynth needs a few years of pre-intervention in order to work
    # need as many complete cases as possible, so only filter out missing data for covariates
    filter(across(forest_density:SWOccurrence, ~ !is.na(.x)))

  # saveRDS(data.use, "~/peregrine_amazon/SCM/deforestation/data/data.use.rds")

  t0 <- Sys.time()

  # https://statsnotebook.io/blog/analysis/matching/#r-codes-for-outcome-analysis-step-2
  match_obj <- matchit(
    deforested ~
      # forest variables
      forest_density + # land_use_change + forest_fragmentation + edge_loss +
      # environmental variables
      NDVI + LST_Day + min_LST_Day + max_LST_Day +
      HNTL + Precip + min_Precip + max_Precip +
      min_Precip_Month + max_Precip_Month +
      mean_Elevation + var_Elevation +
      SWChange_abs + SWRecurrence + SWSeasonality + SWOccurrence,
    data.use, method = "nearest", distance = "glm", link = 'logit'
  )

  Sys.time() - t0

  saveRDS(match_obj, paste0("~/peregrine_amazon/SCM/data/match_obj_edgeloss_", lagYear, ".rds"))

  summary(match_obj)

  matched_data <- match.data(match_obj)

  # Run regression model with psychological distress as the outcome, and smoker as the only predictor
  # We need to specify the weights - Matched participants have a weight of 1, unmatched participants
  res <- glm(Pfal.Malaria ~ deforested, data = matched_data, weights = weights, family = "quasipoisson")

  summary(res)

  # Test the coefficient using cluster robust standard error
  coeftest(res, vcov. = vcovCL, cluster = ~subclass)
  # Calculate confidence intervals based on cluster robust standard error
  coefci(res, vcov. = vcovCL, cluster = ~subclass, level = 0.95)
}

psmForestFragmentationFunc(1)
psmForestFragmentationFunc(2)
psmForestFragmentationFunc(3)
psmForestFragmentationFunc(4)
psmForestFragmentationFunc(5)

psmEdgeLossFunc(1)
psmEdgeLossFunc(2)
psmEdgeLossFunc(3)
psmEdgeLossFunc(4)
psmEdgeLossFunc(5)


