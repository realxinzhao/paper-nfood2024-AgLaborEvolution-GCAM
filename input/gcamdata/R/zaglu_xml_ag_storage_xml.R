# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_batch_ag_storage_xml
#'
#' Construct XML data structure for \code{ag_storage.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_storage.xml}.
module_aglu_batch_ag_storage_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      "L101.ag_Storage_Mt_R_C_Y")

  MODULE_OUTPUTS <-
    c(XML = "ag_storage.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # There should be no storage if there is no consumption!
    # Argentina in 2010 had no palm consumption


    # Fiber in Euro_E had no storage
    # too smaller extent a concern since we can add tiny values
    # solutions issues were gone after adding tiny values for storage
    # may also be true for consumption


    #COMM_STORAGE <- c("Corn", "Wheat", "Rice", "RootTuber", "OtherGrain")
    COMM_STORAGE <- c("Corn",  "Legumes", "MiscCrop", "NutsSeeds",
                      "OilCrop", "OtherGrain", "OilPalm", "Rice", "RootTuber", "Soybean",
                      "SugarCrop",  "Wheat", "FiberCrop") # "Fruits", "Vegetables",
    # COMM_STORAGE <- c("Corn", "Wheat", "Rice", "RootTuber", "OtherGrain", "NutsSeeds",
    #                   "OilCrop", "Legumes")
    # COMM_STORAGE <- c("Corn", "Wheat", "Rice", "RootTuber", "OtherGrain", "root_tuber")

    COMM_STORAGE <- c("FiberCrop")

    COMM_STORAGE <- c("Corn",  "Legumes", "MiscCrop", "NutsSeeds",
                      "OilCrop", "OtherGrain", "OilPalm", "Rice", "RootTuber", "Soybean",
                      "SugarCrop",  "Wheat", "FiberCrop", "Fruits", "Vegetables",
                      "Beef",  "Dairy", "Pork", "Poultry", "SheepGoat")

    COMM_STORAGE <- c("Corn")

    L101.ag_Storage_Mt_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) %>%
      filter(GCAM_commodity %in% COMM_STORAGE) %>%
      filter(year %in% 2015) %>%
      filter(element == "Closing stocks") %>%
      mutate(supplysector = paste0("regional ", tolower(GCAM_commodity))) %>%
      mutate(supplysector = if_else(supplysector == "regional roottuber", "regional root_tuber", supplysector),
             supplysector = if_else(supplysector == "regional nutsseeds", "regional nuts_seeds", supplysector)) %>%
      mutate(value = if_else(value == 0, 0.001, value))->
      AgStock

    L101.ag_Storage_Mt_R_C_Y %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(GCAM_commodity %in% COMM_STORAGE) %>%
      filter(year == 2015) %>%
      spread(element, value) %>%
      mutate(StorageLoss = `Closing stocks` * StorageLossRate) %>%
      mutate(LossCoef = 1 - StorageLossRate) %>%
      mutate(supplysector = paste0("regional ", tolower(GCAM_commodity))) %>%
      mutate(supplysector = if_else(supplysector == "regional roottuber", "regional root_tuber", supplysector),
             supplysector = if_else(supplysector == "regional nutsseeds", "regional nuts_seeds", supplysector)) %>%
      select(supplysector, region, year, LossCoef) ->
      AgStockLossCoef


    supplysec <- AgStock %>%
      dplyr::transmute(region, supplysector,
                output.unit = "Mt", input.unit = "Mt",
                price.unit = "1975$/kg", logit.year.fillout = 1975, logit.exponent = -3, logit.type = NA)

    subsec <-
      AgStock %>%
      dplyr::transmute(region, supplysector,
                subsector = supplysector, logit.year.fillout = 1975, logit.exponent = 0, logit.type = NA)

    subsec_sw <-
      AgStock %>%
      dplyr::transmute(region, supplysector,
                subsector = supplysector, year.fillout = 1975, share.weight = 1)

    fst_interp <-
      AgStock %>%
      dplyr::transmute(region, supplysector,
                subsector = supplysector,
                food.storage.technology = "ag-storage",
                apply.to = "share-weight",
                from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["FoodTechInterp"]])

    Model_Period_Length <- 5
    fst_extra <-
      AgStock %>% select(-year) %>%
      repeat_add_columns(  tibble(year = c(MODEL_BASE_YEARS, min(MODEL_FUTURE_YEARS)))) %>%
      dplyr::transmute(region, supplysector,
                subsector = supplysector,
                food.storage.technology = "ag-storage",
                year,
                share.weight = 1,
                logit.exponent = 0.6, # 0.6
                closing.stock = value,
                loss.coefficient = 1,  carried.forward = value) %>%
      dplyr::group_by_at(vars(region:food.storage.technology)) %>%
      mutate(lifetime = lead(year, 2) - year) %>%
      mutate(lifetime = if_else(year == 1990, 16, lifetime),
             lifetime = if_else(year == 2005, 6, lifetime)) %>%
      ungroup() %>%
      replace_na(list(lifetime = Model_Period_Length * 2)) %>%
      select(LEVEL2_DATA_NAMES[["FoodTech"]])

    fst_extra %>%
      left_join_error_no_match(
        AgStockLossCoef %>% select(-year), by = c("region", "supplysector")) %>%
      mutate(loss.coefficient = LossCoef) %>%
      select(-LossCoef) -> fst_extra

    fst_coef <-
      fst_extra %>% mutate(minicam.energy.input = gsub("regional", "total", supplysector),
                           coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCoef"]])

    fst_cost <-
      fst_extra %>% mutate(minicam.non.energy.input = "storage-cost", input.cost = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCost"]])


    fst_RESSecOut <-
      fst_extra %>%
      left_join(
        data.frame(res.secondary.output = COMM_STORAGE) %>%
          mutate(supplysector = paste0("regional ", tolower(res.secondary.output)) ) %>%
          mutate(supplysector = if_else(supplysector == "regional roottuber", "regional root_tuber", supplysector),
                 supplysector = if_else(supplysector == "regional nutsseeds", "regional nuts_seeds", supplysector)),
        by = "supplysector"
        ) %>%
      mutate(minicam.non.energy.input = "storage-cost",
                           output.ratio = 1, pMultiplier = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechRESSecOut"]])

    fst_shwt <-
      fst_extra %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])






    # Produce outputs
    ag_storage.xml <-
      create_xml("ag_storage.xml") %>%
      add_logit_tables_xml(supplysec, "Supplysector") %>%
      add_logit_tables_xml(subsec, "SubsectorLogit") %>%
      add_xml_data(subsec_sw, "SubsectorShrwtFllt") %>%
      add_xml_data(fst_interp, "FoodTechInterp") %>%
      add_xml_data(fst_extra, "FoodTech") %>%
      add_xml_data(fst_coef, "FoodTechCoef") %>%
      add_xml_data(fst_cost, "FoodTechCost") %>%
      add_xml_data(fst_RESSecOut, "FoodTechRESSecOut") %>%
      add_xml_data(fst_shwt, "SubsectorShrwt") #%>% add_precursors()


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
