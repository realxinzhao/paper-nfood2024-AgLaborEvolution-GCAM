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
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y")

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

    # define commodity (TODO: move to constants.R later)

    # aglu.STORAGE_COMMODITIES <-
    #   c("Corn",  "Legumes", "MiscCrop", "NutsSeeds", "OilCrop", "OtherGrain", "OilPalm",
    #     "Rice", "RootTuber", "Soybean", "SugarCrop",  "Wheat", "FiberCrop") # "Fruits", "Vegetables",
    #
    # aglu.STORAGE_COMMODITIES <-
    #   c("Corn",  "Legumes", "MiscCrop", "NutsSeeds", "OilCrop", "OtherGrain", "OilPalm",
    #     "Rice", "RootTuber", "Soybean", "SugarCrop",  "Wheat", "FiberCrop", "Fruits", "Vegetables",
    #     "Beef",  "Dairy", "Pork", "Poultry", "SheepGoat")


    aglu.STORAGE_COMMODITIES <- c("Corn")

    # Get storage data from the adjusted SUA balances for aglu.STORAGE_COMMODITIES
    L109.ag_ALL_Mt_R_C_Y %>%
      gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID) %>%
      bind_rows(
        L109.an_ALL_Mt_R_C_Y %>%
          gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID)
      ) %>%
      # Keep relevant elements, storage comm., and base years only
      filter(GCAM_commodity %in% aglu.STORAGE_COMMODITIES,
             element %in% c("Opening stocks", "Closing stocks", "InterAnnualStockLoss"),
             year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) %>%
      spread(element, value) %>%
      mutate(LossCoef = 1 - InterAnnualStockLoss / `Closing stocks`) %>%
      group_by(GCAM_commodity, region) %>%
      # compute Carryforward here
      mutate(Carryforward = lead(`Opening stocks`),
             Carryforward = if_else(is.na(Carryforward),
                                    `Closing stocks` * LossCoef, Carryforward)) %>% ungroup %>%
      # adjust naming here for now
      mutate(supplysector = paste0("regional ", tolower(GCAM_commodity)),
             supplysector = if_else(supplysector == "regional roottuber", "regional root_tuber", supplysector),
             supplysector = if_else(supplysector == "regional nutsseeds", "regional nuts_seeds", supplysector)) %>%
      select(supplysector, region, year, LossCoef, `Closing stocks`, `Opening stocks`, Carryforward) ->
      L113.ag_Storage_Mt_R_C_Y_adj

    # Pull region and sector to prepare for tables for xml generating
    L113.ag_Storage_Mt_R_C_Y_adj %>%
        distinct(region, supplysector) ->
      L113.ag_Storage_region_supplysector


    # Tables for xmls
    supplysec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       output.unit = "Mt", input.unit = "Mt",
                       price.unit = "1975$/kg", logit.year.fillout = 1975, logit.exponent = -3, logit.type = NA)

    subsec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector, logit.year.fillout = 1975, logit.exponent = 0, logit.type = NA)

    subsec_sw <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector, year.fillout = 1975, share.weight = 1)

    fst_interp <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology = "ag-storage",
                       apply.to = "share-weight",
                       from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["FoodTechInterp"]])

    #[ToDo: Ellie, will need to update carried.forward here]
    # However, note that carried.forward is indeed the opening stock in the next period
    # This was computed earlier

    #[ToDo: Ellie, please double check lifetime here, thanks!]

    Model_Period_Length <- 5 # [ToDo: adding csv tables for years, logit exponents, and headers]
    L113.ag_Storage_Mt_R_C_Y_adj %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology = "ag-storage",
                       year,
                       share.weight = 1,
                       logit.exponent = 0.6, # 0.6
                       storage.cost = 0,
                       closing.stock = `Closing stocks`,
                       loss.coefficient = LossCoef,
                       opening.stock = Carryforward) %>%
      dplyr::group_by_at(vars(region:food.storage.technology)) %>%
      mutate(lifetime = lead(year, 2) - year) %>%
      mutate(lifetime = if_else(year == 1990, 16, lifetime),
             lifetime = if_else(year == 2005, 6, lifetime)) %>%
      ungroup() %>%
      replace_na(list(lifetime = Model_Period_Length * 2)) %>%
      select(LEVEL2_DATA_NAMES[["FoodTech"]]) -> fst_extra_1

    # add a future year here
    fst_extra_1 %>% bind_rows(
      fst_extra_1 %>% filter(year == max(MODEL_BASE_YEARS)) %>%
        mutate(year = min(MODEL_FUTURE_YEARS),
               # setting carried.forward and closing stock to zero because the values are not used
               opening.stock = 0,
               closing.stock = 0)
    ) -> fst_extra



    fst_coef <-
      fst_extra %>% mutate(minicam.energy.input = gsub("regional", "total", supplysector),
                           coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCoef"]])

    #[ToDo: Ellie, will need to update cost or add cost headers]
    # we can start with cost = 0 in placeholders
    fst_cost <-
      fst_extra %>% mutate(minicam.non.energy.input = "storage-cost", input.cost = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCost"]])


    fst_RESSecOut <-
      fst_extra %>%
      left_join(
        data.frame(res.secondary.output = aglu.STORAGE_COMMODITIES) %>%
          mutate(supplysector = paste0("regional ", tolower(res.secondary.output)) ) %>%
          mutate(supplysector = if_else(supplysector == "regional roottuber", "regional root_tuber", supplysector),
                 supplysector = if_else(supplysector == "regional nutsseeds", "regional nuts_seeds", supplysector)),
        by = "supplysector"
      ) %>%
      mutate(output.ratio = 1, pMultiplier = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechRESSecOut"]])

    fst_shwt <-
      fst_extra %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])





    # There should be no storage if there is no consumption!
    # Argentina in 2010 had no palm consumption


    # Fiber in Euro_E had no storage
    # too smaller extent a concern since we can add tiny values
    # solutions issues were gone after adding tiny values for storage
    # may also be true for consumption







    #[ToDo: adding precursors]
    # Produce outputs ----
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
