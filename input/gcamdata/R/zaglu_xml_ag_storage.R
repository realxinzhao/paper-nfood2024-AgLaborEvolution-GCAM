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
    c("L113.StorageTechTable")

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


    # Pull region and sector to prepare for tables for xml generating
    L113.StorageTechTable %>%
      distinct(region, supplysector, food.storage.technology) ->
      L113.ag_Storage_region_supplysector


    # Tables for xmls
    supplysec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       output.unit = "Mt", input.unit = "Mt",
                       price.unit = "1975$/kg", logit.year.fillout = min(MODEL_BASE_YEARS),
                       logit.exponent = -3, logit.type = NA)

    subsec <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector, logit.year.fillout = min(MODEL_BASE_YEARS),
                       logit.exponent = 0, logit.type = NA)

    subsec_sw <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       year.fillout = min(MODEL_BASE_YEARS), share.weight = 1)

    fst_interp <-
      L113.ag_Storage_region_supplysector %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology,
                       apply.to = "share-weight",
                       from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["FoodTechInterp"]])


    fst_extra <-
      L113.StorageTechTable %>%
      select(LEVEL2_DATA_NAMES[["FoodTech"]])

    fst_coef <-
      L113.StorageTechTable %>%
      mutate(coefficient = 1, market.name = region) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechCoef"]])

    # The cost here is not really useful for storage. It is cost to main product
    # fst_cost <-
    #   L113.StorageTechTable %>% mutate(minicam.non.energy.input = "storage-cost", input.cost = 0) %>%
    #   select(LEVEL2_DATA_NAMES[["FoodTechCost"]])


    fst_RESSecOut <-
      L113.StorageTechTable %>%
      mutate(res.secondary.output = GCAM_commodity) %>%
      mutate(output.ratio = 1, pMultiplier = 0) %>%
      select(LEVEL2_DATA_NAMES[["FoodTechRESSecOut"]])

    fst_shwt <-
      L113.StorageTechTable %>%
      select(LEVEL2_DATA_NAMES[["SubsectorShrwt"]])


    # fst_RESSecOut %>% filter(supplysector != "regional vegetables") ->
    #   fst_RESSecOut

    # Produce outputs ----
    ag_storage.xml <-
      create_xml("ag_storage.xml") %>%
      add_logit_tables_xml(supplysec, "Supplysector") %>%
      add_logit_tables_xml(subsec, "SubsectorLogit") %>%
      add_xml_data(subsec_sw, "SubsectorShrwtFllt") %>%
      add_xml_data(fst_interp, "FoodTechInterp") %>%
      add_xml_data(fst_extra, "FoodTech") %>%
      add_xml_data(fst_coef, "FoodTechCoef") %>%
      #add_xml_data(fst_cost, "FoodTechCost") %>%
      add_xml_data(fst_RESSecOut, "FoodTechRESSecOut") %>%
      add_xml_data(fst_shwt, "SubsectorShrwt") %>%
      add_precursors("L113.StorageTechTable")


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
