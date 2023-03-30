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
    c("GCAM_AgLU_SUA_APE_1973_2019")

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



    supplysec <-
      data.frame(region = "USA", supplysector = "regional corn", output.unit = "Mt", input.unit = "Mt",
               price.unit = "1975$/kg", logit.year.fillout = 1975, logit.exponent = -3, logit.type = NA)

    subsec <-
      data.frame(region = "USA", supplysector = "regional corn", subsector = "regional corn",
                 logit.year.fillout = 1975, logit.exponent = 0, logit.type = NA)

    subsec_sw <-
      data.frame(region = "USA", supplysector = "regional corn", subsector = "regional corn",
                 year.fillout = 1975, share.weight = 1)


    fst_interp <-
      data.frame(region = "USA", supplysector = "regional corn", subsector = "regional corn",
                 food.storage.technology = "ag-storage",
                 apply.to = "share-weight",
                 from.year = MODEL_FINAL_BASE_YEAR, to.year = max(MODEL_YEARS), interpolation.function = "fixed") %>%
      select(LEVEL2_DATA_NAMES[["FoodTechInterp"]])


    Model_Period_Length <- 5
    fst_extra <-
      data.frame(region = "USA", supplysector = "regional corn", subsector = "regional corn",
                 food.storage.technology = "ag-storage",
                 year = c(MODEL_BASE_YEARS, min(MODEL_FUTURE_YEARS)),
                 share.weight = 1,
                 logit.exponent = 0.6,
                 closing.stock = 0,   loss.coefficient = 1,  carried.forward = 0) %>%
        dplyr::group_by_at(vars(region:food.storage.technology)) %>%
        mutate(lifetime = lead(year, 2) - year) %>%
        mutate(lifetime = if_else(year == 1990, 16, lifetime),
               lifetime = if_else(year == 2005, 6, lifetime)) %>%
        ungroup() %>%
        replace_na(list(lifetime = Model_Period_Length * 2)) %>%
        select(LEVEL2_DATA_NAMES[["FoodTech"]])

    fst_coef <-
      fst_extra %>% mutate(minicam.energy.input = "total corn", coefficient = 1, market.name = region) %>%
        select(LEVEL2_DATA_NAMES[["FoodTechCoef"]])

    fst_cost <-
      fst_extra %>% mutate(minicam.non.energy.input = "storage-cost", input.cost = 0) %>%
        select(LEVEL2_DATA_NAMES[["FoodTechCost"]])

    fst_RESSecOut <-
      fst_extra %>% mutate(minicam.non.energy.input = "storage-cost",
                         res.secondary.output = "Corn", output.ratio = 1) %>%
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
