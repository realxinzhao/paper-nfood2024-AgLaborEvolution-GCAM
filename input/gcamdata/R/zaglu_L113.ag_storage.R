# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L113_ag_storage
#'
#' Preparing and processing agricultural storage data
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L113.ag_Storage_Mt_R_C_Y_adj}.
#' @details This chunk calculates ag storage values, losses, and costs by GCAM region and commodity.
#' @importFrom assertthat assert_that
#' @importFrom dplyr filter group_by mutate select transmute
#' @importFrom tidyr gather spread replace_na
#' @author XZ 2023
module_aglu_L113_ag_storage <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/GCAM_region_names",
      FILE = "aglu/A_agStorageSector",
      "L109.ag_ALL_Mt_R_C_Y",
      "L109.an_ALL_Mt_R_C_Y")

  MODULE_OUTPUTS <-
    c("L113.StorageTechAndPassThrough")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # [ToDo: adding forest (L110) to here later when seeing fit]
    # so regional forest and total forest would be separated


    # 1. Get storage data from the adjusted SUA balances ----
    # And get parameters ready
    L109.ag_ALL_Mt_R_C_Y %>%
      gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID) %>%
      bind_rows(
        L109.an_ALL_Mt_R_C_Y %>%
          gather(element, value, -GCAM_commodity, -year, -GCAM_region_ID)
      ) %>%
      # Keep relevant elements, storage comm., and base years only
      filter(element %in% c("Opening stocks", "Closing stocks", "InterAnnualStockLoss"),
             year %in% MODEL_BASE_YEARS) %>%
      # Keep sectors in A_agStorageSector which are the sectors with regional markets
      # And join sector mappings and parameters
      # all storage commodities are in L109 SUA data; this was asserted in the earlier stage
      inner_join(A_agStorageSector, by = "GCAM_commodity") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(-GCAM_region_ID) ->
      L113.ag_Storage_Mt_R_C_Y_adj1


    # 2. Compute loss-coefficients and arrange data to get table needed ----
    L113.ag_Storage_Mt_R_C_Y_adj1 %>%
      spread(element, value) %>%
      mutate(LossCoef = 1 - InterAnnualStockLoss / `Closing stocks`) %>%
      replace_na(list(LossCoef = 0)) %>%
      group_by(GCAM_commodity, region) %>%
      # compute Carry-forward here
      mutate(Carryforward = lead(`Opening stocks`),
             Carryforward = if_else(is.na(Carryforward),
                                    `Closing stocks` * LossCoef, Carryforward)) %>% ungroup %>%
      select(supplysector, region, year, LossCoef, `Closing stocks`, `Opening stocks`,
             Carryforward, logit.exponent, technology, minicam_energy_input, GCAM_commodity, storage_model) ->
      L113.ag_Storage_Mt_R_C_Y_adj



    # 3. Prepare a storage tech data table for generating XMLs. ----

    # Calculate lifetime in the carry-over structure
    # may need adjustments here when years/steps are changing
    L113.StorageLifeTime <-
      tibble(year = c(MODEL_BASE_YEARS, MODEL_FUTURE_YEARS)) %>%
      mutate(lifetime = lead(year, 2) - year) %>%
      mutate(lifetime = if_else(year == 1990, 16, lifetime),
             lifetime = if_else(year == 2005, 6, lifetime))

    # Construct the key table for storage tech.
    # ToDo: storage.cost
    L113.StorageTechAndPassThrough <-
      L113.ag_Storage_Mt_R_C_Y_adj %>%
      dplyr::transmute(region, supplysector,
                       subsector = supplysector,
                       food.storage.technology = technology,
                       year,
                       share.weight = 1,
                       logit.exponent,
                       storage.cost = 0,
                       closing.stock = `Closing stocks`,
                       loss.coefficient = LossCoef,
                       opening.stock = Carryforward,
                       minicam.energy.input = minicam_energy_input,
                       GCAM_commodity, storage_model) %>%
      left_join_error_no_match(L113.StorageLifeTime, by = "year")

    # add a future year here for storage commodities (storage_model == TRUE)
    L113.StorageTechAndPassThrough <-
      L113.StorageTechAndPassThrough %>%
      bind_rows(
        L113.StorageTechAndPassThrough %>% filter(storage_model == TRUE) %>%
          filter(year == max(MODEL_BASE_YEARS)) %>%
          mutate(year = min(MODEL_FUTURE_YEARS),
                 # setting carried.forward and closing stock to zero
                 # The two are not important because the values are not used
                 opening.stock = closing.stock * loss.coefficient,
                 closing.stock = opening.stock,
                 lifetime = sort(MODEL_FUTURE_YEARS)[2] - max(MODEL_BASE_YEARS))
      )

    # Produce outputs ----
    L113.StorageTechAndPassThrough %>%
      add_title("Ag storage data and parameter assumptions") %>%
      add_units("various") %>%
      add_precursors("common/GCAM_region_names",
                     "aglu/A_agStorageSector",
                     "L109.ag_ALL_Mt_R_C_Y",
                     "L109.an_ALL_Mt_R_C_Y") ->
      L113.StorageTechAndPassThrough


    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
