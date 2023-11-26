# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_L2082.ag_laborcapital_irr_mgmt
#'
#' Specifies labor and capital coefficients for all technologies; adjusts nonLandVariableCost to remove labor and capital cost.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L2082.AgCoef_laborcapital_ag_irr_mgmt}, \code{L2082.AgCoef_laborcapital_bio_irr_mgmt},
#' \code{L2082.AgCost_ag_irr_mgmt_adj}, \code{L2082.AgCost_bio_irr_mgmt_adj}.
#' @details This chunk compute IO coefficients for labor and capital uses in Ag and forest
#' Adjust nonLandVariableCost to remove the explicitly computed labor and capital costs.
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows filter if_else left_join mutate select
#' @importFrom tidyr replace_na gather
#' @importFrom tibble tibble
#' @author XZ DS Nov 2023
module_aglu_L2082.ag_laborcapital_irr_mgmt <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2062.AgCost_ag_irr_mgmt_adj",
      "L2062.AgCost_bio_irr_mgmt_adj",
      "L2052.AgCost_For",
      "L202.StubTechCost_an",
      "L100.GTAPCostShare_AgLU_reg_comm",
      FILE = "common/iso_GCAM_regID",
      FILE = "aglu/USDA/Ag_labor_USDA",
      FILE = "aglu/Ag_labor_ILO",
      FILE = "aglu/USDA/Ag_capital_USDA",
      FILE = "aglu/AGLU_ctry",
      FILE = "common/GCAM_region_names",
      FILE = "water/basin_to_country_mapping",
      "L161.ag_rfdProd_Mt_R_C_Y_GLU",
      "L161.ag_irrProd_Mt_R_C_Y_GLU",
      "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level",
      "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
      "L2052.AgProdChange_ag_irr_ref",
      "L2052.AgProdChange_bio_irr_ref",
      "L2012.AgProduction_For",
      FILE = "socioeconomics/gcam_macro_TFP_open",
      FILE = "socioeconomics/GTAP/GCAM_GTAP_Agsector_mapping",
      FILE = "socioeconomics/GTAP/GTAP_sector_aggregation_mapping",
      FILE = "socioeconomics/GTAP/GCAM_GTAP_region_mapping",
      OPTIONAL_FILE = "socioeconomics/GTAP/GTAPv10_baseview_SF01_VFA")

  MODULE_OUTPUTS <-
    c("L2082.AgCoef_laborcapital_ag_irr_mgmt",
      "L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
      "L2082.AgCoef_laborcapital_bio_irr_mgmt",
      "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
      "L2082.AgCoef_laborcapital_for",
      "L2082.AgCoef_laborcapital_for_tfp_MA",
      "L2082.StubTechCoef_laborcapital_an",
      "L2082.StubTechCoef_laborcapital_an_tfp_MA",
      "L2082.AgCost_ag_irr_mgmt_adj",
      "L2082.AgCost_bio_irr_mgmt_adj",
      "L2082.AgCost_For_adj",
      "L2082.StubTechCost_an_adj",
      "L2082.region_laborprice",
      "L2082.region_capitalprice",
      "L2082.laborcapital_ag_irr_mgmt_expenditure",
      "L2082.laborcapital_an_expenditure",
      "L2082.laborcapital_for_expenditure")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    year <- value <- GCAM_region_ID <- GCAM_commodity <- GLU <- GLU_name <- IRR_RFD <-
      MGMT <- region <- AgSupplySector <- AgSupplySubsector <- AgProductionTechnology <-
      minicam.energy.input <- coefficient <- WaterContent <- nonLandVariableCost <-
      FertCost <- GCAM_subsector <- NULL  # silence package check notes

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # Labor and Capital cost share as GCAM nonland or nonfeed costs (mainly material and energy)
    # the cost share will be used for partitioning ag labor and captial
    # Note that AgLU self use is also removed
    # map all historical years with an interpolation
    L100.GTAPCostShare_AgLU_reg_comm %>%
      select(-year, -value) %>% distinct() %>%
      repeat_add_columns(tibble::tibble(year = HISTORICAL_YEARS)) %>%
      left_join(L100.GTAPCostShare_AgLU_reg_comm,
                by = c("GCAM_region_ID", "GCAM_commodity", "input", "year")) %>%
      group_by(GCAM_region_ID, GCAM_commodity, input) %>%
      mutate(value = approx_fun(year, value, rule = 2)) %>%
      ungroup() %>%
      filter(!input %in% c("Chemicals", "Land", "Water", "AgLU")) %>%
      group_by(GCAM_region_ID, year, GCAM_commodity) %>%
      mutate(value = value / sum(value)) %>% ungroup() %>%
      filter(input %in% c("Labor", "Capital")) %>%
      mutate(input = paste(input, "Ag",sep =  "_")) %>%
      spread(input, value) ->
      L100.GTAP_AgLaborCapitalCostShare


    #Step 1. Get data ready ----
    ## Using the GTAP cost share ----
    L100.GTAP_AgLaborCapitalCostShare %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") ->
      Agcostshare_R_GTAP_hist

    # add future periods
    Agcostshare_R_GTAP_hist %>%
      filter(year == 2015) %>% select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(Agcostshare_R_GTAP_hist) ->
      Agcostshare_R_GTAP


    # Ag, Forest and Fishery labor share ----
    # use ILO raw data to calculate sectoral labor share when data is available --
    # use GTAP labor expenditure share by sector to calculate the labor share for regions missing ILO data --

    # TODO: add the data into csv input folder
    Ag_labor_ILO %>%
      select(country = ref_area.label, sex.label, activity = classif1.label, year = time, value = obs_value)  %>%
      filter(sex.label == "Sex: Total") %>%
      filter(grepl("Crop", activity)|grepl("Fishing", activity)|grepl("Forestry", activity)) %>%
      filter(grepl("ISIC-Rev.4", activity)) %>%
      mutate(class = "AG",
             class = ifelse(grepl("Forestry", activity), "FOR", class),
             class = ifelse(grepl("Fishing", activity), "FISH", class),
             value = ifelse(is.na(value), 0, value)) %>%
      select(-sex.label, -activity) %>%
      spread(class, value) %>%
      select(country, year, AG, FISH, FOR) %>%
      na.omit() -> # 141 countries to 130 countries
      ILO_labor

    # TODO: the iso country mapping update
    ILO_labor %>%
      as_tibble() %>%
      mutate(country = gsub("Côte d'Ivoire", "Cote dIvoire", country),
             country = gsub("Congo, Democratic Republic of the", "Congo", country),
             country = gsub("Czechia", "Czech Republic", country),
             country = gsub("Kosovo", "Serbia", country),
             country = gsub("Lao People's Democratic Republic", "Lao Peoples Democratic Republic", country),
             country = gsub("North Macedonia", "Macedonia, the former Yugoslav Republic of", country),
             country = gsub("Occupied Palestinian Territory", "Palestinian Territory, Occupied", country),
             country = gsub("Eswatini", "Lesotho", country),
             country = gsub("Timor-Leste", "Timor Leste", country),
             country = gsub("Türkiye", "Turkey", country),
             country = gsub("United States", "United States of America", country),
             country = gsub("Türkiye", "Turkey", country),
             country = gsub("Wallis and Futuna Islands", "Wallis and Futuna", country)) %>%
      left_join(iso_GCAM_regID %>% rename(country = country_name), by = "country") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(region, year) %>%
      summarise(AG = sum(AG),
                FISH = sum(FISH),
                FOR = sum(FOR)) %>%
      filter(year >= 2010)  %>%
      group_by(region) %>%
      summarise(AG = sum(AG, na.rm = T),
                FISH = sum(FISH, na.rm = T),
                FOR = sum(FOR, na.rm = T))  %>%
      mutate(TOTAL = AG + FISH + FOR, # obtain sectoral labor share in AG
             AG = AG / TOTAL,
             FISH = FISH / TOTAL,
             FOR = FOR / TOTAL,
             check = AG + FISH + FOR) %>% # check: sum = 1
      ungroup() %>%
      select(region, FISH, FOR) ->
      ILO_labor_share

    # ILO missing data for
    # "Australia_NZ" "Canada"       "China"        "Russia"       "South Africa" "Taiwan"       "Argentina"

    GTAP_AgLU_sector <- GCAM_GTAP_Agsector_mapping %>%
      distinct(output = GTAP_sectors)

    # TODO: need to update the pre-built data to include: L100.GTAPCostShare_AgLU_reg
    GTAPv10_baseview_SF01_VFA %>%
      # Keep relevant AgLU sectors only
      right_join(GTAP_AgLU_sector,  by = "output") %>%
      # aggregate input sectors for simplicity
      left_join_error_no_match(GTAP_sector_aggregation_mapping %>% select(input = GTAPv10, input1 = GCAM_sector), by = "input") %>%
      group_by(region, year, sector = output, input = input1) %>%
      summarize(value = sum(value), .groups = "drop") ->
      L100.GTAPCostShare_AgLU_MilUSD

    # Aggregate to GCAM regions

    L100.GTAPCostShare_AgLU_MilUSD %>%
      rename(region_GTAP = region) %>%
      left_join_error_no_match(
        GCAM_GTAP_region_mapping %>% select(region_GTAP = GTAPv10_region, region_GCAM = GCAM_region),
        by = "region_GTAP") %>%
      left_join_error_no_match(
        GCAM_region_names %>% select(region_GCAM = region, GCAM_region_ID),
        by = "region_GCAM") %>%
      group_by(GCAM_region_ID, year, sector, input) %>%
      summarize(value = sum(value), .groups = "drop") ->
      L100.GTAPCostShare_AgLU_reg

    # Map to GCAM sectors and Calculate shares
    GCAM_GTAP_Agsector_mapping %>%
      select(GCAM_commodity, sector = GTAP_sectors, Primary) %>%
      # the mapping is two-way; only cost shares are useful!
      full_join(
        L100.GTAPCostShare_AgLU_reg, by = "sector") %>%
      mutate(input = if_else(Primary == F & input %in% c("Labor", "Capital"),
                             paste0(input, "_Proc"), input) ) %>%
      group_by(GCAM_region_ID, GCAM_commodity, input, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      # filter(region %in% c("Australia_NZ", "Canada", "China","Russia","South Africa", "Taiwan" ,"Argentina")) %>%
      filter(input == "Labor") %>%
      filter(year == 2014) %>%
      group_by(region) %>%
      mutate(TOTAL = sum(value)) %>%
      spread(GCAM_commodity, value) %>%
      mutate(FISH = OtherMeat_Fish / TOTAL,
             FOR = Forest / TOTAL) %>%
      select(region, FISH, FOR) %>%
      ungroup() ->
      GTAP_labor_share

    # For forestry (FOR), use ILO when available, otherwise, use GTAP.
    # Top 5 countries with forest resource --
    # the Russian Federation (20.1%), Brazil (12.2%), Canada (8.6%), USA (7.6%) and China (5.4%)
    # https://www.globalforestwatch.org/dashboards/global/
    # Eastern Europe (Ukraine) loss 11% of 2000 by 2022
    # Canada loss 12% of 2000 by 2022
    # USA loss 17% of 2000 by 2022

    # For fishery, USA's estimates is too big, use GTAP, otherwise use ILO when available.
    # SAN is high due to Guyana: https://www.gyeiti.org/fisheries#anchor
    # In 2017 the fisheries industry contributed 1.84% to the GDP (GYEITI Report)
    # The fishing industry employed some 15,000 people 2014. It is estimated that 4,000 to 5,000 people were directly employed in the sector and many more indirectly.
    # The ILO fishing employment in Guyana fall in Guyana's government website information.

    ILO_labor_share %>% rename(FISH_ILO = FISH, FOR_ILO = FOR) %>%
      full_join(GTAP_labor_share %>% rename(FISH_GTAP = FISH, FOR_GTAP = FOR),
                by = "region") %>%
      mutate(FISH_ILO = ifelse(is.na(FISH_ILO), FISH_GTAP, FISH_ILO), # use GTAP to fill missing ILO
             FOR_ILO = ifelse(is.na(FOR_ILO), FOR_GTAP, FOR_ILO), # use GTAP to fill missing ILO
             FISH = ifelse(region == "USA", FISH_GTAP, FISH_ILO), # use GTAP for USA FISH
             FOR = FOR_ILO) %>% # use ILO whenever available for forestry (FOR)
      mutate(AG = 1 - FISH - FOR) %>%
      select(region, FISH, FOR, AG) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") ->
      FOR_FISH_share_L

    GCAM_GTAP_Agsector_mapping %>%
      select(GCAM_commodity, sector = GTAP_sectors, Primary) %>%
      # the mapping is two-way; only cost shares are useful!
      full_join(
        L100.GTAPCostShare_AgLU_reg, by = "sector") %>%
      mutate(input = if_else(Primary == F & input %in% c("Labor", "Capital"),
                             paste0(input, "_Proc"), input) ) %>%
      group_by(GCAM_region_ID, GCAM_commodity, input, year) %>%
      summarize(value = sum(value), .groups = "drop") %>%
      left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
      filter(input == "Capital") %>%
      filter(year == 2014) %>%
      group_by(region) %>%
      mutate(TOTAL = sum(value)) %>%
      spread(GCAM_commodity, value) %>%
      mutate(FISH = OtherMeat_Fish / TOTAL,
             FOR = Forest / TOTAL) %>%
      select(region, FISH, FOR) %>%
      ungroup() ->
      GTAP_capital_share

    GTAP_capital_share %>%
      mutate(AG = 1 - FISH - FOR) %>%
      left_join_error_no_match(GCAM_region_names, by = "region") ->
      FOR_FISH_share_K

    ## Get USDA Ag capital and labor data ready ----
    # aggregate into GCAM regions; Note that USUA included 176 regions
    # aglu.MODEL_MEAN_PERIOD_LENGTH (5-year) average

    Ag_labor_USDA %>%
      gather_years() %>%
      filter(!is.na(value)) %>%  # NAs are the region non exist
      mutate(ISO3 = tolower(ISO3)) %>%
      rename(iso = ISO3) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(labor_ppl = 1000 * sum(value)) %>% # units: 1000 ppl to ppl
      left_join_error_no_match(FOR_FISH_share_L, by = "GCAM_region_ID") %>%
      mutate(labor_ppl = labor_ppl * AG) %>%
      select(-FISH, -FOR, -AG) %>%
      ungroup() ->
      AgLabor_R

    Ag_capital_USDA %>% # million $ (2015 constant price)
      gather_years() %>%
      filter(!is.na(value)) %>%  # NAs are the region non exist
      mutate(iso = tolower(ISO3)) %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      # convert to real prices in GCAM in 1975$ million
      mutate(value = value * gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, aglu.DEFLATOR_BASE_YEAR)) %>%
      group_by(GCAM_region_ID, year) %>%
      # convert to USD
      summarize(capital_1975USD = sum(value) * 10^6) %>%
      left_join_error_no_match(FOR_FISH_share_K, by = "GCAM_region_ID") %>%
      mutate(capital_1975USD = capital_1975USD * AG) %>%
      select(-FISH, -FOR, -AG) %>%
      ungroup() ->
      AgCapital_R

    AgLabor_R %>%
      left_join_error_no_match(AgCapital_R, by = c("GCAM_region_ID", "year")) %>%
      tidyr::gather(factor, value, labor_ppl, capital_1975USD) %>%
      # Adding 5-year moving average here
      group_by(GCAM_region_ID, factor, year) %>%
      mutate(value = if_else(is.na(Moving_average(value, periods = aglu.MODEL_MEAN_PERIOD_LENGTH)),
                             value, Moving_average(value, periods = aglu.MODEL_MEAN_PERIOD_LENGTH))) %>%
      ungroup() %>%
      filter(year %in% aglu.AGLU_HISTORICAL_YEARS) ->
      AgLaborCapital_R

    # Step 2. Adjust costs based on labor and capital cost shares ----
    # That is, split nonLandVariableCost into labor, capital costs and the remaining nonLandVariableCost
    ## Using predefined cost shares in L100.GTAP_AgLaborCapitalCostShare to derive costs
    # adding a minimum value of labor and capital costs of 0.001 (crops), 0.003 (livestock) or 2 (forest) 1975$ per unit of output And adjust nonLandVariableCost

    # Step 2.1. Do the adjustments for crops  ----

    L2062.AgCost_ag_irr_mgmt_adj %>%
      left_join_error_no_match(
        Agcostshare_R_GTAP %>% rename(AgSupplySector = GCAM_commodity), by = c("region", "AgSupplySector", "year")) %>%
      mutate(Labor_UC = pmax(0.003, nonLandVariableCost * Labor_Ag), # Adding a min value of 0.003 for UnitCost of Labor (1975$/kg)
             # if nonLandVariableCost was zero, e.g., no production, change IO to zero as well
             Labor_UC = if_else(nonLandVariableCost == 0, 0, Labor_UC),
             Capital_UC = pmax(0.003, nonLandVariableCost * Capital_Ag), # Adding a min value of 0.003
             Capital_UC = if_else(nonLandVariableCost == 0, 0, Capital_UC),
             nonLandVariableCost = nonLandVariableCost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag) ->
      L2082.laborcapital_ag_irr_mgmt

    # update the NLC (h) -----
    L2082.laborcapital_ag_irr_mgmt %>%
      select(names(L2062.AgCost_ag_irr_mgmt_adj)) ->
      L2082.AgCost_ag_irr_mgmt_adj

    # Join production data to calculate factor expenditures for base years
    L2082.laborcapital_ag_irr_mgmt %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L181.ag_Prod_Mt_R_C_Y_GLU_irr_level %>%
          filter(year %in% MODEL_BASE_YEARS) %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          left_join_error_no_match(basin_to_country_mapping[ c("GLU_code", "GLU_name")],
                                   by = c("GLU" = "GLU_code")) %>%
          transmute(region, year, prod_Mt = value, Irr_Rfd, level,
                    AgProductionTechnology = paste(GCAM_subsector, GLU_name,toupper(Irr_Rfd), level, sep = "_")) ,
        by = c("region", "AgProductionTechnology", "year") ) %>%
      mutate(Exp_Labor_Bil1975USD = Labor_UC * prod_Mt,
             Exp_Capital_Bil1975USD = Capital_UC * prod_Mt) ->
      L2082.laborcapital_ag_irr_mgmt_expenditure

    # Step 2.2. Do the adjustments for bio crops  ----
    L2062.AgCost_bio_irr_mgmt_adj %>%
      left_join_error_no_match(
        Agcostshare_R_GTAP %>% rename(AgSupplySector = GCAM_commodity), by = c("region", "AgSupplySector", "year")) %>%
      mutate(Labor_UC = pmax(0.003, nonLandVariableCost * Labor_Ag),              # Adding a min value of 0.003
             Labor_UC = if_else(nonLandVariableCost == 0, 0, Labor_UC),
             Capital_UC =  nonLandVariableCost * Capital_Ag,
             Capital_UC = pmax(0.003, Capital_UC),
             Capital_UC = if_else(nonLandVariableCost == 0, 0, Capital_UC),
             nonLandVariableCost = nonLandVariableCost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag) ->
      L2082.laborcapital_bio_irr_mgmt

    ## update the NLC (h) -----
    L2082.laborcapital_bio_irr_mgmt %>%
      select(names(L2062.AgCost_bio_irr_mgmt_adj)) ->
      L2082.AgCost_bio_irr_mgmt_adj

    # Step 2.3. Do the adjustments for livestock sectors  ----
    # fish is not included for now!

    L202.StubTechCost_an %>%
      left_join_error_no_match(
        Agcostshare_R_GTAP %>% rename(supplysector = GCAM_commodity), by = c("region", "supplysector", "year")) %>%
      mutate(Labor_UC = pmax(0.01, input.cost * Labor_Ag), # Adding a min value for a unit.cost of Labor to avoid non-positive unit cost of Labor
             Labor_UC = if_else(input.cost == 0, 0, Labor_UC), # if input.cost was zero, e.g., no production, change IO to zero as well
             Capital_UC = pmax(0.01, input.cost * Capital_Ag),
             Capital_UC = if_else(input.cost == 0, 0, Capital_UC),
             input.cost = input.cost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag)->
      L2082.laborcapital_an # no tfp

    ## update the NLC (h) -----
    L2082.laborcapital_an %>%
      select(names(L202.StubTechCost_an)) ->
      L2082.StubTechCost_an_adj

    # Join production data to calculate factor expenditures for base years
    # base years data are the same across w/ and w/o tfp
    L2082.laborcapital_an %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L107.an_Prod_Mt_R_C_Sys_Fd_Y %>%
          filter(year %in% MODEL_BASE_YEARS) %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          transmute(region, year, prod_Mt = value, supplysector = GCAM_commodity,
                    subsector = system, stub.technology = feed),
        by = c("region", "supplysector", "subsector", "stub.technology", "year") ) %>%
      mutate(Exp_Labor_Bil1975USD = Labor_UC * prod_Mt,
             Exp_Capital_Bil1975USD = Capital_UC * prod_Mt) ->
      L2082.laborcapital_an_expenditure

    # Step 3. Calculate market prices for labor (wage) and capital (rate of return)  ----
    # USDA numbers are for livestock and crops; connect them to values to calculate prices
    #TODO: might include regional specific annual working hour in the future, here 2021 OECD average working hour a year is used (1716 h)

    # calculate total expenditures for labor and capital
    L2082.laborcapital_ag_irr_mgmt_expenditure %>%
      select(region, year, Exp_Labor_Bil1975USD, Exp_Capital_Bil1975USD) %>%
      bind_rows(L2082.laborcapital_an_expenditure %>%
                  select(region, year, Exp_Labor_Bil1975USD, Exp_Capital_Bil1975USD) ) %>%
      group_by(region, year) %>%
      summarize_at(vars(Exp_Labor_Bil1975USD, Exp_Capital_Bil1975USD), sum) %>%
      ungroup() %>%
      # Join USDA quantity
      left_join_error_no_match(
        AgLaborCapital_R %>% filter(year %in% MODEL_BASE_YEARS) %>%
          left_join_error_no_match(GCAM_region_names, by = "GCAM_region_ID") %>%
          spread(factor, value),
        by = c("region", "year") ) %>%
      mutate(PriceLabor_K1975USDPerppl = Exp_Labor_Bil1975USD / labor_ppl * 10^6 ,
             PriceCapital_USDPerUSD = Exp_Capital_Bil1975USD / capital_1975USD * 10^9) %>%
      select(region, year, PriceLabor_K1975USDPerppl, PriceCapital_USDPerUSD)->
      L2082.laborcapital_price

    L2082.laborcapital_price %>%
      select(region, year, price.L = PriceLabor_K1975USDPerppl) ->
      L2082.region_laborprice #  unit: 1975 K$/ppl

    L2082.laborcapital_price %>%
      select(region, year, price.K = PriceCapital_USDPerUSD) ->
      L2082.region_capitalprice # unit: 1975$/1975$

    # Step 4. Process IO coefficients (unit cost / input prices) ----

    # Step 4.1 Crops: differentiate IO coefficients across techs  ----
    # the differentiation will be with in hi-lo per IRR/RFD
    # Introduce a new parameter per per IRR/RFD to differentiate capital unit cost in hi-tech
    # delta = theta * hi-tech Capital unit cost
    # Capital unit cost in hi-tech = Capital unit cost in hi-tech + delta ; higher capital IO, capital intensive
    # Labor unit in cost in hi-tech  = Labor unit cost in hi-tech - delta ; lower labor IO
    # updated hi-tech: higher capital IO and lower labor IO compared to previous hi-tech factor IOs

    theta_IRR = 0
    theta_RFD = 0

    L2082.laborcapital_ag_irr_mgmt_expenditure %>%
      select(-nonLandVariableCost, -Exp_Capital_Bil1975USD, -Exp_Labor_Bil1975USD) %>%
      left_join_error_no_match(
        L2082.laborcapital_price, by = c("region", "year"))%>%
      #filter(region == "USA", AgSupplySubsector == "Fruits_FraserR", year == 2015) %>%
      mutate(theta = if_else(Irr_Rfd == "irr", theta_IRR, theta_RFD)) %>%
      group_by(region, year, AgSupplySector, AgSupplySubsector, Irr_Rfd) %>%
      mutate(ProdRatio = if_else(prod_Mt == 0, 1, prod_Mt / prod_Mt [level == "hi"]), # zero prods using ProdRatio = 1
             delta = theta * Capital_UC[level == "hi"],
             delta = if_else(level == "hi", delta, -delta/ProdRatio) ) %>%
      mutate(Capital_UC = Capital_UC + delta,
             Labor_UC = Labor_UC - delta) %>%
      ungroup() %>%
      # IO units: Mppl per Mt for labor; B$ per Mt ($/kg) for capital
      mutate(Laber_IO = Labor_UC/PriceLabor_K1975USDPerppl,
             Capital_IO = Capital_UC/PriceCapital_USDPerUSD)  %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Labor_Ag = Laber_IO, Capital_Ag = Capital_IO) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt_hist

    # bind future years
    L2082.AgCoef_laborcapital_ag_irr_mgmt_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.AgCoef_laborcapital_ag_irr_mgmt_hist) ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt

    # Step 4.2 biomass crops IO coefficients ----
    # using the same approach used above for IO differentiation across hi & lo

    L2082.laborcapital_bio_irr_mgmt %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      select(-nonLandVariableCost) %>%
      left_join_error_no_match(
        L2082.laborcapital_price, by = c("region", "year")) %>%
      mutate(Irr_Rfd = if_else(grepl("IRR", AgProductionTechnology), "irr", "rfd"),
             level = if_else(grepl("hi", AgProductionTechnology), "hi", "lo")) %>%
      mutate(theta = if_else(Irr_Rfd == "irr", theta_IRR, theta_RFD)) %>%
      group_by(region, year, AgSupplySector, AgSupplySubsector, Irr_Rfd) %>%
      # assuming prod ratio between to be 1
      mutate(ProdRatio = 1,
             delta = theta * Capital_UC[level == "hi"],
             delta = if_else(level == "hi", delta, -delta/ProdRatio) ) %>%
      mutate(Capital_UC = Capital_UC + delta,
             Labor_UC = Labor_UC - delta) %>%
      ungroup() %>%
      # IO units: Mppl per Mt for labor; $ / $ for capital
      mutate(Laber_IO = Labor_UC/PriceLabor_K1975USDPerppl,
             Capital_IO = Capital_UC/PriceCapital_USDPerUSD)  %>%
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Labor_Ag = Laber_IO, Capital_Ag = Capital_IO) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt_hist

    # bind future years
    L2082.AgCoef_laborcapital_bio_irr_mgmt_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.AgCoef_laborcapital_bio_irr_mgmt_hist) ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt


    # Step 4.3 animal IO coefficients ----

    # Note that we could have further differentiate the IO coef. across an techs
    # using the theta method applied above for crops
    # but IOs are already different between mixed vs. Pastoral (higher productivity in mixed)

    L2082.laborcapital_an %>%
      select(-input.cost, -minicam.non.energy.input) %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L2082.laborcapital_price, by = c("region", "year")) %>%
      # compute IO coefficients
      # IO units: Mppl per Mt for labor; $ / kg for capital
      transmute(region, year, supplysector, subsector, stub.technology,
                Labor_Ag = Labor_UC/ PriceLabor_K1975USDPerppl,
                Capital_Ag =  Capital_UC / PriceCapital_USDPerUSD) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) %>%
      mutate(market.name = region) ->
      L2082.StubTechCoef_laborcapital_an_hist

    # bind future years w/o tfp
    L2082.StubTechCoef_laborcapital_an_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.StubTechCoef_laborcapital_an_hist) ->
      L2082.StubTechCoef_laborcapital_an

    # Step 4.4 forest IO coefficients ----

    # obtain forest labor --
    Ag_labor_USDA %>%
      gather_years() %>%
      filter(!is.na(value)) %>%  # NAs are the region non exist
      mutate(ISO3 = tolower(ISO3)) %>%
      rename(iso = ISO3) %>%
      left_join_error_no_match(iso_GCAM_regID, by = "iso") %>%
      group_by(GCAM_region_ID, year) %>%
      summarize(labor_ppl = 1000 * sum(value)) %>% # units: 1000 ppl to ppl
      left_join_error_no_match(FOR_FISH_share_L, by = "GCAM_region_ID") %>%
      mutate(labor_ppl = labor_ppl * FOR) %>%
      select(-FISH, -FOR, -AG) %>%
      ungroup() ->
      AgLabor_FOR

    Ag_capital_USDA %>% # million $ (2015 constant price)
      gather_years() %>%
      filter(!is.na(value)) %>%  # NAs are the region non exist
      mutate(iso = tolower(ISO3)) %>%
      left_join_error_no_match(iso_GCAM_regID %>% select(iso, GCAM_region_ID), by = "iso") %>%
      # convert to real prices in GCAM in 1975$ million
      mutate(value = value * gdp_deflator(gcam.REAL_PRICE_BASE_YEAR, aglu.DEFLATOR_BASE_YEAR)) %>%
      group_by(GCAM_region_ID, year) %>%
      # convert to USD
      summarize(capital_1975USD = sum(value) * 10^6) %>%
      left_join_error_no_match(FOR_FISH_share_K, by = "GCAM_region_ID") %>%
      mutate(capital_1975USD = capital_1975USD * FOR) %>%
      select(-FISH, -FOR, -AG) %>%
      ungroup() ->
      AgCapital_FOR # 1975 $

    L2012.AgProduction_For %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      group_by(region, year) %>%
      mutate(RegFor = sum(calOutputValue)) %>%
      left_join_error_no_match(AgLabor_FOR %>% filter(year %in% MODEL_BASE_YEARS),
                               by = c("region", "year")) %>%
      left_join_error_no_match(AgCapital_FOR %>% filter(year %in% MODEL_BASE_YEARS) %>% select(-GCAM_region_ID),
                               by = c("region", "year")) %>%
      mutate(Laber_IO = ifelse(calOutputValue == 0, 0, labor_ppl / 10^6 / RegFor) , # labor IO: # mpl / bm3
             Capital_IO = ifelse(calOutputValue == 0, 0, capital_1975USD / 10^9 / RegFor)) %>%   # capital IO: # bil 1975$ / bm3
      select(region, AgSupplySector, AgSupplySubsector, AgProductionTechnology, year,
             Labor_Ag = Laber_IO, Capital_Ag = Capital_IO) %>%
      tidyr::gather(minicam.energy.input, coefficient, Labor_Ag, Capital_Ag) %>%
      ungroup() ->
      L2082.AgCoef_laborcapital_for_hist

    # Step 4.4.1 Do the NLC adjustments for Forest  ----

    L2082.AgCoef_laborcapital_for_hist %>%
      spread(minicam.energy.input, coefficient) %>%
      left_join_error_no_match(L2082.region_laborprice, by = c("region", "year")) %>%
      left_join_error_no_match(L2082.region_capitalprice, by = c("region", "year")) %>%
      mutate(Labor_UC = price.L * Labor_Ag,
             Capital_UC = price.K * Capital_Ag,
             Labor_UC = pmax(2, Labor_UC), # Adding a min value of 2
             Capital_UC = pmax(2, Capital_UC)) -> # Adding a min value of 2)
      L2082.laborcapital_for_hist

    L2082.laborcapital_for_hist %>%
      filter(year == MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      select(names(L2082.laborcapital_for_hist)) %>%
      bind_rows(L2082.laborcapital_for_hist) %>%
      left_join_error_no_match(L2052.AgCost_For,
                               by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology","year")) %>%
      mutate(Labor_UC = if_else(nonLandVariableCost == 0, 0, Labor_UC),  # if nonLandVariableCost was zero, e.g., no production, change IO to zero as well
             Capital_UC = if_else(nonLandVariableCost == 0, 0, Capital_UC),
             nonLandVariableCost = nonLandVariableCost - Labor_UC - Capital_UC) %>%
      select(-Capital_Ag, -Labor_Ag) ->
      L2082.laborcapital_For # 1975$/m3

    # bind future years
    L2082.AgCoef_laborcapital_for_hist %>%
      filter(year %in% MODEL_FINAL_BASE_YEAR) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_FUTURE_YEARS)) %>%
      bind_rows(L2082.AgCoef_laborcapital_for_hist) ->
      L2082.AgCoef_laborcapital_for

    ## update the NLC (h) -----

    L2082.laborcapital_For %>%
      select(names(L2052.AgCost_For)) ->
      L2082.AgCost_For_adj

    # report factor expenditure for forest sector
    L2082.laborcapital_For %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(
        L2012.AgProduction_For %>%
          filter(year %in% MODEL_BASE_YEARS),
        by = c("region", "AgSupplySector", "AgSupplySubsector", "AgProductionTechnology", "year")) %>%
      mutate(Exp_Labor_Bil1975USD = Labor_UC * calOutputValue, # $/m3 * bm3 = bil1975USD
             Exp_Capital_Bil1975USD = Capital_UC * calOutputValue) ->
      L2082.laborcapital_for_expenditure

    # Step 5 include tfp --------

    # use GCAM-macro calibated MA tfp as the ag labor growth ----
    gcam_macro_TFP_open %>%
      filter(scenario == "gSSP2") %>%
      filter(year >= 2020) %>%
      filter(region == "South America_Southern") %>%
      mutate(region = "South America_Northern") %>%
      bind_rows(gcam_macro_TFP_open %>%
                  filter(scenario == "gSSP2") %>%
                  filter(year >= 2020) %>%
                  filter(region != "South America_Northern")) ->
      M_tfp

    # adjust tfp value: if calibrated tfp < 1, then change it to 1
    M_tfp %>%
      bind_rows(M_tfp %>%
                  filter(year == 2020) %>%
                  mutate(productivity = 1) %>%
                  select(-year) %>%
                  repeat_add_columns(tibble(year = MODEL_BASE_YEARS))) %>%
      mutate(mult = ifelse(productivity < 1, 1, productivity),
             LaborIOProdMult = 1 / mult) ->
      LaborIOProdMult_MA

    LaborIOProdMult <- LaborIOProdMult_MA

    L2082.AgCoef_laborcapital_ag_irr_mgmt %>%
      left_join_error_no_match(LaborIOProdMult, by = c("region", "year")) %>%
      mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*LaborIOProdMult, coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.AgCoef_laborcapital_ag_irr_mgmt)) ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA

    L2082.StubTechCoef_laborcapital_an %>%
      left_join_error_no_match(LaborIOProdMult, by = c("region", "year")) %>%
      mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*LaborIOProdMult, coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.StubTechCoef_laborcapital_an)) ->
      L2082.StubTechCoef_laborcapital_an_tfp_MA

    L2082.AgCoef_laborcapital_bio_irr_mgmt %>%
      left_join_error_no_match(LaborIOProdMult, by = c("region", "year")) %>%
      mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*LaborIOProdMult, coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.AgCoef_laborcapital_bio_irr_mgmt)) ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA

    L2082.AgCoef_laborcapital_for %>%
      left_join_error_no_match(LaborIOProdMult, by = c("region", "year")) %>%
      mutate(coefficient = ifelse(minicam.energy.input == "Labor_Ag", coefficient*LaborIOProdMult, coefficient)) %>% # apply tfp to labor only: coefficient includes both labor and capital IO
      ungroup() %>%
      select(colnames(L2082.AgCoef_laborcapital_for)) ->
      L2082.AgCoef_laborcapital_for_tfp_MA

    # Produce outputs ----

    L2082.laborcapital_ag_irr_mgmt_expenditure %>%
      add_title("labor and capital expenditure of crop production") %>%
      add_units("billion 1975 USD") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.laborcapital_ag_irr_mgmt_expenditure") %>%
      add_precursors("L2082.laborcapital_ag_irr_mgmt",
                     "L181.ag_Prod_Mt_R_C_Y_GLU_irr_level",
                     "Agcostshare_R_GTAP",
                     "L2062.AgCost_ag_irr_mgmt_adj") ->
      L2082.laborcapital_ag_irr_mgmt_expenditure

    L2082.laborcapital_an_expenditure %>%
      add_title("labor and capital expenditure of livestock production") %>%
      add_units("billion 1975 USD") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.laborcapital_ag_irr_mgmt_expenditure") %>%
      add_precursors("L2082.laborcapital_an",
                     "L107.an_Prod_Mt_R_C_Sys_Fd_Y",
                     "Agcostshare_R_GTAP",
                     "L202.StubTechCost_an") ->
      L2082.laborcapital_an_expenditure

    L2082.laborcapital_for_expenditure %>%
      add_title("labor and capital expenditure for forest") %>%
      add_units("billion 1975 USD") %>%
      add_comments("Note: expenditure divided by factor price gives factor input level") %>%
      add_legacy_name("L2082.laborcapital_ag_irr_mgmt_expenditure") %>%
      add_precursors("L2012.AgProduction_For",
                     "Agcostshare_R_GTAP",
                     "L2052.AgCost_For") ->
      L2082.laborcapital_for_expenditure

    L2082.AgCoef_laborcapital_ag_irr_mgmt %>%
      add_title("labor and capital coefficients for agricultural technologies no tfp") %>%
      add_units("index") %>%
      add_comments("Note: we are using theta to differentiate coefficient across four management technologies (irrigated, rainfed, hi and lo") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_ag_irr_mgmt") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2062.AgCost_ag_irr_mgmt_adj") ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt

    L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA %>%
      add_title("labor and capital coefficients for agricultural technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("Note: we are using theta to differentiate coefficient across four management technologies (irrigated, rainfed, hi and lo") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2062.AgCost_ag_irr_mgmt_adj",
                     "L2052.AgProdChange_ag_irr_ref") ->
      L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA

    L2082.AgCoef_laborcapital_bio_irr_mgmt %>%
      add_title("labor and capital coefficients for bioenergy technologies no tfp") %>%
      add_units("index") %>%
      add_comments("Compute bioenergy labor and capital coefficients") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_bio_irr_mgmt") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2062.AgCost_bio_irr_mgmt_adj") ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt

    L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA %>%
      add_title("labor and capital coefficients for bioenergy technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("Compute bioenergy labor and capital coefficients") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2062.AgCost_bio_irr_mgmt_adj") ->
      L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA

    L2082.AgCoef_laborcapital_for_tfp_MA %>%
      add_title("labor and capital coefficients for forest technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("Compute bioenergy labor and capital coefficients") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_for_tfp_MA") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2062.AgCost_for_adj") ->
      L2082.AgCoef_laborcapital_for_tfp_MA

    L2082.AgCost_ag_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per kg") %>%
      add_comments("Subtract cost of labor and capital from non-land variable cost.") %>%
      add_legacy_name("L2082.AgCost_ag_irr_mgmt_adj") %>%
      same_precursors_as(L2082.AgCoef_laborcapital_ag_irr_mgmt)  ->
      L2082.AgCost_ag_irr_mgmt_adj

    L2082.AgCost_bio_irr_mgmt_adj %>%
      add_title("Adjusted non-land variable cost for agricultural technologies") %>%
      add_units("1975$ per GJ") %>%
      add_comments("Subtract cost of labor and capital from non-land variable cost.") %>%
      add_legacy_name("L2082.AgCost_bio_irr_mgmt_adj") %>%
      same_precursors_as(L2082.AgCoef_laborcapital_bio_irr_mgmt) ->
      L2082.AgCost_bio_irr_mgmt_adj

    L2082.AgCoef_laborcapital_for %>%
      add_title("labor and capital coefficients for forest technologies") %>%
      add_units("index") %>%
      add_comments("Compute forest labor and capital coefficients") %>%
      add_legacy_name("L2082.AgCoef_laborcapital_for") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2052.AgCost_For") ->
      L2082.AgCoef_laborcapital_for

    L2082.AgCost_For_adj %>%
      add_title("Adjusted non-land variable costs of forest prodction by region / GLU") %>%
      add_units("1975$ per kg") %>%
      add_comments("Technologies are not specified for forest") %>%
      add_legacy_name("L2082.AgCost_For_adj") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L2052.AgCost_For") ->
      L2082.AgCost_For_adj

    L2082.StubTechCost_an_adj %>%
      add_title("Costs of animal production technologies") %>%
      add_units("1975$/kg") %>%
      add_comments("Adjusted animal non feed cost") %>%
      add_comments("This is the non-feed cost after labor and capital; i.e., all costs of producing animal commodities except for the feed.") %>%
      add_legacy_name("L2082.StubTechCost_an_adj") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare",
                     "L202.StubTechCost_an") ->
      L2082.StubTechCost_an_adj

    L2082.StubTechCoef_laborcapital_an %>%
      add_title("labor and capital coefficients for livestock technologies no tfp") %>%
      add_units("index") %>%
      add_comments("labor and capital coefficients for livestock sectors except fish") %>%
      add_legacy_name("L2082.StubTechCoef_laborcapital_an") %>%
      add_precursors("L202.StubTechCost_an") ->
      L2082.StubTechCoef_laborcapital_an

    L2082.StubTechCoef_laborcapital_an_tfp_MA %>%
      add_title("labor and capital coefficients for livestock technologies with MA tfp") %>%
      add_units("index") %>%
      add_comments("labor and capital coefficients for livestock sectors except fish") %>%
      add_legacy_name("L2082.StubTechCoef_laborcapital_tfp_MA") %>%
      add_precursors("L202.StubTechCost_an") ->
      L2082.StubTechCoef_laborcapital_an_tfp_MA

    L2082.region_laborprice %>%
      add_title("Regional agricultural labor market price") %>%
      add_units("1975K$/ppl") %>%
      add_legacy_name("L2082.region_laborprice") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare") ->
      L2082.region_laborprice

    L2082.region_capitalprice %>%
      add_title("Regional agricultural capital market price") %>%
      add_units("1975 $/$") %>%
      add_legacy_name("L2082.region_capitalprice") %>%
      add_precursors("L100.GTAP_AgLaborCapitalCostShare") ->
      L2082.region_capitalprice

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
