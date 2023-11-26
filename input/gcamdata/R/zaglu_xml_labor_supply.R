# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_labor_supply_xml
#'
#' Construct XML data structure for \code{ag_labor_supply.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_labor_supply.xml}. The corresponding file in the
#' original data system was \code{batch_ag_labor_supply_xml.R} (aglu XML).
module_aglu_labor_supply_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c(FILE = "common/iso_GCAM_regID",
      FILE = "aglu/AGLU_ctry",
      FILE = "common/GCAM_region_names",
      FILE = "water/basin_to_country_mapping",
      "L2082.laborcapital_ag_irr_mgmt_expenditure",
      "L2082.laborcapital_an_expenditure",
      "L2082.laborcapital_for_expenditure",
      "L2082.region_laborprice",
      "L2082.region_capitalprice",
      "L201.Pop_gSSP2",
      "L100.Pop_thous_SSP_ctry_Yfut") # TODO: include other SSP population in the future

  MODULE_OUTPUTS <-
    c(XML = "ag_labor_supply.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)


    # build labor supply

    # process for ag ----------------------------------------------------------------------------------------
    AGLU_ctry %>%
      mutate(iso = ifelse(iso == "rom", "rou", iso)) ->
      AGLU_ctry

    # use factor expenditure and factor price to obtain regional factor demand for MODEL_BASE_YEARS
    # read in labor expenditure of ag (crop), an (livestock) and forest (for)
    LA_ag <- L2082.laborcapital_ag_irr_mgmt_expenditure %>%
      group_by(region, year) %>%
      summarize(Exp_Labor_Bil1975USD = sum(Exp_Labor_Bil1975USD)) %>%
      left_join_error_no_match(L2082.region_laborprice, by = c("region", "year")) %>%  # price.L: K$/ppl
      mutate(labor_ag = Exp_Labor_Bil1975USD / price.L) # mpl

    LA_an <- L2082.laborcapital_an_expenditure %>%
      group_by(region, year) %>%
      summarize(Exp_Labor_Bil1975USD = sum(Exp_Labor_Bil1975USD)) %>%
      left_join_error_no_match(L2082.region_laborprice, by = c("region", "year")) %>%  # price.L: K$/ppl
      mutate(labor_an = Exp_Labor_Bil1975USD / price.L) # mpl

    LA_for <- L2082.laborcapital_for_expenditure %>%
      group_by(region, year) %>%
      summarize(Exp_Labor_Bil1975USD = sum(Exp_Labor_Bil1975USD)) %>%
      left_join_error_no_match(L2082.region_laborprice, by = c("region", "year")) %>%  # price.L: K$/ppl
      mutate(labor_for = Exp_Labor_Bil1975USD / price.L)

    LA <- LA_ag %>%
      select(region, year, labor_ag) %>%
      left_join_error_no_match(LA_an %>%
                                 select(region, year, labor_an),
                               by = c("region", "year")) %>%
      left_join_error_no_match(LA_for %>%
                                 select(region, year, labor_for),
                               by = c("region", "year")) %>%
      mutate(labor = labor_ag + labor_an + labor_for) # ppl -> mpl

    # L201.Pop_gSSP2 %>% # population
    #   left_join_error_no_match(RuralShare, by = c("region", "year")) %>%
    #   mutate(LF = rural / 100 * totalPop / 1000) -> # rural share, kpl -> mpl
    #   LF

    # obtain rural population ----

    # read in historical rural population from WorldBank: https://data.worldbank.org/indicator/SP.RUR.TOTL?locations=CN
    WB_rural <- read.csv("C:/Model/heatstress/API_SP.RUR.TOTL_DS2_en_csv_v2_5455093.csv", fileEncoding = 'UTF-8-BOM', skip = 4, header = T) %>%
      gather_time() %>%
      mutate(iso = tolower(Country.Code)) %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      select(year, region, GCAM_region_ID, value) %>%
      na.omit() %>%
      filter(year %in% MODEL_YEARS) %>%
      group_by(region, year) %>%
      summarize(rural = sum(value)) # ppl

    # extract urban population share across all SSP
    UrbanShare <- read.csv("C:/Model/heatstress/SspDb_country_data_2013-06-12.csv", fileEncoding = 'UTF-8-BOM', header = T) %>%
      filter(VARIABLE == "Population|Urban|Share") %>%
      gather_time() %>%
      filter(year %in% MODEL_FUTURE_YEARS)

    # No data in Taiwan, use Korea's urban share
    UrbanShare_all <- UrbanShare %>%
      filter(REGION == "KOR") %>%
      mutate(REGION = "TWN") %>% bind_rows(UrbanShare)

    # calculate future rural population with SSP rural share and population
    RuralPop_SSP <- UrbanShare_all %>%
      mutate(scenario = substr(SCENARIO, 1, 4),
             iso = tolower(REGION),
             iso = ifelse(iso == "rou", "rom", iso),
             value = approx_fun(year, value, rule = 2)) %>% # linear interpolation for missing share
      rename(share = value) %>%
      left_join(L100.Pop_thous_SSP_ctry_Yfut %>%
                  rename(Pop_thous = value),
                by = c("scenario", "iso", "year")) %>%
      mutate(Rural_pop = (100 - share) / 100 * Pop_thous * 1000 ) %>%
      left_join(iso_GCAM_regID, by = "iso") %>%
      left_join(GCAM_region_names, by = "GCAM_region_ID") %>%
      group_by(scenario, region, year) %>%
      summarize(Rural_pop = sum(Rural_pop)) # ppl

    # TODO: add all SSPs later
    # extract future rural population for the SSP of interest
    SSP <- "SSP2"
    RuralPop_SSP %>%
      filter(scenario == SSP) %>%
      ungroup() %>%
      select(-scenario) -> # WB and SSP (derived) both have 2020 data, choose the larger one to use
      RuralPop_futr

    # use 2020 data to fill the the MODEL BASE YEARS for Taiwan

    RuralPop_futr %>%
      filter(region == "Taiwan",
             year == 2020) %>%
      select(-year) %>%
      repeat_add_columns(tibble(year = MODEL_BASE_YEARS)) %>%
      select(region, year, Rural_pop) ->
      RuralPop_TW_hist

    # WB 2020 and SSP 2020 does not match, use the scaler to adjust SSP derived rural population
    WB2020 <- WB_rural %>%
      filter(year == 2020) %>%
      rename(WB = rural)

    SSP2020 <- RuralPop_SSP %>%
      filter(year == 2020,
             scenario == "SSP2") %>%
      rename(SSP = Rural_pop)

    scaler <- SSP2020 %>%
      left_join(WB2020, by = c("region", "year")) %>%
      mutate(scaler = WB/SSP,
             scaler = ifelse(region == "Taiwan", 1, scaler)) %>%   # Taiwan miss WB data
      ungroup()

    RuralPop_futr_adj <- RuralPop_futr %>%
      left_join(scaler %>% select(region, scaler), by = c("region")) %>%
      mutate(Rural_pop_adj = Rural_pop * scaler) # update rural pop from 2020 - 2100

    # bind historical and future rural population
    WB_rural %>% # historical
      rename(Rural_pop = rural) %>%
      filter(year < 2020) %>% # use SSP-adj-ed 2020 rural pop
      bind_rows(RuralPop_TW_hist) %>%
      bind_rows(RuralPop_futr_adj %>% select(region, year, Rural_pop = Rural_pop_adj)) ->
      RuralPop # ppl

    # link ag labor force with rural population ----
    LF <- RuralPop %>%
      mutate(LF = Rural_pop/10^6) # ppl to mpl

    LA %>%
      select(region, year, labor) %>%
      left_join_error_no_match(LF %>%
                                 filter(year %in% MODEL_BASE_YEARS),
                               by = c("region", "year")) %>%
      select(region, year, labor, LF) %>%
      mutate(R = labor/LF) ->  # calculate the ag labor to LF ratio, ranges from 0 to 1
      LR

    # read in historical wage rate for labor supply function calibration

    LP <- L2082.region_laborprice %>% filter(year %in% MODEL_BASE_YEARS) %>%
      left_join_error_no_match(LR, by = c("year","region")) %>%
      rename(wage = price.L)

    # obtain points from the calibrated labor supply function A*(w^r) to build piece-wise function
    # by obtain R value with a set of wage rates

    region_list = unique(LR$region)
    LS_fn_list = list() # labor supply function list, ordered with A, R, gamma, lower and upper wage limit in supply curve

    # need to adjust the maxresource and R value to make sure the calibrated supply function also include all historical years' (wage, R)
    # use 2015 data to calibrate the function parameter A, and then back up the R with the historical wage rate, and then back up the max resource
    # alert: this way, the maxresource might not be identical to POP in some region before 2015 (especially for ag-intensive area)

    # use historical wage to develop the wage range in the supply function

    # BASE: elasticity = 0.7, same across regions ------------------------------
    N_grade = 10
    for (i in 1:length(region_list)){
      dfi = LP %>% filter(region == region_list[i])
      w2015 = as.numeric(dfi[dfi$year == 2015,"wage"])
      R2015 = as.numeric(dfi[dfi$year == 2015,"R"])
      gamma = 0.7
      A = R2015/(w2015^gamma)
      wmin = min(dfi$wage)
      wmax = max(dfi$wage)
      region = region_list[[i]]
      wage_range = list()
      for (j in 1:N_grade){
        grade = j
        available = (j-1)/N_grade
        price = (available/A)^(1/gamma)
        out = c(region, grade, price, available, A, gamma)
        wage_range[[j]] = out
        wage_range[[j+1]] = c(region, j+1, w2015, R2015, A, gamma) # add 2015 data on the supply curve
      }
      supply = do.call(rbind, wage_range)
      LS_fn_list[[i]] = supply
    }
    labor_supply <-  do.call(rbind, LS_fn_list) %>%
      as.data.frame() %>%
      select(region = V1, grade = V2, price = V3, available = V4, A = V5, L_elas = V6) %>%
      mutate(available = as.numeric(available),
             A = as.numeric(A),
             L_elas = as.numeric(L_elas)) %>%
      group_by(region) %>%
      arrange(available) %>% # order the supply by availability
      mutate(grade = row_number(available)) # update grade number based on availability

    # update historical MAX
    LP %>%
      left_join(labor_supply %>%
                  select(region, A, L_elas) %>%
                  unique(),
                by = "region") %>%
      mutate(R_new = A*(wage)^L_elas,
             MAX = labor/R_new) ->
      LP_new

    MAX <- LF %>% # build MAX for XML
      left_join(LP_new %>%
                  select(region, year, MAX), by = c("region", "year")) %>%
      group_by(region) %>%
      mutate(maxSubResource = LF, # mpl
             maxSubResource = ifelse(year <= 2015, MAX, maxSubResource), # mpl
             # maxx = LF[year == 2050], # Max remain the same after 2080 to avoid solution failure
             # maxSubResource = ifelse(year >= 2050, maxx, maxSubResource), # mpl
             sub.renewable.resource = "Labor_Ag",
             renewresource = 'Labor_Ag') %>%
      ungroup() %>%
      select(LEVEL2_DATA_NAMES$GrdRenewRsrcMaxNoFillOut)

    labor_supply %>%
      mutate(renewresource = 'Labor_Ag',
             sub.renewable.resource = "Labor_Ag",
             extractioncost = as.numeric(price),
             grade = paste0('grade',grade)) %>%
      select(LEVEL2_DATA_NAMES$RenewRsrcCurves) %>%
      filter(available <= 1) -> # TODO: remove this to allow migration urban -> rural
      labor_supply_fn

    L2082.region_laborprice %>%
      filter(year %in% MODEL_BASE_YEARS) %>%
      mutate(price = price.L,
             resource = 'Labor_Ag') %>%
      select(LEVEL2_DATA_NAMES$RsrcPrice) ->
      labor_price

    labor_price %>%
      mutate(output.unit = 'mpl',
             price.unit = '1975 K$/ppl',
             market = region) %>%
      select(LEVEL2_DATA_NAMES$Rsrc)->
      labor_resource

    DeleteUnlimitRsrc.labor <- as.data.frame(region_list) %>%
      mutate(unlimited.resource = 'Labor_Ag',
             region = region_list) %>%
      select(LEVEL2_DATA_NAMES$DeleteUnlimitRsrc)

    labor_technology <- labor_resource %>%
      repeat_add_columns(tibble(year = MODEL_YEARS)) %>%
      mutate(subresource = resource,
             technology = resource,
             share.weight = ifelse(year > 2015, 1, 0)) %>%
      select(LEVEL2_DATA_NAMES$ResTechShrwt)

    # Produce outputs --------------------------------------------------------------------------------------------------
    create_xml("ag_labor_supply.xml") %>%
      add_node_equiv_xml("resource") %>%
      add_xml_data(DeleteUnlimitRsrc.labor, "DeleteUnlimitRsrc") %>% # delete unlimited resource
      add_xml_data(labor_resource, "Rsrc") %>%
      add_xml_data(labor_price, "RsrcPrice") %>%
      add_xml_data(labor_supply_fn, "RenewRsrcCurves") %>%
      add_xml_data(MAX, "GrdRenewRsrcMaxNoFillOut") %>%
      add_node_equiv_xml("subresource") %>%
      add_xml_data(labor_technology, "ResTechShrwt") ->
      ag_labor_supply.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
