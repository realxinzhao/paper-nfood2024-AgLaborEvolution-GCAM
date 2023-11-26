# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_aglu_ag_Fert_IRR_MGMT_xml
#'
#' Construct XML data structure for \code{ag_Fert_IRR_MGMT.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{ag_Fert_IRR_MGMT.xml}. The corresponding file in the
#' original data system was \code{batch_ag_Fert_IRR_MGMT.xml} (aglu XML).
module_aglu_ag_input_IRR_MGMT_xml <- function(command, ...) {

  MODULE_INPUTS <-
    c("L2082.AgCoef_laborcapital_ag_irr_mgmt",
      "L2082.AgCoef_laborcapital_bio_irr_mgmt",
      "L2082.AgCoef_laborcapital_for",
      "L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
      "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
      "L2082.AgCoef_laborcapital_for_tfp_MA",
      "L2062.AgCoef_Fert_ag_irr_mgmt",
      "L2062.AgCoef_Fert_bio_irr_mgmt")

  MODULE_OUTPUTS <-
    c(XML = "ag_input_IRR_MGMT.xml",
      XML = "ag_input_IRR_MGMT_tfp_MA.xml")

  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(MODULE_OUTPUTS)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs ----
    get_data_list(all_data, MODULE_INPUTS, strip_attributes = TRUE)

    # ===================================================

    # Produce outputs
    create_xml("ag_input_IRR_MGMT.xml") %>%
      add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2082.AgCoef_laborcapital_for, "AgCoef") %>%
      add_precursors("L2082.AgCoef_laborcapital_ag_irr_mgmt",
                     "L2082.AgCoef_laborcapital_bio_irr_mgmt",
                     "L2082.AgCoef_laborcapital_for",
                     "L2062.AgCoef_Fert_ag_irr_mgmt",
                     "L2062.AgCoef_Fert_bio_irr_mgmt") ->
      ag_input_IRR_MGMT.xml

    create_xml("ag_input_IRR_MGMT_tfp_MA.xml") %>%
      add_xml_data(L2062.AgCoef_Fert_ag_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA, "AgCoef") %>%
      add_xml_data(L2062.AgCoef_Fert_bio_irr_mgmt, "AgCoef") %>%
      add_xml_data(L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA, "AgCoef") %>%
      add_xml_data(L2082.AgCoef_laborcapital_for_tfp_MA, "AgCoef") %>%
      add_precursors("L2082.AgCoef_laborcapital_ag_irr_mgmt_tfp_MA",
                     "L2082.AgCoef_laborcapital_bio_irr_mgmt_tfp_MA",
                     "L2082.AgCoef_laborcapital_for_tfp_MA",
                     "L2062.AgCoef_Fert_ag_irr_mgmt",
                     "L2062.AgCoef_Fert_bio_irr_mgmt") ->
      ag_input_IRR_MGMT_tfp_MA.xml

    return_data(MODULE_OUTPUTS)
  } else {
    stop("Unknown command")
  }
}
