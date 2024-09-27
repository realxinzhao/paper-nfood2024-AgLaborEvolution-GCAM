# Overview

This Repo includes the GCAM model and related files for replicating GCAM runs performed for the study:  

**Agroeconomic and environmental implications of future labor market evolution**  

Di Sheng, James A. Edmonds, Pralit Patel, Stephanie T. Waldhoff, Brian C. O’Neill, Marshall A. Wise, Xin Zhao* 

Joint Global Change Research Institute, Pacific Northwest National Laboratory  
*Corresponding Author. Email: xin.zhao@pnnl.gov

**The model was built based on [GCAM v7.0](https://jgcri.github.io/gcam-doc/v7.0/toc.html) and includes data and assumption changes, which are documented in the paper. The compressed folder, "Sheng-AgLaborEvolution-Config," contains the configuration and addon XML files (for sensitivity scenarios) necessary for the running the model.**
## Read Me in Sheng-AgLaborEvolution-Config.7z

### GCAM Model Setup Instructions

#### gcam.exe
While users can compile the model from the provided source code (CPP), a pre-compiled `gcam.exe` is available in the `exe/` directory of the zip folder. Users need to unzip this file and place it in the `exe/` directory of the GCAM folder as `exe/gcam.exe`.

#### XML Files
The XML files are model input data files that users need to build by running the `gcamdata` package (using `driver` or `dirver_drake` functions) included in this repository.

##### Addon XMLs for Sensitivity Scenarios
Addon XMLs used in sensitivity scenarios are provided in the `input/xml_sen` directory within the zipped folder. Users need to unzip these files and place them in the `input/xml_sen` directory of the GCAM folder. Note that the R code (need `gcamdata`) for generating the sensitivity XMLs is included in `input/extra/generate_sensitivity_xml.R`.

#### Configuration Files
Configuration files are already included in the `exe/` directory of the repo. But they are also includedin the compressed file.

#### Scenario Mapping (corrected)

- `config_para_static`: Static
- `config_para_evo`: Evolving

- `config_para_ls_SSP5`: High transition
- `config_para_ls_SSP3`: Low transition

- `config_para_gamma_hi`: High Elas.
- `config_para_gamma_lo`: Low Elas.

- `config_para_eta6`: High productivity
- `config_para_eta1`: Low productivity

#### Repo for processing output and visulaization
There is a separate repo [`realxinzhao/paper-nfood2024-AgLaborEvolution-DisplayItems`](https://github.com/realxinzhao/paper-nfood2024-AgLaborEvolution-DisplayItems) for processing GCAM results and generating data and figures used in the paper. 
The output data is archived at https://zenodo.org/records/13852236.


# Global Change Analysis Model (GCAM)

The Joint Global Change Research Institute (JGCRI) is the home and
primary development institution for GCAM, an integrated assessment
tool for exploring consequences and responses to global
change. Climate change is a global issue that impacts all regions of
the world and all sectors of the global economy. Thus, any responses
to the threat of climate change, such as policies or international
agreements to limit greenhouse gas emissions, can have wide ranging
consequences throughout the energy system as well as on land use and
land cover. Integrated assessment models endeavor to represent all
world regions and all sectors of the economy in an economic framework
in order to explore interactions between sectors and understand the
potential ramifications of climate mitigation actions.

GCAM has been developed at PNNL for over 20 years and is now a freely
available community model and documented online (See below). The team
at JGCRI is comprised of economists, engineers, energy experts, forest
ecologists, agricultural scientists, and climate system scientists who
develop the model and apply it to a range of science and policy
questions and work closely with Earth system and ecosystem modelers to
integrate the human decision components of GCAM into their analyses.

## Model Overview

GCAM is a dynamic-recursive model with technology-rich representations
of the economy, energy sector, land use and water linked to a climate
model that can be used to explore climate change mitigation policies
including carbon taxes, carbon trading, regulations and accelerated
deployment of energy technology. Regional population and labor
productivity growth assumptions drive the energy and land-use systems
employing numerous technology options to produce, transform, and
provide energy services as well as to produce agriculture and forest
products, and to determine land use and land cover. Using a run period
extending from 1990 – 2100 at 5 year intervals, GCAM has been used to
explore the potential role of emerging energy supply technologies and
the greenhouse gas consequences of specific policy measures or energy
technology adoption including; CO2 capture and storage, bioenergy,
hydrogen systems, nuclear energy, renewable energy technology, and
energy use technology in buildings, industry and the transportation
sectors. GCAM is an Representative Concentration Pathway (RCP)-class
model. This means it can be used to simulate scenarios, policies, and
emission targets from various sources including the Intergovernmental
Panel on Climate Change (IPCC). Output includes projections of future
energy supply and demand and the resulting greenhouse gas emissions,
radiative forcing and climate effects of 16 greenhouse gases, aerosols
and short-lived species at 0.5×0.5 degree resolution, contingent on
assumptions about future population, economy, technology, and climate
mitigation policy.

## Documentation

* [GCAM Documentation](http://jgcri.github.io/gcam-doc/)
* [Getting Started with GCAM](http://jgcri.github.io/gcam-doc/user-guide.html)
* [GCAM Community](http://www.globalchange.umd.edu/models/gcam/gcam-community/)
* [GCAM Videos and Tutorial Slides](https://gcims.pnnl.gov/community)

## Selected Publications

Calvin, K., Patel, P., Clarke, L., Asrar, G., Bond-Lamberty, B., Cui, R. Y., Di Vittorio, A., Dorheim, K., Edmonds, J., Hartin, C., Hejazi, M., Horowitz, R., Iyer, G., Kyle, P., Kim, S., Link, R., McJeon, H., Smith, S. J., Snyder, A., Waldhoff, S., and Wise, M.: GCAM v5.1: representing the linkages between energy, water, land, climate, and economic systems, Geosci. Model Dev., 12, 677–698, https://doi.org/10.5194/gmd-12-677-2019, 2019.

Edmonds, J., and J. Reilly (1985)Global Energy: Assessing the Future (Oxford University Press, New York) pp.317.

Edmonds, J., M. Wise, H. Pitcher, R. Richels, T. Wigley, and C. MacCracken. (1997) “An Integrated Assessment of Climate Change and the Accelerated Introduction of Advanced Energy Technologies”, Mitigation and Adaptation Strategies for Global Change, 1, pp. 311-39

Kim, S.H., J. Edmonds, J. Lurz, S. J. Smith, and M. Wise (2006) “The ObjECTS Framework for Integrated Assessment: Hybrid Modeling of Transportation ” Energy Journal (Special Issue #2) pp 51-80.

[Full list of GCAM publications](http://jgcri.github.io/gcam-doc/references.html)
