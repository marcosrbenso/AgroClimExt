[![DOI:10.5194/egusphere-2023-3002](http://img.shields.io/badge/DOI-10.5194/egusphere.2023.3002-0080ff.svg)](https://doi.org/10.5194/egusphere-2023-3002)
![alt text](https://github.com/marcosrbenso/ClimateImpactML/blob/main/Fig_readme_flowchart.png)
# Machine Learning (ML) models for explaining the impact of climate risks on crop yield

**ClimateImpactML** is a GitHub repository that contains instructions to reproduce results from *"A data-driven framework for assessing climatic impact-drivers in the context of food security"*. which is under review in the journal Natural Hazards and Earth Systems Science (NHESS) [![DOI:10.5194/egusphere-2023-3002](http://img.shields.io/badge/DOI-10.5194/egusphere.2023.3002-0080ff.svg)](https://doi.org/10.5194/egusphere-2023-3002)

## Code references
### 00_Climatic_ImpactDrivers.R

This script was writen to compute monthly climatic impact-drivers (CID) from daily weather data including precipitation, minimum temperature, and maximum temperature. For this example data from Era5 was used to derive indices aggregated at municipality level for Brazil.

The following packages are required to run this code.

- []()
- []()
- []()
- []()

### 01_CropData.R

This script was written to process annual crop yield data from different sources to remove trends and heteroscedasticity from data. This procedure aims to reduce the impacts of technological changes over the years on yields.

The following packages are required to run this code.

- [blockCV](https://cran.r-project.org/web/packages/blockCV/blockCV.pdf)
- [geobr](https://cran.r-project.org/web/packages/geobr/geobr.pdf)
- [sp](https://cran.r-project.org/web/packages/sp/index.html)
- [tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html)

### 02.RF_Model1.R

This script was written to select the most relevant CIDs for climate impacts studies on crop yields. The same approach can be applied to other sectors.

The following packages are required to run this code.

- [CAST](https://cran.r-project.org/web/packages/CAST/CAST.pdf)
- [caret](https://cran.r-project.org/web/packages/caret/caret.pdf)
- [doParallel](https://cran.r-project.org/web/packages/doParallel/doParallel.pdf)
- []()

### 03.XGBoost_Shap_Model

The following packages are required to run this code.

- []()
- []()
- []()
- []()

## Data references



The data used in this repo can be found in <!-- [![DOI](https://sandbox.zenodo.org/badge/DOI/10.5281/zenodo.12612860.svg)](https://handle.stage.datacite.org/10.5281/zenodo.12612860) -->
