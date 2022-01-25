# Lorber-Egeghy-East Model R Package (LEEMR)
### Description
This package contains three functions: The Lorber-Egeghy-East Model (`LEEM`), `LEEM_Concentration`, and `LEEM_Template`.
- `LEEM` generates exposure estimates from multiple studies reporting only sparse summary statistics of concentrations.
- `LEEM_Concentration` generates concentration estimates from multiple studies reporting only sparse summary statistics of concentrations.
- `LEEM_Template` prints a template for `LEEM` and `LEEM_Concentation` to the user's working directory
- Sample data is provided as `LEEMR_Example_Data`.

Detailed function descriptions are embedded in R descriptions. Use `?LEEMR` after installation for information.

### Running the LEEMR
Documentation and code are hosted as a package on Github. To run the LEEMR, ensure `devtools` is installed. The package is installed as follows:
```
library(devtools)
install_github("AlexanderEast/LEEMR",force = TRUE)
```
After installation, the package can be loaded: 
```
library(LEEMR)
```
### Application

An early application of the `LEEMR` is demonstrated in 
[this](https://www.researchgate.net/publication/353781564_Computational_estimates_of_daily_aggregate_exposure_to_PFOAPFOS_from_2011_to_2017_using_a_basic_intake_model) paper, which estimates daily aggregate PFOA and PFOS intake for US adults and children. 

### Contact

Please email alexander.w.east@gmail.com with any questions.

### Disclaimer

With respect to this software, neither the EPA, nor any of their employees, makes any warranty, express or implied, including the warranties of merchantability and fitness for a particular purpose, or assumes any legal liability or responsibility for the accuracy, completeness, or usefulness of any information, apparatus, product, or process disclosed.
