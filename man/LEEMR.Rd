\name{Lorber-Egeghy-East Model R Package Description}
\alias{LEEMR}
\title{Package Description}
\description{

The Lorber-Egeghy-East Model R Package (LEEMR) was developed to estimate daily exposure to Per- and Polyfluoroalkyl Substances (PFAS) (\href{https://pubmed.ncbi.nlm.nih.gov/20145679/}{Egeghy & Lorber (2011)}, \href{https://pubmed.ncbi.nlm.nih.gov/21517063/}{Lorber & Egeghy (2011)}, \href{https://www.nature.com/articles/s41370-021-00374-w}{East et al. (2021)}). The model has since been expanded to work across chemical, media, and any number of exposure factors. The deterministic model generates lognormal points from an entered collection of summmary statistics originating from unique datasets. As such, this model is useful for quickly interpreting large volumes of data for exposure and concentration estimates.

There are three functions in the package: the full exposure model, a one-media concentration model, and a template generator.


\describe{
    \item{1. \code{\link{LEEM}}: the full exposure model}

\describe{
    \item{2. \code{\link{LEEM_Concentration}}: one media, any number of chemicals, concentration estimates from a dataframe of summary statistics}

3. \code{\link{LEEM_Template}}:
  generates an Excel template with correct column names for both concentration and exposure model input.

Sample data is provided as "LEEMR_Example_Data" to test both models and provides a useful example as to input data use and format.
Copy/pastable examples are provided in both function descriptions (\code{\link{LEEM}} ,  \code{\link{LEEM_Concentration}}).
}
