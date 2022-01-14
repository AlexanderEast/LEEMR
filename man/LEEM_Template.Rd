\name{Lorber-Egeghy-East Model Template (LEEM Template)}
\alias{LEEM_Template}
\title{Formatting for the input to the Lorber Egeghy Model.}
\description{
A function which creates an Excel file with the required column names to run the full LEEMR.
Two sheets are created: one for studies with concentration summary statistics, and another for factors to be assigned to the generated lognormal curves (normally exposure factors). The sample data provided in the package ("LEEMR_Example_Data") may also be used as a template but will contain extant columns.

This function produces only the required column names for input to the LEEMR. If running \code{\link{LEEM_Concentration}} the factors sheet is not used.

No arguments are passed to LEM_Template. An Excel file is generated in the user's working directory.

}
\usage{
LEM_Template()

}
\arguments{
None.
}
