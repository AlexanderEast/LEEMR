\name{Lorber-Egeghy-East Model Template (LEEM Template)}
\alias{LEEM_Template}
\title{Formatting for the input to the Lorber Egeghy East Model.}
\description{
A function which creates an Excel file with the required column names to run the full \code{\link{LEEM}}.
Two sheets are created: one for studies with concentration summary statistics, and another for factors to be assigned to the generated lognormal curves (normally exposure factors). The sample data provided, "LEEMR_Example_Data", may also be used as a template but will contain extant columns.

This function produces only the required column names for input to the \code{\link{LEEM}}. If running \code{\link{LEEM_Concentration}} the factors sheet is not used. Any number of additional columns may be included and passed through the model.

No arguments are passed to LEM_Template. An Excel file is generated in the user's working directory.

}
\usage{
LEEM_Template()

}
\arguments{
None.
}
