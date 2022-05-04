\name{Lorber-Egeghy-East Model Template (LEEM Template)}
\alias{LEEM_Template}
\title{Formatting for the input to the Lorber Egeghy East Model}
\description{
A function which returns a list of dataframes with the required column names to run the full \code{\link{LEEM}}.
Three sheets are created: one for studies with concentration summary statistics, another for factors to be assigned to the generated lognormal curves (normally exposure factors), and absorption factors for each media-chemical relationship. The sample data provided, \emph{LEEMR_Example_Data}, may also be used as a template but will contain extant columns.

This function produces  the required column names for input to the \code{\link{LEEM}} and breif column descriptions. If running \code{\link{LEEM_Concentration}} the factors and absorption sheets are not used. Any number of additional columns may be included and passed through the model.

No arguments are passed to \emph{LEEM_Template}. A list of empty dataframes is returned.
For best use, assign this to an object or export.

}
\usage{
leemrtemplate <- LEEM_Template()

}
\arguments{
None.
}
