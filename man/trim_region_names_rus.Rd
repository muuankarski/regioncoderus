% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/trim_region_names.R
\name{trim_region_names_rus}
\alias{trim_region_names_rus}
\title{Trim Russian language region codes}
\usage{
trim_region_names_rus(x)
}
\arguments{
\item{x}{Vector to be trimmed}
}
\description{
Converts long country names into one of many different coding schemes.
Translates from one scheme to another. Converts country name or coding
scheme to the official short English country name. Creates a new variable
with the name of the continent or region to which each country belongs.
}
\note{
Supports the following coding schemes: Correlates of War character,
CoW-numeric, ISO3-character, ISO3-numeric, ISO2-character, IMF numeric, International
Olympic Committee, FIPS 10-4, FAO numeric, United Nations numeric,
World Bank character, official English short country names (ISO), continent, region.
}
\keyword{trim_region_names_rus}

