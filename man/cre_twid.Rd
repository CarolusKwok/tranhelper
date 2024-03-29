% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cre_twid.R
\name{cre_twid}
\alias{cre_twid}
\title{Find minimum transect width}
\usage{
cre_twid(
  shape = "rec",
  T_len = 60,
  T_num = 2,
  S_num = 18,
  S_wid = 2,
  S_len = 2,
  S_bor = 5,
  flip_x = F,
  flip_y = F
)
}
\arguments{
\item{shape}{shape of the site, either "rec" (rectangle) or "ell" (ellipse)}

\item{T_len}{Length of transect (in m)}

\item{T_num}{Number of transects in a group}

\item{S_num}{Number of sites}

\item{S_wid}{Width of each site (in m)}

\item{S_len}{Length of each site (in m)}

\item{S_bor}{Border of each site (in m)}

\item{flip_x}{Flips the pattern on the x axis}

\item{flip_y}{Flips the pattern on the y axis}
}
\value{

}
\description{
Find minimum transect width
}
\examples{
cre_twid()
}
