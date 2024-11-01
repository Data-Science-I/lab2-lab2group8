% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DRG_plot_stat.R
\name{plot_payments_by_DRG}
\alias{plot_payments_by_DRG}
\title{Functions 1 for boxplot of the DRG data}
\usage{
plot_payments_by_DRG(
  data,
  payment_type = c("Average Medicare Payments", "Average Total Payments",
    "Average Covered Charges")
)
}
\arguments{
\item{data}{a data frame}

\item{payment_type}{average Medicare payments, the average total payment, or the average covered charges}
}
\value{
A boxplot of \code{payment_type} by DRG code
}
\description{
Functions 1 for boxplot of the DRG data
}
\examples{
plot_payments_by_DRG(drg_data_test, "Average Covered Charges")

}
