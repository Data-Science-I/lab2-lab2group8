#'Functions 1 for boxplot of the DRG data
#'
#'
#' @param data a data frame
#' @param payment_type average Medicare payments, the average total payment, or the average covered charges
#'
#' @return A boxplot of \code{payment_type} by DRG code
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#'
#' @examples
#' plot_payments_by_DRG(drg_data_test, "Average Covered Charges")
#'
plot_payments_by_DRG <- function(data, payment_type = c("Average Medicare Payments", "Average Total Payments", "Average Covered Charges")) {

  # option for the average Medicare payments, the average total payment, or the average covered charges.
  payment_column <- switch(payment_type,
                           "Average Medicare Payments" = "Average Medicare Payments",
                           "Average Total Payments" = "Average Total Payments",
                           "Average Covered Charges" = "Average Covered Charges")

  # Create the boxplot
  library(ggplot2)
  ggplot(data, aes(x = `DRG Definition`, y = .data[[payment_column]])) +
    geom_boxplot() +
    labs(
      title = paste("Boxplot of", payment_type, "by DRG Code"),
      x = "DRG Code",
      y = payment_type
    ) +
    scale_x_discrete(labels = function(x) gsub("\\D.*", "", x)) + # use DRG code on x-axis
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#'Functions 2 for calculating statistics of the DRG data
#'
#'
#' @param data a data frame
#' @param stat_type statistics over all of the DRG codes for average Medicare payments
#'
#' @return A statistics of \code{stat_type} by DRG code
#' @export
#'
#' @importFrom readr read_csv
#' @importFrom dplyr summarise
#'
#' @examples
#' calculate_DRG_stats(drg_data, "mean")
#'
calculate_DRG_stats <- function(data, stat_type = c("mean", "median", "sd")) {

  # option for calculate either the mean, median, or standard deviation of the DRG codes
  library(dplyr)
  result <- data %>%
    group_by(`DRG Definition`) %>%
    summarise(AverageMedicarePayments = .data[["Average Medicare Payments"]]) %>%
    summarise(
      stat = switch(stat_type,
                    "mean" = mean(AverageMedicarePayments, na.rm = TRUE),
                    "median" = median(AverageMedicarePayments, na.rm = TRUE),
                    "sd" = sd(AverageMedicarePayments, na.rm = TRUE)) # statistical analysis
    )
  return(result)
}
