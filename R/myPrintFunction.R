#' print method
#'
#' @param obj
#'
#' @return list
#' @export
#'
#'
print.Rttest <- function(obj) {
  library(kableExtra)
  cat("alpha value -->", obj$alpha_value, "\n", "\n")
  cat("confidence interval for mu_x-mu_y --> [", obj$confidence_interval, "]", "\n", "\n")
  cat("p-value is -->", obj$p_value, "\n", "\n")

  cat("data frame containing x and y:\n")

  # using kable to print the data frame
  kable(obj$data_frame, format = "html") %>%
    kable_styling(bootstrap_options = "bordered", full_width = F)
}
