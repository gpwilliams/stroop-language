papa_engine <- function(num, 
                        digits = 3, 
                        tolerance = .001, 
                        nsmall = 3, 
                        asterisk = TRUE) {
  # pAPA: Formats p-values to APA standards, removing leading zeroes
  # and formatting values below the tolerance to < the tolerance
  # by default this tolerance is .001, and p-values are reported
  # to 3 digits. 
  # This keeps trailing zeroes to 3 decimal places by default (nsmall)
  # unless p == 1, where only 2 decimals are reported.
  # Inputs:
  #   num = Numeric vector of p-values
  #   digits = Digits by which to round (default = 3)
  #   tolerance = Limit by which to report smaller values with < tolerance
  #   nsmall = Number of trailing zeroes to keep when rounding
  #   asterisk = Whether or not to include asterisks for different 
  #     "levels" of significance. These values are * <.05; ** <.01; *** <.001
  # note we do not use format.pval as this gives results to 3sf
  if (!is.numeric(num)) {
    stop("Input is non-numeric and cannot be rounded.")
  } else if (is.na(num)) {
    output_pval <- "NA"
  } else {
    # format p-value
    rounded_pval <- round(num, digits)
    rounded_formatted_pval <- format(rounded_pval, nsmall = nsmall)
    if (rounded_pval < tolerance) {
      output_pval <- paste0("<", tolerance)
    } else {
      output_pval <- rounded_formatted_pval
    }
    # additional formatting
    if (rounded_pval == 1) {
      # remove trailing zero (to keep 3sf)
      substr(output_pval, 1, nchar(output_pval)-1)
    } else {
      # if number is less than tolerance (default of .001)
      if (rounded_pval < tolerance) {
        if (asterisk == TRUE) {
          paste0(gsub("<0", "<", output_pval), "***")
        } else {
          gsub("<0", "<", output_pval)
        }
      } else {
        # if number isn't less than tolerance (default of .001)
        if (asterisk == TRUE) {
          if (rounded_pval < .01) {
            paste0(substring(output_pval, 2), "**")
          } else if (rounded_pval < .05) {
            paste0(substring(output_pval, 2), "*")
          } else {
            substring(output_pval, 2)
          }
        } else {
          substring(output_pval, 2)
        }
      }
    }
  }
}

papa <- function(num, ...) {
  # a vectorised form of the papa_engine function
  # Inputs = numeric vector of values to be turned into p-values
  # ... = additional arguments passed to papa_engine()
  # Outputs = numeric vector of p-values
  sapply(
    num, 
    papa_engine, 
    ...,
    simplify = TRUE
  )
}