
library(MASS)

get_THETA_names_from_tv.ext = function(tv.ext) {
  THETA_names = grep("THETA*", names(tv.ext), value=T, invert=F)
  return(THETA_names)
}

#' @importFrom magrittr %<>%
get_THETA_values_from_tv.ext = function(tv.ext) {
  THETA_values = NULL
  THETA_names = get_THETA_names_from_tv.ext(tv.ext)

  for (name_ in THETA_names) {
    THETA_values %<>% c(tv.ext[[name_]])
  }

  return(THETA_values)
}

#' Generates typical value dataframe from NONMEM ext file structure
#'
#' @param tv.ext dataframe object from loading NONMEM ext file
#'
#' @export
get_TV_from_tv.ext = function(tv.ext) {
  THETA_names  = get_THETA_names_from_tv.ext(tv.ext)
  THETA_values = get_THETA_values_from_tv.ext(tv.ext)

  TV = THETA_values
  names(TV) = THETA_names

  return(TV)
}

get_OMEGA_names_from_tv.ext = function(tv.ext) {
  OMEGA_names = grep("OMEGA*", names(tv.ext), value=T, invert=F)
  return(OMEGA_names)
}

#' @importFrom magrittr %<>%
get_OMEGA_values_from_tv.ext = function(tv.ext) {
  OMEGA_values = NULL
  OMEGA_names  = get_OMEGA_names_from_tv.ext(tv.ext)

  for (name_ in OMEGA_names) {
    OMEGA_values %<>% c(tv.ext[[name_]])
  }

  return(OMEGA_values)
}

#' @importFrom utils tail
get_dim_of_OMEGA_from_names = function(OMEGA_names) {
  last_element = tail(OMEGA_names, n=1)
  nIIV = as.numeric(gsub(".*?([0-9]+).*", "\\1", last_element))
  return(nIIV)
}

# convert upper triangular matrix to symmetric
# https://stackoverflow.com/questions/37613345/r-convert-upper-triangular-part-of-a-matrix-to-symmetric-matrix
ultosymmetric = function(m) {
  m = m + t(m) - diag(diag(m))
  return (m)
}

generate_IIV_matrix_from_tv.ext = function(tv.ext, nIIV=NULL) {
  OMEGA_names  = get_OMEGA_names_from_tv.ext(tv.ext)
  OMEGA_values = get_OMEGA_values_from_tv.ext(tv.ext)

  if (is.null(nIIV)) { nIIV = get_dim_of_OMEGA_from_names(OMEGA_names) }

  omega = matrix(0, nrow = nIIV, ncol = nIIV)
  omega[upper.tri(omega, diag = TRUE)] = OMEGA_values

  omega = ultosymmetric(omega)

  colnames(omega) = paste0("OMEGA(n,", 1:nIIV, ")")
  rownames(omega) = paste0("OMEGA(",  1:nIIV, ",m)")

  return(omega)
}

#' @importFrom magrittr %<>%
get_IIV_fixed_to_zero_cols_from_tv.ext = function(tv.ext) {
  omega = generate_IIV_matrix_from_tv.ext(tv.ext)
  nIIV  = get_omega_dim_from_tv.ext(tv.ext)

  fixed_to_zero_col_ids = NULL
  for (col_ in 1:nIIV) {
    if (sum(abs(omega[,col_]))==0)
      fixed_to_zero_col_ids %<>% c(col_)
  }

  return(fixed_to_zero_col_ids)
}

get_omega_dim_from_tv.ext = function(tv.ext) {
  OMEGA_names  = get_OMEGA_names_from_tv.ext(tv.ext)
  nIIV         = get_dim_of_OMEGA_from_names(OMEGA_names)

  return(nIIV)
}

#' Generates OMEGA dataframe from NONMEM ext file structure
#'
#' @param tv.ext dataframe object from loading NONMEM ext file
#' @param n.sims An int for number of rows in dataframe
#'
#' @importFrom MASS mvrnorm
#' @importFrom magrittr %<>%
#' @export
generate_OMEGA_covariate_matrix = function(tv.ext, n.sims) {
  # generate full OMEGA matrix
  omega = generate_IIV_matrix_from_tv.ext(tv.ext)
  nIIV  = get_omega_dim_from_tv.ext(tv.ext)

  # save names to apply back later
  omega_names = colnames(omega)

  # get columns of fixed to zero ETAs (and conjugate)
  fixed_to_zero_col_ids = get_IIV_fixed_to_zero_cols_from_tv.ext(tv.ext)

  if (!is.null(fixed_to_zero_col_ids)) {
    nonfixed_col_ids = (1:nIIV)[-fixed_to_zero_col_ids]

    # get number of nonfixed parameters
    nIIV_full = nIIV - length(fixed_to_zero_col_ids)

    # restrict to nonfixed ETAs (matrix needs to be pos-def for mvrnorm sampling)
    omega_full = omega[-fixed_to_zero_col_ids, -fixed_to_zero_col_ids]
  } else {
    nIIV_full  = nIIV
    omega_full = omega
  }

  # sample values
  mu       = rep(0, nIIV_full)
  ETA_full = MASS::mvrnorm(n=n.sims, mu, omega_full)

  # fill in fixed values so column corresponds to ETA definition
  if (!is.null(fixed_to_zero_col_ids)) {
    ETA = matrix(0, nrow = n.sims, ncol = nIIV)
    ETA[,nonfixed_col_ids] = ETA_full
  } else {
    ETA = ETA_full
  }

  # add names back (necessary if some columns are zeros)
  colnames(ETA) = omega_names

  return(ETA)
}
