rm(list = ls())
get_data_path <- function(n_trans, n_im){# Chemin vers les données
  data_path <- "data_sets/transects_decoupes_netcdf/"
  paste0(data_path, "figure", n_trans, n_im, ".nc")
}
num_transect <- 2
num_ss_image <- 1
import_data <- function(num_transect, num_ss_image,
                        as.matrix = F){
  nc_image   <- ncdf4::nc_open(get_data_path(num_transect, num_ss_image))
  image_mat  <- ncdf4::ncvar_get(nc_image, "Sv1")
  dimensions <- purrr::map(nc_image$dim, "vals") # Extraie les covariables
  ncdf4::nc_close(nc_image)# Don't forget to close
  if(as.matrix){
    return(image_mat)
  }
  else{
    n_x <- ncol(image_mat)
    n_y <- nrow(image_mat)
    as_exp_grid <- function(z) rep(z, rep(n_y, n_x))
    xs <- 1:n_x; ys <- 1:n_y
    grille <- expand.grid(depth = -(ys), x = xs)
    res <- data.frame(depth = rep(dimensions$dep1, n_x),
                      lon =  as_exp_grid(dimensions$lon1),
                      lat = as_exp_grid(dimensions$lat1),
                      time = as_exp_grid(dimensions$time1),
                      z = as.numeric(image_mat),
                      transect = rep(num_transect, n_x * n_y),# Transect
                      couche = rep(num_ss_image, n_x * n_y))# Couche
    return(res)
  }
}
