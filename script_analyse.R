rm(list = ls())
library(readxl)
library(tidyverse)


# From xlsx file ----------------------------------------------------------



file_name <- "data_sets/data_profil3.xlsx"
read_acoustic_image <- function(file_name_,
                                df = F,
                                x_index = F){
  sheets <- readxl::excel_sheets(file_name_)
  x_tibble <- lapply(sheets,
                     function(sheet)
                       readxl::read_excel(file_name_,
                                          sheet = sheet,
                                          col_names = F,
                                          col_types = "numeric") %>%
                       as.matrix()
                     )
  if(!df){
    names(x_tibble) = c("x", "y", "z")
    return(x_tibble)
  }
  else{
    x_tibble <- lapply(x_tibble, as.double)
    if(x_index){
      x_tibble[[1]] <- 1:length(x_tibble[[1]])
    }
    return(data.frame(expand.grid(y = rev(x_tibble[[2]]),
                                  x = x_tibble[[1]]),
                      z = x_tibble[[3]]))
  }
}
ex <- read_acoustic_image(file_name, df = F, x_index = F)
ggplot(ex, aes(x = x, y = y)) + geom_raster(aes(fill = z) ) + scale_fill_viridis_c()
sapply(ex, dim)



# From netcdf -------------------------------------------------------------

rm(list = ls())
library(ncdf4)
file_name <- "data_sets/EK60_38kHz_2013-02-09T06Z_2013-03-10T23Z.nc"
my_ncdf <- nc_open(file_name) 
time_depth <- my_ncdf[["dim"]] %>% map("vals")
vars <- names(my_ncdf$var)
variables <- map(vars, ncvar_get, nc = my_ncdf)
names(variables) <- vars
pos_df <- bind_cols(time = time_depth$time,
                    latitude  = variables$latitude,
                    longitude = variables$longitude) %>% 
  as.data.frame()
print("Lignes dupliquées")
sum(duplicated(pos_df))
which(duplicated(pos_df))
all.equal(pos_df[1887,], pos_df[1888,])
z <- variables$Sv
plot(z[,1887], z[,1888])
abline(a = 0, b = 1)

compute_distance <- function(point1, point2){
  rayon_terre <- 6371.008# Rayon terre en km
  diff_lat <- point2["latitude"] - point1["latitude"]
}
library(geosphere)
n <- nrow(pos_df)
pos_df$dist_origine <-  c(0, map_dbl(2:n, function(i){
  x1 <- as.numeric(pos_df[i - 1, c("longitude", "latitude")])
  x2 <- as.numeric(pos_df[i, c("longitude", "latitude")])
  geosphere::distHaversine(x1, x2, r= 6371.008)
})) %>%  
  cumsum()

sel1 <- 1:287
pos <- pos_df[sel1, ]
z1 <- z[, sel1]

my_df <- data.frame(z = as.numeric(z1),
                    expand.grid(depth = rev(1:79), x = sel1))
ggplot(my_df, aes(x = x, y = depth)) + geom_raster(aes(fill = z)) +
  scale_fill_viridis_c()

ggplot(data.frame(ind = 2:n, diff = diff(dist_origine)), aes(x = ind, y = diff)) + 
  geom_point() + geom_hline(yintercept = 1) + scale_y_continuous(trans  = "log")
# Fit a variogram ---------------------------------------------------------


library(sf)
set.seed(123)
sel <- sample(1:nrow(my_df), size = 1000, replace = F)
jojo <- st_as_sf(my_df[sel,], coords = c("y", "x"))
ggplot() + geom_sf(data = jojo, aes(col = z)) + scale_color_viridis_c()
jojo_sp <- as_Spatial(jojo)
dt.vgm <- gstat::variogram(z ~ 1, jojo_sp)
plot(dt.vgm)
class(dt.vgm)

dt.fit <-
  gstat::fit.variogram(dt.vgm, model = gstat::vgm(1,"Lin",100,1)) # fit model

# vgm() list of models

plot(dt.vgm, dt.fit)

