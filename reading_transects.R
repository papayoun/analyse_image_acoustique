rm(list = ls())
library(tidyverse)

get_data_path <- function(n_trans, n_im){
  data_path <- "data_sets/transects_decoupes/"
  paste0(data_path, "transect", n_trans, "_", n_im, ".txt")
}


import_data <- function(num_transect, num_ss_image,
                        as.matrix = F) {
  image_mat <- read.table(get_data_path(num_transect, num_ss_image),
                      sep = ",") %>% as.matrix()
  if(as.matrix){
    return(image_mat)
  }
  else{
    n_x <- ncol(image_mat)
    n_y <- nrow(image_mat)
    xs <- 1:n_x; ys <- 1:n_y
    grille <- expand.grid(depth = -(ys), x = xs)
    res <- data.frame(depth = grille[, "depth"],
                      x = grille[, "x"],
                      z = as.numeric(image_mat),
                      transect = rep(num_transect, n_x * n_y),
                      couche = rep(num_ss_image, n_x * n_y))
    return(res)
  }
}

get_image_res <- function(transect, couche){
  image_df <- import_data(transect, couche) %>% 
    mutate(z_res = lm(z ~ poly(depth, x, degree = 10))$residuals)
}

all_images <- expand.grid(transect = 1:5, couche = 1:3) %>% 
  as.data.frame() %>% 
  purrr::pmap(.f = get_image_res) %>% bind_rows()

image_df <- import_data(1, 1) %>% 
  mutate(z_res = lm(z ~ poly(depth, x, degree = 10))$residuals)
# Checking residuals ------------------------------------------------------

all_images %>% 
  dplyr::filter(transect == 1 & couche == 1) %>%  select(x, depth, z, z_res) %>%
  gather(key = "champ", value = "valeur", -x, -depth,
         convert = T, factor_key = T) %>% 
  group_by(champ) %>% 
  nest() %>% mutate(plots = purrr::map2(data, champ, function(df, ch){
    if(ch == "z"){
      titre = "Champ initial"
    }
    else{
      titre = "Résidus"
    }
    p <- ggplot(df, aes(x = x, y = depth, fill = valeur)) +
      geom_raster() +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(y = "Profondeur", title = titre) + 
      scale_fill_viridis_c() +
      theme(legend.position = "none")
    return(p)
  })) %>% 
  gridExtra::grid.arrange(grobs = .$plots)
    
  gridExtra::grid.arrange()
  ggplot(aes(x = x, y = depth, fill = valeur)) +
  geom_raster(data = filter(image_df, champ == "z")) +
  geom_raster(data = filter(image_df, champ == "z_res")) +
  facet_wrap(~ champ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c()
image_df %>% select(x, depth, z, z_res) %>%
  mutate(z = scale(z), 
         z_res = scale(z_res)) %>% 
  gather(key = "champ", value = "valeur", -x, -depth,
         convert = T, factor_key = T) %>% 
  group_by(champ) %>% 
  ggplot(aes(x = x, y = depth, fill = valeur)) +
  geom_raster() +
  facet_wrap(~ champ, scales = "free", nrow = 2, shrink = F) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c()
p1 <- ggplot(image_df, aes(x = x, y = depth)) +
  geom_raster(aes(fill = z)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_c()
p2 <- ggplot(image_df, aes(x = x, y = depth)) +
  geom_raster(aes(fill = z_res)) +
  scale_fill_viridis_c()
gridExtra::grid.arrange(p1, p2)

library(gstat)
image_gstat <- gstat::gstat(id = "reponse", 
                            formula = z ~ poly(y, x_reg, degree = 9),
                            locations = ~ x + depth,
                            data = image_df)
dt.vgm <- gstat::variogram(image_gstat, alpha = c(0, 90), cutoff = c(40, 20),
                           width = 2)
plot(dt.vgm)

library()
# # Checking rowwise correlations -------------------------------------------
# 
# 
image_mat <- import_data(num_transect, num_ss_image,
                         as.matrix = T)
# (ex <- image_mat[1:5,1:5])
# (test <- matrix(image_df$z, nrow = length(unique(image_df$depth)))[1:5, 1:5])
resid_depth <- lm(z ~  poly(y, x_reg, degree = 9),
                  data = image_df)$residuals
image_mat_res <- matrix(resid_depth,
                        nrow = length(unique(image_df$depth)))
par(mfrow = c(2,1))
image(xs, ys, t(image_mat_res), col = terrain.colors(50))
image(xs, ys, t(image_mat), col = terrain.colors(50))

lag_max <- 100
acf_df <- purrr::map(1:19, function(j){
  mon_acf <- acf(image_mat_res[j, ], lag.max = lag_max, plot = F)
  res <- data.frame(mon_acf$acf[,,1])
  colnames(res) = paste0("prof", j)
  return(res)
}) %>% bind_cols() %>% rowid_to_column()  %>%
  rename(lag = rowid) %>% gather(key = "Couche", value = "Acf", -lag)

ggplot(acf_df, aes(x = lag, y = Acf, color = Couche)) + geom_line() +
  geom_hline(yintercept = 0)

acf1 <- acf(image_mat[1,], lag.max = 200)
names(acf1)
acf1$acf
X11()
par(mfrow=c(5, 1))
sapply(1:5, function(i)
  acf(image_mat[i,], lag.max = 200, main = i))

image_mat_compress <- image_mat[, seq(1, 238, by = 5)]
X11()
corrplot::corrplot(cor(t(image_mat)))
corrplot::corrplot(cor(image_mat_compress))
