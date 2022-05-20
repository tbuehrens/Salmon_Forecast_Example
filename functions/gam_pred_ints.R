#prediction intervals GAM
gam_pred_ints<-function(fit_gam,new_dat,quants){
  beta <- coef(fit_gam)
  V <- vcov(fit_gam)
  num_beta_vecs <- 10000
  Cv <- chol(V)
  set.seed(1)
  nus <- rnorm(num_beta_vecs * length(beta))
  beta_sims <- beta + t(Cv) %*% matrix(nus, nrow = length(beta), ncol = num_beta_vecs)
  covar_sim <- predict(fit_gam, newdata = new_dat, type = "lpmatrix")
  linpred_sim <- covar_sim %*% beta_sims
  invlink <- function(x) exp(x)
  exp_val_sim <- invlink(linpred_sim)
  y_sim <- matrix(rnegbin(n = prod(dim(exp_val_sim)), 
                          mu = exp_val_sim, 
                          theta = fit_gam$family$getTheta(TRUE)),
                  nrow = nrow(exp_val_sim), 
                  ncol = ncol(exp_val_sim))
  y_sim<-data.frame(y_sim)%>%
    as_tibble()%>%
    rowid_to_column()%>%
    pivot_longer(!rowid)%>%
    group_by(rowid)%>%
    summarise(value=quantile(value,quants,na.rm=T),q=quants,.groups='drop')%>%
    pivot_wider(values_from = value,names_from = q)%>%
    ungroup() %>%
    dplyr::select(!rowid)
}