binscatter <- function(y, x, df, z, group, weights, n.cut = 20, cuts = NULL, cluster){
  require(multiwayvcov)
  
  # using tibbles broke the function for Ari.
  df <- as.data.frame(df)
  
  if(missing(group)) {
    df$group <- 1
  } else {
    df$group <- df[,group]
  }
  
  if(missing(weights)){
    df$weights <- 1
  } else {
    df$weights <- df[, weights]
  }
  
  if (missing(z) == FALSE) {
    x_res_string <- paste0("df$",x," ~ ",paste0("df$",z, collapse= " + "))
    print(x_res_string)
    y_res_string <- paste0("df$",y," ~ ",paste0("df$",z, collapse= " + "))
    print(y_res_string)
    df$xRes <- residuals(lm(as.formula(x_res_string), weights = df[,weights], na.action="na.exclude")) + 
      weighted.mean(df[,x], df[,weights], na.rm = TRUE)
    df$yRes <- resuduals(lm(as.formula(y_res_string), weights = df[,weights], na.action="na.exclude")) + 
      weighted.mean(df[,y], df[,weights], na.rm = TRUE)
    reg_string <- paste0("df$",y," ~ df$",x," + ",paste0("df$",z, collapse= " + "))
  } else {
    df$xRes <- df[,x]
    df$yRes <- df[,y]
    reg_string <- paste0("df$",y," ~ df$",x)
  }
  
  # print(paste0("Binscatter with ",reg_string))
  lin_model <- lm(as.formula(reg_string), weights=df$weights)
  if (missing(cluster) == FALSE) {
    lin_model.vcov_cluster <- cluster.vcov(lin_model, as.factor(df[,cluster]))
    cluster_se <- sqrt(lin_model.vcov_cluster[2,2])
  } else {
    cluster_se <- NULL
  }
  
  if (missing(cuts)){
    qts <- unique(quantile( df$xRes, seq(from=0,to=1,length.out=n.cut+1), na.rm = TRUE))
  } else {
    qts <- cuts
  }
  
  df_bin <- df %>%
    mutate(cut = as.numeric(cut(xRes, qts, include.lowest = TRUE))) %>%
    group_by(cut,group) %>% 
    summarise(x = weighted.mean(xRes, weights, na.rm =  TRUE), 
              y = weighted.mean(yRes, weights,  na.rm = TRUE))
  
  list("df_bin" = df_bin, "in_model" = lin_model, "cluster_se" = cluster_se)
  
}

