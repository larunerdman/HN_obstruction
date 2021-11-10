### 
###   SILENT TRIAL ANALYSIS FOLLOWING RE-TRAINING OF ORIGINAL MODEL 
###     APRIL 2021
###
###

library(readxl)
library(reshape2)
library(rjson)
library(RJSONIO)
library(V8)
library(ggplot2)

##
## FUNCTIONS
##

get_auc_ci = function(true_vals, pred_vals, n_its){

  true_auc = mltools::auc_roc(preds = pred_vals, actuals = true_vals)
  n_data = length(pred_vals)

  resampled_aucs = sapply(1:n_its, function(i){
    samp_idx = sample(1:n_data, size = n_data, replace=TRUE)
    auc_out = mltools::auc_roc(pred_vals[samp_idx],actuals = true_vals[samp_idx])
    return(auc_out)
  })

  sorted_aucs = sort(resampled_aucs)
  ci = quantile(sorted_aucs,c(0.025, 0.975))

  out_list = list("auc" = true_auc, "ci" = ci)
  return(out_list)
}

find_sensitivity_thresh = function(pred, label, sensitivity = 0.95){
  sorted_lab = label[order(pred, decreasing = FALSE)]
  sorted_pred = sort(pred, decreasing = FALSE)
  n = length(sorted_lab)

  sen = sapply(1:length(sorted_lab), function(i){
    return(sum(sorted_lab[i:n])/sum(sorted_lab))
  })

  sen_enough = sen[sen > sensitivity]
  threshold = sorted_pred[length(sen_enough)]

  return(threshold)
}

get_tnrs = function(in_list, sensitivity = 0.95, thresh_set_split = "val",
                    epoch = 47, test_sub = NULL, n_its = 500, fold = 1, st=TRUE, stan=TRUE){

  # browser()

  train_pred = in_list$train[[fold]][[epoch]][["pred"]]
  train_label = in_list$train[[fold]][[epoch]][["target"]]

  val_pred = in_list$val[[fold]][[epoch]][["pred"]]
  val_label = in_list$val[[fold]][[epoch]][["target"]]

  if(is.null(test_sub)){
    test_pred = in_list$test[[fold]][[epoch]][["pred"]]
    test_label = in_list$test[[fold]][[epoch]][["target"]]
  } else{
    test_pred = in_list$test[[fold]][[epoch]][["pred"]][in_list$test[[fold]][[epoch]][["id"]] %in% test_sub]
    test_label = in_list$test[[fold]][[epoch]][["target"]][in_list$test[[fold]][[epoch]][["id"]] %in% test_sub]
  }

  threshold = find_sensitivity_thresh(pred = in_list[[thresh_set_split]][[fold]][[epoch]][["pred"]],
                                      label = in_list[[thresh_set_split]][[fold]][[epoch]][["target"]],
                                      sensitivity = sensitivity)

  train_tnr = sum(train_label[train_pred < threshold] == 0)/sum(train_label == 0)
  val_tnr = sum(val_label[val_pred < threshold] == 0)/sum(val_label == 0)
  test_tnr = sum(test_label[test_pred < threshold] == 0)/sum(test_label == 0)

  out_list = list("train" = list("tnr" = train_tnr),
                  "val" = list("tnr" = val_tnr),
                  "test" = list("tnr" = test_tnr))
  train_tnrs = c()
  val_tnrs = c()
  test_tnrs = c()

  if(st){
    st_pred = in_list$st[[fold]][[epoch]][["pred"]]
    st_label = in_list$st[[fold]][[epoch]][["target"]]
    st_tnr = sum(st_label[st_pred < threshold] == 0)/sum(st_label == 0)
    out_list[["st"]] = list("tnr" = st_tnr)
    st_tnrs = c()
  }
  if(stan){
    stan_pred = in_list$stan[[fold]][[epoch]][["pred"]]
    stan_label = in_list$stan[[fold]][[epoch]][["target"]]
    stan_tnr = sum(stan_label[stan_pred < threshold] == 0)/sum(stan_label == 0)
    out_list[["stan"]] = list("tnr" = stan_tnr)
    stan_tnrs = c()
  }

  for(it in 1:n_its){

    train_sample = sample(1:length(train_pred),size = length(train_pred), replace = TRUE)
    val_sample = sample(1:length(val_pred),size = length(val_pred), replace = TRUE)
    test_sample = sample(1:length(test_pred),size = length(test_pred), replace = TRUE)

    train_pred_s = train_pred[train_sample]
    train_label_s = train_label[train_sample]

    val_pred_s = val_pred[val_sample]
    val_label_s = val_label[val_sample]

    test_pred_s = test_pred[test_sample]
    test_label_s = test_label[test_sample]

    train_tnrs = c(sum(train_label_s[train_pred_s < threshold] == 0)/sum(train_label_s == 0), train_tnrs)
    val_tnrs = c(sum(val_label_s[val_pred_s < threshold] == 0)/sum(val_label_s == 0), val_tnrs)
    test_tnrs = c(sum(test_label_s[test_pred_s < threshold] == 0)/sum(test_label_s == 0), test_tnrs)

    if(st){
      st_sample = sample(1:length(st_pred),size = length(st_pred), replace = TRUE)
      st_pred_s = st_pred[st_sample]
      st_label_s = st_label[st_sample]
      st_tnrs = c(sum(st_label_s[st_pred_s < threshold] == 0)/sum(st_label_s == 0), st_tnrs)
    }
    if(stan){
      stan_sample = sample(1:length(stan_pred),size = length(stan_pred), replace = TRUE)
      stan_pred_s = stan_pred[stan_sample]
      stan_label_s = stan_label[stan_sample]
      stan_tnrs = c(sum(stan_label_s[stan_pred_s < threshold] == 0)/sum(stan_label_s == 0), stan_tnrs)
    }

  }

  train_tnrs = sort(train_tnrs)
  val_tnrs = sort(val_tnrs)
  test_tnrs = sort(test_tnrs)

  out_list[["train"]][["ci"]] = quantile(train_tnrs, c(0.025, 0.975))
  out_list[["val"]][["ci"]] = quantile(val_tnrs, c(0.025, 0.975))
  out_list[["test"]][["ci"]] = quantile(test_tnrs, c(0.025, 0.975))

  if(st){
    out_list[["st"]][["ci"]] = quantile(st_tnrs, c(0.025, 0.975))
  }
  if(stan){
    out_list[["stan"]][["ci"]] = quantile(stan_tnrs, c(0.025, 0.975))
  }

  return(out_list)
}


get_fnrs = function(in_list, sensitivity = 0.95, thresh_set_split = "val",
                    epoch = 47, test_sub = NULL, n_its = 500, fold = 1, st=TRUE, stan=TRUE,
                    return_threshold=FALSE){

  # browser()

  train_pred = in_list$train[[fold]][[epoch]][["pred"]]
  train_label = in_list$train[[fold]][[epoch]][["target"]]

  val_pred = in_list$val[[fold]][[epoch]][["pred"]]
  val_label = in_list$val[[fold]][[epoch]][["target"]]

  if(is.null(test_sub)){
    test_pred = in_list$test[[fold]][[epoch]][["pred"]]
    test_label = in_list$test[[fold]][[epoch]][["target"]]
  } else{
    test_pred = in_list$test[[fold]][[epoch]][["pred"]][in_list$test[[fold]][[epoch]][["id"]] %in% test_sub]
    test_label = in_list$test[[fold]][[epoch]][["target"]][in_list$test[[fold]][[epoch]][["id"]] %in% test_sub]
  }


  threshold = find_sensitivity_thresh(pred = in_list[[thresh_set_split]][[fold]][[epoch]][["pred"]],
                                      label = in_list[[thresh_set_split]][[fold]][[epoch]][["target"]],
                                      sensitivity = sensitivity)

  # print(paste0(thresh_set_split, " threshold for epoch ", epoch, " fold ", fold, ": ", threshold))

  train_fnr = sum(train_label[train_pred < threshold])/sum(train_label)
  val_fnr = sum(val_label[val_pred < threshold])/sum(val_label)
  test_fnr = sum(test_label[test_pred < threshold])/sum(test_label)

  out_list = list("train" = list("fnr" = train_fnr),
                  "val" = list("fnr" = val_fnr),
                  "test" = list("fnr" = test_fnr))
  train_fnrs = c()
  val_fnrs = c()
  test_fnrs = c()

  if(st){
    st_pred = in_list$st[[fold]][[epoch]][["pred"]]
    st_label = in_list$st[[fold]][[epoch]][["target"]]
    st_fnr = sum(st_label[st_pred < threshold])/sum(st_label)
    out_list[["st"]] = list("fnr" = st_fnr)
    st_fnrs = c()
  }
  if(stan){
    stan_pred = in_list$stan[[fold]][[epoch]][["pred"]]
    stan_label = in_list$stan[[fold]][[epoch]][["target"]]
    stan_fnr = sum(stan_label[stan_pred < threshold])/sum(stan_label)
    out_list[["stan"]] = list("fnr" = stan_fnr)
    stan_fnrs = c()
  }

  for(it in 1:n_its){

    train_sample = sample(1:length(train_pred),size = length(train_pred), replace = TRUE)
    val_sample = sample(1:length(val_pred),size = length(val_pred), replace = TRUE)
    test_sample = sample(1:length(test_pred),size = length(test_pred), replace = TRUE)

    train_pred_s = train_pred[train_sample]
    train_label_s = train_label[train_sample]

    val_pred_s = val_pred[val_sample]
    val_label_s = val_label[val_sample]

    test_pred_s = test_pred[test_sample]
    test_label_s = test_label[test_sample]

    train_fnrs = c(sum(train_label_s[train_pred_s < threshold])/sum(train_label_s), train_fnrs)
    val_fnrs = c(sum(val_label_s[val_pred_s < threshold])/sum(val_label_s), val_fnrs)
    test_fnrs = c(sum(test_label_s[test_pred_s < threshold])/sum(test_label_s), test_fnrs)

    if(st){
      st_sample = sample(1:length(st_pred),size = length(st_pred), replace = TRUE)
      st_pred_s = st_pred[st_sample]
      st_label_s = st_label[st_sample]
      st_fnrs = c(sum(st_label_s[st_pred_s < threshold])/sum(st_label_s), st_fnrs)
    }
    if(stan){
      stan_sample = sample(1:length(stan_pred),size = length(stan_pred), replace = TRUE)
      stan_pred_s = stan_pred[stan_sample]
      stan_label_s = stan_label[stan_sample]
      stan_fnrs = c(sum(stan_label_s[stan_pred_s < threshold])/sum(stan_label_s), stan_fnrs)
    }

  }

  train_fnrs = sort(train_fnrs)
  val_fnrs = sort(val_fnrs)
  test_fnrs = sort(test_fnrs)

  out_list[["train"]][["ci"]] = quantile(train_fnrs, c(0.025, 0.975))
  out_list[["val"]][["ci"]] = quantile(val_fnrs, c(0.025, 0.975))
  out_list[["test"]][["ci"]] = quantile(test_fnrs, c(0.025, 0.975))

  if(st){
    out_list[["st"]][["ci"]] = quantile(st_fnrs, c(0.025, 0.975))
  }
  if(stan){
    out_list[["stan"]][["ci"]] = quantile(stan_fnrs, c(0.025, 0.975))
  }

  if(return_threshold){
    return(threshold)
  } else{
    return(out_list)
  }
}

## DATA LIST:
##  - train/val/test
##  - fold
##  - epoch
##  - id, prediction, target

create_fnr_graph_df = function(in_list, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                               test_set_sub = NULL, groups=c("train","val","test","st","stan")){
  df_names = c("split","epoch","fnr","lcl","ucl")
  out_df = data.frame(matrix(nrow = 0, ncol = length(df_names)))

  n_epochs = length(in_list[["train"]][[fold]])


  # browser()

  for(i in 1:n_epochs){
    my_fnrs = get_fnrs(in_list=in_list, sensitivity = sensitivity, thresh_set_split = thresh_set_split,
                       epoch = i, test_sub = test_set_sub, n_its = n_its, fold = fold)
    for(group in groups){
      my_row = c(group,i,my_fnrs[[group]]$fnr, my_fnrs[[group]]$ci[1], my_fnrs[[group]]$ci[2])
      out_df = rbind(out_df, my_row)

    }
  }
  names(out_df) = df_names
  out_df$fnr = as.numeric(out_df$fnr)
  out_df$lcl = as.numeric(out_df$lcl)
  out_df$ucl = as.numeric(out_df$ucl)
  out_df$epoch = as.numeric(out_df$epoch)

  return(out_df)
}



create_tnr_graph_df = function(in_list, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                               test_set_sub = NULL, groups=c("train","val","test","st","stan")){
  df_names = c("split","epoch","tnr","lcl","ucl")
  out_df = data.frame(matrix(nrow = 0, ncol = length(df_names)))

  n_epochs = length(in_list[["train"]][[fold]])


  # browser()

  for(i in 1:n_epochs){
    my_fnrs = get_tnrs(in_list=in_list, sensitivity = sensitivity, thresh_set_split = thresh_set_split,
                       epoch = i, test_sub = test_set_sub, n_its = n_its, fold = fold)
    for(group in groups){
      my_row = c(group,i,my_fnrs[[group]]$tnr, my_fnrs[[group]]$ci[1], my_fnrs[[group]]$ci[2])
      out_df = rbind(out_df, my_row)

    }
  }
  names(out_df) = df_names
  out_df$tnr = as.numeric(out_df$tnr)
  out_df$lcl = as.numeric(out_df$lcl)
  out_df$ucl = as.numeric(out_df$ucl)
  out_df$epoch = as.numeric(out_df$epoch)

  return(out_df)
}


create_fnr_tnr_all_folds = function(in_list, sensitivity = 0.95, thresh_set_split = "val", n_its = 500,
                                    test_set_sub = NULL, groups=c("train","val","test","st","stan")){

  df_names = c("split","epoch","value","lcl","ucl", "metric","fold")
  out_df = data.frame(matrix(nrow = 0, ncol = length(df_names)))

  for(i in 1:length(in_list[["train"]])){
    fnr_dfs = create_fnr_graph_df(in_list=in_list, sensitivity = sensitivity, thresh_set_split = thresh_set_split,
                                  n_its = n_its, fold = i, test_set_sub = test_set_sub, groups=groups)
    fnr_dfs$metric = "FNR"
    fnr_dfs$fold = i
    names(fnr_dfs)[3] = "value"

    tnr_dfs = create_tnr_graph_df(in_list=in_list, sensitivity = sensitivity, thresh_set_split = thresh_set_split,
                                  n_its = n_its, fold = i, test_set_sub = test_set_sub, groups=groups)
    tnr_dfs$metric = "TNR"
    tnr_dfs$fold = i
    names(tnr_dfs)[3] = "value"

    out_df = rbind(out_df, fnr_dfs,tnr_dfs)
  }

  sen_df = out_df[out_df$metric == "FNR",]
  sen_df$value = 1-sen_df$value
  sen_df$lcl = 1-sen_df$lcl
  sen_df$ucl = 1-sen_df$ucl
  sen_df$metric = "Sensitivity"

  spec_df = out_df[out_df$metric == "TNR",]
  spec_df$metric = "Specificiity"

  out_df = rbind(spec_df,sen_df)

  return(out_df)
}

####
####  @ LE TO DO!! (BELOW) WRITE FUNCTION FOR POOLED SEN/SPEC
####

create_fnr_tnr_pooled_folds = function(in_list, sensitivity = 0.95, thresh_set_split = "val", epoch,
                                       n_its = 500, test_set_sub = NULL, groups=c("train","val","test","st","stan")){
  
  df_names = c("split","epoch","value","lcl","ucl", "metric","fold")
  out_df = data.frame(matrix(nrow = 0, ncol = length(df_names)))  
  
  for(i in 1:length(in_list[["train"]])){
    fnr_dfs = create_fnr_graph_df(in_list=in_list, sensitivity = sensitivity, thresh_set_split = thresh_set_split, 
                                  n_its = n_its, fold = i, test_set_sub = test_set_sub, groups=groups)
    fnr_dfs$metric = "FNR"
    fnr_dfs$fold = i
    names(fnr_dfs)[3] = "value"
    
    tnr_dfs = create_tnr_graph_df(in_list=in_list, sensitivity = sensitivity, thresh_set_split = thresh_set_split, 
                                  n_its = n_its, fold = i, test_set_sub = test_set_sub, groups=groups)
    tnr_dfs$metric = "TNR"
    tnr_dfs$fold = i
    names(tnr_dfs)[3] = "value"
    
    out_df = rbind(out_df, fnr_dfs,tnr_dfs)    
  }
  
  sen_df = out_df[out_df$metric == "FNR",]
  sen_df$value = 1-sen_df$value
  sen_df$lcl = 1-sen_df$lcl
  sen_df$ucl = 1-sen_df$ucl
  sen_df$metric = "Sensitivity"
  
  spec_df = out_df[out_df$metric == "TNR",]
  spec_df$metric = "Specificiity"
  
  out_df = rbind(spec_df,sen_df)
  
  return(out_df)
}

create_auc_test_df_all_folds = function(in_list, num_its=500){

  # browser()

  list_df = data.frame(matrix(nrow = 0, ncol = 6))
  names(list_df) = c("split","epoch","auc","auc_lcl","auc_ucl", "fold")

  for(fold in 1:length(in_list)){
    out_df = create_auc_test_df(in_list=in_list, num_its = num_its, fold=fold)
    out_df$fold = fold

    list_df = rbind(list_df,out_df)
  }

  return(list_df)
}

create_auc_test_df = function(in_list, num_its=500, fold = 1){

  # browser()

  list_df = data.frame(matrix(nrow = 0, ncol = 5))
  names(list_df) = c("split","epoch","auc","auc_lcl","auc_ucl")

  for(i in 1:length(in_list)){
    if(!is.null(names(in_list))){
      split_name = names(in_list)[i]
    } else{
      split_name = paste0("Split:",i)
    }

    for(epoch in 1:length(in_list[[i]][[fold]])){
      auc_list = get_auc_ci(true_vals = in_list[[i]][[fold]][[epoch]]$target,
                            in_list[[i]][[fold]][[epoch]]$pred,
                            n_its = num_its)
      auc_val = auc_list$auc
      my_lcl = auc_list$ci[1]
      my_ucl = auc_list$ci[2]

      my_row = data.frame(split=split_name, epoch, auc=auc_val, lcl=my_lcl, ucl=my_ucl)

      list_df = rbind(list_df,my_row)
    }

  }

  return(list_df)
}

prep_st_data = function(st_dat, most_recent=FALSE){

  st_dat$r_surg = 0
  st_dat$r_surg[st_dat$Surgery.Indicated == 1 & st_dat$Surgery.Side == 2] = 1

  st_dat$l_surg = 0
  st_dat$l_surg[st_dat$Surgery.Indicated == 1 & st_dat$Surgery.Side == 1] = 1

  st_dat_long_L = reshape(st_dat[st_dat$Side %in% c(1,3),], idvar = c("ST.Study.ID","PHN.ID"),
                          direction = "long",
                          varying = list(c("Prehdict.L","X.2.Prehdict.L",
                                           "X.3.Prehdict.L","X.4.Prehdict.L",
                                           "X.5.Prehdict.L","X.6.Prehdict.L",
                                           "X.7.Prehdict.L","X.8.Prehdict.L",
                                           "X.9.Prehdict.L","X.10.Prehdict.L",
                                           "X.11.Prehdict.L","X.12.Prehdict.L",
                                           "X.13.Prehdict.L"),
                                         c("Date.US.1","Date.US.2","Date.US.3","Date.US.4",
                                           "Date.US.5","Date.US.6","Date.US.7","Date.US.8",
                                           "Date.US.9","Date.US.10","Date.US.11","Date.US.12",
                                           "Date.US.13"),
                                         c("Machine...13", "Machine...22", "Machine...31",
                                           "Machine...40", "Machine...49", "Machine...58",
                                           "Machine...67", "Machine...76", "Machine...84",
                                           "Machine...92","Machine...100","Machine...108",
                                           "Machine...116"),
                                         c("ApD.L","X.2.ApD.L","X.3.ApD.L","X.4.ApD.L","X.5.ApD.L",
                                           "X.6.ApD.L","X.7.ApD.L","X.8.ApD.L","X.9.ApD.L","X.10.ApD.L",
                                           "X.11.ApD.L","X.12.ApD.L","X.13.ApD.L"),
                                         c("SFU.L","X.2.SFU.L", "X.3.SFU.L","X.4.SFU.L","X.5.SFU.L",
                                           "X.6.SFU.L","X.7.SFU.L","X.8.SFU.L","X.9.SFU.L","X.10.SFU.L",
                                           "X.11.SFU.L","X.12.SFU.L","X.13.SFU.L")),
                          timevar = "US_num",
                          v.names = c("Prehdict","US_date", "US_machine","ApD","SFU"))

  st_dat_long_L$surg = st_dat_long_L$l_surg
  st_dat_long_L$view_side = "Left"

  st_dat_long_R = reshape(st_dat[st_dat$Side %in% c(2,3),], idvar = c("ST.Study.ID","PHN.ID"),
                          direction = "long",
                          varying = list(c("Prehdict.L","X.2.Prehdict.L",
                                           "X.3.Prehdict.L","X.4.Prehdict.L",
                                           "X.5.Prehdict.L","X.6.Prehdict.L",
                                           "X.7.Prehdict.L","X.8.Prehdict.L",
                                           "X.9.Prehdict.L","X.10.Prehdict.L",
                                           "X.11.Prehdict.L","X.12.Prehdict.L",
                                           "X.13.Prehdict.L"),
                                         c("Date.US.1","Date.US.2","Date.US.3","Date.US.4",
                                           "Date.US.5","Date.US.6","Date.US.7","Date.US.8",
                                           "Date.US.9","Date.US.10","Date.US.11","Date.US.12",
                                           "Date.US.13"),
                                         c("Machine...13", "Machine...22", "Machine...31",
                                           "Machine...40", "Machine...49", "Machine...58",
                                           "Machine...67", "Machine...76", "Machine...84",
                                           "Machine...92","Machine...100","Machine...108",
                                           "Machine...116"),
                                         c("ApD.R","X.2.ApD.R","X.3.ApD.R","X.4.ApD.R","X.5.ApD.R",
                                           "X.6.ApD.R","X.7.ApD.R","X.8.ApD.R","X.9.ApD.R","X.10.ApD.R",
                                           "X.11.ApD.R","X.12.ApD.R","X.13.ApD.R"),
                                         c("SFU.R","X.2.SFU.R", "X.3.SFU.R","X.4.SFU.R","X.5.SFU.R",
                                           "X.6.SFU.R","X.7.SFU.R","X.8.SFU.R","X.9.SFU.R","X.10.SFU.R",
                                           "X.11.SFU.R","X.12.SFU.R","X.13.SFU.R")),
                          timevar = "US_num",
                          v.names = c("Prehdict","US_date", "US_machine","ApD","SFU"))

  st_dat_long_R$surg = st_dat_long_R$r_surg
  st_dat_long_R$view_side = "Right"

  ol_cols = c("ST.Study.ID","PHN.ID","Sex","surg","SFU","Postal.Code","Prehdict","view_side",
              "US_num", "US_machine","ApD","US_date","DOB","Etiology","Staff.MD","Side","Who.Indicated.Sx")

  rl_st_dat_long = rbind(st_dat_long_L[,ol_cols],st_dat_long_R[,ol_cols])
  rl_st_dat_long$PC_region = substr(rl_st_dat_long$Postal.Code,start = 1,stop = 1)
  rl_st_dat_long$Prehdict = as.numeric(rl_st_dat_long$Prehdict)
  rl_st_dat_long = rl_st_dat_long[!is.na(rl_st_dat_long$Prehdict),]
  rl_st_dat_long = rl_st_dat_long[rl_st_dat_long$Prehdict < 1 & rl_st_dat_long$Prehdict > 0,]
  rl_st_dat_long$age_at_US_days = rl_st_dat_long$US_date - rl_st_dat_long$DOB
  rl_st_dat_long$age_at_US_wk = difftime(rl_st_dat_long$US_date, rl_st_dat_long$DOB,units="weeks")

  study_pts = unique(rl_st_dat_long$ST.Study.ID)

  rl_st_dat_long$age_groups = NA
  rl_st_dat_long$age_groups[rl_st_dat_long$age_at_US_days < 830] = "under_2"
  rl_st_dat_long$age_groups[rl_st_dat_long$age_at_US_days >= 830 & rl_st_dat_long$age_at_US_days < 1825] = "age_2_to_5"
  rl_st_dat_long$age_groups[rl_st_dat_long$age_at_US_days >= 1825] = "over_5"

  rl_st_dat_long$ApD = as.numeric(rl_st_dat_long$ApD)
  rl_st_dat_long$apd_groups = NA
  rl_st_dat_long$apd_groups[rl_st_dat_long$ApD <= 6] = "under_6"
  rl_st_dat_long$apd_groups[rl_st_dat_long$ApD <= 9 & rl_st_dat_long$ApD > 6] = "apd_6_to_9"
  # rl_st_dat_long$apd_groups[rl_st_dat_long$ApD <= 15 & rl_st_dat_long$ApD > 9] = "apd_9_to_15"
  # rl_st_dat_long$apd_groups[rl_st_dat_long$ApD > 15] = "over_15"

  rl_st_dat_long$apd_groups[rl_st_dat_long$ApD <= 14 & rl_st_dat_long$ApD > 9] = "apd_9_to_14"
  rl_st_dat_long$apd_groups[rl_st_dat_long$ApD > 14] = "over_14"


  names(rl_st_dat_long)[1] = "ID"

  if(most_recent){
    most_recent_st = data.frame(matrix(nrow = 0, ncol = ncol(rl_st_dat_long)))

    for(id in unique(rl_st_dat_long$ID)){
      id_df = rl_st_dat_long[rl_st_dat_long$ID == id,]
      max_vis_num = max(id_df$US_num)
      id_max_df = id_df[id_df$US_num == max_vis_num,]

      most_recent_st = rbind(most_recent_st,id_max_df)
    }

    return(most_recent_st)

  } else{
    return(rl_st_dat_long)
  }

}

add_platt_preds = function(in_list, scale_ref = "val", epoch = 1,
                           sensitivity = 0.95){
  in_list_names = names(in_list)
  to_scale = in_list_names[in_list_names != scale_ref]

  folds = length(in_list[[scale_ref]])

  ## creating and applying scaling model
  for(fold in 1:folds){

    threshold = get_fnrs(in_list = in_list, epoch = epoch,fold = fold, return_threshold=TRUE,sensitivity = sensitivity)

    # browser()

    scale_set = data.frame(label = in_list[[scale_ref]][[fold]][[epoch]][["target"]],
                           pred = in_list[[scale_ref]][[fold]][[epoch]][["pred"]])

    scale_mod = glm(label ~ pred, scale_set, family = binomial)

    print(summary(scale_mod))

    in_list[[scale_ref]][[fold]][[epoch]][["pred_scaled"]] = predict(scale_mod, newdata = data.frame(pred = in_list[[scale_ref]][[fold]][[epoch]][["pred"]]),type = "response")
    in_list[[scale_ref]][[fold]][[epoch]][["threshold"]] = predict(scale_mod, newdata = data.frame(pred = threshold), type = "response")

    for(group in to_scale){
      in_list[[group]][[fold]][[epoch]][["pred_scaled"]] = predict(scale_mod, newdata = data.frame(pred = in_list[[group]][[fold]][[epoch]][["pred"]]),type = "response")
    }
  }

  # browser()

  return(in_list)
}

### 
###    SICKKIDS DATA
###

st_df = read.csv("C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial/SilentTrial_Datasheet.csv",header = TRUE, as.is = TRUE)
### ISOLATING MOST RECENT OBSERVATIONS
head(st_df)
st_df$img_id = paste0("STOID",st_df$ID, "_", st_df$view_side, "_", st_df$US_num)
most_recent_st = data.frame(matrix(nrow = 0, ncol = ncol(st_df)))
for(id in unique(st_df$ID)){
  id_df = st_df[st_df$ID == id,]
  max_vis_num = max(id_df$US_num)
  id_max_df = id_df[id_df$US_num == max_vis_num,]
  
  most_recent_st = rbind(most_recent_st,id_max_df)
}
head(most_recent_st)

## No test data -- model used to test Stanford and Silent trial
raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_TrainOnly_47epochs_bs16_lr0.001_RCFalse_covTrue_OSFalse_20210531.json")

## Best results: no ordered split -- model re-trained to use to test Stanford and Silent trial data
# raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_47epochs_bs16_lr0.001_RCTrue_covTrue_OSFalse_20210414.json")

# raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_47epochs_bs16_lr0.001_RCTrue_covTrue_OSTrue_20210413_st_stan.json")
# raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_47epochs_bs16_lr0.001_RCTrue_covTrue_OSTrue.json")
raw_in2 = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_47epochs_bs16_lr0.001_RCTrue_covFalse_OSTrue.json")

test = v8()
test$assign("dat", JS(raw_in))
best_model_output = test$get("dat")

test2 = v8()
test2$assign("dat", JS(raw_in2))
nocov_model_output = test2$get("dat")

## DATA LIST: 
##  - train/val/test
##  - fold
##  - epoch
##  - id, prediction, target

length(best_model_output[[1]][[1]])
names(best_model_output)
length(best_model_output$train)
names(best_model_output$train[[1]][["46"]])
best_model_output$test[[1]][["46"]][["target"]]
best_model_output$test[[1]][["46"]][["id"]]

identical(best_model_output$test[[1]][["46"]][["id"]], best_model_output$test[[3]][["46"]][["id"]])

find_sensitivity_thresh(pred =  best_model_output$train[[1]][["46"]][["pred"]],
                        label = best_model_output$train[[1]][["46"]][["target"]])

find_sensitivity_thresh(pred =  best_model_output$val[[1]][["46"]][["pred"]],
                        label = best_model_output$val[[1]][["46"]][["target"]])



##
##   PLOT FNRs
##

## best model
best_mod_fnrs = create_fnr_graph_df(in_list = best_model_output, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                                    test_set_sub = NULL)
best_mod_fnrs2 = create_fnr_graph_df(in_list = best_model_output, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                                     test_set_sub = NULL)
str(best_mod_fnrs2)

aggregate(best_mod_fnrs2$fnr, by = list(best_mod_fnrs2$split), min)
aggregate(best_mod_fnrs2$fnr, by = list(best_mod_fnrs2$split), function(x) which(x == min(x)))

theme_set(
  theme_bw(base_size = 20)
)

ggplot(best_mod_fnrs2, aes(x = epoch, y = fnr, col = split)) + 
  geom_line(size = 1.1) + ylab("FNR") + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + theme(title = element_text("With Cov")) + 
  geom_abline(intercept = 0.1, slope = 0, lwd = 1.2, lty = 2, col = "red")

## no cov model
nocov_mod_fnrs = create_fnr_graph_df(in_list = nocov_model_output, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                                     test_set_sub = NULL)

aggregate(nocov_mod_fnrs$fnr, by = list(best_mod_fnrs$split), min)
aggregate(nocov_mod_fnrs$fnr, by = list(best_mod_fnrs$split), function(x) which(x == min(x)))

theme_set(
  theme_bw(base_size = 20)
)
ggplot(nocov_mod_fnrs, aes(x = epoch, y = fnr, col = split)) + 
  geom_line(size = 1.1) + ylab("FNR") + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + theme(title = element_text("With Cov"))


##
##   PLOT TNRs
##

## best model
best_mod_tnrs2 = create_tnr_graph_df(in_list = best_model_output, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                                     test_set_sub = NULL)
str(best_mod_tnrs2)

aggregate(best_mod_tnrs2$tnr, by = list(best_mod_tnrs2$split), max)
aggregate(best_mod_tnrs2$tnr, by = list(best_mod_tnrs2$split), function(x) which(x == max(x)))

theme_set(
  theme_bw(base_size = 20)
)

ggplot(best_mod_tnrs2, aes(x = epoch, y = tnr, col = split)) + 
  geom_line(size = 1.1) + ylab("TNR") + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + theme(title = element_text("With Cov"))

## no cov model
nocov_mod_fnrs = create_fnr_graph_df(in_list = nocov_model_output, sensitivity = 0.95, thresh_set_split = "val", n_its = 500, fold = 1,
                                     test_set_sub = NULL)

aggregate(nocov_mod_fnrs$fnr, by = list(best_mod_fnrs$split), min)
aggregate(nocov_mod_fnrs$fnr, by = list(best_mod_fnrs$split), function(x) which(x == min(x)))

theme_set(
  theme_bw(base_size = 20)
)
ggplot(nocov_mod_fnrs, aes(x = epoch, y = fnr, col = split)) + 
  geom_line(size = 1.1) + ylab("FNR") + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + theme(title = element_text("With Cov"))


##
##   PLOT TNRs + FNRs
##

best_fnr_tnr = create_fnr_tnr_all_folds(in_list = best_model_output, sensitivity = 0.95, thresh_set_split = "val", n_its = 500,
                                        test_set_sub = NULL, groups=c("train","val","st","stan"))

head(best_fnr_tnr)

ggplot(best_fnr_tnr, aes(x = epoch, y = value, col = split)) + 
  geom_line(size = 1.1) + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + facet_grid(metric ~ .)

best_fnr_tnr[best_fnr_tnr$epoch == 30,]

best_fnr_tnr[best_fnr_tnr$epoch == 33,]


## TRAIN/VAL/TEST only 
best_tr_val_test = best_fnr_tnr[best_fnr_tnr$split %in% c("train","val","test"),]
best_val_test = best_fnr_tnr[best_fnr_tnr$split %in% c("val","test"),]

ggplot(best_val_test, aes(x = epoch, y = value, col = split)) + 
  geom_line(size = 1.1) + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + theme(title = element_text("With Cov")) + facet_grid(metric ~ fold)

best_val_test_agg = aggregate(best_val_test$value, list(best_val_test$split, best_val_test$metric, best_val_test$epoch), mean)
names(best_val_test_agg) = c("DataSplit","Metric","Epoch","x")
head(best_val_test_agg)
min(best_val_test_agg$x)

best_val_test_agg[best_val_test_agg$x == max(best_val_test_agg$x), ]
best_val_test_agg[best_val_test_agg$x == min(best_val_test_agg$x), ]

best_val_test_agg[best_val_test_agg$Epoch == 30, ]
best_val_test_agg[best_val_test_agg$Epoch == 33, ]

ggplot(best_val_test_agg, aes(x = Epoch, y = x, col = DataSplit)) + 
  geom_line(size = 1.1) + ylim(0,1) + 
  geom_point(size = 2) + theme(title = element_text("With Cov")) + facet_grid(~ Metric)


##
##   SUMMARIZE PERFORMANCE AT EPOCH 30 AND EPOCH 33
##


best_fnr_tnr[best_fnr_tnr$epoch == 1 & best_fnr_tnr$metric == "FNR",]
best_fnr_tnr[best_fnr_tnr$epoch == 1 & best_fnr_tnr$metric == "TNR",]

best_fnr_tnr[best_fnr_tnr$epoch == 33 & best_fnr_tnr$metric == "FNR",]
best_fnr_tnr[best_fnr_tnr$epoch == 33 & best_fnr_tnr$metric == "TNR",]


##
##   PLOT AUROCs
##

best_model_aucs_df = create_auc_test_df_all_folds(in_list = best_model_output)

ggplot(best_model_aucs_df, aes(x = epoch, y = auc, col = split)) + 
  geom_line(size = 1.1) + ylim(0,1) + 
  geom_errorbar(size = 1, aes(ymin = lcl, ymax = ucl)) + 
  geom_point(size = 2) + theme(title = element_text("With Cov")) + facet_grid( ~ fold)


### EXPLORE OTHER FOLDS? CURRENTLY ONLY 1ST FOLD
### ^ CREATE SAME GRAPH AND FUNCTIONS FOR FNR? 

##
##   COMPUTE FNRs
##


### READ IN CLINICAL INFO ; SUBSET HN ONLY, R/L KIDNEY, ETC
### GET CI's
test = get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 11)

get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 11)
get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 15)
get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 25)
get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 27)
get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 35)
get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 45)
get_fnrs(best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 47)


get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 11)
get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 15)
get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 25)
get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 27)
get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 35)
get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 45)
get_fnrs(nocov_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 47)



##
##   PLOTTING TEST DATA AT EPOCH 30 AND EPOCH 33
##

## COMPUTE FOR SPECIIFIC SUBSETS OF TEST INDIVIDUALS

## Best results: no ordered split -- model re-trained to use to test Stanford and Silent trial data
raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_47epochs_bs16_lr0.001_RCTrue_covTrue_OSFalse_20210414.json")
sk_test = v8()
sk_test$assign("dat", JS(raw_in))
sk_test_best_model_output = sk_test$get("dat")

table(c(sk_test_best_model_output$train[[1]][[30]]$target,sk_test_best_model_output$val[[1]][[30]]$target))

test_mod0_pred = list()
test_mod1_pred = list()

for(i in 1:5){
  test_mod0_pred[[i]] = sk_test_best_model_output$test[[i]][["30"]]$pred
  test_mod1_pred[[i]] = sk_test_best_model_output$test[[i]][["33"]]$pred
}

id = sk_test_best_model_output$test[[1]][["0"]]$id
target = sk_test_best_model_output$test[[1]][["0"]]$target

preds_mod0 = data.frame(Reduce(cbind,test_mod0_pred))
preds_mod1 = data.frame(Reduce(cbind,test_mod1_pred))

names(preds_mod0) = paste0("pred_mod0_epoch",1:5)
names(preds_mod1) = paste0("pred_mod1_epoch",1:5)

test_pred_df = cbind(id, target, preds_mod0, preds_mod1)
table(test_pred_df$target)

epoch = 33
scaled_sk_test_best_model_output = add_platt_preds(in_list = sk_test_best_model_output, epoch = epoch)

fold = 1
# get_fnrs(scaled_sk_test_best_model_output,thresh_set_split = "val",sensitivity = 0.95, epoch = 33,fold = fold)
## scale threshold! -- maybe return this in the function above?   
out_thresh = scaled_sk_test_best_model_output[["val"]][[fold]][[epoch]][["threshold"]]
plot_df_33 = data.frame(pred = scaled_sk_test_best_model_output[["test"]][[fold]][[epoch]][["pred_scaled"]], 
                        target = scaled_sk_test_best_model_output[["test"]][[fold]][[epoch]][["target"]])
plot_df_33$target = factor(plot_df_33$target, levels = c(0, 1), labels = c("no Obstruction", "Obstruction"))

ggplot(plot_df_33, aes(x = target, y = pred, fill = target)) + geom_violin() + 
  geom_hline(yintercept = out_thresh, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# write.csv(plot_df_33, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/test_mod1.csv")


##
##   PLOTTING ST DATA AT EPOCH 30 AND EPOCH 33
##

SENSITIVITY = 0.95

## No test data -- model usedto test Stanford and Silent trial
raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SickKids_origST_TrainOnly_40epochs_bs16_lr0.001_RCFalse_covTrue_OSFalse_v7.json")
test = v8()
test$assign("dat", JS(raw_in))
st_stan_test = test$get("dat")
st_stan_test = add_platt_preds(in_list = st_stan_test, epoch = 10,sensitivity = SENSITIVITY)
st_stan_test = add_platt_preds(in_list = st_stan_test, epoch = 13,sensitivity = SENSITIVITY)
st_stan_test = add_platt_preds(in_list = st_stan_test, epoch = 16,sensitivity = SENSITIVITY)
st_stan_test = add_platt_preds(in_list = st_stan_test, epoch = 30,sensitivity = SENSITIVITY)

identical(st_stan_test$st[[1]][[30]]$id,
          st_stan_test$st[[1]][[33]]$id)

st_preds = data.frame(st_pred_mod_ep10 = st_stan_test$st[[1]][[10]]$pred_scaled,
                      st_pred_mod_ep13 = st_stan_test$st[[1]][[13]]$pred_scaled,
                      st_pred_mod_ep16 = st_stan_test$st[[1]][[16]]$pred_scaled,
                      st_pred_mod_ep30 = st_stan_test$st[[1]][[30]]$pred_scaled,
                      id = st_stan_test$st[[1]][[30]]$id,
                      target = st_stan_test$st[[1]][[30]]$target)
st_preds$target = factor(st_preds$target, levels = c(0,1), labels = c("no Obstruction", "Obstruction"))
head(st_preds)

fold=1

(dat_thresh_ep10 = st_stan_test[["val"]][[fold]][[10]][["threshold"]])
ggplot(st_preds, aes(x = target, y = st_pred_mod_ep10, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep10, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep13 = st_stan_test[["val"]][[fold]][[13]][["threshold"]])
ggplot(st_preds, aes(x = target, y = st_pred_mod_ep13, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep13, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep16 = st_stan_test[["val"]][[fold]][[16]][["threshold"]])
ggplot(st_preds, aes(x = target, y = st_pred_mod_ep16, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep16, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

theme_set(
  theme_minimal(base_size = 20)
)
(dat_thresh_ep30 = st_stan_test[["val"]][[fold]][[30]][["threshold"]])
ggplot(st_preds, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep30) + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

write.csv(st_preds, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/st_mod0_mod1_20210613_v7.csv")



##
##   PLOTTING STAN DATA AT EPOCH 30 AND EPOCH 33
##
identical(st_stan_test$stan[[1]][[30]]$id,
          st_stan_test$stan[[1]][[33]]$id)

stan_preds = data.frame(stan_pred_mod_ep10 = st_stan_test$stan[[1]][[10]]$pred_scaled,
                      stan_pred_mod_ep13 = st_stan_test$stan[[1]][[13]]$pred_scaled,
                      stan_pred_mod_ep16 = st_stan_test$stan[[1]][[16]]$pred_scaled,
                      stan_pred_mod_ep30 = st_stan_test$stan[[1]][[30]]$pred_scaled,
                      id = st_stan_test$stan[[1]][[30]]$id,
                      target = st_stan_test$stan[[1]][[30]]$target)
stan_preds$target = factor(stan_preds$target, levels = c(0,1), labels = c("no Obstruction", "Obstruction"))
head(stan_preds)

(dat_thresh_ep10 = st_stan_test[["val"]][[fold]][[10]][["threshold"]])
ggplot(stan_preds, aes(x = target, y = stan_pred_mod_ep10, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep10, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep13 = st_stan_test[["val"]][[fold]][[13]][["threshold"]])
ggplot(stan_preds, aes(x = target, y = stan_pred_mod_ep13, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep13, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep16 = st_stan_test[["val"]][[fold]][[16]][["threshold"]])
ggplot(stan_preds, aes(x = target, y = stan_pred_mod_ep16, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep16, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep30 = st_stan_test[["val"]][[fold]][[30]][["threshold"]])
ggplot(stan_preds, aes(x = target, y = stan_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

write.csv(stan_preds, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/stan_mod0_mod1_20210613_v7.csv")

##
##   PLOTTING UIOWA DATA AT SELECTED EPOCHS
##
identical(st_stan_test$ui[[1]][[30]]$id,
          st_stan_test$ui[[1]][[33]]$id)

ui_preds = data.frame(ui_pred_mod_ep10 = st_stan_test$ui[[1]][[10]]$pred_scaled,
                        ui_pred_mod_ep13 = st_stan_test$ui[[1]][[13]]$pred_scaled,
                        ui_pred_mod_ep16 = st_stan_test$ui[[1]][[16]]$pred_scaled,
                        ui_pred_mod_ep30 = st_stan_test$ui[[1]][[30]]$pred_scaled,
                        id = st_stan_test$ui[[1]][[30]]$id,
                        target = st_stan_test$ui[[1]][[30]]$target)
ui_preds$target = factor(ui_preds$target, levels = c(0,1), labels = c("no Obstruction", "Obstruction"))
head(ui_preds)
table(ui_preds$target)

(dat_thresh_ep10 = st_stan_test[["val"]][[fold]][[10]][["threshold"]])
ggplot(ui_preds, aes(x = target, y = ui_pred_mod_ep10, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep10, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep13 = st_stan_test[["val"]][[fold]][[13]][["threshold"]])
ggplot(ui_preds, aes(x = target, y = ui_pred_mod_ep13, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep13, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep16 = st_stan_test[["val"]][[fold]][[16]][["threshold"]])
ggplot(ui_preds, aes(x = target, y = ui_pred_mod_ep16, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep16, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

(dat_thresh_ep30 = st_stan_test[["val"]][[fold]][[30]][["threshold"]])
ggplot(ui_preds, aes(x = target, y = ui_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = dat_thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction")

write.csv(ui_preds, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/ui_mod0_mod1_20210613_v7.csv")

