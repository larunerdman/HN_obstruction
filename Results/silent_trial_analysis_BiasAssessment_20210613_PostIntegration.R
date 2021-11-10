
library(readxl)
library(reshape2)
library(rjson)
library(RJSONIO)
library(V8)
library(ggplot2)

      ### FUNCTIONS

get_hn_only = function(in_df){
  left_df = in_df[in_df$Side == 1 & in_df$view_side == "Left",]
  right_df = in_df[in_df$Side == 2 & in_df$view_side == "Right",]
  both_df = in_df[in_df$Side == 3,]
  
  out_df = rbind(left_df, right_df, both_df)
  
  return(out_df)
}

get_sen_spec = function(score_vec, target, threshold){
  score0 = score_vec[target == 0]
  score1 = score_vec[target == 1]

  # browser()
  
  tnr = sum(score0 < threshold)/length(score0)
  fnr = sum(score1 < threshold)/length(score1)
  
  out_list = list("specificity" = list("est" = tnr),
                  "sensitivity" = list("est" = 1-fnr))
  return(out_list)
}

get_sen_spec_withCI = function(score_vec, target, threshold, n_it, out_vecs=FALSE){
  
  out_list = get_sen_spec(score_vec=score_vec,
                             target=target,
                             threshold=threshold)
  n = length(score_vec)
  
  sen_vec = c()
  spec_vec = c()
  for(i in 1:n_it){
   set.seed(i)
    
    # browser()
    samp = sample(1:n,size = n,replace = TRUE)
    true_tnr_fnr = get_sen_spec(score_vec=score_vec[samp],
                                target=target[samp],
                                threshold=threshold)
    sen_vec = c(sen_vec, true_tnr_fnr$sensitivity)
    spec_vec = c(spec_vec, true_tnr_fnr$specificity)
  }
  
  # browser()
  sen_vec = sort(unlist(sen_vec))
  spec_vec = sort(unlist(spec_vec))

  out_list[["sensitivity"]][["ci"]] = quantile(sen_vec,c(0.025, 0.975))
  out_list[["specificity"]][["ci"]] = quantile(spec_vec,c(0.025, 0.975))
  
  if(out_vecs){
    
    out_list[["sensitivity"]][["vec"]] = c(sen_vec, rep(NA, n_it - length(sen_vec)))
    out_list[["specificity"]][["vec"]] = c(spec_vec, rep(NA, n_it - length(spec_vec)))
    
    
  }
  
  return(out_list)
  
}  

sen_spec_split = function(score_vec, target, split_vec, threshold, n_it, out_vecs=FALSE){
  split_vals = unique(split_vec)
  
  out_list = list()
  
  for(split_val in split_vals){
    
    # browser()
    
    split_score = na.omit(score_vec[split_vec == split_val])
    split_target = na.omit(target[split_vec == split_val])
    
    out_list[[split_val]] = get_sen_spec_withCI(score_vec = split_score, 
                                                target = split_target, 
                                                threshold = threshold, 
                                                n_it = n_it, 
                                                out_vecs = out_vecs)
  }

  return(out_list)  
}

add_platt_preds = function(in_list, scale_ref = "val", epoch = 1){
  in_list_names = names(in_list)
  to_scale = in_list_names[in_list_names != scale_ref]
  
  folds = length(in_list[[scale_ref]])
  
  ## creating and applying scaling model
  for(fold in 1:folds){
    
    threshold = get_fnrs(in_list = in_list, epoch = epoch,fold = fold, return_threshold=TRUE)
    
    # browser()
    
    scale_set = data.frame(label = in_list[[scale_ref]][[fold]][[epoch]][["target"]], 
                           pred = in_list[[scale_ref]][[fold]][[epoch]][["pred"]])
    
    scale_mod = glm(label ~ pred, scale_set, family = binomial)    
    
    in_list[[scale_ref]][[fold]][[epoch]][["pred_scaled"]] = predict(scale_mod, newdata = data.frame(pred = in_list[[scale_ref]][[fold]][[epoch]][["pred"]]),type = "response")
    in_list[[scale_ref]][[fold]][[epoch]][["threshold"]] = predict(scale_mod, newdata = data.frame(pred = threshold), type = "response")
    
    for(group in to_scale){
      in_list[[group]][[fold]][[epoch]][["pred_scaled"]] = predict(scale_mod, newdata = data.frame(pred = in_list[[group]][[fold]][[epoch]][["pred"]]),type = "response")
    }
  }  
  
  # browser()
  
  return(in_list)
}

get_most_recent = function(in_dataset){
  
  out_dataset = data.frame(matrix(nrow = 0, ncol = ncol(in_dataset)))
  names(out_dataset) = names(in_dataset)
  
  for(id in unique(in_dataset$pt_id)){
    pt_df = in_dataset[in_dataset$pt_id == id, ]
    max_row = pt_df[pt_df$USNumber == max(as.numeric(pt_df$USNumber)),][1,]
    
    out_dataset = rbind(out_dataset,max_row)
  }
  
  names(out_dataset) = names(in_dataset) 
  
  return(out_dataset)
}


      ###
      ###     SILENT TRIAL DATA 
      ###


st_df = read.csv(file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/st_mod0_mod1_20210613_v3.csv")
st_details = read.csv("C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial/SilentTrial_Datasheet.csv",header = TRUE, as.is = TRUE)
head(st_df)
dim(st_df)
head(st_details)
st_details$STID = paste0("STID", st_details$ID, "_", st_details$view_side, "_", st_details$US_num, "_1")

st_merged = merge(st_df, st_details, by.x = "id", by.y = "STID")
head(st_merged)
st_merged$pt_id = unlist(lapply(strsplit(st_merged$id,"_"),function(x){x[1]}))
length(unique(st_merged$pt_id))

st_uniq = st_merged[!duplicated(st_merged$ID),]
table(st_uniq$target)

## confirming HN only 
st_hn_only = get_hn_only(in_df = st_merged)

st_hn_only$id_num = as.numeric(substr(st_hn_only$id,5,7))

st_hn_only$sex = factor(st_hn_only$Sex, levels = c(1,2), labels = c("male","female"))
st_hn_only$target01 = ifelse(st_hn_only$target == "no Obstruction", 0, 1)
st_hn_only$apd_groups = factor(st_hn_only$apd_groups, levels = c("under_6", "apd_6_to_9", "apd_9_to_14", "over_14", "unmeasured"))
st_hn_only$apd_groups[is.na(st_hn_only$apd_groups)] = "unmeasured"
st_hn_only$HN_Side_fac = factor(st_hn_only$Side, levels = 1:3, labels = c("Left", "Right", "Both"))
st_hn_only$USNumber = factor(st_hn_only$US_num, levels = sort(unique(st_hn_only$US_num)))

st_hn_only$age_grps = NA
st_hn_only$age_grps[st_hn_only$age_at_US_wk < 102] = "under2"
st_hn_only$age_grps[st_hn_only$age_at_US_wk >= 102 & 
                      st_hn_only$age_at_US_wk < 260] = "age2to5"
st_hn_only$age_grps[st_hn_only$age_at_US_wk >= 260] = "over5"
st_hn_only$age_grps_fac = factor(st_hn_only$age_grps, levels = c("under2","age2to5","over5"))

st_hn_only$pcode = toupper(substr(st_hn_only$Postal.Code,1,1))
st_hn_only$PostalCode = factor(st_hn_only$pcode, levels = c("M", "N", "L", "P", "K"))

st_hn_only$prep = ifelse(st_hn_only$id_num <= 300, "prep1", "prep2")
st_hn_only$prep = factor(st_hn_only$prep, levels = c("prep1","prep2"))

st_hn_only$US_machine[is.na(st_hn_only$US_machine)] = "Unknown"

### most recent only dataset: 
hn_only_most_recent = get_most_recent(in_dataset = st_hn_only)
head(hn_only_most_recent)

write.csv(hn_only_most_recent$ID[hn_only_most_recent$target01 == 1], file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial_surgery_ids.csv", row.names = F,quote=F)

## thresholds -- "silent_trial_analysis_20210512_Postintegration.R"
# thresh0 = 0.06991365
# thresh1 = 0.07683571

## thresholds -- "silent_trial_analysis_20210525_Postintegration.R"
# thresh0 = 0.0672798529267311
# thresh1 = 0.0584363117814064

## thresholds -- "silent_trial_analysis_20210526_Postintegration.R"
# thresh0 = 0.0448510348796844
# thresh1 = 0.0540983192622662


## thresholds -- "silent_trial_analysis_20210601_Postintegration.R"
# thresh_ep10 = 0.04172684
# thresh_ep13 = 0.09474803
# thresh_ep16 = 0.07822857
# thresh_ep30 = 0.08997483

## thresholds -- "silent_trial_analysis_20210601_Postintegration.R" v2
# thresh_ep10 = 0.04172684
# thresh_ep13 = 0.09474803
# thresh_ep16 = 0.04251918
# thresh_ep30 = 0.0769038

## thresholds -- "silent_trial_analysis_20210601_Postintegration.R" v3
    ## 95%
# thresh_ep10 = 0.05734023
# thresh_ep13 = 0.1368169
# thresh_ep16 = 0.04429787
# thresh_ep30 = 0.08489182
    ## 90%
thresh_ep10 = 0.064072
thresh_ep13 = 0.136817
thresh_ep16 = 0.06031923
thresh_ep30 = 0.08497446

## thresholds -- "silent_trial_analysis_20210601_Postintegration.R" v4
# thresh_ep10 = 0.07552522
# thresh_ep13 = 0.1358826
# thresh_ep16 = 0.08460248
# thresh_ep30 = 0.08553968

## thresholds -- "silent_trial_analysis_20210601_Postintegration.R" v5
# thresh_ep10 = 0.06011559
# thresh_ep13 = 0.1394527
# thresh_ep16 = 0.04456173
# thresh_ep30 = 0.09027933

## thresholds -- "silent_trial_analysis_20210601_Postintegration.R" v6
    ## 95%
# thresh_ep10 = 0.0573693
# thresh_ep13 = 0.1307468
# thresh_ep16 = 0.06627052
# thresh_ep30 = 0.07498052 
    ## 90%
# thresh_ep10 = 0.06915262
# thresh_ep13 = 0.1307482
# thresh_ep16 = 0.06941357
# thresh_ep30 = 0.07796597

## thresholds -- "silent_trial_analysis_20210601_Postintegration.R" v7
    ## 95%
# thresh_ep10 = 0.05704036
# thresh_ep13 = 0.1037747 
# thresh_ep16 = 0.05372948
# thresh_ep30 = 0.1047439 
    ## 90%
# thresh_ep10 = 0.07061297
# thresh_ep13 = 0.1038086
# thresh_ep16 = 0.05575772
# thresh_ep30 = 0.104746

####
#### SILENT TRIAL MOST-RECENT ONLY 
####

## overall 
get_sen_spec_withCI(score_vec = hn_only_most_recent$st_pred_mod_ep10, 
                    target = hn_only_most_recent$target01, 
                    threshold = thresh_ep10, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = hn_only_most_recent$st_pred_mod_ep13, 
                    target = hn_only_most_recent$target01, 
                    threshold = thresh_ep13, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = hn_only_most_recent$st_pred_mod_ep16, 
                    target = hn_only_most_recent$target01, 
                    threshold = thresh_ep16, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                    target = hn_only_most_recent$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500, out_vecs = FALSE)


# st_hn_only$st_pred_mod1[st_hn_only$target01 == 0 & st_hn_only$st_pred_mod1 < thresh1]


## prep
table(hn_only_most_recent$target01, hn_only_most_recent$prep)
table(hn_only_most_recent$prep)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$prep, 
               threshold = thresh_ep30, 
               n_it = 500)

test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$prep, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


# sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod1, 
#                target = hn_only_most_recent$target01, 
#                split_vec = hn_only_most_recent$prep, 
#                threshold = thresh1, 
#                n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_point() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~prep) 


## sex
table(hn_only_most_recent$target01, hn_only_most_recent$Sex)

table(hn_only_most_recent$Sex)
table(hn_only_most_recent$target)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$sex, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~sex)


test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$sex, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## apd groups
table(hn_only_most_recent$target01, hn_only_most_recent$apd_groups)
table(hn_only_most_recent$apd_groups)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$apd_groups, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~apd_groups)


test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$apd_groups, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

### FIND ANOTHER WAY TO GRAPH THIS 

sen$Metric = "Sensitivity"
spec$Metric = "Specificity"
sen_spec = rbind(sen,spec)
sen_spec$ApD = factor(sen_spec$variable,
                      levels = c("under_6","apd_6_to_9","apd_9_to_14","over_14"),
                      labels = c("<6","6-9","9-14",">14"))
ss_mean = aggregate(sen_spec$value,
                    by = list(sen_spec$ApD, 
                              sen_spec$Metric),
                    FUN=function(x){mean(na.omit(x))})
names(ss_mean) = c("ApD","Metric","mean")
ss_lcl = aggregate(sen_spec$value,
                   by = list(sen_spec$ApD, 
                             sen_spec$Metric),
                   FUN=function(x){max(0,mean(na.omit(x)) - 1.96*sd(na.omit(x)))})
names(ss_lcl) = c("ApD","Metric","lcl")
ss_ucl = aggregate(sen_spec$value,
                   by = list(sen_spec$ApD, 
                             sen_spec$Metric),
                   FUN=function(x){min(1, mean(na.omit(x)) + 1.96*sd(na.omit(x)))})
names(ss_ucl) = c("ApD","Metric","ucl")

ss = Reduce(function(x,y){merge(x,y,by = c("ApD","Metric"))},
            list(ss_mean, ss_lcl, ss_ucl))
# ss = ss[ss$USNumber %in% paste0("US",1:6),]

ggplot(ss, aes(x = ApD, y = mean, col = Metric)) + 
  geom_point(size=3) + ylim(0,1) + 
  geom_errorbar(size = 1.2, aes(ymin = lcl, ymax = ucl)) + facet_grid(~Metric)


## HN side
table(hn_only_most_recent$target01, hn_only_most_recent$Side)
table(hn_only_most_recent$Side)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$Side, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~HN_Side_fac)


test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$HN_Side_fac, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## kidney side
table(hn_only_most_recent$target01, hn_only_most_recent$view_side)
table(hn_only_most_recent$view_side)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$view_side, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~view_side)

test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$view_side, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## age
table(hn_only_most_recent$target01,hn_only_most_recent$age_grps_fac)
table(hn_only_most_recent$age_grps_fac)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$age_grps_fac, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~age_grps_fac)

test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$age_grps_fac, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## postal code 
table(hn_only_most_recent$target01, hn_only_most_recent$PostalCode)
table(hn_only_most_recent$PostalCode)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$PostalCode, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~PostalCode)

test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$PostalCode, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## US num -- MAKE FIGURE OF THIS FOR THE PAPER 
table(hn_only_most_recent$target01,hn_only_most_recent$USNumber)
table(hn_only_most_recent$USNumber)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$USNumber, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~USNumber)

test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$USNumber, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

sen$Metric = "Sensitivity"
spec$Metric = "Specificity"
sen_spec = rbind(sen,spec)
sen_spec$USNumber = gsub("X","US",sen_spec$variable)
ss_mean = aggregate(sen_spec$value,
                    by = list(sen_spec$USNumber, 
                              sen_spec$Metric),
                    FUN=function(x){mean(na.omit(x))})
names(ss_mean) = c("USNumber","Metric","mean")
ss_lcl = aggregate(sen_spec$value,
                   by = list(sen_spec$USNumber, 
                             sen_spec$Metric),
                   FUN=function(x){max(0,mean(na.omit(x)) - 1.96*sd(na.omit(x)))})
names(ss_lcl) = c("USNumber","Metric","lcl")
ss_ucl = aggregate(sen_spec$value,
                   by = list(sen_spec$USNumber, 
                             sen_spec$Metric),
                   FUN=function(x){min(1, mean(na.omit(x)) + 1.96*sd(na.omit(x)))})
names(ss_ucl) = c("USNumber","Metric","ucl")

ss = Reduce(function(x,y){merge(x,y,by = c("USNumber","Metric"))},
            list(ss_mean, ss_lcl, ss_ucl))
ss = ss[ss$USNumber %in% paste0("US",1:6),]

ggplot(ss, aes(x = USNumber, y = mean, col = Metric)) + 
  geom_point(size=3) + ylim(0,1) + 
  geom_errorbar(size = 1.2, aes(ymin = lcl, ymax = ucl)) + facet_grid(~Metric)


ggplot(sen_spec, aes(x = USNumber, y = value, fill = Metric)) + 
  geom_boxplot()



## US MACHINE
table(hn_only_most_recent$target01,hn_only_most_recent$US_machine)
table(hn_only_most_recent$US_machine)

sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
               target = hn_only_most_recent$target01, 
               split_vec = hn_only_most_recent$US_machine, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(hn_only_most_recent, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~US_machine)

test = sen_spec_split(score_vec = hn_only_most_recent$st_pred_mod_ep30, 
                      target = hn_only_most_recent$target01, 
                      split_vec = hn_only_most_recent$US_machine, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

sen$Metric = "Sensitivity"
spec$Metric = "Specificity"
sen_spec = rbind(sen,spec)
# sen_spec$US_machine = gsub("X","US",sen_spec$variable)
ss_mean = aggregate(sen_spec$value,
                    by = list(sen_spec$US_machine, 
                              sen_spec$Metric),
                    FUN=function(x){mean(na.omit(x))})
names(ss_mean) = c("US_machine","Metric","mean")
ss_lcl = aggregate(sen_spec$value,
                   by = list(sen_spec$US_machine, 
                             sen_spec$Metric),
                   FUN=function(x){max(0,mean(na.omit(x)) - 1.96*sd(na.omit(x)))})
names(ss_lcl) = c("US_machine","Metric","lcl")
ss_ucl = aggregate(sen_spec$value,
                   by = list(sen_spec$US_machine, 
                             sen_spec$Metric),
                   FUN=function(x){min(1, mean(na.omit(x)) + 1.96*sd(na.omit(x)))})
names(ss_ucl) = c("US_machine","Metric","ucl")

ss = Reduce(function(x,y){merge(x,y,by = c("US_machine","Metric"))},
            list(ss_mean, ss_lcl, ss_ucl))
ss = ss[ss$US_machine %in% paste0("US",1:6),]

ggplot(ss, aes(x = US_machine, y = mean, col = Metric)) + 
  geom_point(size=3) + ylim(0,1) + 
  geom_errorbar(size = 1.2, aes(ymin = lcl, ymax = ucl)) + facet_grid(~Metric)


ggplot(sen_spec, aes(x = US_machine, y = value, fill = Metric)) + 
  geom_boxplot()


## plot change in sen/spec 
head(hn_only_most_recent)

hn_only_most_recent = hn_only_most_recent[order(hn_only_most_recent$id_num),]

col_names = c("id_num","sen","sen_lcl","sen_ucl","spec","spec_lcl","spec_ucl")
plot_df = data.frame(matrix(nrow = 0, ncol = length(col_names)))



### EP30
for(id_num in unique(hn_only_most_recent$id_num)){
  df_sub = hn_only_most_recent[hn_only_most_recent$id_num <= id_num,]
  sen_spec = get_sen_spec_withCI(score_vec = df_sub$st_pred_mod_ep30,
                                 target = df_sub$target01,
                                 threshold = thresh_ep30, n_it = 500)
  in_vec = c(id_num, sen_spec$sensitivity$est,
             sen_spec$sensitivity$ci,
             sen_spec$specificity$est,
             sen_spec$specificity$ci)
  
  plot_df = rbind(plot_df, in_vec)
}

names(plot_df) = col_names
head(plot_df,n = 40)


ggplot(plot_df, aes(x = id_num, y = sen)) + 
  geom_line() +
  geom_errorbar(size = 1, aes(ymin = sen_lcl, ymax = sen_ucl)) + 
  geom_point(size = 2) + ylim(c(0,1)) + 
  ylab("Cummulative Sensitivity") + xlab("ID number")

ggplot(plot_df, aes(x = id_num, y = spec)) + 
  geom_line() +
  geom_errorbar(size = 1, aes(ymin = spec_lcl, ymax = spec_ucl)) + 
  geom_point(size = 2) + ylim(c(0,1)) + 
  ylab("Cummulative Specificity") + xlab("ID number")

### plotting ood level
ggplot(hn_only_most_recent, aes(x = id_num, y = ood_level)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + ylim(c(0,1))

### plotting ood avg
out = smooth::sma(hn_only_most_recent$ood_level, order = 20)
hn_only_most_recent$ood_avg = out$fitted

ggplot(hn_only_most_recent, aes(x = id_num, y = ood_avg)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + ylim(c(0,1))


####
#### SILENT TRIAL OVERALL
####


## overall 
get_sen_spec_withCI(score_vec = st_hn_only$st_pred_mod_ep10, 
                    target = st_hn_only$target01, 
                    threshold = thresh_ep10, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = st_hn_only$st_pred_mod_ep13, 
                    target = st_hn_only$target01, 
                    threshold = thresh_ep13, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = st_hn_only$st_pred_mod_ep16, 
                    target = st_hn_only$target01, 
                    threshold = thresh_ep16, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = st_hn_only$st_pred_mod_ep30, 
                    target = st_hn_only$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500, out_vecs = FALSE)


# st_hn_only$st_pred_mod1[st_hn_only$target01 == 0 & st_hn_only$st_pred_mod1 < thresh1]


## prep
table(st_hn_only$target01, st_hn_only$prep)
table(st_hn_only$prep)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$prep, 
               threshold = thresh_ep30, 
               n_it = 500)

test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$prep, 
               threshold = thresh_ep30, 
               n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


# sen_spec_split(score_vec = st_hn_only$st_pred_mod1, 
#                target = st_hn_only$target01, 
#                split_vec = st_hn_only$prep, 
#                threshold = thresh1, 
#                n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_point() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~prep) 


## sex
table(st_hn_only$target01, st_hn_only$Sex)


sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$sex, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~sex)


test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$sex, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## apd groups
table(st_hn_only$target01, st_hn_only$apd_groups)
table(st_hn_only$apd_groups)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$apd_groups, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~apd_groups)


test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$apd_groups, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

  ### FIND ANOTHER WAY TO GRAPH THIS 

sen$Metric = "Sensitivity"
spec$Metric = "Specificity"
sen_spec = rbind(sen,spec)
sen_spec$ApD = factor(sen_spec$variable,
                      levels = c("under_6","apd_6_to_9","apd_9_to_14","over_14"),
                      labels = c("<6","6-9","9-14",">14"))
ss_mean = aggregate(sen_spec$value,
                    by = list(sen_spec$ApD, 
                              sen_spec$Metric),
                    FUN=function(x){mean(na.omit(x))})
names(ss_mean) = c("ApD","Metric","mean")
ss_lcl = aggregate(sen_spec$value,
                   by = list(sen_spec$ApD, 
                             sen_spec$Metric),
                   FUN=function(x){max(0,mean(na.omit(x)) - 1.96*sd(na.omit(x)))})
names(ss_lcl) = c("ApD","Metric","lcl")
ss_ucl = aggregate(sen_spec$value,
                   by = list(sen_spec$ApD, 
                             sen_spec$Metric),
                   FUN=function(x){min(1, mean(na.omit(x)) + 1.96*sd(na.omit(x)))})
names(ss_ucl) = c("ApD","Metric","ucl")

ss = Reduce(function(x,y){merge(x,y,by = c("ApD","Metric"))},
            list(ss_mean, ss_lcl, ss_ucl))
# ss = ss[ss$USNumber %in% paste0("US",1:6),]

ggplot(ss, aes(x = ApD, y = mean, col = Metric)) + 
  geom_point(size=3) + ylim(0,1) + 
  geom_errorbar(size = 1.2, aes(ymin = lcl, ymax = ucl)) + facet_grid(~Metric)


## HN side
table(st_hn_only$target01, st_hn_only$Side)
table(st_hn_only$Side)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$Side, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~HN_Side_fac)


test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$HN_Side_fac, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
boxplot(sen$value~sen$variable)

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)
boxplot(spec$value~spec$variable)

ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## kidney side
table(st_hn_only$target01, st_hn_only$view_side)
table(st_hn_only$view_side)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$view_side, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~view_side)

test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$view_side, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## age
table(st_hn_only$target01,st_hn_only$age_grps_fac)
table(st_hn_only$age_grps_fac)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$age_grps_fac, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~age_grps_fac)

test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$age_grps_fac, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## postal code 
table(st_hn_only$target01, st_hn_only$PostalCode)
table(st_hn_only$PostalCode)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$PostalCode, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~PostalCode)

test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$PostalCode, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()


## US num -- MAKE FIGURE OF THIS FOR THE PAPER 
table(st_hn_only$target01,st_hn_only$USNumber)
table(st_hn_only$USNumber)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$USNumber, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~USNumber)

test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$USNumber, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

sen$Metric = "Sensitivity"
spec$Metric = "Specificity"
sen_spec = rbind(sen,spec)
sen_spec$USNumber = gsub("X","US",sen_spec$variable)
ss_mean = aggregate(sen_spec$value,
                    by = list(sen_spec$USNumber, 
                              sen_spec$Metric),
                    FUN=function(x){mean(na.omit(x))})
names(ss_mean) = c("USNumber","Metric","mean")
ss_lcl = aggregate(sen_spec$value,
                    by = list(sen_spec$USNumber, 
                              sen_spec$Metric),
                    FUN=function(x){max(0,mean(na.omit(x)) - 1.96*sd(na.omit(x)))})
names(ss_lcl) = c("USNumber","Metric","lcl")
ss_ucl = aggregate(sen_spec$value,
                    by = list(sen_spec$USNumber, 
                              sen_spec$Metric),
                    FUN=function(x){min(1, mean(na.omit(x)) + 1.96*sd(na.omit(x)))})
names(ss_ucl) = c("USNumber","Metric","ucl")

ss = Reduce(function(x,y){merge(x,y,by = c("USNumber","Metric"))},
            list(ss_mean, ss_lcl, ss_ucl))
ss = ss[ss$USNumber %in% paste0("US",1:6),]

ggplot(ss, aes(x = USNumber, y = mean, col = Metric)) + 
  geom_point(size=3) + ylim(0,1) + 
  geom_errorbar(size = 1.2, aes(ymin = lcl, ymax = ucl)) + facet_grid(~Metric)


ggplot(sen_spec, aes(x = USNumber, y = value, fill = Metric)) + 
  geom_boxplot()



## US MACHINE
table(st_hn_only$target01,st_hn_only$US_machine)
table(st_hn_only$US_machine)

sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
               target = st_hn_only$target01, 
               split_vec = st_hn_only$US_machine, 
               threshold = thresh_ep30, 
               n_it = 500)

ggplot(st_hn_only, aes(x = target, y = st_pred_mod_ep30, fill = target)) + geom_violin() + 
  geom_hline(yintercept = thresh_ep30, lwd = 1.2, lty = 2, col = "red") + ylim(0,1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + ylab("Prediction") + facet_grid(~US_machine)

test = sen_spec_split(score_vec = st_hn_only$st_pred_mod_ep30, 
                      target = st_hn_only$target01, 
                      split_vec = st_hn_only$US_machine, 
                      threshold = thresh_ep30, 
                      n_it = 500, out_vecs = TRUE)

sen = melt(data.frame(lapply(test, function(x){x[['sensitivity']][['vec']]})))

kruskal.test(sen$value, sen$variable)
ggplot(sen, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

spec = melt(data.frame(lapply(test, function(x){x[['specificity']][['vec']]})))

kruskal.test(spec$value, spec$variable)

ggplot(spec, aes(x = variable, y = value, fill = variable)) + 
  geom_violin()

sen$Metric = "Sensitivity"
spec$Metric = "Specificity"
sen_spec = rbind(sen,spec)
# sen_spec$US_machine = gsub("X","US",sen_spec$variable)
ss_mean = aggregate(sen_spec$value,
                    by = list(sen_spec$US_machine, 
                              sen_spec$Metric),
                    FUN=function(x){mean(na.omit(x))})
names(ss_mean) = c("US_machine","Metric","mean")
ss_lcl = aggregate(sen_spec$value,
                   by = list(sen_spec$US_machine, 
                             sen_spec$Metric),
                   FUN=function(x){max(0,mean(na.omit(x)) - 1.96*sd(na.omit(x)))})
names(ss_lcl) = c("US_machine","Metric","lcl")
ss_ucl = aggregate(sen_spec$value,
                   by = list(sen_spec$US_machine, 
                             sen_spec$Metric),
                   FUN=function(x){min(1, mean(na.omit(x)) + 1.96*sd(na.omit(x)))})
names(ss_ucl) = c("US_machine","Metric","ucl")

ss = Reduce(function(x,y){merge(x,y,by = c("US_machine","Metric"))},
            list(ss_mean, ss_lcl, ss_ucl))
ss = ss[ss$US_machine %in% paste0("US",1:6),]

ggplot(ss, aes(x = US_machine, y = mean, col = Metric)) + 
  geom_point(size=3) + ylim(0,1) + 
  geom_errorbar(size = 1.2, aes(ymin = lcl, ymax = ucl)) + facet_grid(~Metric)


ggplot(sen_spec, aes(x = US_machine, y = value, fill = Metric)) + 
  geom_boxplot()


## plot change in sen/spec 
head(st_hn_only)

st_hn_only = st_hn_only[order(st_hn_only$id_num),]

col_names = c("id_num","sen","sen_lcl","sen_ucl","spec","spec_lcl","spec_ucl")
plot_df = data.frame(matrix(nrow = 0, ncol = length(col_names)))



### EP30
for(id_num in unique(st_hn_only$id_num)){
  df_sub = st_hn_only[st_hn_only$id_num <= id_num,]
  sen_spec = get_sen_spec_withCI(score_vec = df_sub$st_pred_mod_ep30,
                          target = df_sub$target01,
                          threshold = thresh_ep30, n_it = 500)
  in_vec = c(id_num, sen_spec$sensitivity$est,
             sen_spec$sensitivity$ci,
             sen_spec$specificity$est,
             sen_spec$specificity$ci)
  
  plot_df = rbind(plot_df, in_vec)
}

names(plot_df) = col_names
head(plot_df,n = 40)


ggplot(plot_df, aes(x = id_num, y = sen)) + 
  geom_line() +
  geom_errorbar(size = 1, aes(ymin = sen_lcl, ymax = sen_ucl)) + 
  geom_point(size = 2) + ylim(c(0,1)) + 
  ylab("Cummulative Sensitivity") + xlab("ID number")

ggplot(plot_df, aes(x = id_num, y = spec)) + 
  geom_line() +
  geom_errorbar(size = 1, aes(ymin = spec_lcl, ymax = spec_ucl)) + 
  geom_point(size = 2) + ylim(c(0,1)) + 
  ylab("Cummulative Specificity") + xlab("ID number")

    ### plotting ood level
ggplot(st_hn_only, aes(x = id_num, y = ood_level)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + ylim(c(0,1))

    ### plotting ood avg
out = smooth::sma(st_hn_only$ood_level, order = 20)
st_hn_only$ood_avg = out$fitted

ggplot(st_hn_only, aes(x = id_num, y = ood_avg)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + ylim(c(0,1))

      ###
      ###     STANFORD DATA 
      ###

### ADD STANFORD DATA SHEET

stan_df = read.csv(file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/stan_mod0_mod1_20210613_v3.csv")
stan_datasheet = read_excel("C:/Users/lauren erdman/OneDrive - SickKids/HN/Stanford/AI Hydronephrosis Data deidentified.xlsx")
stan_df$target01 = ifelse(stan_df$target == "no Obstruction", 0, 1)

head(stan_datasheet)
head(stan_df)
stan_datasheet$img_id = paste0(stan_datasheet$anon_mrn,"_",stan_datasheet$`side of hydronephrosis`,"_",stan_datasheet$anon_accession,"_1")

#### GET MACHINE INFORMATION !! 

stan_merge = merge(stan_df, stan_datasheet, by.x = "id", by.y = "img_id")
head(stan_merge)
table(table(stan_merge$anon_mrn))
table(stan_merge$target, stan_merge$sex)
table(stan_merge$sex)
table(stan_merge$`side of hydronephrosis`,stan_merge$target)

stan_uniq = stan_merge[!duplicated(stan_merge$anon_mrn),]
table(stan_uniq$target)
table(tolower(stan_uniq$ethnicity))

dim(stan_merge)
length(unique(stan_merge$anon_mrn))
dim(stan_df)

stan_merge$age_grps = NA
stan_merge$age_grps[stan_merge$age < 2] = "under2"
stan_merge$age_grps[stan_merge$age >= 2 & stan_merge$age <= 5] = "age2to5"
stan_merge$age_grps[stan_merge$age > 5] = "over5"

table(stan_merge$age_grps)
table(stan_merge$age_grps, stan_merge$target)

stan_train = readRDS("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/Stan60%train_ids.rds")

stan40_test = stan_merge[!(stan_merge$id %in% stan_train),]

## overall -- all kidneys

get_sen_spec_withCI(score_vec = stan_df$stan_pred_mod_ep10, 
                    target = stan_df$target01, 
                    threshold = thresh_ep10, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = stan_df$stan_pred_mod_ep13, 
                    target = stan_df$target01, 
                    threshold = thresh_ep13, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = stan_df$stan_pred_mod_ep16, 
                    target = stan_df$target01, 
                    threshold = thresh_ep16, 
                    n_it = 500)


get_sen_spec_withCI(score_vec = stan_df$stan_pred_mod_ep30, 
                    target = stan_df$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500)


## overall -- HN kidneys only 
### NOTE THAT EARLIER EPOCH SHOWS STRONGER PERFORMANCE
get_sen_spec_withCI(score_vec = stan_merge$stan_pred_mod_ep10, 
                    target = stan_merge$target01, 
                    threshold = thresh_ep10, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = stan_merge$stan_pred_mod_ep13, 
                    target = stan_merge$target01, 
                    threshold = thresh_ep13, 
                    n_it = 500)

get_sen_spec_withCI(score_vec = stan_merge$stan_pred_mod_ep16, 
                    target = stan_merge$target01, 
                    threshold = thresh_ep16, 
                    n_it = 500)


get_sen_spec_withCI(score_vec = stan_merge$stan_pred_mod_ep30, 
                    target = stan_merge$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500)

    ## 40% test set only 
get_sen_spec_withCI(score_vec = stan40_test$stan_pred_mod_ep30, 
                    target = stan40_test$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500)


    ## split by sex
sen_spec_split(score_vec = stan_merge$stan_pred_mod_ep30, 
               target = stan_merge$target01, 
               split_vec = stan_merge$sex, 
               threshold = thresh_ep30, 
               n_it = 500)

    ## split by side
sen_spec_split(score_vec = stan_merge$stan_pred_mod_ep30, 
               target = stan_merge$target01, 
               split_vec = stan_merge$`side of hydronephrosis`, 
               threshold = thresh_ep30, 
               n_it = 500)

## split by age group
sen_spec_split(score_vec = stan_merge$stan_pred_mod_ep30, 
               target = stan_merge$target01, 
               split_vec = stan_merge$age_grps, 
               threshold = thresh_ep30, 
               n_it = 500)

## split by US Machine
# sen_spec_split(score_vec = stan_merge$stan_pred_mod_ep30, 
#                target = stan_merge$target01, 
#                split_vec = stan_merge$, 
#                threshold = thresh_ep30, 
#                n_it = 500)

###
###     IOWA DATA 
###

### ADD IOWA DATA SHEET

ui_df = read.csv(file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/ui_mod0_mod1_20210613_v3.csv")
ui_datasheet = read.csv("C:/Users/lauren erdman/OneDrive - SickKids/HN/UIowa/UIowa_Datasheet2.csv", header = TRUE, as.is = TRUE)

head(ui_df)
dim(ui_df)
head(ui_datasheet)

ui_df$pt_id = unlist(lapply(strsplit(ui_df$id,"_"),function(x){x[1]}))
ui_df$target01 = ifelse(ui_df$target == "no Obstruction", 0, 1)

table(ui_df$target)

ui_merge = merge(ui_df, ui_datasheet, by.y = "Name", by.x = "pt_id")
head(ui_merge)

ui_merge$age_days = difftime(ui_merge$Ultrasound.Date, ui_merge$DOB, units = "days")

ui_merge$age_grp = NA
ui_merge$age_grp[ui_merge$age_days < 365*2] = "under2"
ui_merge$age_grp[ui_merge$age_days >= 365*2 & ui_merge$age_days <= 365*5] = "age2to5"
ui_merge$age_grp[ui_merge$age_days > 365*5] = "over5"

table(ui_merge$age_grp, ui_merge$target)
table(ui_merge$age_grp)

table(ui_merge$Gender, ui_merge$target)
table(ui_merge$Gender)

table(ui_merge$U.B, ui_merge$target)
table(ui_merge$U.B)

table(ui_merge$target)

ui_train = readRDS("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/UI60%train_ids.rds")

ui40_test = ui_merge[!(ui_merge$pt_id %in% ui_train),]

## overall 
get_sen_spec_withCI(score_vec = ui_df$ui_pred_mod_ep10,
                    target = ui_df$target01,
                    threshold = thresh_ep10,
                    n_it = 500)

get_sen_spec_withCI(score_vec = ui_df$ui_pred_mod_ep13,
                    target = ui_df$target01,
                    threshold = thresh_ep13,
                    n_it = 500)

get_sen_spec_withCI(score_vec = ui_df$ui_pred_mod_ep16,
                    target = ui_df$target01,
                    threshold = thresh_ep16,
                    n_it = 500)

get_sen_spec_withCI(score_vec = ui_df$ui_pred_mod_ep30, 
                    target = ui_df$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500)

    ## 40% test 
get_sen_spec_withCI(score_vec = ui40_test$ui_pred_mod_ep30, 
                    target = ui40_test$target01, 
                    threshold = thresh_ep30, 
                    n_it = 500)

    ## split by sex
sen_spec_split(score_vec = ui_merge$ui_pred_mod_ep30, 
               target = ui_merge$target01, 
               split_vec = ui_merge$Gender, 
               threshold = thresh_ep30, 
               n_it = 500)


    ## split by HN side
sen_spec_split(score_vec = ui_merge$ui_pred_mod_ep30, 
               target = ui_merge$target01, 
               split_vec = ui_merge$U.B, 
               threshold = thresh_ep30, 
               n_it = 500)

    ## split by age grp
sen_spec_split(score_vec = ui_merge$ui_pred_mod_ep30, 
               target = ui_merge$target01, 
               split_vec = ui_merge$age_grp, 
               threshold = thresh_ep30, 
               n_it = 500)
