###
###   FUNCTIONS
###

ids_to_df = function(in_vec){

  full_ids = unlist(lapply(strsplit(in_vec,"_"),function(x)x[1]))
  num_ids = substr(full_ids,5,nchar(full_ids))  
  kidney_side = unlist(lapply(strsplit(in_vec,"_"),function(x)x[2]))
  visit_num = unlist(lapply(strsplit(in_vec,"_"),function(x)x[3]))

  out_df = data.frame(full_ids, num_ids, kidney_side, visit_num)
    
  return(out_df)
}

###
###   LIBRARIES
###

library(ggplot2)

## silent trial data

st_hn_df = data.frame(read_excel("C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial/HN Silent Trial_20210411.xlsx"))
head(st_hn_df)

st_hn_df$Date.Nuc.Scan
st_hn_df$Date.Nuc.Scan.2
st_hn_df$Date.Nuc.Scan..3

st_hn_df$renalscan1 = 0 
st_hn_df$renalscan1[!is.na(st_hn_df$Date.Nuc.Scan)] = 1

st_hn_df$renalscan2 = 0 
st_hn_df$renalscan2[!is.na(st_hn_df$Date.Nuc.Scan.2)] = 1

st_hn_df$renalscan3 = 0 
st_hn_df$renalscan3[!is.na(st_hn_df$Date.Nuc.Scan..3)] = 1


### Silent trial IDs from the manuscript: 
sitrial_embed = read.csv("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/orig_st_results/SilentTrial_embeddings.csv", header=FALSE, as.is=TRUE)
sitrial_ids = sitrial_embed[,ncol(sitrial_embed)]

sitrial_id_df = ids_to_df(sitrial_ids)
head(sitrial_id_df)

sitri_merged = merge(sitrial_id_df, st_hn_df, by.x = "num_ids", by.y = "ST.Study.ID", all.x = TRUE)
head(sitri_merged)

### Merge predictions 
dat_thresh_ep30 = 0.03094032

###
###     SILENT TRIAL DATA 
###

st_hn_only = readRDS(file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial/ST_postStanfinetune_20210818.rds")


all_sitri_merge = merge(sitri_merged[,c("num_ids","full_ids","kidney_side","visit_num","renalscan1","renalscan2","Date.Nuc.Scan","Date.Nuc.Scan.2",
                                        "Date.US.1","Date.US.2","Date.US.3","Date.US.4","Date.US.5","Date.US.6","Date.US.7","Date.US.8")],
                        st_hn_only[,c("ID","PHN.ID","Sex","target","surg","st_pred_mod_ep30","SFU","view_side","US_num")],
                        by.x = c("num_ids","kidney_side","visit_num"), 
                        by.y = c("ID","view_side","US_num"))

dim(all_sitri_merge)
dim(sitri_merged)
dim(st_hn_only)
head(st_hn_only)
head(sitri_merged)

head(all_sitri_merge)

## add vector counting the number of nuc scans performed after US for 
  ## correctly predicted, non-obstructed HN cases
all_sitri_merge$num_NCs_after = NA
all_sitri_merge$visit_date = NA

for(i in 1:nrow(all_sitri_merge)){
  visit_num = all_sitri_merge$visit_num[i]
  cat("\nVisit number: ")
  cat(visit_num)

  visit_date = as.Date(all_sitri_merge[i,paste0("Date.US.",visit_num)])
  all_sitri_merge$visit_date[i] = visit_date
  cat("\nVisit dat: ")
  cat(visit_date)

  no_nucs = ifelse(sum(all_sitri_merge[i,c("renalscan1","renalscan2")]) == 0, 1, 0)

  if(!is.na(visit_date)){
    if(no_nucs == 0){
      if(visit_date < as.Date(all_sitri_merge[i,"Date.Nuc.Scan"])){
        if(!is.na(as.Date(all_sitri_merge[i,"Date.Nuc.Scan.2"]))){
          if(visit_date < as.Date(all_sitri_merge[i,"Date.Nuc.Scan.2"])){
            all_sitri_merge$num_NCs_after[i] = 2 
          } else{
            all_sitri_merge$num_NCs_after[i] = 1 
          } 
        } else{
          all_sitri_merge$num_NCs_after[i] = 1 
        }
      } else{
        all_sitri_merge$num_NCs_after[i] = 0 
      }
    } else{
      all_sitri_merge$num_NCs_after[i] = 0 
    }
  } else{
    all_sitri_merge$num_NCs_after[i] = NA 
  }
  
  cat("\nNumber of Nuc Scans: ")
  cat(all_sitri_merge$num_NCs_after[i])
  
}

all_sitri_merge$num_NCs_after = factor(all_sitri_merge$num_NCs_after, levels = c(0,1,2))

all_sitri_merge$prediction = NA
all_sitri_merge$prediction[all_sitri_merge$st_pred_mod_ep30 <= dat_thresh_ep30] = "No Obstruction"
all_sitri_merge$prediction[all_sitri_merge$st_pred_mod_ep30 > dat_thresh_ep30] = "Obstruction"
all_sitri_merge$prediction = factor(all_sitri_merge$prediction,levels = c("No Obstruction", "Obstruction"))


ggplot(all_sitri_merge[!is.na(all_sitri_merge$num_NCs_after),],
       aes(y = st_pred_mod_ep30, x = num_NCs_after, col = prediction)) + 
  geom_point(position = "jitter") + 
  facet_grid(visit_num~target)


plot_df = all_sitri_merge[all_sitri_merge$num_NCs_after != 0,]
plot_df = plot_df[!is.na(plot_df$num_NCs_after),]

theme_set(
  theme_bw(base_size = 10)
)

## Only plotting predictions from ultrasounds that had  1 or 2 nuc scans following

ggplot(plot_df,
       aes(y = st_pred_mod_ep30, x = num_NCs_after, col = prediction)) + 
  ylab("Predicted Probability of Obstruction") + 
  xlab("Number of Nuclear Scans Following Ultrasound") + 
  geom_point(position = "jitter") + 
  facet_grid(~target)

ggplot(plot_df,
       aes(y = st_pred_mod_ep30, x = num_NCs_after, col = prediction)) + 
  ylab("Predicted Probability of Obstruction") + 
  xlab("Number of Nuclear Scans Following Ultrasound") + 
  geom_point(position = "jitter") + 
  facet_grid(visit_num~target)

table(plot_df$prediction,plot_df$num_NCs_after,plot_df$target)

### look at first ultrasound only 
first_us = plot_df[plot_df$visit_num == 1,]
table(first_us$prediction,first_us$target, first_us$num_NCs_after)
table(first_us$prediction,first_us$target)

### look at second ultrasound only 
second_us = plot_df[plot_df$visit_num == 2,]
table(second_us$prediction,second_us$target, second_us$num_NCs_after)
table(second_us$prediction,second_us$target)

########################
## scrap/previous material
########################

train_ids = readRDS("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/training_ids_20210803.rds")
head(train_ids)
length(train_ids)

base_train_ids = unique(unlist(lapply(strsplit(train_ids,split = "_"),function(x)x[[1]])))
length(base_train_ids)
head(base_train_ids)
orig_id_nums = as.numeric(substr(base_train_ids,5,nchar(base_train_ids))[substr(base_train_ids,1,4) == "ORIG"])
st_id_nums = as.numeric(substr(base_train_ids,5,nchar(base_train_ids))[substr(base_train_ids,1,4) == "STID"])

orig_sub$study_id

sum(orig_id_nums %in% orig_sub$study_id)
sum(orig_id_nums %in% st_sub$ST.Study.ID)

sum(st_id_nums %in% orig_sub$study_id)
sum(st_id_nums %in% st_sub$ST.Study.ID)


sub_comb_df = comb_df[comb_df$id %in% base_train_ids,]
dim(sub_comb_df)

str(comb_df$id)
str(base_train_ids)

sum(comb_df$id %in% base_train_ids)
sum(base_train_ids %in% comb_df$id)

## orig IDs not mapping... 

