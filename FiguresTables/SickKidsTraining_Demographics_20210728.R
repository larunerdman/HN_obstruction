
library(readxl)

raw_in = readLines("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/preprocessed_images_SickKidswST_filenames_20210411.json")

library(V8)
test = v8()
test$assign("dat", JS(raw_in))
sk_list = test$get("dat")

names(sk_list)

sk_list[["ORIG382"]]

# orig_df = read_excel("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/Copy_PHN-RefluxEtiologiesML_Raw_20201220.xlsx",sheet = "in")
# head(orig_df)

orig_df = read.csv("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/Orig_data_age_apd_sfu_20210804.csv",header=TRUE, as.is = TRUE)
head(orig_df)
orig_df$full_id = paste0("ORIG",orig_df$study_id,"_",orig_df$hn_side,"_",orig_df$US_num,"_1")
orig_df$short_id = paste0("ORIG",orig_df$study_id)

st_df = read.csv("C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial/SilentTrial_Datasheet.csv", header=TRUE, as.is=TRUE)
st_df$full_id = paste0("STID",st_df$ID, "_", st_df$view_side, "_", st_df$US_num, "_1")
head(st_df)

in_list=sk_list

make_inlist_df = function(in_list){

  out_names = c("id", "surgery","us_num", "sag","trv","side","age","apd","sfu","us_machine")
  out_df = data.frame(matrix(nrow=0,ncol=length(out_names)))
  
  ids = names(in_list)
  
  for( id in ids ){
    
   sub_list = in_list[[id]]
   
   sides = c("Left","Right")[c("Left","Right") %in% names(sub_list)]
   
   for(side in sides){
     
     side_list = sub_list[[side]]
     
     us_nums = na.omit(as.numeric(names(side_list)))
     
     for(us_num in us_nums){
       
       us_num_list = side_list[[as.character(us_num)]]
       
       # if(id == "ORIG273"){
       #   browser()
       # }

       # browser()
       age = ifelse(length(us_num_list$Age_wks) == 0, NA, us_num_list$Age_wks)
       apd = ifelse(length(us_num_list$ApD) == 0, NA, us_num_list$ApD)
       sfu = ifelse(length(us_num_list$SFU) == 0, NA, us_num_list$SFU)
       mach = ifelse(length(us_num_list$US_machine) == 0, NA, us_num_list$US_machine)
       
       sag = ifelse(length(us_num_list$sag) == 0, NA, us_num_list$sag)
       trv = ifelse(length(us_num_list$trv) == 0, NA, us_num_list$trv)
       
       surgery = ifelse(length(side_list$surgery) == 0, NA, side_list$surgery)
       
       add_vec = c(id, surgery, us_num, sag, trv, side, age, apd, sfu, mach)
       
       out_df = rbind(out_df, add_vec)

     }
   }
  }
  
  names(out_df) = out_names
  out_df[out_df == "NA"] = NA
  return(out_df)
  
}

sk_df = make_inlist_df(in_list = sk_list)

head(sk_df)

sk_df[sk_df == "NA"] = NA
sk_nomiss_df = sk_df[complete.cases(sk_df),]
dim(sk_nomiss_df)
dim(sk_df)
# View(sk_df[complete.cases(sk_df),])
unique(substr(sk_nomiss_df$id,1,2))
unique(substr(sk_df[!is.na(sk_df$age),]$id,1,2))

sk_df_run = sk_df[!is.na(sk_df$sag) & !is.na(sk_df$trv) & !is.na(sk_df$age),]
head(sk_df_run)
sk_df_run[substr(sk_df_run$id,1,4) == "ORIG",]
dim(sk_df_run)
sk_df_run$full_id = paste0(sk_df_run$id,"_",sk_df_run$side,"_",sk_df_run$us_num,"_1")

train_ids = readRDS("C:/Users/lauren erdman/Desktop/kidney_img/HN/SickKids/training_ids_20210803.rds")
head(train_ids)
length(train_ids)

sk_df_sub = sk_df_run[sk_df_run$full_id %in% train_ids,]
dim(sk_df_sub)
head(sk_df_sub)

sk_df_sub$apd[na.omit(match(orig_df$full_id, sk_df_sub$full_id))] = orig_df$ApD[orig_df$full_id %in% sk_df_sub$full_id]
head(sk_df_sub)
sk_df_sub[substr(sk_df_sub$id,1,4) == "ORIG",]
sk_df_sub[is.na(sk_df_sub$apd_group),]
head(orig_df)
hist(sk_df_sub$apd)

sk_df_sub$set = NA
sk_df_sub$set[substr(sk_df_sub$id,1,4) == "ORIG"] = "OriginalDatasheet"
sk_df_sub$set[substr(sk_df_sub$id,1,4) == "STID"] = "SilentTrialDatasheet"
sk_df_sub$id_num = substr(sk_df_sub$id, 5, nchar(sk_df_sub$id))

dim(sk_df_sub)
table(sk_df_sub$surgery)

head(sk_df_sub)

mandy_df = sk_df_sub[sk_df_sub$surgery == 1, c("id_num", "set")]
mandy_df = mandy_df[!duplicated(mandy_df$id_num, mandy_df$set),]
dim(mandy_df)

# sub_df = sk_df_sub[!duplicated(sk_df_sub$id_num),,]
# table(sub_df$surgery)

# write.csv(mandy_df, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/training_surgery_ids.csv", row.names = F, quote=F)

sk_df_sub$sex = NA
sk_df_sub$sex[na.omit(match(orig_df$full_id, sk_df_sub$full_id))] = orig_df$gender[orig_df$full_id %in% sk_df_sub$full_id]
sk_df_sub$sex[na.omit(match(st_df$full_id, sk_df_sub$full_id))] = st_df$Sex[st_df$full_id %in% sk_df_sub$full_id]
sk_df_sub$sex[na.omit(match(orig_df$short_id, sk_df_sub$id))] = orig_df$gender[orig_df$short_id %in% sk_df_sub$id]

for(i in 1:nrow(sk_df_sub)){
  if(sk_df_sub$id[i] %in% orig_df$short_id){
    sk_df_sub$sex[i] = orig_df$gender[orig_df$short_id == sk_df_sub$id[i]][1]
  }
}

sk_df_sub$hn_side = NA
sk_df_sub$hn_side[na.omit(match(orig_df$full_id, sk_df_sub$full_id))] = orig_df$laterality[orig_df$full_id %in% sk_df_sub$full_id]
sk_df_sub$hn_side[na.omit(match(st_df$full_id, sk_df_sub$full_id))] = st_df$Side[st_df$full_id %in% sk_df_sub$full_id]
sk_df_sub$hn_side[is.na(sk_df_sub$hn_side)] = ifelse(sk_df_sub$side[is.na(sk_df_sub$hn_side)] == "Left",1,2)

sk_df_sub[is.na(sk_df_sub$sex),]

hist(as.numeric(sk_df_sub$age))
sk_df_sub$age = as.numeric(sk_df_sub$age)
sk_df_sub$age_grp = NA
sk_df_sub$age_grp[sk_df_sub$age < 104] = "under2"
sk_df_sub$age_grp[sk_df_sub$age >= 104 & sk_df_sub$age < 260] = "age2to5"
sk_df_sub$age_grp[sk_df_sub$age >= 260] = "over5"

sk_df_sub$us_machine[sk_df_sub$us_machine == "TreeST"] = "OutsideST"

sk_df_sub$USMachine = sk_df_sub$us_machine
sk_df_sub$USMachine[sk_df_sub$us_machine == "ge-medical-systems"] = "GE"
sk_df_sub$USMachine[sk_df_sub$us_machine == "GEST"] = "GE"
sk_df_sub$USMachine[sk_df_sub$us_machine == "philips-medical-systems"] = "Philips"
sk_df_sub$USMachine[sk_df_sub$us_machine == "PhilipsST"] = "Philips"
sk_df_sub$USMachine[sk_df_sub$us_machine == "samsung-medison-co-ltd"] = "Samsung"
sk_df_sub$USMachine[sk_df_sub$us_machine == "SamsungST"] = "Samsung"
sk_df_sub$USMachine[sk_df_sub$us_machine == "ToshibaST"] = "Toshiba"
sk_df_sub$USMachine[sk_df_sub$us_machine == "toshiba-mec"] = "Toshiba"
sk_df_sub$USMachine[sk_df_sub$us_machine == "toshiba-mec-us"] = "Toshiba"
sk_df_sub$USMachine[sk_df_sub$us_machine == "ToshibaST"] = "Toshiba"

sk_df_sub$apd = as.numeric(sk_df_sub$apd)
sk_df_sub$apd_group = NA
sk_df_sub$apd_group[sk_df_sub$apd < 6] = "under6"
sk_df_sub$apd_group[sk_df_sub$apd >= 6 & sk_df_sub$apd < 9] = "apd6to9"
sk_df_sub$apd_group[sk_df_sub$apd >= 9 & sk_df_sub$apd < 14] = "apd9to14"
sk_df_sub$apd_group[sk_df_sub$apd >= 14] = "over14"
sk_df_sub$apd_group[is.na(sk_df_sub$apd)] = "unmeasured"

# saveRDS(sk_df_sub, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/silent_trial/training_details.rds")

test_df2 = sk_df_sub[!duplicated(sk_df_sub$id),]
test_df2 = test_df2[test_df2$surgery == 1,]
table(test_df2$surgery)
mandy_df = sk_df_sub[sk_df_sub$id %in% test_df2$id, c("id","id_num", "set")]
mandy_df= mandy_df[!duplicated(mandy_df$id),]
dim(mandy_df)

# sub_df = sk_df_sub[!duplicated(sk_df_sub$id_num),,]
# table(sub_df$surgery)

write.csv(mandy_df, file = "C:/Users/lauren erdman/Desktop/kidney_img/HN/training_surgery_ids_20210823.csv", row.names = F, quote=F)


table(sk_df_sub$surgery)

length(unique(sk_df_sub$id))
table(sk_df_sub$surgery[!duplicated(sk_df_sub$id)])

table(sk_df_sub$surgery, sk_df_sub$side)
table(sk_df_sub$side)

table(sk_df_sub$surgery, sk_df_sub$hn_side)
table(sk_df_sub$hn_side)

table(sk_df_sub$surgery, sk_df_sub$sex)
table(sk_df_sub$sex)

sk_df_sub$id[is.na(sk_df_sub$sex)]

table(sk_df_sub$surgery, sk_df_sub$apd_group)
table(sk_df_sub$apd_group)
sk_df_sub$full_id[is.na(sk_df_sub$apd_group)]
# hist(sk_df_sub$apd)

orig_df$ApD[orig_df$full_id %in% sk_df_sub$full_id[is.na(sk_df_sub$apd_group)]]

table(sk_df_sub$surgery, sk_df_sub$USMachine)
table(sk_df_sub$USMachine)

table(sk_df_sub$surgery, sk_df_sub$age_grp)
table(sk_df_sub$age_grp)

table(sk_df_sub$surgery, sk_df_sub$us_num)
table(sk_df_sub$us_num)
str(table(sk_df_sub$surgery, sk_df_sub$us_num))


table(sk_df_sub$surgery, sk_df_sub$us_machine)
table(sk_df_sub$us_machine)


