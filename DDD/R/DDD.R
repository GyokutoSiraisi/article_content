
# 準備 ----------------------------------------------------------------------

## パッケージの読み込み ----
pacman::p_load(tidyverse,
               magrittr,
               arrow,
               fixest)

## データの読み込み ----
dat <- arrow::read_csv_arrow("output/dat.csv")


# 分析 ----------------------------------------------------------------------

## 全世帯
dat_all <- dat %>% dplyr::filter(couple == "all" & child == "under5") 
res_1 <- feols(Mom_Emp ~ cap_r + unemp + Dad_Emp, 
               data = dat_all)
res_2 <- feols(Mom_Emp ~ cap_r + unemp + Dad_Emp | pref + year, 
                 data = dat_all, 
                 cluster = "pref")
etable(res_1, res_2)

## 核家族と3世代
dat_nuc  <- dat %>% dplyr::filter(couple == "nuclear" & child == "under5") 
dat_3gen <- dat %>% dplyr::filter(couple == "3_gen" & child == "under5")  
res_3 <- feols(Mom_Emp ~ cap_r + unemp + Dad_Emp | pref + year, 
               data = dat_nuc,
               cluster = "pref")
res_4 <- feols(Mom_Emp ~ cap_r + unemp + Dad_Emp | pref + year, 
               data = dat_3gen, 
               cluster = "pref")
etable(res_3, res_4)

## 三重差分法（5歳以下と6～14歳の子供）
# category: 第3の差分をとるグループダミー (1 or 0)
# cluster: 標準誤差のクラスタリング（通常は地域や個人IDなど）
dat_nuc_2  <- dat %>%
  dplyr::filter(couple == "nuclear") %>%
  mutate(age_d = if_else(child == "under5", 1, 0))
dat_3gen_2 <- dat %>% dplyr::filter(couple == "3_gen") %>%
  mutate(age_d = if_else(child == "under5", 1, 0))    
res_5 <- feols(Mom_Emp ~ cap_r*age_d + unemp + Dad_Emp | pref + year + pref^age_d + year^age_d,
               data = dat_nuc_2,
               cluster = "pref")
res_6 <- feols(Mom_Emp ~ cap_r*age_d + unemp + Dad_Emp | pref + year + pref^age_d + year^age_d, 
               data = dat_3gen_2, 
               cluster = "pref")
etable(res_5, res_6)

etable(res_3, res_5)









