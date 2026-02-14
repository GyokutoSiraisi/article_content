
# パッケージの読み込み --------------------------------------------------------------
pacman::p_load(tidyverse,
               magrittr,
               arrow)
source("R/pref_summarize.R")

# 1990 --------------------------------------------------------------------

## 就業率 ----
## 統計名	国勢調査 平成２年国勢調査 第2次基本集計 都道府県編
## 表番号	01301 	
## 表題	  夫妻の就業・非就業（６），夫婦のいる一般世帯の家族類型（４Ａ），子供の有無・年齢（８），夫婦のいる一般世帯数・親族人員

dat_1990 <- 
  arrow::read_csv_arrow("data/1990.csv", skip = 13) %>% 
  dplyr::select(region = "全域・集中の別030002",
                year   = "時間軸（年次）",
                code   = "県５０万以上市030045 コード",
                pref   = "県５０万以上市030045",
                couple = "夫婦一般世帯030421",
                child  = "子供の有・年Ａ030197",
                total  = "総数（労働力状態不詳を含む）",
                both_emp = "夫・妻とも就業",
                wife_emp = "夫が非就業，妻が就業",
                hub_emp  = "夫が就業，妻が非就業") %>% 
  dplyr::filter(region  == "全域" &
                code    %% 1000 == 0 &
                (couple == "夫婦のいる一般世帯" |
                 couple == "夫婦のいる核家族世帯" | 
                 couple =="夫婦のいるその他の親族世帯（同居の親あり）") &
                (child   == "最年少の子供が６歳未満" |
                 child   == "最年少の子供が６－１４歳")) %>% 
  dplyr::mutate(year     = readr::parse_number(year),
                total    = readr::parse_number(total),
                both_emp = readr::parse_number(both_emp),
                wife_emp = readr::parse_number(wife_emp),
                hub_emp  = readr::parse_number(hub_emp),
                Mom_Emp  = (both_emp+wife_emp) / total,
                Dad_Emp  = (both_emp+hub_emp ) / total,
                couple   = case_when(
                  couple == "夫婦のいる一般世帯" ~ "all",
                  couple == "夫婦のいる核家族世帯" ~ "nuclear",
                  couple == "夫婦のいるその他の親族世帯（同居の親あり）" ~ "3_gen"),
                child    = case_when(
                  child   == "最年少の子供が６歳未満"   ~ "under5",
                  child   == "最年少の子供が６－１４歳" ~ "6to14"
                )) %>% 
  dplyr::select(year, pref, couple, child, total, Mom_Emp, Dad_Emp)

## 保育所定員率 ----

nursery_1990 <-
  arrow::read_csv_arrow("data/1990_social.csv", 
                        read_options = CsvReadOptions$create(encoding = "SJIS")) %>%
  dplyr::mutate(pref = ifelse(pref %in% names(city_map), city_map[pref], pref)) %>%
  group_by(pref) %>%
  summarise(cap = sum(cap)) %>%
  ungroup() %>% 
  mutate(year = 1990)

# 1995 --------------------------------------------------------------------

## 就業率 ----
## 統計名	国勢調査 平成７年国勢調査 第3次基本集計 都道府県編  	
## 表番号	01201 	
## 表題	  夫婦の就業・非就業（６），夫婦のいる一般世帯の家族類型（４），子供の有無・年齢（８），夫婦のいる一般世帯数・親族人員 

dat_1995 <-
  arrow::read_csv_arrow("data/1995.csv", skip = 13) %>% 
  dplyr::select(region = "全域・集中の別030107",
                year   = "時間軸（年次）",
                code   = "県市郡５０上市030130 コード",
                pref   = "県市郡５０上市030130",
                couple = "夫婦有家族類型030925",
                child  = "子供有無年齢８030939",
                total  = "総数（不詳を含む）",
                both_emp = "夫・妻とも就業",
                wife_emp = "夫が非就業，妻が就業",
                hub_emp  = "夫が就業，妻が非就業") %>% 
  dplyr::filter(region  == "全域" &
                code    %% 1000 == 0 &
                (couple == "総数（夫婦のいる一般世帯）" |
                couple == "核家族世帯" | 
                couple =="その他の親族世帯（同居の親あり）")  &
                (child   == "６歳未満（最年少の子供）" |
                 child   == "６～１４歳")) %>% 
  dplyr::mutate(year     = readr::parse_number(year),
                total    = readr::parse_number(total),
                both_emp = readr::parse_number(both_emp),
                wife_emp = readr::parse_number(wife_emp),
                hub_emp  = readr::parse_number(hub_emp),
                Mom_Emp  = (both_emp+wife_emp) / total,
                Dad_Emp  = (both_emp+hub_emp ) / total,
                couple   = case_when(
                  couple == "総数（夫婦のいる一般世帯）" ~ "all",
                  couple == "核家族世帯" ~ "nuclear",
                  couple == "その他の親族世帯（同居の親あり）" ~ "3_gen"),
                child    = case_when(
                  child   == "６歳未満（最年少の子供）"   ~ "under5",
                  child   == "６～１４歳" ~ "6to14"
                )) %>% 
  dplyr::select(year, pref, couple, child, total, Mom_Emp, Dad_Emp)

## 保育所定員率 ----

nursery_1995 <-
  arrow::read_csv_arrow("data/1995_social.csv", 
                        read_options = CsvReadOptions$create(encoding = "SJIS")) %>%
  dplyr::mutate(pref = ifelse(pref %in% names(city_map), city_map[pref], pref)) %>%
  group_by(pref) %>%
  summarise(cap = sum(cap)) %>%
  ungroup() %>% 
  mutate(year = 1995)


# 2000 --------------------------------------------------------------------

## 就業率 ----
## 統計名	国勢調査 平成12年国勢調査 第2次基本集計（労働力状態，就業者の産業，就業時間など） 都道府県結果 	
## 表番号	02100 	
## 表題	  夫婦の就業・非就業（６）、夫婦のいる一般世帯の家族類型（５）、子供の有無・年齢（２４）、夫婦のいる一般世帯数・親族人員 

# path <- list.files(path = "data/2000", full.names = T)
# file_list <- map(path, ~ read_csv(., locale = locale(encoding = "SJIS"), show_col_types = FALSE))
# combined_data <- bind_rows(file_list)
# combined_data %>% write_dataset(path = "output/2000.parquet", format = "parquet")

dat_2000 <-
  open_dataset("output/2000.parquet") %>%
  dplyr::filter(str_ends(area_code, "000") &
                unit == "世帯" &
                cat01_code == "00700") %>% 
  dplyr::select(year   = "時間軸（年次）",
                pref   = "県市区町村030195",
                couple = "夫婦有家族類型031222",
                child  = "子供の有無・年齢031230",
                emp = "夫婦就業状態６031229",
                value) %>% 
  dplyr::filter((couple == "総数（夫婦のいる一般世帯）" |
                couple == "核家族世帯" | 
                couple =="その他の親族世帯（同居の親あり）")
                ) %>% 
  dplyr::collect() %>%
  dplyr::mutate(year     = readr::parse_number(year),
                couple   = case_when(
                  couple == "総数（夫婦のいる一般世帯）" ~ "all",
                  couple == "核家族世帯" ~ "nuclear",
                  couple == "その他の親族世帯（同居の親あり）" ~ "3_gen"),
                child    = case_when(
                  child   == "　　最年少の子供が０歳" 　~ "under5",
                  child   == "　　最年少の子供が１歳" 　~ "under5",
                  child   == "　　最年少の子供が２歳" 　~ "under5",
                  child   == "　　最年少の子供が３歳"　 ~ "under5",
                  child   == "　　最年少の子供が４歳" 　~ "under5",
                  child   == "　　最年少の子供が５歳"　 ~ "under5",
                  child   == "　　最年少の子供が６歳"　 ~ "6to14",
                  child   == "　　最年少の子供が７歳"　 ~ "6to14",
                  child   == "　　最年少の子供が８歳"　 ~ "6to14",
                  child   == "　　最年少の子供が９歳"　 ~ "6to14",
                  child   == "　　最年少の子供が１０歳" ~ "6to14",
                  child   == "　　最年少の子供が１１歳" ~ "6to14",
                  child   == "　　最年少の子供が１２歳" ~ "6to14",
                  child   == "　　最年少の子供が１３歳" ~ "6to14",
                  child   == "　　最年少の子供が１４歳" ~ "6to14",
                  TRUE ~ child
                )) %>% 
  dplyr::filter(child == "under5" | child == "6to14") %>% 
  dplyr::group_by(year, pref, couple, child, emp) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from  = emp,   
                     values_from = value) %>% 
  dplyr::rename("both_emp" = "　夫・妻とも就業",
                "hub_emp"  = "　夫が就業，妻が非就業",
                "wife_emp" = "　夫が非就業，妻が就業",
                "total"    = "総数（労働力状態不詳を含む）") %>% 
  dplyr::mutate(Mom_Emp  = (both_emp+wife_emp) / total,
                Dad_Emp  = (both_emp+hub_emp ) / total) %>% 
  dplyr::select(year, pref, couple, child, total, Mom_Emp, Dad_Emp)

## 保育所定員率 ----
col_name = c("pref", rep("hoge", 3), "cap", rep("hoge", 11))
nursery_2000 <-
  arrow::read_csv_arrow("data/2000_social.csv", # 事前に一部の行削除
                        read_options = CsvReadOptions$create(encoding = "SJIS",
                                                             skip_rows = 8,
                                                             column_names = col_name),
                        col_select = !starts_with("hoge")) %>%
  dplyr::slice(-48, -61) %>% 
  dplyr::mutate(pref = str_remove_all(pref, "[\\s0-9０-９]"),
                pref = case_when(
                  pref == "北海道"     ~ "北海道",
                  pref == "東京"       ~ "東京都",
                  pref == "大阪"       ~ "大阪府",
                  pref == "京都"       ~ "京都府",
                  str_ends(pref, "市") ~ pref,
                  TRUE ~  paste0(pref, "県")),
                pref = ifelse(pref %in% names(city_map), city_map[pref], pref)) %>%
  group_by(pref) %>%
  summarise(cap = sum(cap)) %>%
  ungroup() %>% 
  mutate(year = 2000)

# 2005 --------------------------------------------------------------------

## 就業率 ----
## 統計名	国勢調査 平成17年国勢調査 労働力状態，就業者の産業，就業時間など（第2次基本集計） 都道府県結果 	
## 表番号	01600 	
## 表題	夫の就業・非就業（３区分）、妻の就業・非就業（３区分）、世帯の家族類型（３区分）、子供の有無・年齢（２４）、夫婦のいる一般世帯数、親族人員

# path <- list.files(path = "data/2005", full.names = T)
# file_list <- map(path, ~ readr::read_csv(., locale = locale(encoding = "SJIS"), show_col_types = FALSE))
# combined_data <- bind_rows(file_list)
# combined_data %>% write_dataset(path = "output/2005.parquet", format = "parquet")

dat_2005 <-
  open_dataset("output/2005.parquet", format = "parquet") %>%
  dplyr::filter(str_ends(area_code, "000") &
                unit == "世帯" &
                cat01_code == "00700") %>% 
  dplyr::select(year     = "時間軸（年次）",
                pref     = "地域030289",
                couple   = "世帯の家族類型031566",
                child    = "子供の有無・年齢031230",
                emp_hub  = "夫の就業非就業031576",
                emp_wife = "妻の就業非就業031577",
                value) %>% 
  dplyr::filter((couple == "夫婦のいる一般世帯" |
                   couple == "　核家族世帯" | 
                   couple =="　その他の親族世帯（同居の親あり）")) %>% 
  dplyr::collect() %>%
  dplyr::mutate(year     = readr::parse_number(year),
                couple   = case_when(
                  couple == "夫婦のいる一般世帯" ~ "all",
                  couple == "　核家族世帯" ~ "nuclear",
                  couple == "　その他の親族世帯（同居の親あり）" ~ "3_gen"),
                child    = case_when(
                  child   == "　　最年少の子供が０歳" 　~ "under5",
                  child   == "　　最年少の子供が１歳" 　~ "under5",
                  child   == "　　最年少の子供が２歳" 　~ "under5",
                  child   == "　　最年少の子供が３歳"　 ~ "under5",
                  child   == "　　最年少の子供が４歳" 　~ "under5",
                  child   == "　　最年少の子供が５歳"　 ~ "under5",
                  child   == "　　最年少の子供が６歳"　 ~ "6to14",
                  child   == "　　最年少の子供が７歳"　 ~ "6to14",
                  child   == "　　最年少の子供が８歳"　 ~ "6to14",
                  child   == "　　最年少の子供が９歳"　 ~ "6to14",
                  child   == "　　最年少の子供が１０歳" ~ "6to14",
                  child   == "　　最年少の子供が１１歳" ~ "6to14",
                  child   == "　　最年少の子供が１２歳" ~ "6to14",
                  child   == "　　最年少の子供が１３歳" ~ "6to14",
                  child   == "　　最年少の子供が１４歳" ~ "6to14",
                  TRUE ~ child
                )) %>% 
  dplyr::filter(child == "under5" | child == "6to14") %>% 
  dplyr::group_by(year, pref, couple, child, emp_hub, emp_wife) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from  = c(emp_hub, emp_wife),   
                     values_from = value) %>% 
  dplyr::rename("hub_emp" = "　夫が就業者_総数（妻の労働力状態「不詳」を含む。）",
                "wife_emp" = "総数（夫の労働力状態「不詳」を含む。）_　妻が就業者",
                "total"    = "総数（夫の労働力状態「不詳」を含む。）_総数（妻の労働力状態「不詳」を含む。）") %>% 
  dplyr::mutate(Mom_Emp  = wife_emp / total,
                Dad_Emp  = hub_emp  / total) %>% 
  dplyr::select(year, pref, couple, child, total, Mom_Emp, Dad_Emp)

## 保育所定員率 ----
col_name = c("pref", rep("hoge", 3), "cap", rep("hoge", 11))
nursery_2005 <-
  arrow::read_csv_arrow("data/2005_social.csv", # 事前に一部の行削除
                        read_options = CsvReadOptions$create(encoding = "SJIS",
                                                             skip_rows = 8,
                                                             column_names = col_name),
                        col_select = !starts_with("hoge")) %>%
  dplyr::slice(-48, -63, -65, -92) %>% 
  dplyr::mutate(pref = str_remove_all(pref, "\\s"),
                cap = readr::parse_number(cap),
                pref = ifelse(pref %in% names(city_map), city_map[pref], pref)) %>%
  group_by(pref) %>%
  summarise(cap = sum(cap)) %>%
  ungroup() %>% 
  mutate(year = 2005)



# 2010 --------------------------------------------------------------------

## 就業率 ----
## 統計名	国勢調査 平成22年国勢調査 産業等基本集計（労働力状態，就業者の産業など） 	
## 表番号	02300 	
## 表題	  世帯の家族類型(３区分)(夫婦のいる３世代世帯－特掲)，子供の有無・数・年齢(49区分)，夫婦の就業・非就業(４区分)(雇用者－特掲)別夫婦のいる一般世帯数及び一般世帯人員 

# path <- list.files(path = "data/2010", full.names = T)
# file_list <- map(path, ~ readr::read_csv(., locale = locale(encoding = "SJIS"),
#                                          show_col_types = FALSE,
#                                          col_types = "cccccccccccccccccd"))
# combined_data <- bind_rows(file_list)
# combined_data %>% write_dataset(path = "output/2010.parquet", format = "parquet")

dat_2010 <-
  arrow::open_dataset("output/2010.parquet", format = "parquet") %>%
  dplyr::filter(str_ends(area_code, "000") &
                area_code != "00000" & 
                unit == "世帯" &
                cat01_code == "00710") %>% 
  dplyr::select(year     = "時間軸（年次）",
                pref     = "地域（2010）",
                couple   = "夫婦のいる一般世帯の家族類型2010",
                child    = "子供の年齢等2010",
                emp_hub  = "夫の就業・非就業の別2010",
                emp_wife = "妻の就業・非就業の別2010",
                value) %>% 
  dplyr::filter((couple == "総数（夫婦のいる一般世帯の家族類型）" |
                 couple == "夫婦のいる核家族世帯" | 
                 couple == "夫婦のいるその他の世帯（同居の親あり）")) %>% 
  dplyr::collect() %>% 
  dplyr::mutate(year     = readr::parse_number(year),
                couple   = case_when(
                  couple == "総数（夫婦のいる一般世帯の家族類型）" ~ "all",
                  couple == "夫婦のいる核家族世帯" ~ "nuclear",
                  couple == "夫婦のいるその他の世帯（同居の親あり）" ~ "3_gen"),
                child    = case_when(
                  child   == "最年少の子供　0歳" 　~ "under5",
                  child   == "最年少の子供　1歳" 　~ "under5",
                  child   == "最年少の子供　2歳" 　~ "under5",
                  child   == "最年少の子供　3歳" 　~ "under5",
                  child   == "最年少の子供　4歳" 　~ "under5",
                  child   == "最年少の子供　5歳" 　~ "under5",
                  child   == "最年少の子供　6歳"　 ~ "6to14",
                  child   == "最年少の子供　7歳"　 ~ "6to14",
                  child   == "最年少の子供　8歳"　 ~ "6to14",
                  child   == "最年少の子供　9歳"　 ~ "6to14",
                  child   == "最年少の子供　10歳"　 ~ "6to14",
                  child   == "最年少の子供　11歳"　 ~ "6to14",
                  child   == "最年少の子供　12歳"　 ~ "6to14",
                  child   == "最年少の子供　13歳"　 ~ "6to14",
                  child   == "最年少の子供　14歳"　 ~ "6to14",
                  TRUE ~ child
                )) %>% 
  dplyr::filter(child == "under5" | child == "6to14") %>% 
  dplyr::group_by(year, pref, couple, child, emp_hub, emp_wife) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from  = c(emp_hub, emp_wife),   
                     values_from = value) %>% 
  dplyr::rename("hub_emp"  = "夫が就業者_総数（妻の就業・非就業）",
                "wife_emp" = "総数（夫の就業・非就業）_妻が就業者",
                "total"    = "総数（夫の就業・非就業）_総数（妻の就業・非就業）") %>% 
  dplyr::mutate(Mom_Emp  = wife_emp / total,
                Dad_Emp  = hub_emp  / total) %>% 
  dplyr::select(year, pref, couple, child, total, Mom_Emp, Dad_Emp)

## 保育所定員率 ----
col_name = c("pref", rep("hoge", 3), "cap", rep("hoge", 14))
nursery_2010 <-
  arrow::read_csv_arrow("data/2010_social.csv",
                        read_options = CsvReadOptions$create(encoding = "SJIS",
                                                             skip_rows = 8,
                                                             column_names = col_name),
                        col_select = !starts_with("hoge")) %>%
  dplyr::slice(-1, -49, -69, -110:-219) %>% 
  dplyr::mutate(pref = str_remove_all(pref, "\\s"),
                cap = readr::parse_number(cap)) %>%
  mutate(pref = ifelse(pref %in% names(city_map), city_map[pref], pref)) %>%
  group_by(pref) %>%
  summarise(cap = sum(cap)) %>%
  ungroup() %>% 
  mutate(year = 2010)

## 夫の年齢 ----
## 統計名	国勢調査 平成22年国勢調査 産業等基本集計（労働力状態，就業者の産業など）
## 表番号	02000
## 表題	夫の年齢(５歳階級)，子供の有無・数・年齢(121区分)，夫婦の就業・非就業(４区分)(雇用者－特掲)別夫婦のいる一般世帯数及び一般世帯人員

# path <- list.files(path = "data/2010_age_hub", full.names = T)
# file_list <- map(path, ~ arrow::read_csv_arrow(.))
# age_2010_hub <-
#   bind_rows(file_list) %>%
#   dplyr::filter("地域（2010）" == "全国" &
#                 "夫の就業・非就業の別2010" == "総数（夫の就業・非就業）" &
#                 "" == "")

## 妻の年齢 ----
## 統計名	国勢調査 平成22年国勢調査 産業等基本集計（労働力状態，就業者の産業など） 	
## 表番号	02100 	
## 表題	妻の年齢(５歳階級)，子供の有無・数・年齢(121区分)，夫婦の就業・非就業(４区分)(雇用者－特掲)別夫婦のいる一般世帯数及び一般世帯人員
# 
# age_2010_wife <- arrow::read_csv_arrow("data/2010_age_wife/FEH_00200521_251201083833.csv",
#                                       skip = 0)
# path <- list.files(path = "data/2010_age_wife", full.names = T)
# file_list <- map(path, ~ arrow::read_csv_arrow(.))
# combined_data <- bind_rows(file_list)



# 失業率 ---------------------------------------------------------------------

unemp <-
  arrow::read_csv_arrow("data/unemp_r.csv", skip = 1,
                        col_types = schema("地域 コード" = string())) %>% 
  dplyr::select(year  = "調査年",
                code  = "地域 コード",
                pref  = "地域",
                unemp = "#F01301_完全失業率【％】") %>% 
  dplyr::mutate(year  = readr::parse_number(year),
                unemp = unemp/100) %>% 
  dplyr::filter(year >= 1990 & year <= 2010)


# 子どもの数 -------------------------------------------------------------------
child <-
  arrow::read_csv_arrow("data/child.csv", skip = 1,
                        col_types = schema("地域 コード" = string())) %>% 
  dplyr::select(year    = "調査年",
                code    = "地域 コード",
                pref    = "地域",
                child_n = "A1405_0～5歳人口【人】") %>% 
  dplyr::mutate(year  = readr::parse_number(year)) %>% 
  dplyr::filter(year >= 1990 & year <= 2010)



# データの合成 ------------------------------------------------------------------

dat_1990 %<>% left_join(nursery_1990, by = c("pref", "year"))
dat_1995 %<>% left_join(nursery_1995, by = c("pref", "year"))
dat_2000 %<>% left_join(nursery_2000, by = c("pref", "year"))
dat_2005 %<>% left_join(nursery_2005, by = c("pref", "year"))
dat_2010 %<>% left_join(nursery_2010, by = c("pref", "year"))

dat <-
  rbind(dat_1990, dat_1995, dat_2000, dat_2005, dat_2010) %>% 
  dplyr::left_join(unemp, by = c("year", "pref")) %>% 
  dplyr::left_join(child, by = c("year", "pref", "code")) %>% 
  dplyr::select(year, code, pref, everything()) %>% 
  dplyr::mutate(cap_r = cap/child_n)

readr::write_csv(dat, file = "output/dat.csv")
readr::write_excel_csv(dat, file = "output/dat.csv")

date_2000 <- dat_2000 %>% dplyr::left_join(child, by = c("year", "pref")) %>%
  left_join(nursery_2000, by = c("pref", "year")) %>%
  mutate(ddd = cap/children)

dat_e <- date_2000 %>% filter(couple == "nuclear" &
                          child  == "under5")
weighted.mean(dat_e$ddd, dat_e$total)
mean(dat_e$ddd)


dat_e <- dat %>% filter(year   == 1990,
                        couple == "3_gen" &
                        child  == "under5")
weighted.mean(dat_e$cap_r, dat_e$total)
mean(dat_e$cap_r)







