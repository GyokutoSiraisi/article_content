
# 準備 ----------------------------------------------------------------------

## パッケージの読み込み
pacman::p_load(tidyverse,
               magrittr,
               labelled,
               tinylabels)

## データの読み込み
df <-　datasets::iris %>% 
  　　 as_tibble()



# ラベルを付ける -----------------------------------------------------------------

## R標準機能 ====================================

### ラベルの付与
attr(df$Species, "label") <- "学名（3種類）"

### ラベルの確認
attr(df$Species, "label")

### ラベルの除去
attr(df$Species, "label") <- NULL

### 複数のラベルを付与
attr(df$Species,      "label") <- "学名（3種類）"
attr(df$Sepal.Length, "label") <- "萼片の長さ（cm）"
attr(df$Sepal.Width,  "label") <- "萼片の幅（cm）"
attr(df$Petal.Length, "label") <- "花弁の長さ（cm）"
attr(df$Petal.Width,  "label") <- "花弁の幅（cm）"

### 複数のラベルを確認
str(df)
lapply(df, function(x){attr(x, "label")}) # labelだけを表示

### 複数のラベルを除去
for (i in seq_along(df)) {
  attr(df[[i]], "label") <- NULL
}



## labelled パッケージ ==================================

### ラベルの付与
var_label(df$Species) <- "学名（3種類）"

### ラベルの確認
var_label(df$Species)

### ラベルの除去
var_label(df$Species) <- NULL
df$Species <- remove_labels(df$Species) # こちらも可

### 複数のラベルを付与
var_label(df) <- list(
  Species      = "学名（3種類）",
  Sepal.Length = "萼片の長さ（cm）",
  Sepal.Width  = "萼片の幅（cm）",
  Petal.Length = "花弁の長さ（cm）",
  Petal.Width  = "花弁の幅（cm）"
)

### 複数のラベルを付与（tidyverse風）
df %<>%
  set_variable_labels(
    Species      = "学名（3種類）",
    Sepal.Length = "萼片の長さ（cm）",
    Sepal.Width  = "萼片の幅（cm）",
    Petal.Length = "花弁の長さ（cm）",
    Petal.Width  = "花弁の幅（cm）"
  )

### 複数のラベルを確認
var_label(df)

### 複数のラベルを除去
df <- remove_labels(df)



## tinylabels パッケージ ==================================

### ラベルの付与
variable_label(df$Species) <- "学名（3種類）"
variable_label(df) <- c(Species = "学名（3種類）") # こちらも可

### ラベルの確認
variable_label(df$Species)

### ラベルの除去
df$Species <- unlabel(df$Species)

### 複数のラベルを付与
variable_label(df) <- c(
  Species      = "学名（3種類）",
  Sepal.Length = "萼片の長さ（cm）",
  Sepal.Width  = "萼片の幅（cm）",
  Petal.Length = "花弁の長さ（cm）",
  Petal.Width  = "花弁の幅（cm）"
)

### 複数のラベルを付与（tidyverse風）
df %<>%
  label_variables(
    Species      = "学名（3種類）",
    Sepal.Length = "萼片の長さ（cm）",
    Sepal.Width  = "萼片の幅（cm）",
    Petal.Length = "花弁の長さ（cm）",
    Petal.Width  = "花弁の幅（cm）"
  )

### 複数のラベルを確認
variable_label(df)

### 複数のラベルを除去
df <- unlabel(df)



# ラベルを保存する（csv, RData, RDS ------------------------------------------------

## RData
save(df, file = "output/df.RData")
# load("output/df.RData")

## RDS
saveRDS(df, "output/df.rds")
# df <- readr::read_rds("output/df.rds")

## csv
### データはそのままcsvに
readr::write_excel_csv(df, "output/iris.csv")

### ラベルを前処理をしてからcsvに
df_label <-
  tibble(
    variable = colnames(df),
    label    = labelled::var_label(df, unlist = TRUE)
  )
readr::write_excel_csv(df_list, "output/iris_label.csv")

### csvから読み込む
df_label <-
  arrow::read_csv_arrow("output/iris_label.csv") %>% 
  deframe()

df <- arrow::read_csv_arrow("output/iris.csv") %>%
  labelled::set_variable_labels(.labels = df_label)



# 説明以外の用途（検索・作図） ----------------------------------------------------------

## 検索
look_for(df, "萼")　# 「萼」というラベルが含まれる変数を検索

## 作図
ggplot(df, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point()





