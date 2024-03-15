#------------------------------------------------------------
#This R script was presented as a poster at the 76th Annual Meeting of the Japanese Association for Mathematical Sociology on March 17, 2024. 
#The title was "Reevaluation of the 'Under-Enrollment' Problem in Private Universities: Focusing on Time-Dependent Treatments and Confounding Factors."
#If you need a translation of the following description, which is in Japanese, please feel free to contact us, and we will provide you with an English translation.
#------------------------------------------------------------

#------------------------------------------------------------
#準備
#------------------------------------------------------------
#Rのバージョン確認
R.version

#ワークスペースのクリア: オブジェクト一括削除
rm(list = ls())

#データ取り込み
d <- read.csv(file.choose())
str(d)

#------------------------------------------------------------
#パッケージの読み込み
#------------------------------------------------------------
library("plm")

#------------------------------------------------------------
#変数の生成
#------------------------------------------------------------
#欠測値を含む行を削除
d_no_na <- na.omit(d)
#入学定員充足率を計算
admission_rate <- d_no_na$enrolment / d_no_na$capa
#exposureを定義
exposure <- ifelse(admission_rate >= 1, 0, 1) #1以上であれば0, 0未満であれば1
#wave変数をtime変数に変換
time <- d_no_na$wave - 2012
#結果の確認
summary(admission_rate)
summary(exposure)
summary(time)

#------------------------------------------------------------
#固定効果モデル
#------------------------------------------------------------
#データをplm用の形式に変換
conversion <- pdata.frame(d_no_na, index=c("id","wave"), drop.index=TRUE)
head(conversion)
#推定
model_plm <- plm(outcome ~ exposure + confounder + students + status + scienced + hospitald + womend + cityd + foundation, data = conversion,
                 method = "within", effect = "twoways")
summary(model_plm)
# model_plmオブジェクトの標本サイズを取得
n_obs <- nobs(model_plm)
n_obs