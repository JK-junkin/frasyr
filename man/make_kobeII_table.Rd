% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.r
\encoding{UTF-8}
\name{make_kobeII_table}
\alias{make_kobeII_table}
\title{beta.simluationの結果などを読んで、kobeII talbeに整形する関数}
\usage{
make_kobeII_table(
  kobeII_data,
  res_vpa,
  year.catch = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.ssb = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.Fsakugen = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.ssbtarget = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.ssblimit = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.ssbban = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.ssbmin = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.ssbmax = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.aav = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.risk = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  year.catchdiff = (max(as.numeric(colnames(res_vpa$naa))) + 1:10),
  Btarget = 0,
  Blimit = 0,
  Bban = 0,
  Bspecific = NULL
)
}
\arguments{
\item{kobeII_data}{beta.simulationまたはconvert_future_list_tableの返り値}

\item{res_vpa}{VPAの結果}

\item{Bspecific}{特定の資源量を一回でも下回るリスクを計算する。下回るリスクを判断する年はyear.riskに従う}
}
\description{
beta.simluationの結果などを読んで、kobeII talbeに整形する関数
}
\details{
tidy形式になっているkobeII_dataにおいて、HCR_name, betaの列のラベルの組み合わせを一つの管理方式として、その管理方式ごとに少尉予測の結果を集計する
}
