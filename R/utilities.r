#'
#' @import ggplot2
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import readr
#' @import forcats
#' @import stringr
#' @import assertthat
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#' @importFrom dplyr filter
#'
NULL

# 将来予測や管理基準値計算に使われる関数群 ----

# 個体群動態に関する基本的関数 ----

#'
#' 年齢別パラメータを与えて、１年分前進計算する
#'
#' @param min.age 最小の年齢（０歳がデフォルト）。数でなくて名前
#' @param max.age 最大の年齢。数でなくて名前なので、０からスタートして２歳までの場合には、min.age=0, max.age=2とするか、min.age=1, max.age=3とする
#'
#' @export
#' @encoding UTF-8

calc.rel.abund <- function(sel,Fr,na,M,waa,waa.catch=NULL,maa,min.age=0,max.age=Inf,Pope=TRUE,ssb.coef=0){

  if(is.null(waa.catch)) waa.catch <- waa
  rel.abund <- rep(NA, na)
  rel.abund[1] <- 1
    
  for (i in seq_len(na-1)) {
    rel.abund[i+1] <- rel.abund[i]*exp(-M[i]-sel[i]*Fr)
  }
  rel.abund[na] <- rel.abund[na-1]*exp(-M[na-1]-sel[na-1]*Fr)*(1-exp(-((max.age-min.age)-(na-2))*(M[na]+sel[na]*Fr)))/(1-exp(-M[na]-sel[na]*Fr))

  if(isTRUE(Pope)){
    ypr1 <- rel.abund*waa.catch[1:na]*(1-exp(-sel[1:na]*Fr))*exp(-M[1:na]/2)
  }
  else{
    # use Baranov catch equation
    ypr1 <- rel.abund*(1-exp(-sel[1:na]*Fr-M[1:na]))*sel[1:na]*Fr/
      (sel[1:na]*Fr+M[1:na])*waa.catch[1:na]
  }
  spr <- rel.abund*waa[1:na]*maa[1:na]*exp(-ssb.coef*(sel[1:na]*Fr+M[1:na]))
  return(list(rel.abund=rel.abund,ypr=ypr1,spr=spr))
}

#'
#' 年齢別生物パラメータとFと漁獲量を与えると与えた漁獲量と一致するFへの乗数を返す
#'
#' @param max_exploitation_rate 潜在的に漁獲できる漁獲量＜入力した漁獲量の場合、潜在的に漁獲できる漁獲量の何％まで実際に漁獲するか
#' @param max_F F at ageの最大値となる値の上限をどこにおくか
#'
#' @export
#' @encoding UTF-8

caa.est.mat <- function(naa,saa,waa,M,catch.obs,Pope,set_max1=TRUE,max_exploitation_rate=0.99,max_F=exp(10)){

  tmpfunc <- function(logx,catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,out=FALSE,Pope=Pope){
    if(Pope==1 | Pope==TRUE) is.pope <- TRUE else is.pope <- FALSE
    x <- exp(logx)
    if(is.pope){
      caa <- naa*(1-exp(-saa*x))*exp(-M/2)
    }
    else{
      caa <- naa*(1-exp(-saa*x-M))*saa*x/(saa*x+M)
    }
    wcaa <- caa*waa
    if(out==FALSE){
        return(log((sum(wcaa,na.rm=T)-catch.obs)^2))
    }
    else{
      return(caa)
    }
  }

  C0 <- sum(tmpfunc(logx=100,catch.obs=catch.obs,naa=naa,saa=rep(1,length(saa)),waa=waa,M=M,Pope=Pope,out=TRUE) * waa)
  if(C0 < catch.obs){
    warning("The expected catch (", catch.obs, ") is over potential maximum catch (",round(C0,5),"). The expected catch is replaced by",round(C0,3),"x", max_exploitation_rate)
    catch.obs <- C0 * max_exploitation_rate

  }

  # caa.est.mat_wrongで初期値を設定するとはやそう
  tmp <- optimize(tmpfunc,c(-10,log(max_F)),catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,Pope=Pope,out=FALSE)#,tol=.Machine$double.eps)
  tmp2 <- tmpfunc(logx=tmp$minimum,catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,Pope=Pope,out=TRUE)
  realized.catch <- sum(tmp2*waa)
  if(abs(realized.catch/catch.obs-1)>0.1) warning("expected catch:",catch.obs,", realized catch:",realized.catch)
  return(list(x=exp(tmp$minimum),caa=tmp2,realized.catch=realized.catch, expected.catch=catch.obs))
}

#'
#' 年齢別生物パラメータとFと漁獲量を与えると与えた漁獲量と一致するFへの乗数を返す (optimを使わないでやろうとしたけどだめだったやつ）
#'
#' これだと漁獲量をぴったりに返すFのvectorは得られるが、もとの選択率に一致しない
#'
#' @param naa numbers at age
#' @param saa selectivity at age
#' @param waa weight at age
#' @param M natural mortality at age
#' @param max_exploitation_rate 潜在的に漁獲できる漁獲量＜入力した漁獲量の場合、潜在的に漁獲できる漁獲量の何％まで実際に漁獲するか
#' @param max_F F at ageの最大値となる値の上限をどこにおくか
#'
#' @export
#' @encoding UTF-8

caa.est.mat_wrong <- function(naa,saa,waa,M,catch.obs,Pope,max_exploitation_rate=0.99,max_F=exp(10)){

  C0 <- catch_equation(naa,exp(100),waa,M,Pope) %>% sum()
  if(C0 < catch.obs){
    warning("The expected catch (", catch.obs, ") is over potential maximum catch (",round(C0,5),"). The expected catch is replaced by",round(C0,3),"x", max_exploitation_rate)
    catch.obs <- C0 * max_exploitation_rate
  }

  caa_original <- catch_equation(naa,saa,1,M,Pope)
  catch_ratio <- catch.obs / sum(caa_original * waa)
  caa_expected <- caa_original * catch_ratio
  if(Pope==0){
    Fvector <- solv.Feq(caa_expected, naa, M)
  }
  else{
    Fvector <- solv.Feq.Pope(caa_expected, naa, M)
  }

  if( max_F < max(Fvector)) Fvector <- max_F * Fvector/max(Fvector)

  realized.catch <- catch_equation(naa,Fvector,waa,M,Pope)
  if(abs(sum(realized.catch)/catch.obs-1)>0.1) warning("expected catch:",catch.obs,", realized catch:",realized.catch)
  multi <- mean(Fvector/saa)
  return(list(x=multi,caa=realized.catch,realized.catch=sum(realized.catch), expected.catch=catch.obs, Fvector=Fvector))
}

# #　上の関数とどっちが使われているか不明,,,多分使われていないのでコメントアウトする
# caa.est <- function(naa,saa,waa,M,catch.obs,Pope){
#   saa <- saa/max(saa)
#   tmpfunc <- function(x,catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,out=FALSE,Pope=Pope){
#     if(isTRUE(Pope)){
#       caa <- naa*(1-exp(-saa*x))*exp(-M/2)
#     }
#     else{
#       caa <- naa*(1-exp(-saa*x-M))*saa*x/(saa*x+M)
#     }
#     wcaa <- caa*waa
#     if(out==FALSE){
#       return((sum(wcaa,na.rm=T)-catch.obs)^2)
#     }
#     else{
#       return(caa)
#     }
#   }
#   tmp <- optimize(tmpfunc,c(0,5),catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,Pope=Pope,out=FALSE)
#   tmp2 <- tmpfunc(x=tmp$minimum,catch.obs=catch.obs,naa=naa,saa=saa,waa=waa,M=M,Pope=Pope,out=TRUE)
#   return(list(x=tmp$minimum,caa=tmp2))
# }

catch_equation <- function(naa,faa,waa,M,Pope=1){
  if(Pope==1 | Pope==TRUE) is.pope <- TRUE else is.pope <- FALSE
  if(is.pope){
    wcaa_mat <- naa*(1-exp(-faa))*exp(-M/2) * waa
  }
  else{
    wcaa_mat <- naa*(1-exp(-faa-M))*faa/(faa+M) * waa
  }
  return(wcaa_mat)
}

#' @export
#' @encoding UTF-8

solv.Feq <- function(cvec,nvec,mvec){
  Fres <- rep(0,length(cvec))
  # cat(nvec," ")
  for(i in 1:length(cvec)){
    F0 <- cvec[i]/nvec[i]
    F1 <- cvec[i]*(F0+mvec[i])/nvec[i]/(1-exp(-F0-mvec[i]))
    if(round(cvec[i],6)<round(nvec[i],6)){
      while(abs(F0-F1)>0.0001 ){
        F0 <- F1
        F1 <- cvec[i]*(F0+mvec[i])/nvec[i]/(1-exp(-F0-mvec[i]))
        if(F0-F1==-Inf) cat("\n",cvec[i]," ",nvec[i]," \n")
      }
      Fres[i] <- F1
    }
    else{
      Fres[i] <- 10
      cat("Warning: catch exceeded tot_num at: ",i," ",
          round(cvec[i],6)," ",round(nvec[i],6),"\n")
    }
  }
  Fres
}

#' @export
solv.Feq.Pope <- function(cvec,nvec,mvec){
  -log(1-cvec/nvec/exp(-mvec/2))
}

#' VPAの結果に格納されている生物パラメータから世代時間を計算
#'
#' @export
#' @encoding UTF-8

Generation.Time <- function(vpares,
                            maa.year=2014:2015,
                            maa=NULL,
                            M.year=2014:2015,
                            M=NULL,
                            age=NULL,
                            Plus = 19
){

  if(is.null(maa)){
    maa <- vpares$input$dat$maa
    maa <- rowMeans(maa[,colnames(maa) %in% maa.year,drop=F],na.rm=T)
    maa <- maa[!is.na(maa)]
  }
  if(is.null(M)){
    M <- vpares$input$dat$M
    M <- rowMeans(M[,colnames(M) %in% M.year,drop=F],na.rm=T)
    M <- M[!is.na(M)]
  }

  if(is.null(age)) age <- as.numeric(names(maa))
  if(Plus>0){
    maa <- c(maa, rep(1,Plus))
    M <- c(M, rep(M[length(M)],Plus))
    age <- c(age, max(age)+1:Plus)
  }
  A <- length(M)
  L <- c(1,exp(-cumsum(M[-A])))
  G <- sum(age*L*maa)/sum(L*maa)

  return(G)
}

### dynamics MSYを計算してみる
dyn.msy <- function(naa.past,naa.init=NULL,fmsy,a,b,resid,resid.year,waa,maa,M,SR=TRUE){
  nyear <- length(resid)
  if(is.null(naa.init)) nage <- nrow(naa.past) else nage <- length(naa.init)
  naa <- matrix(0,nage,nyear)
  ssb <- numeric()
  if(is.null(naa.init)) naa[,1] <- naa.past[,colnames(naa.past)==min(resid.year)]
  else naa[,1] <- naa.init
  colnames(naa) <- resid.year
  if(is.null(naa.init)){
    waa <- waa[,colnames(naa.past)%in%resid.year]
    maa <- maa[,colnames(naa.past)%in%resid.year]
    M <- M[,colnames(naa.past)%in%resid.year]
  }
  for(i in 2:nyear){
    ssb[i-1] <- sum(naa[,i-1]*waa[,i-1]*maa[,i-1],na.rm=T)
    if(SR==TRUE){
      naa[1,i] <- HS(ssb[i-1],a,b)*exp(resid[i])
    }
    else{
      naa[1,i] <- naa.past[1,i]
    }
    for(j in 2:(nage-1)) naa[j,i] <- naa[j-1,i-1] * exp(-fmsy[j-1]-M[j-1,i-1])
    naa[nage,i] <- naa[nage-1,i-1] * exp(-fmsy[j-1]-M[j-1,i-1]) + naa[nage,i-1] * exp(-fmsy[nage]-M[nage,i-1])
  }
  i <- nyear ; ssb[i] <- sum(naa[,i]*waa[,i]*maa[,i])
  list(naa=naa,ssb=ssb)
}



# 再生産関係を仮定しない管理基準値計算 ----

#'
#' 再生産関係を仮定しない管理基準値計算(SPR,YPR,F0.1,Fmax)のための関数
#'
#' @param res VPAの出力結果(NULLも可)。ここがNULLの場合（VPAの出力結果を与えない場合）でも、Fcurrent, waa, maa, M, waa.catch, max.age, min.age, Popeを別途指定することによって、管理基準値計算ができるようになる
#' @param Fcurrent 仮定する選択率．NULLの場合，res$Fc.at.ageが使われる
#' @param waa 仮定する年齢別体重。直接の値を入れるか，waa.yearで年を指定するやり方のどちらでも動く。直接指定するほうが優先。
#' @param maa 仮定する年齢別成熟率。直接の値を入れるか，waa.yearで年を指定するやり方のどちらでも動く。直接指定するほうが優先。
#' @param M 仮定する年齢別死亡率。直接の値を入れるか，waa.yearで年を指定するやり方のどちらでも動く。直接指定するほうが優先。
#' @param waa.catch　仮定する年齢別体重（漁獲量計算用）。直接の値を入れるか，waa.yearで年を指定するやり方のどちらでも動く。直接指定するほうが優先。
#' @param M.year 年を指定して生物パラメータを仮定する場合．年の範囲の平均値が用いられる．NULLの場合，VPA最終年の値が使われる
#' @param waa.year 年を指定して生物パラメータを仮定する場合．年の範囲の平均値が用いられる．NULLの場合，VPA最終年の値が使われる
#' @param maa.year 年を指定して生物パラメータを仮定する場合．年の範囲の平均値が用いられる．NULLの場合，VPA最終年の値が使われる
#' @param rps.year Fmedの計算に使うRPSの年の範囲．NULLの場合，全範囲が用いられる
#' @param rps.vector Fmedの計算に使うRPSのベクトル。rps.yearよりもこちらが優先される。
#' @param max.age 加入年齢を０歳としたときに、SPR計算で考慮される最大の年齢の名前（何行目とかじゃないことに注意, デフォルトはInf）。min.ageも同様。VPA結果を与える場合にはVPA結果から自動的にもってくる。
#' @param min.age VPA結果を与える場合にはVPA結果から自動的にもってくるが、VPA結果を与えない場合、加入年齢を入力する
#' @param  pSPR = seq(10,90,by=10), # F\%SPRを計算するときの％SPR
#' @param d 0.001
#' @param  Fem.init 経験的管理基準値(Fmed, Fmean, Fhigh, Flow)の初期値 (default=0.5)
#' @param  Fmax.init Fmaxの初期値 (default=1.5)
#' @param  F0.1.init F0.1の初期値 (default=0.7)
#' @param  iterlim
#' @param  plot 結果のプロットを表示するかどうか
#' @param  Pope Popeの式を使うか
#' @param  F.range YPR, SPR曲線を書くときのFの範囲（Fの最大値のスケール）、かつ、F\%SPRを計算するときの初期値を決めるために利用される。F\%SPRの推定がうまくいかない場合はこの範囲を調整してください。
#' @encoding UTF-8
#'
#' @note F_SPRのF管理基準値の初期値は　与えられたFのもとでのSPR/目的のSPR　を初期値とするように調整されるので不要。プラスグループを考慮するかどうかはVPAの結果のinput$plus.groupから自動判別される。
#'
#' @examples
#' data(res_vpa)
#' # VPAデータを使う場合
#' res_refF1 <- ref.F(res=res_vpa,Fcurrent=frasyr::apply_year_colum(res_vpa$faa,2015:2017),
#'                 waa.year=2015:2017,maa.year=2015:2017,M.year=2015:2017)
#'
#' # 生物パラメータをデータとして与える場合
#' res_refF2 <- ref.F(res=NULL,Fcurrent=rep(0.1,5),
#'                    waa=rep(100,5),maa=c(0,0,1,1,1),M=rep(0.3,5),waa.catch=rep(100,5),
#'                    rps.vector=NULL, # Fmedを計算したりする場合のRPSのベクトル.NULLでもOK
#'                    Pope=TRUE,min.age=0,pSPR=c(30,40))
#'
#'
#' @export
#' @import tibble
#' @encoding UTF-8
#'

# ref.F
ref.F <- function(
  res=NULL, # VPAの結果のオブジェクト
  Fcurrent=NULL, # Fcurrentの仮定．NULLの場合，res$Fc.at.ageが使われる
  waa=NULL, # 仮定する生物パラメータ．直接の値を入れるか，年を指定するやり方のどちらでも動く。直接指定するほうが優先。
  maa=NULL,
  M=NULL,
  waa.catch=NULL,
  M.year=NULL,
  waa.year=NULL, # 年を指定して生物パラメータを仮定する場合．年の範囲の平均値が用いられる．NULLの場合，VPA最終年の値が使われる
  waa.catch.year=NULL,
  maa.year=NULL,
  rps.year = NULL, # Fmedの計算に使うRPSの年の範囲．NULLの場合，全範囲が用いられる
  rps.vector = NULL,
  max.age = Inf,
  min.age = 0,
  d = 0.001,
  Fem.init = 0.5,
  Fmax.init = 1.5, # Fmaxの初期値
  F0.1.init = 0.7, # F0.1の初期値
  pSPR = seq(10,90,by=10), # F%SPRを計算するときの％SPR
  iterlim=1000,
  plot=TRUE,
  Pope=NULL, # 2014.7.4追加
  F.range = seq(from=0,to=2,length=101)  # YPR, SPR曲線を書くときのFの範囲
){

  argname <- ls()
  arglist <- lapply(argname,function(x) eval(parse(text=x)))
  names(arglist) <- argname

  if(!is.null(res)){
    if(is.null(Pope)) Pope <- res$input$Pope

    naa <- res$naa
    ssb <- res$ssb
    ny <- ncol(naa)
    years <- dimnames(naa)[[2]]
    ages <- dimnames(naa)[[1]]

    if(is.null(Fcurrent)){
      Fcurrent <- res$Fc.at.age
    }
    sel <- Fcurrent/max(Fcurrent,na.rm=TRUE)
    na <- sum(!is.na(Fcurrent))

    if(is.null(waa.year)) waa.year <- rev(years)[1]
    if(is.null(maa.year)) maa.year <- rev(years)[1]
    if(is.null(M.year)) M.year <- rev(years)[1]
    if(is.null(waa.catch.year)) waa.catch.year <- rev(years)[1]    

    if(is.null(waa))  waa <- apply_year_colum(res$input$dat$waa,waa.year)
    if(is.null(M))    M   <- apply_year_colum(res$input$dat$M,M.year)
    if(is.null(maa))  maa <- apply_year_colum(res$input$dat$maa,maa.year)
    if(is.null(waa.catch)){
        if(!is.null(res$input$dat$waa.catch)) waa.catch <- apply_year_colum(res$input$dat$waa.catch,waa.catch.year)
        else waa.catch <- waa
    }

    if(is.null(waa.catch)){
      if(is.null(res$input$dat$waa.catch)){
        waa.catch <- waa
      }
      else{
        waa.catch <- apply_year_colum(res$input$dat$waa.catch,waa.year)
      }
    }

    ssb.coef <- ifelse(is.null(res$ssb.coef),0,res$ssb.coef)

    min.age <- min(as.numeric(rownames(res$naa)))
    if(min.age==0) slide.tmp <- TRUE else slide.tmp <- -1:-min.age

    if(!is.null(rps.year)){
      rps.data <- data.frame(year=as.numeric(names(colSums(ssb,na.rm=T))),
                             ssb=as.numeric(colSums(ssb,na.rm=T)),
                             recruit=as.numeric(c(naa[1,slide.tmp],rep(NA,min.age))))
      if (sum(is.na(rps.data$year))>0) rps.data <- rps.data[-which(is.na(rps.data$year)),]
      rps.data$rps <- rps <- rps.data$recruit/rps.data$ssb
      #  rps <- as.numeric(naa[1,]/colSums(ssb,na.rm=TRUE))

      #  if (is.null(rps.year)) rps.year <- years

      tmp <- rps.data$year %in% rps.year
      rps.q <- quantile(rps[tmp], na.rm=TRUE, probs=c(0.1,0.5,0.9))
      rps.q <- c(rps.q,mean(rps[tmp], na.rm=TRUE))
      names(rps.q)[4] <- "mean"
      spr.q <- 1/rps.q
    }
    else{
      rps <- NULL
      rps.q <- NULL
      spr.q <- NULL
      rps.data <- NULL
    }
  }
  if(is.null(res)){ # VPA結果を与えない場合
    sel <- Fcurrent/max(Fcurrent,na.rm=TRUE)
    na <- length(Fcurrent)
    assertthat::assert_that(length(Fcurrent) == na)
    assertthat::assert_that(length(waa) == na)
    assertthat::assert_that(length(maa) == na)
    assertthat::assert_that(length(M)   == na)
    assertthat::assert_that(length(waa.catch) == na)
    assertthat::assert_that(!is.null(Pope))
    ssb.coef <- 0

    if(!is.null(rps.vector)){
      rps <- rps.data <- rps.vector
      rps.q <- quantile(rps, na.rm=TRUE, probs=c(0.1,0.5,0.9))
      rps.q <- c(rps.q,mean(as.numeric(rps), na.rm=TRUE))
      names(rps.q)[4] <- "mean"
      spr.q <- 1/rps.q
    }
    else{
      rps <- NULL
      rps.q <- NULL
      spr.q <- NULL
      rps.data <- NULL
    }
  }

  if(!is.null(res) && res$input$plus.group==FALSE){
    min.age <- min(as.numeric(rownames(res$naa)))
    max.age <- max(as.numeric(rownames(res$naa)))
  }

  calc.rel.abund2_ <- function(Fx,multi){
    calc.rel.abund(Fx,multi,na,M,waa,waa.catch,maa,
                   min.age=min.age,
                   max.age=max.age,
                   Pope=Pope,ssb.coef=ssb.coef)
  }

  original.spr <- calc.rel.abund2_(Fcurrent,1)
  original.spr0 <- calc.rel.abund2_(Fcurrent,0)
  original.perspr <- sum(original.spr$spr,na.rm=T)/sum(original.spr0$spr,na.rm=T)

  # Fcurrent
  Fcurrent_max_mean <- c(max(Fcurrent,na.rm=T), mean(Fcurrent,na.rm=T))

  # grid search
  Fcurrent_max <- Fcurrent_max_mean[1]
  F.range <- sort(c(F.range,  Fcurrent_max))
  spr0 <- sum(original.spr0$spr,na.rm=T)
  tmp <- lapply(F.range, function(x) calc.rel.abund2_(sel,x))
  ypr <- sapply(tmp,function(x) sum(x$ypr,na.rm=T))
  pspr <- sapply(tmp,function(x) sum(x$spr,na.rm=T))/spr0*100
  ypr.spr <- data.frame(F.range=F.range,ypr=ypr,pspr=pspr)
  ypr.spr$Frange2Fcurrent  <- ypr.spr$F.range/Fcurrent_max

  # F.spr

  spr.f.est <- function(log.p, out=FALSE, sub="med", spr0=NULL){
    Fr <- exp(log.p)

    tmp <- calc.rel.abund2_(sel,Fr)
    rel.abund <- tmp$rel.abund
    spr <- sum(tmp$spr,na.rm=T)
    if (isTRUE(out)) obj <- spr
    else{
      if(sub=="mean") obj <- (spr-spr.q[4])^2
      if(sub=="low") obj <- (spr-spr.q[3])^2
      if(sub=="med") obj <- (spr-spr.q[2])^2
      if(sub=="high") obj <- (spr-spr.q[1])^2
      if(is.numeric(sub)) obj <- (spr/spr0-sub/100)^2

    }

    return(obj)
  }

  spr0 <- spr.f.est(-Inf, out=TRUE)

  if(!is.null(rps)){
    Fmed.res <- nlm(spr.f.est, Fem.init, out=FALSE, sub="med", iterlim = iterlim)
    Fmean.res <- nlm(spr.f.est, Fem.init, out=FALSE, sub="mean", iterlim = iterlim)
    Flow.res <- nlm(spr.f.est, Fem.init, out=FALSE, sub="low", iterlim = iterlim)
    Fhigh.res <- nlm(spr.f.est, Fem.init, out=FALSE, sub="high", iterlim = iterlim)

    Fmean <- exp(Fmean.res$estimate)
    Fmed <- exp(Fmed.res$estimate)
    Flow <- exp(Flow.res$estimate)
    Fhigh <- exp(Fhigh.res$estimate)
  }
  else{
    Fmean <- Fmed <- Flow <- Fhigh <- NA
  }

  if (!is.null(pSPR)){
    FpSPR <- NULL

    for (i in pSPR){
      tmp <- which.min(abs(ypr.spr$pspr-i))+c(-1,1)
      tmp <- ifelse(tmp<=0,1,tmp)
      tmp <- ifelse(tmp>length(ypr.spr$F.range),length(ypr.spr$F.range),tmp)
      Fspr.init <- log(ypr.spr$F.range[tmp]) #original.perspr/i*100
      Fspr.init[Fspr.init==-Inf] <- -10
      FpSPR.res <- optimize(spr.f.est, Fspr.init,out=FALSE,sub=i,spr0=spr0)
      #nlm(spr.f.est, Fspr.init, out=FALSE, sub=i, spr0=spr0, iterlim=iterlim)
      #      cat("Estimate F%spr: initial value=", Fspr.init," : estimated value=",exp(FpSPR.res$estimate),"\n")
      FpSPR <- c(FpSPR, exp(FpSPR.res$minimum))
    }
    names(FpSPR) <- paste(pSPR,"%SPR",sep="")
  }

  # Fmax

  ypr.f.est <- function(log.p, out=FALSE){
    Fr <- exp(log.p)

    tmp <- calc.rel.abund2_(sel,Fr)
    rel.abund <- tmp$rel.abund
    ypr <- sum(tmp$ypr,na.rm=T)

    if (isTRUE(out)) obj <- ypr else obj <- -ypr

    return(obj)
  }

  Fmax.res <- nlm(ypr.f.est, log(Fmax.init), out=FALSE)

  Fmax <- exp(Fmax.res$estimate)

  # F0.1

  Fp <- function(log.p, out=FALSE){
    Fr <- exp(log.p)

    tmp <- calc.rel.abund2_(sel,Fr)
    rel.abund <- tmp$rel.abund
    ypr <- sum(tmp$ypr,na.rm=T)
    if (isTRUE(out)) obj <- ypr else obj <- -ypr

    return(obj)
  }

  F0.1.est <- function(log.p){
    p <- exp(log.p)
    ref.trend <- (Fp(log(d))-Fp(log(0)))/d
    trend <- (Fp(log(p+d)) - Fp(log(p)))/d

    obj <- (ref.trend/10 - trend)^2

    obj
  }

  F0.1.res <- nlm(F0.1.est,log(F0.1.init))

  F0.1 <- exp(F0.1.res$estimate)


  # output
  f.mean <- function(x) mean(x*sel, na.rm=T)

  Fmean <- c(Fmean, f.mean(Fmean))
  Fmed <- c(Fmed, f.mean(Fmed))
  Flow <- c(Flow, f.mean(Flow))
  Fhigh <- c(Fhigh, f.mean(Fhigh))
  Fmax <- c(Fmax, f.mean(Fmax))
  F0.1 <- c(F0.1, f.mean(F0.1))

  names(Fcurrent_max_mean) <- names(Fmed) <- names(Fmean) <- names(Flow) <- names(Fhigh) <- names(Fmax) <- names(F0.1) <- c("max","mean")
  Res <- list(min.age=min.age, max.age=max.age, rps.q=rps.q, spr.q=spr.q, Fcurrent=Fcurrent_max_mean,
              sel=sel, Fmed=Fmed, Flow=Flow, Fhigh=Fhigh, Fmax=Fmax, F0.1=F0.1, Fmean=Fmean,rps.data=rps.data)

  if (!is.null(pSPR)){
    FpSPR <- rbind(FpSPR, sapply(FpSPR, f.mean))
    rownames(FpSPR) <- c("max","mean")
    Res$FpSPR <- FpSPR
  }

  #---- make summary
  Res$summary <- as.data.frame(Res[substr(names(Res),1,1)=="F"])
  Res$summary <- rbind(Res$summary,Res$summary[1,]/Res$summary[1,1])
  dimnames(Res$summary)[[1]][3] <- "Fref/Fcur"

  Res$currentSPR <- list(SPR=sum(original.spr$spr,na.rm=T),
                         perSPR=original.perspr,
                         YPR=sum(original.spr$spr,na.rm=T),
                         Fcurrent=Fcurrent)

  Res$ypr.spr  <- ypr.spr #data.frame(F.range=F.range,ypr=ypr,spr=spr)
  Res$waa <- waa
  Res$waa.catch <- waa.catch
  Res$maa <- maa
  Res$arglist <- arglist
  Res$spr0 <- spr0

  class(Res) <- "ref"

  if(isTRUE(plot)){
    plot_Fref(Res)
  }
  return(Res)
}

#' 毎年のFの\%SPRやターゲットした\%SPRに相当するFの大きさを計算する
#'
#' VPA計算結果を使って毎年のF at ageがどのくらいのSPR, YPRに相当するかを計算する。また、各年のFが、目標としたSPR（target.SPR）を達成するためのF(Ftarget)の何倍(F/Ftarget)に相当するかも計算する。F/Ftargetは数値的に探索するが、そのときのF/Ftargetの上限をFmaxにて指定する。十分大きい値（デフォルトは１０）を与えておけば大丈夫だが、Ftargetが非常に小さい数字になりそうな場合にはこの値をもっと大きくとるなどする。また、SPRの計算は、デフォルトでは等比級数の和の公式を使って、無限大の年齢までSPRを足しているが、max.ageを指定することで、有限の年齢までの和も計算できる。
#'
#' @param dres vpa関数の返り値
#' @param target.SPR 目標とするSPR。この値を入れると、結果の$ysdata$"F/Ftarget"で、その年のFが目標としたSPR(％)を達成するためのF（Ftarget）の何倍になっているかを返す。デフォルトは30が入っている。このとき、SPRを計算するための生物パラメータ（年齢別体重・成熟率・死亡率）はそれぞれの年で仮定されているものを用いる。
#' @param Fmax F/Ftargetを推定するときに探索するFの乗数の最大値
#' @param max.age SPRやYPRの計算をするときに最大何歳まで考慮するか（年齢のラベルではなく、ベクトルの長さ。デフォルトは無限大)。VPA計算でプラスグループを考慮していない（dres$input$plus.group==FALSE)場合には自動的に設定される。
#'
#' @encoding UTF-8
#'
#' @examples
#' data(res_vpa)
#' Fratio <- get.SPR(res_vpa,target.SPR=12)$ysdata$"F/Ftarget"
#' plot(colnames(res_vpa$naa),Fratio,type="b")
#'
#'
#' @export
#' @encoding UTF-8

get.SPR <- function(dres,target.SPR=30,Fmax=10,max.age=Inf){
  dres$ysdata <- matrix(0,ncol(dres$faa),5)
  dimnames(dres$ysdata) <- list(colnames(dres$faa),c("perSPR","YPR","SPR","SPR0","F/Ftarget"))
  for(i in 1:ncol(dres$faa)){
    dres$Fc.at.age <- dres$faa[,i] # Fc.at.ageに対象年のFAAを入れる
    if(!all(dres$Fc.at.age==0, na.rm=T)){
      byear <- colnames(dres$faa)[i] # 何年の生物パラメータを使うか

      a <- ref.F(dres,waa.year=byear,maa.year=byear,M.year=byear,rps.year=2000:2011,
                 pSPR=round(target.SPR),
                 F.range=c(seq(from=0,to=ceiling(max(dres$Fc.at.age,na.rm=T)*Fmax),
                               length=301),max(dres$Fc.at.age,na.rm=T)),plot=FALSE)
      # YPRと%SPR
      dres$ysdata[i,1:2] <- (as.numeric(rev(a$ypr.spr[which(a$ypr.spr$Frange2Fcurrent==1)[1],2:3])))
      # SPR
      dres$ysdata[i,3] <- a$spr0*dres$ysdata[i,1]/100
      # SPR0
      dres$ysdata[i,4] <- a$spr0
      # relative F
      dres$ysdata[i,5] <- 1/a$summary[3,grep("SPR",colnames(a$summary))][1]
    }
    else{
      break;
    }
  }
  dres$ysdata <- as.data.frame(dres$ysdata)
  dres$target.SPR <- target.SPR
  return(dres)
}

#' vpaデータ, 選択率の参照年1, 漁獲圧の参照年2を与えて、参照年2の漁獲圧のもとで参照年1の選択率となるF at ageを作成する関数。漁獲圧の変換は％SPR換算
#'
#' sel_year 選択率の参照年
#' faa_year 漁獲圧の参照年
#'
#' @encoding UTF-8
#' @export

convert_faa_perSPR <- function(res_vpa, sel_year, faa_year, Fcurrent_MSY=NULL, Fsel_MSY=NULL){
  if(is.null(Fcurrent_MSY)){
    Fcurrent_MSY <- apply_year_colum(res_vpa$faa,target_year=faa_year)
  }
  Fcurrent.per.spr <- ref.F(res_vpa,Fcurrent=Fcurrent_MSY,waa.year=faa_year,M.year=faa_year,
                            maa.year=faa_year,plot=FALSE,pSPR=NULL)$currentSPR$perSPR
  cat("%SPR in Fcurrent ",Fcurrent.per.spr,"\n")
  if(is.null(Fsel_MSY)){
    Fsel_MSY <- apply_year_colum(res_vpa$faa,target_year=sel_year)
  }

  Fmultiplier <- ref.F(res_vpa,Fcurrent=Fsel_MSY,waa.year=faa_year,M.year=faa_year,
                       maa.year=faa_year,pSPR=Fcurrent.per.spr*100,plot=FALSE)
  cat("----------------------------- \n ")
  cat("%SPR in Fsel_MSY",Fmultiplier$currentSPR$perSPR," = ")
  Fmultiplier <- rev(Fmultiplier$summary)[3,1] %>% unlist()
  cat("(F multiplier=",Fmultiplier,")\n")
  Fcurrent_MSY_new <- Fsel_MSY * Fmultiplier
  Fref_tmp <- ref.F(res_vpa,Fcurrent=Fcurrent_MSY_new,waa.year=faa_year,
                    M.year=faa_year,maa.year=faa_year,
                    pSPR=Fcurrent.per.spr*100,plot=FALSE)
  cat("%SPR in new-Fcurrent",Fref_tmp$currentSPR$perSPR,"\n")
  cat("----------------------------- \n ")
  matplot(cbind(Fcurrent_MSY_new,Fcurrent_MSY,Fsel_MSY),type="b",pch=1:3,ylab="F",xlab="Age (recruit age=1)")
  legend("bottomright",pch=1:3,col=1:3,c("Fcurrent","Fsel_MSY","new-Fcurrent"))
  return(Fcurrent_MSY_new)
}




#' @export
#' @encoding UTF-8
make_summary_table <- function(mat_data,side=1,probs=c(0.1,0.5,0.8)){
  res_mat <- cbind(apply(mat_data,side,mean),
                   t(apply(mat_data,side,quantile,probs=probs)))
  colnames(res_mat)[1] <- "average"

  as.data.frame(res_mat)
}


# 結果の入出力&サマリーの作成 ----

#'
#' VPA結果をcsvファイルに出力する
#'
#' @param res  VPAの結果
#' @param srres fit.SRの結果
#' @param msyres est.MSYの結果
#' @param fres_current future_vpaの結果(Fcurrent)
#' @param fres_HCR future_vpaの結果(F with HCR)
#' @param kobeII kobeII.matrixの結果
#' @param filename csvファイルとpdfファイルの両方のファイル名を指定する場合（拡張子なしで指定）
#' @param csvname csvファイルのファイル名
#' @param pdfname pdfファイルのファイル名
#' @encoding UTF-8
#' @export

out.vpa <- function(res=NULL,    # VPA result
                    srres=NULL,  # fit.SR result
                    msyres=NULL, # est.MSY result
                    fres_current=NULL,   # future projection result
                    fres_HCR=NULL,   # future projection result
                    kobeII=NULL, # kobeII result
                    kobe.ratio=NULL, # kobe.ratio
                    filename="vpa", # filename without extension
                    csvname=NULL,
                    pdfname=NULL,
                    ci.future=c(0.1,0.5,0.9)
){
  old.par <- par()
  exit.func <- function(){
    dev.off()
    options(warn=0)
  }
  on.exit(exit.func())

  if(!is.null(filename)){
    csvname <- paste(filename,".csv",sep="")
    pdfname <- paste(filename,".pdf",sep="")
  }

  pdf(pdfname)
  par(mfrow=c(3,2),mar=c(3,3,2,1))
  options(warn=-1)

  write.table2 <- function(x,title.tmp="",is.plot=TRUE,...){
    if(is.plot){
      if(!is.null(dim(x))){
        matplot(colnames(x),t(x),type="b",ylim=c(0,max(x,na.rm=T)),pch=substr(rownames(x),1,1))
      }
      else{
        barplot(x)
      }
      title(title.tmp)
    }
    if(!is.null(dim(x))){
      tmp <- matrix("",nrow(x)+1,ncol(x)+1)
      tmp[-1,-1] <- as.character(unlist(x))
      tmp[-1,1] <- rownames(x)
      tmp[1,-1] <- colnames(x)
    }
    else{
      tmp <- x
    }
    write.table(tmp,append=T,sep=",",quote=FALSE,file=csvname,col.names=F,row.names=F,...)
  }

  write(paste("# frasyr outputs at ",date()," & ",getwd()),file=csvname)

  if(!is.null(res)){
    write("# VPA results",file=csvname, append=T)

    write("\n# catch at age",file=csvname,append=T)
    write.table2(res$input$dat$caa,title.tmp="Catch at age")

    write("\n# maturity at age",file=csvname,append=T)
    write.table2(res$input$dat$maa,title.tmp="Maturity at age")

    write("\n# weight at age for biomass calculation",file=csvname,append=T)
    write.table2(res$input$dat$waa,title.tmp="Weight at age (for biomass)")

    if(!is.null(res$input$dat$waa.catch)){
      write("\n# weight at age for catch calculation",file=csvname,append=T)
      write.table2(res$input$dat$waa.catch,title.tmp="Weight at age (for catch)")
    }

    write("\n# M at age",file=csvname,append=T)
    write.table2(res$input$dat$M,title.tmp="M at age")

    write("\n# fishing mortality at age",file=csvname,append=T)
    write.table2(res$faa,title.tmp="F at age")

    write("\n# Current F",file=csvname,append=T)
    write.table2(res$Fc.at.age,title.tmp="Current F")

    write("\n# numbers at age",file=csvname,append=T)
    write.table2(res$naa,title.tmp="Numbers at age")

    write("\n# total and spawning biomass ",file=csvname,append=T)
    x <- rbind(colSums(res$ssb, na.rm=T),colSums(res$baa, na.rm=T),colSums(res$wcaa, na.rm=T))
    rownames(x) <- c("Spawning biomass","Total biomass","Catch biomass")
    write.table2(x,title.tmp="Total and spawning biomass")

    write("\n# YPR & SPR history ",file=csvname,append=T)
    get.SPR(res)$ysdata %>% as_tibble() %>% select(-"F/Ftarget") %>%
      write_csv(path=csvname,append=T, col_names=TRUE)
  }

  if(!is.null(srres)){

    write("\n# SR fit data",file=csvname,append=T)
    srres$input$SRdata %>% as_tibble() %>%  mutate(weight=srres$input$w) %>%
      write_csv(path=csvname,append=T,col_names=TRUE)

    write("\n# SR fit resutls",file=csvname,append=T)
    if(class(srres)=="fit.SR"){
      sr_summary <-
        as_tibble(srres$pars) %>% mutate(AICc   = srres$AICc,
                                         AIC    = srres$AIC,
                                         method = srres$input$method,
                                         type   = srres$input$SR,
                                         AR     = srres$input$AR,
                                         out.AR = srres$input$out.AR)
      write_csv(sr_summary,path=csvname,append=T,
                col_names=TRUE)
    }
    if(class(srres)=="fit.SRregime"){
      tibble(AICc   =srres$AICc,
             AIC    =srres$AIC,
             method=srres$input$method,
             type  =srres$input$SR) %>%
        write_csv(path=csvname,append=T,col_names=TRUE)

      partable <- srres$regime_pars 
      if(!is.null(srres$steepness)) partable <- partable %>% left_join(srres$steepness)
      # tentative 
      write_csv(partable, path=csvname,append=T,col_names=TRUE)

    }
  }

  if(!is.null(msyres)){
    write("\n# MSY Reference points",file=csvname,append=T)
    write_csv(msyres$summary,path=csvname,append=T,
              col_names=TRUE)
  }

  tmpfunc <- function(fres){
    if(class(fres)=="future_new"){
      fres <- format_to_old_future(fres)
    }

    write("\n# future F at age",file=csvname,append=T)
    write.table2(apply(fres$faa,c(1,2),mean),title.tmp="Average future F at age")

    write("\n# future numbers at age",file=csvname,append=T)
    write.table2(apply(fres$naa,c(1,2),mean),title.tmp="Average future numbers at age")

    write("\n# future maturity at age",file=csvname,append=T)
    write.table2(apply(fres$maa,c(1,2),mean),title.tmp="Average future numbers at age")

    write("\n# future weight at age",file=csvname,append=T)
    write.table2(apply(fres$waa,c(1,2),mean),title.tmp="Average future numbers at age")

    write("\n# future total spawning biomass",file=csvname,append=T)
    make_summary_table(fres$vssb,1,probs=ci.future) %>%
      write_csv(path=csvname,append=TRUE, col_names = TRUE)

    write("\n# future total biomass",file=csvname,append=T)
    make_summary_table(fres$vbiom,1,probs=ci.future) %>%
      write_csv(path=csvname,append=TRUE, col_names = TRUE)

    write("\n# future total catch",file=csvname,append=T)
    make_summary_table(fres$vwcaa,1,probs=ci.future) %>%
      write_csv(path=csvname,append=TRUE, col_names = TRUE)
  }

  if(!is.null(fres_current)){
    write("\n# future projection under F current",file=csvname,append=T)
    tmpfunc(fres_current)
  }

  if(!is.null(fres_HCR)){
    write("\n# future projection under HCR",file=csvname,append=T)
    tmpfunc(fres_HCR)
  }

  if(!is.null(kobeII)){
    write("\n# Kobe II table",file=csvname,append=T)
    kobeII.table_name <- names(kobeII)
    for(i in 1:length(kobeII.table_name)){
      tmptable <- kobeII[kobeII.table_name[i]][[1]]
      if(!is.na(tmptable) && nrow(tmptable)>0){
        write(str_c("\n# ",kobeII.table_name[i]),file=csvname,append=T)
        write_csv(tmptable,path=csvname,append=TRUE,
                  col_names = TRUE)
      }
    }
  }

  if(!is.null(kobe.ratio)){
    write("\n# Kobe ratio",file=csvname,append=T)
    kobe.ratio %>%
        write_csv(path=csvname,append=T, col_names=TRUE)
  }

}

#' csvファイルとしてまとめられた資源計算結果を読み込んでRのオブジェクトにする
#'
#' @param release.label 放流データを読み込みたい場合。ここで指定したラベルをつけて、年齢を行、年を列とするデータを入れる（catch at ageと同じ構造だが、年齢は必要な年齢だけ取り出した形で良い）
#' @param tfile 資源計算結果がまとめられたcsvファイルの名前
#' @param Pope  VPA計算時にどっちを使っているかここで設定する（TRUE or FALSE）。デフォルトはNULLで、その場合にはcaa,faa,naaの関係から自動判別するが、自動判別の結果が出力されるので、それをみて正しく判断されているか確認してください。
#' @param plus.group プラスグループを考慮するかどうか。こちらについても、NULLの場合にはfaaとnaaの関係から自動判別するが、結果を一応確認すること。
#'
#' @encoding UTF-8
#'
#'
#'
#' @export

read.vpa <- function(tfile,
                     caa.label="catch at age",
                     maa.label="maturity at age",
                     waa.label="weight at age",
                     waa.biomass.label="weight at age for biomass calculation",
                     waa.catch.label="weight at age for catch calculation",
                     M.label="M at age",
                     faa.label="fishing mortality at age",
                     Fc.label="Current F",
                     naa.label="numbers at age",
                     release.label="release dat",
                     Blimit=NULL,
                     Pope=NULL,
                     plus.group=NULL,
                     fc.year=NULL){

  tmpdata <- read.csv(tfile,header=F,as.is=F,colClasses="character")

  tmpfunc <- function(tmpdata,label,type=NULL){
    flags <- which(substr(tmpdata[,1],1,1)=="#")
    flags <- c(flags, nrow(tmpdata)+1)
    flag.name <- tmpdata[flags,1]
    flag.name <- gsub("#","",flag.name)
    flag.name <- gsub(" ","",flag.name)
    get.flag <- which(flag.name==gsub(" ","",label))
    {if(length(get.flag)>0){
      tdat <- tmpdata[(flags[get.flag]+1):(flags[min(which(flags[get.flag]<flags))]-1),]
      if(!is.null(type)){
        tdat <- tdat[,!apply(tdat=="",2,all)]
        tdat <- as.numeric(tdat)
      }
      else{
        tmp.names <- list(tdat[-1,1],tdat[1,-1])
        tdat <- tdat[,!apply(tdat=="",2,all)]
        tdat <- tdat[!apply(tdat=="",1,all),]
        tdat <- tdat[,!apply(is.na(tdat),2,all)]
        tdat <- tdat[!apply(is.na(tdat),1,all),]
        tdat <- sapply((tdat[-1,-1]),as.numeric)
        tmp.names <- lapply(tmp.names,function(x) x[x!=""])
        tmp.names <- lapply(tmp.names,function(x) x[!is.na(x)])
        if(is.null(dim(tdat))) dim(tdat) <- sapply(tmp.names,length)        
        dimnames(tdat) <- tmp.names
        tdat <- as.data.frame(tdat)
      }
    }
      else{
        tdat <- NULL
      }}
    tdat
  }

  dres <- list()
  dres$naa <- tmpfunc(tmpdata,naa.label)
  dres$faa <- tmpfunc(tmpdata,faa.label)
  dres$Fc.at.age <- tmpfunc(tmpdata,Fc.label,type="Fc")
  #  dres$Fc.at.age <- dres$Fc.at.age[!is.na(dres$Fc.at.age)]
  dres$Fc.at.age <- dres$Fc.at.age[1:nrow(dres$naa)]
  #  if(length(dres$Fc.at.age)!=nrow(dres$naa)) stop("Dimension of Fc.at.age and numbers at age is differerent.")

  dres$input <- list()
  dres$input$dat <- list()
  dres$input$dat$maa <- tmpfunc(tmpdata,maa.label)
  dres$input$dat$caa <- tmpfunc(tmpdata,caa.label)
  dres$input$dat$M <- tmpfunc(tmpdata,M.label)
  dres$input$dat$waa <- tmpfunc(tmpdata,waa.label)
  dres$input$dat$release.dat <- tmpfunc(tmpdata,release.label)
  if(is.null(dres$input$dat$waa)) dres$input$dat$waa <- tmpfunc(tmpdata,waa.biomass.label)
  dres$input$dat$waa.catch <- tmpfunc(tmpdata,waa.catch.label)
  if(is.null(dres$input$dat$waa.catch)) dres$input$dat$waa.catch <- dres$input$dat$waa

  dres$ssb <- dres$input$dat$waa * dres$input$dat$maa * dres$naa
  dres$ssb <- as.data.frame(dres$ssb)

  dres$baa <- dres$input$dat$waa * dres$naa
  dres$baa <- as.data.frame(dres$baa)

  # setting total catch
  dres$wcaa <- dres$input$dat$waa.catch * dres$input$dat$caa
  dres$wcaa <- as.data.frame(dres$wcaa)

  dres$Blimit <- Blimit

  ## catch at ageの計算時にpopeの近似式を使っているかどうか、通常は外から情報として与えてほしいところだが、与えられない場合、入力されたcaa,faa,naaの関係を見て、Popeで計算されているのかそうでないのかを判断してdres$input$Popeに入れる
  if(is.null(Pope)){
    caa.pope  <- dres$naa*(1-exp(-dres$faa))*exp(-dres$input$dat$M/2)
    diff.pope <- mean(unlist(dres$input$dat$caa/caa.pope),na.rm=T)

    faa <- dres$faa
    M <- dres$input$dat$M
    caa.bara <- dres$naa*faa/(faa+M)*(1-exp(-faa-M))
    diff.bara <- mean(unlist(dres$input$dat$caa/caa.bara),na.rm=T)

    if(abs(1-mean(diff.bara))>abs(1-mean(diff.pope))){
      dres$input$Pope <- TRUE

      cat("Pope is TRUE... OK? (mean difference=", 1-mean(diff.pope),")\n")
    }
    else{
      dres$input$Pope <- FALSE
      cat("Pope is FALSE... OK? (mean difference=", 1-mean(diff.bara),")\n")
    }
  }
  else{
    dres$input$Pope <- Pope
  }

  ## プラスグループを考慮しているかどうかを判別する
  if(is.null(plus.group)){
    plus.group <- detect_plus_group(dres)
    cat("Plus group is TRUE... OK?\n")
  }
  dres$input$plus.group <- plus.group

  if(is.null(dres$Fc.at.age) && !is.null(fc.year)) dres$Fc.at.age <- apply(dres$faa[,colnames(dres$faa)%in%fc.year],1,mean)

  # その他、他関数で必要になるVPAへのインプット
  dres$input$last.catch.zero <- FALSE

  return(dres)
}

#' tidy形式のVPAへのインプットデータをdata_dandlerへ渡す形式に変換する（暫定版）
#'
#' @export
#' @encoding UTF-8
#'

to_vpa_data <- function(x, label_name){
  vpa_data <- x %>% dplyr::filter(label==label_name) %>% as.data.frame()
  if(label_name!="abund"){
    rownames(vpa_data) <- str_c("X",vpa_data$year)
    vpa_data <- vpa_data %>% select(-year, -label)
    vpa_data$value <- vpa_data$fishery <- NULL
    vpa_data <- as.data.frame(t(vpa_data))
    rownames(vpa_data) <- str_sub(rownames(vpa_data),5,5)
  }
  else{
    vpa_data <- vpa_data %>%
      select(-starts_with("age_")) %>%
      pivot_wider(names_from=year, values_from=value)
    abund_name <- vpa_data$fishery
    vpa_data <- as.data.frame(vpa_data)
    vpa_data$label <- vpa_data$fishery <- NULL
    rownames(vpa_data) <- abund_name
    colnames(vpa_data) <- str_c("X",colnames(vpa_data))
  }
  vpa_data
}


#' @export
#' @encoding UTF-8
#'

get.stat <- function(fout,eyear=0,tmp.year=NULL, use_new_output=FALSE){

  if(isTRUE(use_new_output)){
    fout <- format_to_old_future(fout)
    col.target <- TRUE
  }
  else{
    col.target <- ifelse(fout$input$N==0,1,-1)
  }
  tc <- fout$caa * fout$waa.catch
  tmp <- as.numeric(fout$vssb[(nrow(fout$vssb)-eyear):nrow(fout$vssb),col.target])
  if(is.null(tmp.year)) tmp.year <- (nrow(fout$vwcaa)-eyear):nrow(fout$vwcaa)
  a <- data.frame("catch.mean"=mean(fout$vwcaa[tmp.year,col.target]),
                  "catch.sd"=sd(fout$vwcaa[tmp.year,col.target]),
                  "catch.geomean"=geomean(fout$vwcaa[tmp.year,col.target]),
                  "catch.median"=median(fout$vwcaa[tmp.year,col.target],na.rm=T),
                  "catch.L10"=quantile(fout$vwcaa[tmp.year,col.target],na.rm=T,probs=0.1),
                  "catch.H10"=quantile(fout$vwcaa[tmp.year,col.target],na.rm=T,probs=0.9),
                  "ssb.mean"=mean(fout$vssb[tmp.year,col.target]),
                  "ssb.sd"=sd(fout$vssb[tmp.year,col.target]),
                  "ssb.geomean"=geomean(fout$vssb[tmp.year,col.target]),
                  "ssb.median"=median(fout$vssb[tmp.year,col.target],na.rm=T),
                  "ssb.L10"=quantile(fout$vssb[tmp.year,col.target],na.rm=T,probs=0.1),
                  "ssb.H10"=quantile(fout$vssb[tmp.year,col.target],na.rm=T,probs=0.9),
                  "biom.mean"=mean(fout$vbiom[tmp.year,col.target]),
                  "biom.sd"=sd(fout$vbiom[tmp.year,col.target]),
                  "biom.geomean"=geomean(fout$vbiom[tmp.year,col.target]),
                  "biom.median"=median(fout$vbiom[tmp.year,col.target],na.rm=T),
                  "biom.L10"=quantile(fout$vbiom[tmp.year,col.target],na.rm=T,probs=0.1),
                  "biom.H10"=quantile(fout$vbiom[tmp.year,col.target],na.rm=T,probs=0.9),
                  "rec.mean"=mean(unlist(fout$naa[1,,])[tmp.year,col.target]),
                  "rec.sd"=sd(unlist(fout$naa[1,,])[tmp.year,col.target]),
                  "rec.geomean"=geomean(unlist(fout$naa[1,,])[tmp.year,col.target]),
                  "rec.median"=median(unlist(fout$naa[1,,])[tmp.year,col.target],na.rm=T),
                  "rec.L10"=quantile(unlist(fout$naa[1,,])[tmp.year,col.target],na.rm=T,probs=0.1),
                  "rec.H10"=quantile(unlist(fout$naa[1,,])[tmp.year,col.target],na.rm=T,probs=0.9),
                  #                    "lower.HSpoint"=lhs,
                  "Fref2Fcurrent"=fout$multi,
                  fmulti=fout$multi
  )
  a$U.mean <- a$catch.mean/a$biom.mean
  a$U.median <- a$catch.median/a$biom.median
  a$U.geomean <- a$catch.geomean/a$biom.geomean

  a$catch.CV <- a$catch.sd/a$catch.mean
  a$ssb.CV <- a$ssb.sd/a$ssb.mean
  a$biom.CV <- a$biom.sd/a$biom.mean
  a$rec.CV <- a$rec.sd/a$rec.mean

  #    Faa <- as.data.frame(t(fout$multi * fout$input$res0$Fc.at.age))
  # Faa <- as.data.frame(t(fout$multi * fout$currentF))
  if("finalmeanF" %in% names(fout)) Faa <- as.data.frame(t(fout$finalmeanF)) else Faa <- as.data.frame(t(fout$multi * fout$futureF))
  colnames(Faa) <- paste("F",dimnames(fout$naa)[[1]],sep="")
  res.stat1 <- cbind(a,Faa) # ここまで、get.stat

  agename <- dimnames(fout$naa)[[1]]
  nage <- dim(fout$naa)[[1]]
  tb <- fout$naa * fout$waa
  if(is.null(fout$waa.catch)) fout$waa.catch <- fout$waa
  ssb <- fout$naa * fout$waa *fout$maa
  tb.mat <- tc.mat <- ssb.mat <- matrix(0,nage,6)
  for(i in 1:nage){
    tb.mat[i,1] <- mean(tb[i,tmp.year,col.target])
    tb.mat[i,2] <- median(tb[i,tmp.year,col.target])
    tb.mat[i,3] <- geomean(tb[i,tmp.year,col.target])
    tb.mat[i,4] <- mean(tb[i,tmp.year,1])
    tb.mat[i,5:6] <- quantile(tb[i,tmp.year,col.target],probs=c(0.1,0.9),na.rm=T)

    tc.mat[i,1] <- mean(tc[i,tmp.year,col.target])
    tc.mat[i,2] <- median(tc[i,tmp.year,col.target])
    tc.mat[i,3] <- geomean(tc[i,tmp.year,col.target])
    tc.mat[i,4] <- mean(tc[i,tmp.year,1])
    tc.mat[i,5:6] <- quantile(tc[i,tmp.year,col.target],probs=c(0.1,0.9),na.rm=T)

    ssb.mat[i,1] <- mean(ssb[i,tmp.year,col.target])
    ssb.mat[i,2] <- median(ssb[i,tmp.year,col.target])
    ssb.mat[i,3] <- geomean(ssb[i,tmp.year,col.target])
    ssb.mat[i,4] <- mean(ssb[i,tmp.year,1])
    ssb.mat[i,5:6] <- quantile(ssb[i,tmp.year,col.target],probs=c(0.1,0.9),na.rm=T)
  }
  tc.mat <- as.numeric(tc.mat)
  tb.mat <- as.numeric(tb.mat)
  ssb.mat <- as.numeric(ssb.mat)

  # mean, ME; median, geomean; geometric mean
  names(tc.mat) <- c(paste("TC-mean-A",agename,sep=""),paste("TC-median-A",agename,sep=""),
                     paste("TC-geomean-A",agename,sep=""),paste("TC-det-A",agename,sep=""),
                     paste("TC-L10-A",agename,sep=""),paste("TC-H10-A",agename,sep=""))
  names(tb.mat) <- c(paste("TB-mean-A",agename,sep=""),paste("TB-median-A",agename,sep=""),
                     paste("TB-geomean-A",agename,sep=""),paste("TB-det-A",agename,sep=""),
                     paste("TB-L10-A",agename,sep=""),paste("TB-H10-A",agename,sep=""))
  names(ssb.mat) <- c(paste("SSB-GA-A",agename,sep=""),paste("SSB-median-A",agename,sep=""),
                      paste("SSB-geomean-A",agename,sep=""),paste("SSB-det-A",agename,sep=""),
                      paste("SSB-L10-A",agename,sep=""),paste("SSB-H10-A",agename,sep=""))
  res.stat2 <- as.data.frame(t(c(tb.mat,tc.mat,ssb.mat)))
  res.stat <- cbind(res.stat1,res.stat2) %>% as_tibble()
  return(res.stat)
}

get.stat3 <- get.stat


get.stat4 <- function(fout,Brefs,
                      refyear=c(2019:2023,2028,2038)){
  col.target <- ifelse(fout$input$N==0,1,-1)
  years <- as.numeric(rownames(fout$vwcaa))

  if(is.null(refyear)){
    refyear <- c(seq(from=min(years),to=min(years)+5),
                 c(min(years)+seq(from=10,to=20,by=5)))
  }

  catch.mean <- rowMeans(fout$vwcaa[years%in%refyear,col.target])
  names(catch.mean) <- str_c("Catch",names(catch.mean))
  catch.mean <- as_tibble(t(catch.mean))

  Btarget.prob <- rowMeans(fout$vssb[years%in%refyear,col.target]>Brefs$Btarget) %>%
    t() %>% as_tibble()
  names(Btarget.prob) <- str_c("Btarget_prob",names(Btarget.prob))

  #    Blow.prob <- rowMeans(fout$vssb[years%in%refyear,col.target]>Brefs$Blow) %>%
  #        t() %>% as_tibble()
  #    names(Blow.prob) <- str_c("Blow_prob",names(Blow.prob))

  Blimit.prob <- rowMeans(fout$vssb[years%in%refyear,col.target]<Brefs$Blimit) %>%
    t() %>% as_tibble()
  names(Blimit.prob) <- str_c("Blimit_prob",names(Blimit.prob))

  Bban.prob <- rowMeans(fout$vssb[years%in%refyear,col.target]<Brefs$Bban) %>%
    t() %>% as_tibble()
  names(Bban.prob) <- str_c("Bban_prob",names(Bban.prob))

  return(bind_cols(catch.mean,Btarget.prob,Blimit.prob,Bban.prob))
}

geomean <- function(x)
{
  ifelse(all(x > 0), exp(mean(log(x))), NA)
}

get.trace <- function(trace){
  trace <- trace  %>% as_tibble() %>%
    select(starts_with("TC-mean"),ssb.mean,fmulti,catch.CV) %>%
    mutate(label=as.character(1:nrow(.)))

  trace <- trace %>% gather(value=value,key=age,-label,-fmulti,-ssb.mean,-catch.CV) %>%
    mutate(age=str_extract(age, "(\\d)+")) %>%
    mutate(age=factor(age)) %>%
    mutate(age=fct_reorder(age,length(age):1))
  return(trace)
}


#' 列が年である行列に対して、年を指定するとその年のあいだの平均値（等）を返す関数
#'
#' @encoding UTF-8
#' @export
#'
#'

apply_year_colum <- function(mat,target_year,stat="mean"){
  if(target_year[1]<0){
    target_year <- rev(colnames(mat))[-target_year] %>%
      as.numeric() %>% sort()
  }
  mat <- as.data.frame(mat)
  apply(mat[as.character(target_year)],1,get(stat))
}


# VPAや将来予測の結果をtidyに ----
convert_df <- function(df,name){
  df %>%
    as_tibble %>%
    mutate(age = as.numeric(rownames(df))) %>%
    gather(key=year, value=value, -age, convert=TRUE) %>%
    group_by(year) %>%
    #        summarise(value=sum(value)) %>%
    mutate(type="VPA",sim="s0",stat=name)
}

#'
#' @export
#' @encoding UTF-8
convert_2d_future <- function(df, name, label="tmp"){
  df %>%
    as_tibble %>%
    mutate(year=rownames(df)) %>%
    gather(key=sim, value=value, -year, convert=TRUE) %>%
    mutate(year=as.numeric(year), stat=name, label=label)
}

#' future_vpaの結果オブジェクトのリストをtibble形式に変換する関数
#'
#' make_kobeII_tableに入れるオブジェクトを作る
#'
#' @param fout_list future_vpaの結果のオブジェクトのリスト
#'
#' @encoding UTF-8
#' @export
#'


convert_future_list_table <- function(fout_list,name_vector=NULL,beta_vector=NULL,label="tmp"){
  

  if(is.null(name_vector)){
    if(!is.null(names(fout_list))) name_vector <- names(fout_list)
    else name_vector <- 1:length(fout_list)
  }

  if(is.null(beta_vector)){
    beta_vector <- name_vector
  }    
  
  res <- purrr::map_dfr(1:length(fout_list),
                 function(i){
                   convert_future_table(fout_list[[i]],label=name_vector[i]) %>%
                     rename(HCR_name=label) %>%
                     mutate(beta=beta_vector[i])
                 })
  return(res)
}

#' future_vpaの結果オブジェクトをtibble形式に変換する関数
#'
#' @param fout future_vpaの結果のオブジェクト
#'
#' @encoding UTF-8
#' @export
#'

convert_future_table <- function(fout,label="tmp"){

  if(class(fout)=="future_new") fout <- format_to_old_future(fout)

  U_table <- fout$vwcaa/fout$vbiom
  if(is.null(fout$Fsakugen)) fout$Fsakugen <- -(1-fout$faa[1,,]/fout$currentF[1])
  if(is.null(fout$recruit))  fout$recruit <- fout$naa[1,,]

  ssb      <- convert_2d_future(df=fout$vssb,   name="SSB",     label=label)
  catch    <- convert_2d_future(df=fout$vwcaa,  name="catch",   label=label)
  biomass  <- convert_2d_future(df=fout$vbiom,  name="biomass", label=label)
  U_table  <- convert_2d_future(df=U_table,     name="U",       label=label)
  beta_gamma    <- convert_2d_future(df=fout$alpha,  name="beta_gamma",   label=label)
  Fsakugen <- convert_2d_future(df=fout$Fsakugen, name="Fsakugen",   label=label)
  recruit  <- convert_2d_future(df=fout$recruit, name="Recruitment",   label=label)
  if(!is.null(fout$Fratio)){
    Fratio <- convert_2d_future(df=fout$Fratio, name="Fratio",   label=label)
  }
  else{
    Fratio <- NULL
  }

  Fsakugen_ratio <- Fsakugen %>%
    mutate(value=value+1)
  Fsakugen_ratio$stat <- "Fsakugen_ratio"

  bind_rows(ssb,catch,biomass,beta_gamma,Fsakugen,Fsakugen_ratio,recruit, U_table, Fratio)
}


convert_vector <- function(vector,name){
  vector %>%
    as_tibble %>%
    mutate(year = as.integer(names(vector))) %>%
    mutate(type="VPA",sim="s0",stat=name,age=NA)
}

#' VPAの結果オブジェクトをtibble形式に変換する関数
#'
#' @param vpares vpaの結果のオブジェクト
#' @encoding UTF-8
#'
#'
#' @export

convert_vpa_tibble <- function(vpares,SPRtarget=NULL){

  if (is.null(vpares$input$dat$waa.catch)) {
    if (class(vpares)=="sam") {
      total.catch <- colSums(vpares$caa*vpares$input$dat$waa,na.rm=T)
    } else {
      total.catch <- colSums(vpares$input$dat$caa*vpares$input$dat$waa,na.rm=T)
    }
  } else {
    total.catch <- colSums(vpares$input$dat$caa*vpares$input$dat$waa.catch,na.rm=T)
  }
  U <- total.catch/colSums(vpares$baa, na.rm=T)

  SSB <- convert_vector(colSums(vpares$ssb,na.rm=T),"SSB") %>%
    dplyr::filter(value>0&!is.na(value))
  Biomass <- convert_vector(colSums(vpares$baa,na.rm=T),"biomass") %>%
    dplyr::filter(value>0&!is.na(value))
  FAA <- convert_df(vpares$faa,"fishing_mortality") %>%
    dplyr::filter(value>0&!is.na(value))
  Recruitment <- convert_vector(colSums(vpares$naa[1,,drop=F]),"Recruitment") %>%
    dplyr::filter(value>0&!is.na(value))

  if(!is.null(SPRtarget)){
    if(is.null(vpares$input$dat$waa.catch)) waa.catch <- vpares$input$dat$waa
    else waa.catch <- vpares$input$dat$waa.catch
    Fratio <- purrr::map_dfc(1:ncol(vpares$naa),
                             function(i){
                               tmp <- !is.na(vpares$faa[,i])
                               calc_Fratio(faa=vpares$faa[tmp,i],
                                           maa=vpares$input$dat$maa[tmp,i],
                                           waa=vpares$input$dat$waa[tmp,i],
                                           M  =vpares$input$dat$M[tmp,i],
                                           waa.catch=waa.catch[tmp],
                                           SPRtarget=SPRtarget,
                                           plus_group=vpares$input$plus.group)
                             })
    colnames(Fratio) <- colnames(vpares$naa)
    Fratio <- convert_df(Fratio,"Fratio")
  }
  else{
    Fratio <- NULL
  }

  all_table <- bind_rows(SSB,
                         Biomass,
                         convert_vector(U[U>0],"U"),
                         convert_vector(total.catch[total.catch>0],"catch"),
                         convert_df(vpares$naa,"fish_number"),
                         FAA,
                         convert_df(vpares$input$dat$waa,"weight"),
                         convert_df(vpares$input$dat$maa,"maturity"),
                         convert_df(vpares$input$dat$caa,"catch_number"),
                         convert_df(vpares$input$dat$M,  "natural_mortality"),
                         Recruitment,
                         Fratio)
}

#' fit.SRの結果をtibble形式に治す
#'
#' @param SR_result fit.SRの結果のオブジェクト
#' @encoding UTF-8
#'
#' @export
#'

convert_SR_tibble <- function(res_SR){
  if(class(res_SR)=="fit.SR"){
    resSRtibble <-bind_rows(tibble(value=as.numeric(res_SR$pars),type="parameter",name=names(res_SR$pars)),
                            res_SR$pred %>% mutate(type="prediction",name="prediction"),
                            res_SR$input$SRdata %>% as_tibble() %>%
                              mutate(type="observed",name="observed",residual=res_SR$resid))
    if(!is.null(res_SR$steepness)) resSRtibble<-bind_rows(resSRtibble,tibble(value=as.numeric(res_SR$steepness),type="parameter",name=names(res_SR$steepness)))
  }
  if(class(res_SR)=="fit.SRregime"){ # regimeあり
    resSR1 <- pivot_longer(res_SR$regime_pars,col=-regime) %>% mutate(type="parameter")
    resSR2 <- res_SR$pred %>% mutate(type="prediction",name="prediction")
    resSR3 <- res_SR$input$SRdata %>% as_tibble() %>%
                              mutate(type="observed",name="observed",residual=res_SR$regime_resid$resid)

    resSRtibble<-bind_rows(resSR1,resSR2,resSR3)

    if(!is.null(res_SR$steepness)) {
      for(j in 1:nrow(res_SR$steepness)){
        res_steepness <- res_SR$steepness[j,]
        res_steepness_tibble <- pivot_longer(res_steepness,col=-regime) %>% mutate(type="parameter")
        resSRtibble<- bind_rows(resSRtibble,res_steepness_tibble)
      }
    }
  }
  return(resSRtibble)
}

#' 管理基準値の表を作成する
#'
#' @param refs_base est.MSYから得られる管理基準値の表
#' @encoding UTF-8
#'
#' @export
#'

make_RP_table <- function(refs_base){
  #    require(formattable)
  #    require(tidyverse,quietly=TRUE)
  table_output <- refs_base %>%
    select(-RP_name) %>% # どの列を表示させるか選択する
    # 各列の有効数字を指定
    mutate(SSB=round(SSB,-floor(log10(min(SSB)))),
           SSB2SSB0=round(SSB2SSB0,2),
           Catch=round(Catch,-floor(log10(min(Catch)))),
           Catch.CV=round(Catch.CV,2),
           U=round(U,2),
           Fref2Fcurrent=round(Fref2Fcurrent,2)) %>%
    rename("管理基準値"=RP.definition,"親魚資源量"=SSB,"B0に対する比"=SSB2SSB0,
           "漁獲量"=Catch,"漁獲量の変動係数"=Catch.CV,"漁獲率"=U,"努力量の乗数"=Fref2Fcurrent)

  table_output  %>%
    # 表をhtmlで出力
    formattable::formattable(list(`親魚資源量`=color_bar("olivedrab"),
                                       `漁獲量`=color_bar("steelblue"),
                                       `漁獲率`=color_bar("orange"),
                                       `努力量の乗数`=color_bar("tomato")))

  #    return(table_output)

}

#' 管理基準値表から目的の管理基準値を取り出す関数
#'
#' @param refs_base est.MSYから得られる管理基準値の表
#' @param RP_name 取り出したい管理基準値の名前
#' @encoding UTF-8
#'
#' @export
#'

derive_RP_value <- function(refs_base,RP_name){
  refs_base[refs_base$RP.definition%in%RP_name,]
}


# kobe II matrix など、パフォーマンスを計算する関数 ----

#'
#' beta.simluationの結果などを読んで、kobeII talbeに整形する関数
#'
#' @param kobeII_data beta.simulationまたはconvert_future_list_tableの返り値
#' @param res_vpa VPAの結果
#' @param Bspecific 特定の資源量を一回でも下回るリスクを計算する。下回るリスクを判断する年はyear.riskに従う
#'

#' @details
#' tidy形式になっているkobeII_dataにおいて、HCR_name, betaの列のラベルの組み合わせを一つの管理方式として、その管理方式ごとに少尉予測の結果を集計する
#' 

#' @export
#'
#' 
#'
#' @encoding UTF-8

make_kobeII_table <- function(kobeII_data,
                              res_vpa,
                              year.catch=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.ssb=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.Fsakugen=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.ssbtarget=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.ssblimit=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.ssbban=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.ssbmin=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.ssbmax=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.aav=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.risk=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              year.catchdiff=(max(as.numeric(colnames(res_vpa$naa)))+1:10),
                              Btarget=0,
                              Blimit=0,
                              Bban=0,
                              Bspecific=NULL){
  # 平均漁獲量
  (catch.mean <- kobeII_data %>%
     dplyr::filter(year%in%year.catch,stat=="catch") %>% # 取り出す年とラベル("catch")を選ぶ
     group_by(HCR_name,beta,year) %>%
     summarise(catch.mean=mean(value)) %>%  # 値の計算方法を指定（漁獲量の平均ならmean(value)）
     # "-3"とかの値で桁数を指定
     spread(key=year,value=catch.mean) %>% ungroup() %>%
     arrange(HCR_name,desc(beta)) %>% # HCR_nameとbetaの順に並び替え
     mutate(stat_name="catch.mean"))

  # 平均親魚
  (ssb.mean <- kobeII_data %>%
      dplyr::filter(year%in%year.ssb,stat=="SSB") %>%
      group_by(HCR_name,beta,year) %>%
      summarise(ssb.mean=mean(value)) %>%
      spread(key=year,value=ssb.mean) %>% ungroup() %>%
      arrange(HCR_name,desc(beta)) %>% # HCR_nameとbetaの順に並び替え
      mutate(stat_name="ssb.mean"))

  # 平均親魚
  (biomass.mean <- kobeII_data %>%
      dplyr::filter(year%in%year.ssb,stat=="biomass") %>%
      group_by(HCR_name,beta,year) %>%
      summarise(biomass.mean=mean(value)) %>%
      spread(key=year,value=biomass.mean) %>% ungroup() %>%
      arrange(HCR_name,desc(beta)) %>% # HCR_nameとbetaの順に並び替え
      mutate(stat_name="biomass.mean"))

  # 親魚, 下10%
  (ssb.ci10 <- kobeII_data %>%
      dplyr::filter(year%in%year.ssb,stat=="SSB") %>%
      group_by(HCR_name,beta,year) %>%
      summarise(ssb.ci10=quantile(value,probs=0.1)) %>%
      spread(key=year,value=ssb.ci10) %>% ungroup() %>%
      arrange(HCR_name,desc(beta)) %>% # HCR_nameとbetaの順に並び替え
      mutate(stat_name="ssb.ci10"))

  # 親魚, 上10%
  (ssb.ci90 <- kobeII_data %>%
      dplyr::filter(year%in%year.ssb,stat=="SSB") %>%
      group_by(HCR_name,beta,year) %>%
      summarise(ssb.ci90=quantile(value,probs=0.9)) %>%
      spread(key=year,value=ssb.ci90) %>% ungroup() %>%
      arrange(HCR_name,desc(beta)) %>% # HCR_nameとbetaの順に並び替え
      mutate(stat_name="ssb.ci90"))

  # 1-currentFに乗じる値=currentFからの努力量の削減率の平均値（実際には確率分布になっている）
  (Fsakugen.table <- kobeII_data %>%
      dplyr::filter(year%in%year.Fsakugen,stat=="Fsakugen") %>% # 取り出す年とラベル("catch")を選ぶ
      group_by(HCR_name,beta,year) %>%
      summarise(Fsakugen=round(mean(value),2)) %>%
      spread(key=year,value=Fsakugen) %>% ungroup() %>%
      arrange(HCR_name,desc(beta)) %>% # HCR_nameとbetaの順に並び替え
      mutate(stat_name="Fsakugen"))

  # SSB>SSBtargetとなる確率
  ssbtarget.table <- kobeII_data %>%
    dplyr::filter(year%in%year.ssbtarget,stat=="SSB") %>%
    group_by(HCR_name,beta,year) %>%
    summarise(ssb.over=round(100*mean(value>Btarget))) %>%
    spread(key=year,value=ssb.over) %>%
    ungroup() %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="Pr(SSB>SSBtarget)")

  # SSB>SSBlimとなる確率
  ssblimit.table <- kobeII_data %>%
    dplyr::filter(year%in%year.ssblimit,stat=="SSB") %>%
    group_by(HCR_name,beta,year) %>%
    summarise(ssb.over=round(100*mean(value>Blimit))) %>%
    spread(key=year,value=ssb.over)%>%
    ungroup() %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="Pr(SSB>SSBlim)")

  # SSB>SSBbanとなる確率
  ssbban.table <- kobeII_data %>%
    dplyr::filter(year%in%year.ssbban,stat=="SSB") %>%
    group_by(HCR_name,beta,year) %>%
    summarise(ssb.over=round(100*mean(value>Bban))) %>%
    spread(key=year,value=ssb.over)%>%
    ungroup() %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="Pr(SSB>SSBban)")

  # SSB>SSBmin(過去最低親魚量を上回る確率)
  ssb.min <- min(unlist(colSums(res_vpa$ssb, na.rm=T)))
  ssbmin.table <- kobeII_data %>%
    dplyr::filter(year%in%year.ssbmin,stat=="SSB") %>%
    group_by(HCR_name,beta,year) %>%
    summarise(ssb.over=round(100*mean(value>ssb.min))) %>%
    spread(key=year,value=ssb.over)%>%
    ungroup() %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="Pr(SSB>SSBmin)")

  # SSB>SSBmax(過去最低親魚量を上回る確率)
  ssb.max <- max(unlist(colSums(res_vpa$ssb, na.rm=T)))
  ssbmax.table <- kobeII_data %>%
    dplyr::filter(year%in%year.ssbmax,stat=="SSB") %>%
    group_by(HCR_name,beta,year) %>%
    summarise(ssb.over=round(100*mean(value>ssb.max))) %>%
    spread(key=year,value=ssb.over)%>%
    ungroup() %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="Pr(SSB>SSBmax)")

  # オプション: Catch AAV mean
  calc.aav <- function(x)sum(abs(diff(x)))/sum(x[-1])
  catch.aav.table <- kobeII_data %>%
    dplyr::filter(year%in%year.aav,stat=="catch") %>%
    group_by(HCR_name,beta,sim) %>%
    dplyr::summarise(catch.aav=(calc.aav(value))) %>%
    group_by(HCR_name,beta) %>%
    summarise(catch.aav.mean=mean(catch.aav)) %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="catch.aav")

  # risk
  calc.aav2 <- function(x){
      xx <- sum(x[-1]/x[-length(x)]<0.5,na.rm=T) # 0が2つ続く場合にNAが発生するがそこは計算から除く
      return(xx)
  }

    #  calc.aav2 <- function(x){ xx <- sum(x[-1]/x[-length(x)]<0.5); if(is.na(xx)) browser() else return(xx)}
  catch.risk <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="catch") %>%
    group_by(HCR_name,beta,sim) %>%
    dplyr::summarise(catch.aav=calc.aav2(value)) %>%
    group_by(HCR_name,beta) %>%
    summarise(value=mean(catch.aav>0)) %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="catch.risk")

  bban.risk <- kobeII_data %>%
    dplyr::filter(year%in%year.risk & stat=="SSB") %>%
    group_by(HCR_name,beta,sim) %>%
    dplyr::summarise(Bban.fail=sum(value<Bban)) %>%
    group_by(HCR_name,beta) %>%
    summarise(value=mean(Bban.fail>0)) %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="bban.risk")

  blimit.risk <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="SSB") %>%
    group_by(HCR_name,beta,sim) %>%
    dplyr::summarise(Blimit.fail=sum(value<Blimit)) %>%
    group_by(HCR_name,beta) %>%
    summarise(value=mean(Blimit.fail>0)) %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="blimit.risk")

  overfishing.risk <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="Fratio") %>%
    group_by(HCR_name,beta,sim) %>%
    dplyr::summarise(overfishing=sum(value>1)) %>%
    group_by(HCR_name,beta) %>%
    summarise(value=mean(overfishing>0)) %>%
    arrange(HCR_name,desc(beta))%>%
    mutate(stat_name="overfishing.risk")

  redzone.risk1 <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="Fratio")

  redzone.risk2 <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="SSB") %>%
    mutate(Bratio=value/Btarget) %>%
    mutate(Fratio=redzone.risk1$value) %>%
    mutate(is.redzone=(Bratio < 1 & round(Fratio,3) > 1)) %>%
    arrange(HCR_name,beta,sim)

#  browser()

  redzone.risk <- redzone.risk2 %>%
    group_by(HCR_name,beta,sim) %>%
     dplyr::summarise(sum.redzone=sum(is.redzone==TRUE)) %>%
    group_by(HCR_name,beta) %>%
    dplyr::summarise(value=mean(sum.redzone>0)) %>%
    arrange(HCR_name,desc(beta)) %>%
    mutate(stat_name="redzone.risk")

  if(!is.null(Bspecific)){
    bspecific.risk <- kobeII_data %>%
      dplyr::filter(year%in%year.risk,stat=="SSB") %>%
      group_by(HCR_name,beta,sim) %>%
      dplyr::summarise(Bspecific.fail=sum(value<Bspecific)) %>%
      group_by(HCR_name,beta) %>%
      summarise(value=mean(Bspecific.fail>0)) %>%
      arrange(HCR_name,desc(beta))%>%
      mutate(stat_name="bspecific.risk")
  }else{
    bspecific.risk <- NA
  }

  # kobe statistics
  overssbtar <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="SSB") %>%
    mutate(is.over.ssbtar= value > Btarget)
  overFtar <- kobeII_data %>%
    dplyr::filter(year%in%year.risk,stat=="Fratio") %>%
    mutate(is.over.Ftar= round(value,2) > 1)
  overssbtar$is.over.Ftar <- overFtar$is.over.Ftar

  kobe.stat <- overssbtar %>%
      mutate("red"   =(is.over.ssbtar==FALSE) & (is.over.Ftar==TRUE),
             "green" =(is.over.ssbtar==TRUE ) & (is.over.Ftar==FALSE),
             "yellow"=(is.over.ssbtar==FALSE) & (is.over.Ftar==FALSE),
             "orange"=(is.over.ssbtar==TRUE ) & (is.over.Ftar==TRUE)) %>%
    group_by(HCR_name,beta,year) %>%
    summarise(red.prob=mean(red,na.rm=T),
              green.prob=mean(green,na.rm=T),
              yellow.prob=mean(yellow,na.rm=T),
              orange.prob=mean(orange,na.rm=T))  %>%
      mutate(stat_name="kobe.stat")


  res_list <- list(catch.mean   = catch.mean,
                   ssb.mean         = ssb.mean,
                   biomass.mean         = biomass.mean,
                   ssb.lower10percent            = ssb.ci10,
                   ssb.upper90percent            = ssb.ci90,
                   prob.over.ssbtarget  = ssbtarget.table,
                   prob.over.ssblimit   = ssblimit.table,
                   prob.over.ssbban     = ssbban.table,
                   prob.over.ssbmin     = ssbmin.table,
                   prob.over.ssbmax     = ssbmax.table,
                   catch.aav       = catch.aav.table,
                   kobe.stat       = kobe.stat,
                   catch.risk = catch.risk,
                   overfishing.risk = overfishing.risk,
                   redzone.risk = redzone.risk,
                   bban.risk = bban.risk,
                   blimit.risk = blimit.risk,
                   bspecific.risk = bspecific.risk)
  return(res_list)

}


#' kobeII matrixの簡易版（Btarget, Blimitは決め打ちでβのみ変える)
#'
#' @param finput future_vpaの返り値の$input
#' @param year_beta_change betaを変更する年の範囲。NULLの場合には全部の年を変える。
#' @param datainput_setting_original make_future_dataに与えて引数を作り直す場合の make_future_dataの返り値
#' @param datainput_setting_extra betaに加えて変えたい追加の設定
#' @param label_name HCR_nameのラベルの名前. NULLの場合、beta_vectorが使われる
#'
#' @encoding UTF-8
#' @export
#'
#'

beta.simulation <- function(finput,beta_vector,year.lag=0,type="old",year_beta_change=NULL,
                            datainput_setting_extra   =NULL,
                            datainput_setting_original=NULL,
                            label_name = NULL,
                            output_type="tidy", ncore = 1){

  if(!is.null(datainput_setting_extra)) assertthat::assert_that(length(datainput_setting_extra)==length(beta_vector))
  if(!is.null(label_name)){
    assertthat::assert_that(length(label_name)==length(beta_vector))
  }
  else{
    label_name <- beta_vector
    }

  tb <- NULL
  future_year <- dimnames(finput$tmb_data$HCR_mat)[[1]]
  if(!is.null(year_beta_change)){
    year_column_beta_change <- future_year %in% year_beta_change
  }
  else{
    year_column_beta_change <- TRUE
  }

  if(ncore==1){
    for(i in 1:length(beta_vector)){
      if(type=="old"){
        stop("old function of future.vpa is not supported now")
      }
      else{
        finput$tmb_data$HCR_mat[year_column_beta_change,,"beta"] <- beta_vector[i]
        if(!is.null(finput$MSE_input_data)) finput$MSE_input_data$input$HCR_beta <- beta_vector[i]
        if(!is.null(datainput_setting_extra)){
          finput$tmb_data <- redo_future(datainput_setting_original, datainput_setting_extra[[i]], only_data=TRUE)$data
        }
        fres_base <- do.call(future_vpa,finput)
        fres_base <- format_to_old_future(fres_base)
      }
      tmp <- convert_future_table(fres_base,label=label_name[i]) %>%
        rename(HCR_name=label)  %>% mutate(beta=beta_vector[i])
      tb <- bind_rows(tb,tmp)
    }
  }
  else{
    library(foreach)
    cl <- parallel::makeCluster(ncore, type="FORK")
    doParallel::registerDoParallel(cl)

    tb <- foreach::foreach(i=1:length(beta_vector),.combine="rbind")%dopar%{
      finput$tmb_data$HCR_mat[year_column_beta_change,,"beta"] <- beta_vector[i]
      if(!is.null(finput$MSE_input_data)) finput$MSE_input_data$input$HCR_beta <- beta_vector[i]
      if(!is.null(datainput_setting_extra)){
        finput$tmb_data <- redo_future(datainput_setting_original, datainput_setting_extra[[i]], only_data=TRUE)$data
      }      
      fres_base <- do.call(future_vpa,finput)
      fres_base <- format_to_old_future(fres_base)
      tmp <- convert_future_table(fres_base,label=label_name[i]) %>%
        rename(HCR_name=label)  %>% mutate(beta=beta_vector[i])
      tmp
    }
    parallel::stopCluster(cl)
  }
  return(tb)
}


#' MSYを達成するときの\%SPRを計算する(calc_future_perSPRのwrapper)
#'
#' @encoding UTF-8
#' @export
#'

calc_perspr <- function(...){
  calc_future_perSPR(...)
}

#' 将来予測の結果オブジェクトから生物パラメータを取り出して、その生物パラメータをベースに、Fvectorで与えたF at ageに対応するSPRを計算して返す
#'
#' @param fout 将来予測のアウトプット（finputがない場合)。future_vpaの結果はformat_to_old_future関数をかまさないと動かない。
#' @param res_vpa Popeの式を使うかどうか、plus_groupの設定のために利用。実際、漁獲量は計算していないので、不要といえば不要。is_popleとplus_groupが設定されていてもこちらを優先する。
#' @param is_pope res_vpaがない場合、popeの式を使うか
#' @param plus_group es_vpaがない場合、プラスグループを考慮するか
#' @param Fvector Fのベクトル
#' @param target.col 将来予測の何列目の年を取り出すか（NULLの場合、最後の年）
#' @param target.year 将来予測の何年目を取り出すか（年の名前）（NULLの場合、最後の年）
#' @param SPRtarget これを与えると、Fvectorを何倍すればここで指定した\%SPRと一致するかを返すようになる
#'
#' @encoding UTF-8
#' @export
#'

calc_future_perSPR <- function(fout=NULL,
                               res_vpa=NULL,
                               biopar=NULL,
                               Fvector,
                               is_pope=NULL,
                               plus_group=NULL,
                        Fmax=10,
                        target.col=NULL,
                        target.year=NULL,
                        SPRtarget=NULL,
                        SPR_unit="digit" # or "%"
){

  if(!is.null(res_vpa)){
    info_source <- "vpa"
    is_pope <- res_vpa$input$Pope
    plus_group <- res_vpa$input$plus.group
  }
  if(!is.null(fout))  info_source  <- "future"
  if(!is.null(biopar))  info_source  <- "bio" # bioが優先される
  
  fout.tmp <- fout

  SPR_multi <- ifelse(SPR_unit=="%", 100, 1)

  # 将来予測結果が与えられた場合
  if(info_source=="future"){
    # シミュレーションが複数回ある場合には、その平均値を用いる
    if(is.null(target.col) && is.null(target.year)){
      waa.tmp       <- fout.tmp$waa      [,dim(fout.tmp$waa)      [[2]],] %>% apply(1,mean)
      waa.catch.tmp <- fout.tmp$waa.catch[,dim(fout.tmp$waa.catch)[[2]],] %>% apply(1,mean)
      maa.tmp       <- fout.tmp$maa      [,dim(fout.tmp$maa)      [[2]],] %>% apply(1,mean)
      M.tmp         <- fout.tmp$M        [,dim(fout.tmp$M)        [[2]],] %>% apply(1,mean)
    }
    else{
      # 年の範囲を指定する場合、年で平均してから、シミュレーション回数で平均する
      if(!is.null(target.year)){
        if(!is.list(target.year)){
          target.year.char <- as.character(target.year)
          waa.tmp       <- fout.tmp$waa      [,target.year.char,,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
          waa.catch.tmp <- fout.tmp$waa.catch[,target.year.char,,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
          maa.tmp       <- fout.tmp$maa      [,target.year.char,,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
          M.tmp         <- fout.tmp$M        [,target.year.char,,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
      }
        else{
        waa.tmp       <- fout.tmp$waa      [,as.character(target.year$waa),,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
        waa.catch.tmp <- fout.tmp$waa.catch[,as.character(target.year$waa.catch),,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
        maa.tmp       <- fout.tmp$maa      [,as.character(target.year$maa),,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
        M.tmp         <- fout.tmp$M        [,as.character(target.year$M),,drop=FALSE] %>% apply(c(1,3),mean) %>% apply(1,mean)
      }
    }
    if(!is.null(target.col)){
      waa.tmp       <- fout.tmp$waa[,target.col,]       %>% apply(1,mean)
      waa.catch.tmp <- fout.tmp$waa.catch[,target.col,] %>% apply(1,mean)
      maa.tmp       <- fout.tmp$maa[,target.col,]       %>% apply(1,mean)
      M.tmp         <- fout.tmp$M[,target.col,]         %>% apply(1,mean)
    }
    }}

  if(info_source=="vpa"){ # 将来予測結果が与えられない場合にはVPA結果からもってくる
    if(!is.list(target.year)){
      target.year.char <- as.character(target.year)        
      waa.tmp       <- res_vpa$input$dat$waa      [target.year.char] %>% apply(1,mean)
      maa.tmp       <- res_vpa$input$dat$maa      [target.year.char] %>% apply(1,mean)
      M.tmp         <- res_vpa$input$dat$M        [target.year.char] %>% apply(1,mean)
      if(!is.null(res_vpa$input$dat$waa.catch)){
        waa.catch.tmp <- res_vpa$input$dat$waa.catch[target.year.char] %>% apply(1,mean)
      }
      else{
        waa.catch.tmp <- waa.tmp
      }
    }
    else{
      waa.tmp       <- res_vpa$input$dat$waa      [as.character(target.year$waa)      ] %>% apply(1,mean)
      maa.tmp       <- res_vpa$input$dat$maa      [as.character(target.year$maa)      ] %>% apply(1,mean)
      M.tmp         <- res_vpa$input$dat$M        [as.character(target.year$M.tmp)    ] %>% apply(1,mean)
      if(!is.null(res_vpa$input$dat$waa.catch)){
        waa.catch.tmp <- res_vpa$input$dat$waa.catch[as.character(target.year$waa.catch)] %>% apply(1,mean)
      }
      else{
        waa.catch.tmp <- waa.tmp
      }        
    }}

  if(info_source=="bio"){
    waa.tmp <- biopar$waa
    maa.tmp <- biopar$maa
    M.tmp <- biopar$M
    waa.catch.tmp <- biopar$waa.catch
  }

  # 緊急措置。本来ならどこをプラスグループとして与えるかを引数として与えないといけない
  # 現状で、すべてのカラムがゼロ＝資源計算では考慮されていないセルとして認識されている
  allsumpars <- waa.tmp+waa.catch.tmp+maa.tmp+M.tmp
  waa.tmp <- waa.tmp[allsumpars!=0]
  waa.catch.tmp <- waa.catch.tmp[allsumpars!=0]
  maa.tmp <- maa.tmp[allsumpars!=0]
  M.tmp <- M.tmp[ allsumpars!=0]
  Fvector <- Fvector %>%  as.numeric()
  Fvector <- Fvector[allsumpars!=0 & !is.na(allsumpars)]

  # SPRを計算
  if(!is.null(SPRtarget)) SPRtarget_tmp <- SPRtarget/SPR_multi*100 else SPRtarget_tmp <- NULL
  tmp <- calc_Fratio(Fvector,waa=waa.tmp,maa=maa.tmp,M=M.tmp,SPRtarget=SPRtarget_tmp,
                     waa.catch=waa.catch.tmp,Pope=is_pope,
                     return_SPR=TRUE,plus_group=plus_group)
  if(is.null(SPRtarget))  return(ifelse(length(tmp)==1,1*SPR_multi,tmp$SPR_original/100*SPR_multi))
  else{
    tmp$SPR_original <- tmp$SPR_original/100*SPR_multi
    tmp$SPR_est <- tmp$SPR_est/100*SPR_multi
    tmp$SPR_target <- tmp$SPR_target/100*SPR_multi
    tmp$waa <- waa.tmp
    tmp$waa.catch <- waa.catch.tmp
    tmp$maa <- maa.tmp
    tmp$M <- M.tmp
    return(tmp)
  }
}

#' kobeIItable から任意の表を指名して取り出す
#'
#' @param kobeII_table \code{make_kobeII_table}の出力
#' @param name \code{kobeII_table}の要素名
#'
#' @encoding UTF-8
pull_var_from_kobeII_table <- function(kobeII_table, name) {
  table <- kobeII.table[[name]]
  table %>%
    dplyr::arrange(desc(beta)) %>%
    dplyr::select(-HCR_name, -stat_name)
}

#' kobeIItableから取り出した表を整形
#'
#' - 報告書に不要な列を除去する
#' - 単位を千トンに変換
#' @param beta_table \code{pull_var_from_kobeII_table}で取得した表
#' @param divide_by 表の値をこの値で除する．トンを千トンにする場合には1000
#' @param round TRUEなら値を丸める．漁獲量は現状整数表示なのでデフォルトはTRUE
format_beta_table <- function(beta_table, divide_by = 1, round = TRUE) {
  beta   <- beta_table %>%
    dplyr::select(beta) %>%
    magrittr::set_colnames("\u03B2") # greek beta in unicode
  values <- beta_table %>%
    dplyr::select(-beta) / divide_by
  if (round == TRUE) return(cbind(beta, round(values)))
  cbind(beta, values)
}

#' 値の大小に応じて表の背景にグラデーションをつける
#' @param beta_table \code{format_beta_table}で整形したβの表
#' @param color 表の背景となる任意の色
colorize_table <- function(beta_table, color) {
  beta_table %>%
    formattable::formattable(list(formattable::area(col = -1) ~
                                    formattable::color_tile("white", color)))
}

#' 表を画像として保存
#'
#' # @inheritParams \code{\link{formattable::as.htmlwidget}}
#' # @inheritParams \code{\link{htmltools::html_print}}
#' # @inheritParams \code{\link{webshot::webshot}}
#' @param table ファイルとして保存したい表
#' @examples
#' \dontrun{
#' your_table %>%
#'  export_formattable(file = "foo.png")
#' }
#' @export
export_formattable <- function(table, file, width = "100%", height = NULL,
                               background = "white", delay = 0.1) {
  widget <- formattable::as.htmlwidget(table, width = width, height = height)
  path   <- htmltools::html_print(widget, background = background, viewer = NULL)
  url    <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot::webshot(url,
                   file = file,
                   selector = ".formattable_widget",
                   delay = delay)
}

#' kobeIItableから任意の表を取得し画像として保存
#'
#' @inheritParams \code{\link{pull_var_from_kobeII_table}}
#' @inheritParams \code{\link{format_beta_table}}
#' @inheritParams \code{\link{colorize_table}}
#' @inheritParams \code{\link{export_formattable}}
export_kobeII_table <- function(name, divide_by, color, fname, kobeII_table) {
  kobeII_table %>%
    pull_var_from_kobeII_table(name) %>%
    format_beta_table(divide_by = divide_by) %>%
    colorize_table(color) %>%
    export_formattable(fname)
}

#' β調整による管理効果を比較する表を画像として一括保存
#'
#' @inheritParams \code{\link{pull_var_from_kobeII_table}}
#' @param fname_ssb 「平均親魚量」の保存先ファイル名
#' @param fname_catch 「平均漁獲量」の保存先ファイル名
#' @param fname_ssb_above_target 「親魚量が目標管理基準値を上回る確率」の保存先ファイル名
#' @param fname_ssb_above_limit 「親魚量が限界管理基準値を上回る確率」の保存先ファイル名
#' @examples
#' \dontrun{
#' export_kobeII_tables(kobeII.table)
#' }
#' @export
export_kobeII_tables <- function(kobeII_table,
                                 fname_ssb = "tbl_ssb.png",
                                 fname_catch = "tbl_catch.png",
                                 fname_ssb_above_target = "tbl_ssb>target.png",
                                 fname_ssb_above_limit = "tbl_ssb>limit.png") {
  blue   <- "#96A9D8"
  green  <- "#B3CE94"
  yellow <- "#F1C040"

  purrr::pmap(list(name = c("ssb.mean", "catch.mean",
                            "prob.over.ssbtarget", "prob.over.ssblimit"),
                   divide_by = c(1000, 1000, 1, 1),
                   color = c(blue, green, yellow, yellow),
                   fname = c(fname_ssb, fname_catch,
                             fname_ssb_above_target, fname_ssb_above_limit)),
              .f = export_kobeII_table,
              kobeII_table = kobeII_table)
}




#'
#' 将来予測の結果のリストを入れると、代表的なパフォーマンス指標をピックアップする
#'
#' @param future_list future_vpaまたはfuture.vpaの返り値のリスト
#' @param res_vpa vpaの返り値
#' @param ABC_year 特にABCに注目したい年
#' @param is_MSE MSEの結果を使うかどうか。MSEの結果の場合には、ABCや加入尾数の真の値との誤差も出力する
#' @param indicator_year ABC_yearから相対的に何年後を指標として取り出すか
#' @param Btarget 目標管理基準値の値
#' @param Blimit 限界管理基準値の値
#' @param Bban 禁漁水準の値
#' @param type 出力の形式。"long"は縦長（ggplotに渡すときに便利）、"wide"は横長（数字を直接比較するときに便利）
#' @param biomass.unit 資源量の単位
#'
#' @export
#' @encoding UTF-8
#'

get_performance <- function(future_list,res_vpa,ABC_year=2021,
                            is_MSE=FALSE,
                            indicator_year=c(0,5,10),Btarget=0, Blimit=0, Bban=0,
                            type=c("long","wide")[1],biomass.unit=10000,...){

  future_list_original <- future_list
  future_list <- purrr::map(future_list,
                            function(x) if(class(x)=="future_new")
                              format_to_old_future(x) else x)

  if(is.null(names(future_list))) names(future_list) <- 1:length(future_list)

  future_tibble <- purrr::map_dfr(1:length(future_list),
                                  function(i) convert_future_table(future_list[[i]],
                                                                   label=names(future_list)[i]) %>%
                                    rename(HCR_name=label) %>% mutate(beta=NA))

  kobe_res <- make_kobeII_table(future_tibble,res_vpa,
                                year.ssb   = ABC_year+indicator_year,
                                year.catch = ABC_year+indicator_year,
                                year.ssbtarget = ABC_year+indicator_year,
                                year.ssblimit  = ABC_year+indicator_year,
                                year.ssbban=NULL, year.ssbmin=NULL, year.ssbmax=NULL,
                                year.aav = c(ABC_year,ABC_year-1),
                                Btarget= Btarget,
                                Blimit = Blimit,
                                Bban   = Bban)

  if(isTRUE(is_MSE)){
    error_table <- purrr::map_dfr(1:length(future_list_original), function(i){
      if("SR_MSE" %in% names(future_list_original[[i]])){
        plot_bias_in_MSE(future_list_original[[i]], out="stat") %>%
          dplyr::filter(year %in% (ABC_year + indicator_year)) %>%
          group_by(year, stat) %>%
          summarise(mean_error=mean(Relative_error_normal)) %>%
          mutate(HCR_name=names(future_list)[i])
      }
      else{
        NULL
      }
    })
    error_table <- error_table %>%
      gather(key=stat_name,value=value,-HCR_name,-year,-stat) %>%
      mutate(unit="",stat_category="推定バイアス") %>%
      mutate(stat_year_name=str_c(stat_category,year)) %>%
      ungroup(year) %>%
      mutate(year=as.character(year)) %>%
      mutate(stat_name=stat) %>%
      select(-stat)
  }
  else{
    error_table <- NULL
  }

  junit <- c("","十","百","千","万")[log10(biomass.unit)+1]

  stat_data <- tibble(stat_name=c("ssb.mean","catch.mean","Pr(SSB>SSBtarget)","Pr(SSB>SSBlim)",
                                  "catch.aav"),
                      stat_category=c("平均親魚量 ", "平均漁獲量 ", "目標上回る確率 ", "限界上回る確率 ",
                                      "漁獲量変動"))

  kobe_res <- purrr::map_dfr(kobe_res[c("ssb.mean", "catch.mean", "prob.over.ssbtarget",
                                        "prob.over.ssblimit", "catch.aav")],
                             function(x) x %>% select(-beta) %>%
                               gather(key=year,value=value,-HCR_name,-stat_name)) %>%
    mutate(value=ifelse(stat_name %in% c("ssb.mean", "catch.mean"), value/biomass.unit, value)) %>%
    mutate(unit =ifelse(stat_name %in% c("ssb.mean", "catch.mean"), str_c(junit, "トン"), "%")) %>%
    mutate(unit =ifelse(stat_name %in% c("catch.aav"), "", unit)) %>%
    left_join(stat_data) %>%
    mutate(stat_year_name=str_c(stat_category,year))

  kobe_res <- bind_rows(kobe_res,error_table)

  if(type=="wide"){
    kobe_res <- kobe_res  %>%
      select(-year, -stat_name, -stat_category) %>%
      spread(key=HCR_name,value=value) %>%
      select(2:ncol(.),1)
  }

  return(tibble::lst(kobe_res,error_table))
}

#'
#' 短期的将来予測における複数の管理方策のパフォーマンスを比較する表を出力する
#'
#' @param future_list 将来予測の結果のリスト
#' @param res_vpa VPAの結果
#' @param ... get_performanceで必要な引数
#'
#' @encoding UTF-8
#' @export
#'

compare_future_performance <- function(future_list,res_vpa,res_MSY,
                                       biomass.unit=1000,is_MSE=FALSE,...){
  perform_res <- get_performance(future_list=future_list, res_vpa=res_vpa,
                                 Btarget=derive_RP_value(res_MSY$summary,"Btarget0")$SSB,
                                 Blimit =derive_RP_value(res_MSY$summary,"Blimit0")$SSB,
                                 Bban   =derive_RP_value(res_MSY$summary,"Bban0")$SSB,
                                 type="long",is_MSE=is_MSE,biomass.unit=biomass.unit,...)

  g1_ssb0 <- perform_res$kobe_res %>% dplyr::filter(stat_name=="ssb.mean") %>%
    ggplot() +
    geom_bar(aes(x=HCR_name,y=value,fill=stat_category),stat="identity") +
    facet_wrap(stat_category~year,ncol=1)+coord_flip()+
    geom_label(aes(x=HCR_name,y=max(value)/2,label=str_c(round(value),unit)),
               alpha=0.5)+
    theme_SH() + theme(legend.position="top")+xlab("Senario") +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values=c("lightblue"))

  g1_ssb <- g1_ssb0 +
    geom_hline(yintercept=derive_RP_value(res_MSY$summary,"Btarget0")$SSB/biomass.unit,
               col="#00533E")+
    geom_hline(yintercept=derive_RP_value(res_MSY$summary,"Blimit0")$SSB/biomass.unit,
               col="#edb918")

  g1_catch0 <- g1_ssb0 %+% dplyr::filter(perform_res$kobe_res, stat_name=="catch.mean")+
    scale_x_discrete(labels=rep("",4))+
    scale_fill_manual(values=c("lightgreen"))

  g1_catch <- g1_catch0 +
    geom_hline(yintercept=derive_RP_value(res_MSY$summary,"Btarget0")$Catch/10000,
               col="#00533E")+
    geom_hline(yintercept=rev(colSums(res_vpa$wcaa,na.rm=T))[1]/biomass.unit,
               col="gray",lty=2)

  g1_probtar <- g1_catch0 %+% dplyr::filter(perform_res$kobe_res, stat_name=="Pr(SSB>SSBtarget)")+
    scale_fill_manual(values=c("lightblue"))+
    geom_hline(yintercept=c(0,50,100),col="gray",lty=2)

  g1_problim <- g1_catch0 %+% dplyr::filter(perform_res$kobe_res, stat_name=="Pr(SSB>SSBlim)")+
    scale_fill_manual(values=c("gray"))+
    geom_hline(yintercept=c(0,50,100),col="gray",lty=2)

  g1_error_table <- perform_res$error_table  %>%
    ggplot() +
    geom_bar(aes(x=HCR_name,y=value,fill=stat_category),stat="identity") +
    facet_grid(year~stat_name)+coord_flip()+
    geom_label(aes(x=HCR_name,y=max(value)/2,label=str_c(round(value,2),unit)),
               alpha=0.5)+
    theme_SH() + theme(legend.position="top")+xlab("Senario") +
    guides(fill=guide_legend(title=""))+
    scale_fill_manual(values=c("lightpink"))

  g1_performance <- gridExtra::marrangeGrob(list(g1_ssb,g1_probtar,g1_catch,g1_problim),
                                            widths=c(1.3,1,1,1),nrow=1,ncol=4)

  list(g1_performance, g1_error_table, perform_res)
  #ggsave(g1_performance, filename="g1_performance.png" ,path=output_folder,
  #           width=20, height= 10)

  #    ggsave(g1_error_table, filename="g1_error_table.png" ,path=output_folder,
  #       width=15, height= 8)

}


#'
#' calculate F/Ftarget based on F_\%SPR multiplier
#'
#' @param faa F at age
#' @param waa weight at age
#' @param maa maturity at age
#' @param M natural morality at age
#' @param SPRtarget target SPR (NULLの場合には最適化しない)
#' @param return_SPR return SPR as well as Fratio
#' @param plus_group プラスグループを考慮するかどうか
#' @param max.age SPR計算を打ち切る最大の年。デフォルトはInf
#'
#' もともとのF at ageの最大がexp(-7)よりも小さい場合にはFratio=0となる。一方で、F at ageをすごく大きくしても指定されたSPRを実現できないような場合のFratioの上限値を50とする。
#'
#' @export
#' @encoding UTF-8
#'


calc_Fratio <- function(faa, waa, maa, M, SPRtarget=30, waa.catch=NULL,Pope=TRUE, return_SPR=FALSE, plus_group=TRUE, max.age=Inf){

  if(plus_group==FALSE) max.age  <- length(faa)

  calc.rel.abund2_ <- function(sel,Fr){
    calc.rel.abund(sel,Fr,na=length(faa),M=M, waa=waa, waa.catch=waa.catch,
                     min.age=1,max.age=max.age,Pope=Pope,ssb.coef=0,maa=maa)
  }

  tmpfunc <- function(x,SPR0=0,...){
    SPR_tmp <- calc.rel.abund2_(faa,exp(x))$spr %>% sum()
    sum(((SPR_tmp/SPR0*100)-SPRtarget)^2)
  }

  if(max(faa, na.rm=T)<exp(-7)){ return(0) }

  else{
    tmp <- !is.na(faa)
    SPR0 <- calc.rel.abund2_(faa,0)$spr %>% sum()
    SPR_original <- calc.rel.abund2_(faa,1)$spr %>% sum()
    SPR_original <- SPR_original/SPR0*100
    if(!is.null(SPRtarget)){
        opt_res <- optimize(tmpfunc,interval=c(-7,log(50)),SPR0=SPR0)
        SPR_est <- calc.rel.abund2_(faa,exp(opt_res$minimum))$spr %>% sum()
        SPR_est <- SPR_est/SPR0 * 100
#        if(abs(SPR_est-SPRtarget)>0.01) {return(NA)}
        Fratio <- 1/exp(opt_res$minimum)
    }
    else{
      SPR_est <- SPR_original
      Fratio <- 1
    }

    if(isTRUE(return_SPR)){
        list(Fratio=Fratio, SPR_est=SPR_est, SPR_target=SPRtarget, SPR_original=SPR_original)
    }
    else{
        return(Fratio)
    }
  }
}


#'
#' 使うフォルダ名を与えると一連の結果の関数を読み込む関数
#'
#' @export
#' @encoding UTF-8
#'

load_folder <- function(folder_name){

  tmpfunc <- function(folder_name, file_name){
    if(isTRUE(file.exists(str_c(folder_name,"/",file_name)))){
      if(isTRUE(str_detect(file_name, pattern=".rda"))){
        a <- load(str_c(folder_name,"/",file_name))
        a <- get(a)
      }
      if(isTRUE(str_detect(file_name, pattern=".csv"))){
        a <- read_csv(str_c(folder_name,"/",file_name))
      }
    }
    else{
      a <- NA
    }
    return(a)
  }

  file_name <- c("res_MSY.rda","res_SR.rda","res_future_0.8HCR.rda","kobeII.table.rda","model_selection.csv")

  res_all <- list()
  for(i in 1:length(file_name)){
    res_all[[i]] <- purrr::map(folder_name, function(x) tmpfunc(x,file_name[i]))
    res_all[[i]] <- res_all[[i]][!is.na(res_all[[i]])]
  }
  names(res_all) <- file_name

  res_all$res_vpa <- purrr::map(res_all$res_MSY.rda, function(x) if(!is.na(x)) x$res_vpa else NA)
  res_all$res_vpa <- res_all$res_vpa[!is.na(res_all$res_vpa)]
  invisible(res_all)
}


#' Make Kobe ratio (?) for frasyr_tool
#'
#' @param result_vpa object createb dy vpa()
#' @param result_msy object created by script 1do_MSYest.R of SC meeting
#' @return A tibble object
#' @export
make_kobe_ratio <- function(result_vpa, result_msy) {

  assertthat::assert_that(
    assertthat::has_name(result_vpa, c("ssb")),
    assertthat::has_name(result_msy, c("summary")),
    assertthat::has_name(result_msy$summary, c("perSPR"))
  )

  return_kobe_ratio <- function() {
    tibble::tibble(year   = get_year(),
                   Fratio = get_f_ratio(),
                   Bratio = calc_b_ratio()) %>%
      ad_hoc_filter()
  }

  get_year <- function() {
    colnames(result_vpa$ssb)
  }

  get_f_ratio <- function() {
    target_spr  <- derive_RP_value(result_msy$summary,"Btarget0")$perSPR * 100
    spr_history <- get.SPR(result_vpa,
                           target_spr, Fmax = 7)

    assertthat::assert_that(
      assertthat::validate_that(is.list(spr_history)),
      assertthat::has_name(spr_history, "ysdata"),
      assertthat::has_name(spr_history$ysdata, "F/Ftarget"),
      assertthat::are_equal(rownames(spr_history$ysdata),
                            colnames(result_vpa$ssb))
    )

    force(spr_history$ysdata$`F/Ftarget`)
  }


  calc_b_ratio <- function() {
    ssb        <- colSums(result_vpa$ssb, na.rm=T)
    target_ssb <- derive_RP_value(result_msy$summary,"Btarget0")$SSB

    force(ssb / target_ssb)
  }

  ad_hoc_filter <- function(koberatio) {
    dplyr::filter(koberatio, !is.na(Bratio)) # When does Bratio become 'NA'?
  }

  return_kobe_ratio()
}

#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.
#'
#' @export

source_lines <- function(file, lines, encoding="UTF-8",...){
    source(textConnection(readLines(file, encoding=encoding)[lines]),...)
}

#' re-calculate projection with different arguments
#'
#' @param data_future make_future_dataの返り値。これ全体でなくて、$inputのみでもOK
#'

redo_future <- function(data_future, input_data_list, SR_sd=NULL, SR_b=NULL, only_data=FALSE,is_regime=(class(data_future$input$res_SR)=="fit.SRregime"), ...){
  
  if("input" %in% names(data_future)) input_data <- data_future$input else input_data <- data_future
  
  if(! all(names(input_data_list) %in% names(input_data))) stop("names of input_data_list is invalid!")

  input_data[names(input_data_list)] <- input_data_list

  if(!is.null(SR_sd)){
    if(is_regime){
      cat("This is regime future\n")
      input_data$res_SR$regime_pars$sd[] <- SR_sd
      input_data$res_SR$pars$sd[] <- SR_sd
    }
    else{
      input_data$res_SR$pars$sd[] <- SR_sd
    }
    if(input_data$resid_type!="lognormal") warning("SR_sd=0 cannot be used in nonparametric recruitment")
  }

  if(!is.null(SR_b)){
    if(is_regime){
      cat("This is regime future\n")
      input_data$res_SR$regime_pars$b <- SR_b
    }
    else{
      input_data$res_SR$pars$b <- SR_b
    }
  }

  future_data <- safe_call(make_future_data,input_data)
  if(only_data==TRUE) return(future_data) else future_vpa(future_data$data,...)
}

#'
#' 将来予測においてFが非常に小さい場合には決定論的予測と確率論的予測の平均がほぼ一致するかを確認するための関数
#'
#' HSを仮定していて、折れ点の下側から将来予測するような場合には、将来予測の最初のほうは一致しない。マッチングする年代などを工夫する必要がある
#'
#' @export
#'

test_sd0_future <- function(data_future,nsim=NULL,nyear=10,future_range=NULL,...){

  is_regime <- !is.null(data_future$input$regime_shift_option)
  {if(is_regime){
    sd_org <- data_future$input$res_SR$regime_pars %>%
      dplyr::filter(regime==data_future$input$regime_shift_option$future_regime) %>%
      select(sd)
  }
  else{
    sd_org <- data_future$input$res_SR$pars$sd
  }}

  # determine sample size
  tol <- 0.007
  if(is.null(nsim)) nsim <- round((0.5/tol)^2) # 1%以下（0.7%）の誤差が期待されるnsim
  cat("nsim for checking sd=0:",nsim,"\n")

  # run 2 funture projections
  res1 <- redo_future(data_future, list(nsim=nsim, nyear=nyear), multi_init=0.01, ...)
  res2 <- redo_future(data_future, list(nsim=2   , nyear=nyear), multi_init=0.01, SR_sd=0, ...)

  a <- try(compare_future_res12(res1,res2,future_range=future_range))

  cat("* Fをなるべく小さくした場合の将来予測において、決定論的予測と確率的予測の平均値がほぼ同じになるか？（ここでnoが出る場合にはバグの可能性があるので管理者に連絡してください）（モデル平均・バックワードリサンプリングの場合にはnoになっちゃいます（今後改善））: ",ifelse(class(a)=="try-error", "not ","OK\n"))

  return(lst(res1,res2,a))
}

compare_future_res12 <- function(res1,res2,tol=0.01, future_range=NULL){
  nyear <- dim(res1$naa)[[2]]
  if(is.null(future_range)) future_range <- res1$input$tmb_data$start_random_rec_year:nyear
  if(dim(res1$naa)[[3]]==dim(res2$naa)[[3]]){
      mean_difference_in_naa <- mean(abs(1-res1$naa[,future_range,]/res2$naa[,future_range,]),na.rm=T)
      mean_difference_in_wcaa <- mean(abs(1-res1$wcaa[,future_range,]/res2$wcaa[,future_range,]),na.rm=T)
  }
  else{
      mean_difference_in_naa <- 1-mean(apply(res1$naa[,future_range,],c(1,2),mean,na.rm=TRUE)/
                                     apply(res2$naa[,future_range,],c(1,2),mean,na.rm=TRUE),na.rm=T)
      mean_difference_in_wcaa <- 1-mean(apply(res1$wcaa[,future_range,],c(1,2),mean,na.rm=TRUE)/
                                      apply(res2$wcaa[,future_range,],c(1,2),mean,na.rm=TRUE),na.rm=T)
  }

  cat("mean_difference in naa=", mean_difference_in_naa,"\n")
  expect_equal(mean_difference_in_naa,0,tol=tol)

  cat("mean_difference in wcaa=", mean_difference_in_wcaa,"\n")
  expect_equal(mean_difference_in_wcaa,0,tol=tol)
}

#'
#' MSEの計算が正しいかを確認するための関数
#'
#' 1. sd=0の場合の結果の比較
#' 2. MSE_nsim=2にしてMSE_sd=0にしても良いかどうか
#'
#' @export
#'

check_MSE_sd0 <- function(data_future, data_MSE=NULL, nsim_for_check=10000, tol=c(0.01,0.01,0.01)){

  data_future_sd0 <- redo_future(data_future,list(nyear=5,nsim=5),only_data=TRUE,SR_sd=0)
  data_future     <- redo_future(data_future,list(nyear=5,nsim=5),only_data=TRUE)
  data_future_10000 <- redo_future(data_future,list(nyear=5,nsim=nsim_for_check),only_data=TRUE)

  if(!is.null(data_MSE)) data_MSE <- redo_future(data_MSE,list(nyear=5,nsim=5),only_data=TRUE)
  else data_MSE <- data_future

    # check MSE program is correct?
  res1 <- future_vpa(tmb_data=data_future_sd0$data,
                         optim_method="none",multi_init = 1,SPRtarget=0.3,
                         do_MSE=TRUE, MSE_input_data=data_future_sd0,MSE_nsim=2)
  res2 <- future_vpa(tmb_data=data_future_sd0$data,
                     optim_method="none",multi_init = 1,SPRtarget=0.3,
                     do_MSE=FALSE, MSE_input_data=data_future_sd0,MSE_nsim=2)
  a1 <- try(compare_future_res12(res1,res2,tol=tol[1]))
  cat("* 真の個体群動態のSD=0のとき, MSEした結果と単純シミュレーションの結果が一致するか？（加入の残差にリサンプリングを使っている場合にはSD=0でも決定論的予測にならないので一致しなくても大丈夫。対数正規分布残差でここがOKにならない場合にはバグの可能性があるので管理者に連絡してください） ",ifelse(class(a1)=="try-error", "not ",""),"OK\n")

  # check sd in MSE=0 is OK?
  res1.time <- system.time(
    res1 <- future_vpa(tmb_data=data_future$data,
                     optim_method="none",
                     multi_init = 1,SPRtarget=0.3,
                     do_MSE=TRUE, MSE_input_data=data_MSE,MSE_nsim=nsim_for_check))

  res2.time <- system.time(
    res2 <- future_vpa(tmb_data=data_future$data,
                     optim_method="none",
                     multi_init = 1,SPRtarget=0.3,
                     do_MSE=TRUE, MSE_input_data=data_MSE,MSE_nsim=2,MSE_sd=0))
  a2 <- try(compare_future_res12(res1,res2,tol=tol[2]))
  cat("* MSEしたとき、ABC算定年までの加入のSDを0として、決定論的な漁獲量をABCとしても、nsim_for_check回分の確率計算の平均漁獲量をABCした場合と同じになるか？（OKであればMSE_sd=0にして計算時間を短縮できる、加入の残差にリサンプリングを使っている場合には同じにならない：MSE_sd=0は使えない） ",ifelse(class(a2)=="try-error", "not ",""),"OK\n")
  cat("res1.time=",res1.time,"res2.time=",res2.time,"\n")

  # first year catch
  res1 <- future_vpa(tmb_data=data_future_10000$data,
                     optim_method="none",multi_init = 1,SPRtarget=0.3,
                     do_MSE=FALSE,MSE_input_data=data_future_10000)

  res2 <- future_vpa(tmb_data=data_future_10000$data,
                     optim_method="none",multi_init = 1,SPRtarget=0.3,
                     do_MSE=TRUE, MSE_nsim=2, MSE_sd=0, MSE_input_data=data_future_10000)
  # ここのtorelanceはそんなに高くない
  a3 <- try(expect_equal(mean(get_wcatch(res1)["2019",])/
               mean(get_wcatch(res2)["2019",]),
               1,tol=tol[3]))
  cat("* HCRを導入する最初の年のABCは通常の将来予測の平均漁獲量と、MSEを十分回数実施したときの平均漁獲量と一致するはず。それぞれnsim_for_check回数分計算した場合、一致するか？（上の２つが通っている場合、ここもOKになるはず。OKにならなかったらバグの可能性があるので管理者に連絡してください）: ",ifelse(class(a3)=="try-error", "not ",""),"OK\n")
  return(lst(a1,a2,a3,res1,res2))
}


#'
#'
#'  @export
#'

take_interval <- function(prob,target){
    x <- which(abs(diff(sign(prob-target)))>0)
    c(x,x+1)
}


#'
#' 将来予測やVPAの結果から生物パラメータをとりだす
#'
#' @param res_obj VPAか将来予測の結果のオブジェクト。どちらでも良い。
#' @param derive_year 生物パラメータとF at ageを取り出す期間（年の名前で指定）
#' @param stat 取り出した期間のパラメータをここで指定する関数で処理する。基本は平均する（mean）
#'
#' 将来予測結果を入れる場合には複数のシミュレーション、年の間の結果をすべてstatする
#'
#' @export
#'

derive_biopar <- function(res_obj=NULL, derive_year=NULL, stat=mean){

  derive_year <- as.character(derive_year)

  if(!is.null(res_obj$input$dat$caa)){
    res_obj$input$dat$faa <- res_obj$faa
    derive_char <- c("M","waa","maa","faa")
    if(!is.null(res_obj$input$dat$waa.catch)) derive_char <- c(derive_char,"waa.catch")
    bio_par <- purrr::map_dfc(res_obj$input$dat[derive_char],
                              function(x) apply(x[,derive_year,drop=F],1,stat))
    if(!is.null(res_obj$input$dat$waa.catch)) bio_par$waa.catch <- bio_par$waa
  }

  if(class(res_obj)=="future"|class(res_obj)=="future_new"){
    bio_list <- res_obj[c("waa","faa")]
    if(is.null(res_obj$maa)) bio_list$maa <- res_obj$input$tmb_data$maa else bio_list$maa <- res_obj$maa
    bio_list$M <- res_obj$input$tmb_data$M
    if(is.null(bio_list$M)) bio_list$M <- res_obj$M
    if(!is.null(res_obj$waa_catch_mat)) bio_list$waa.catch <- res_obj$waa_catch_mat else bio_list$waa.catch <- res_obj$waa.catch
    bio_par <- purrr::map_dfc(bio_list,
                   function(x) apply(x[,derive_year,,drop=F],1,stat))
  }

  bio_par <- bio_par[apply(bio_par,1,sum)!=0,]
  bio_par <- bio_par[!is.na(apply(bio_par,1,sum)),]
  return(bio_par)
}


#' 与えられた個体群動態でプラスグループが考慮されているかどうか
#' @param dres VPAの結果
#'
#' @export

detect_plus_group <- function(dres){
  naa2 <- dres$naa[,2]
  plus_age <- max(which(!is.na(naa2)))
  naa2_plus <- calc_forward(naa=dres$naa,faa=dres$faa,M=dres$input$dat$M,t=1,plus_age=plus_age,plus_group=TRUE)[,2]
  naa2_noplus <- calc_forward(naa=dres$naa,faa=dres$faa,M=dres$input$dat$M,t=1,plus_age=plus_age,plus_group=FALSE)[,2]
  if(sum((naa2-naa2_plus)^2,na.rm=T)<sum((naa2-naa2_noplus)^2,na.rm=T)) plus.group <- TRUE else plus.group <- FALSE
  return(plus.group)
}


#' future_vpaの返り値に要約統計量を加えるための内部関数
#'
#' @param res_future future_futureの返り値
#' @param target 指定するとtargetで指定した列の値のみが抽出される。NULLの場合は全シミュレーションの平均値
#'
#' @export
#' 

derive_future_summary <- function(res_future, target=NULL){

  assertthat::assert_that(class(res_future) == "future_new")
  
  if(is.null(target)){
    tmpfunc <- function(x, fun=mean) apply(x,1,fun)
  }
  if(!is.null(target)){
    tmpfunc <- function(x) x[,target]
  }  

  biomass <- apply(res_future$naa*res_future$waa,c(2,3),sum)
  Fmean <- apply(res_future$faa,c(2,3),sum)
    
  tibble(
    year    = as.numeric(dimnames(res_future$SR_mat[,,"ssb"])[[1]]),
    SSB     = tmpfunc(res_future$SR_mat[,,"ssb"]),
    biomass = tmpfunc(biomass),
    recruit = tmpfunc(res_future$SR_mat[,,"recruit"]),
    intercept = tmpfunc(res_future$SR_mat[,,"intercept"]),
    deviance = tmpfunc(res_future$SR_mat[,,"deviance"]),
    deviance_sd = tmpfunc(res_future$SR_mat[,,"deviance"],fun=sd),            
    catch   = tmpfunc(res_future$HCR_realized[,,"wcatch"]),
    beta    = tmpfunc(res_future$HCR_mat[,,"beta"]),
    Blimit  = tmpfunc(res_future$HCR_mat[,,"Blimit"]),
    Bban    = tmpfunc(res_future$HCR_mat[,,"Bban"]),        
    beta_gamma = tmpfunc(res_future$HCR_realized[,,"beta_gamma"]),
    Fmean      = tmpfunc(Fmean),
    Fratio     = tmpfunc(res_future$HCR_realized[,,"Fratio"]))
}


#'
#' F at ageをVPAの結果から\%SPRで変換したりするための関数
#' 
#' @param res_vpa VPAの結果オブジェクト(Popeの設定やF at ageをこちらからとってくる)
#' @param data_future 将来予測のためのデータ(生物パラメータを将来予測期間から撮ってくる場合に必要)
#' @param faa_vector 漁獲圧を代表するベクトル
#' @param faa_vector_year 漁獲圧を取り出すときの年の範囲(VPA期間限定)
#' @param faa_bio_year 漁獲圧をSPRに換算するときに生物パラメータを取り出す年の範囲(data_futureがある場合将来予測年も指定可能。生物パラメータが密度によって変わる場合)。list(waa = 2014:2018, waa.catch = 2014:2018, maa = 2016:2018,M   = 2014:2018)とすると生物パラメータによって異なる期間の指定も可能。下のsaa_bio_yearも同様
#' @param faa_bio 漁獲圧をSPRに換算するとき用いる生物パラメータそのものtibble(waa=waa, maa=maa, M=M)として指定する
#' @param saa_vector 選択率を代表するベクトル
#' @param saa_vector_year 選択率を取り出すときの年の範囲(VPA期間限定)
#' @param saa_bio_year 選択率をSPRに換算するときに生物パラメータを取り出す年の範囲(data_futureがある場合将来予測年も指定可能。生物パラメータが密度によって変わる場合)
#' @param saa_bio 選択率をSPRに換算するとき用いる生物パラメータそのものtibble(waa=waa, maa=maa, M=M)として指定する
#' 
#' @export
#'
#' 

convert_Fvector <- function(res_vpa=NULL,
                            res_future = NULL,
                            faa_vector=NULL,
                            faa_vector_year=NULL,
                            faa_bio_year=NULL,
                            faa_bio=NULL,                            
                            saa_vector=NULL,
                            saa_vector_year=NULL,
                            saa_bio_year=NULL,
                            saa_bio=NULL){

  assert_that(
    (is.null(faa_vector)   | is.null(faa_vector_year)),
    (is.null(saa_vector)   | is.null(saa_vector_year)),
    (is.null(saa_bio_year) | is.null(saa_bio)),
    (is.null(faa_bio_year) | is.null(faa_bio))
  )
  
  if(is.null(faa_vector)) faa_vector <- apply_year_colum(res_vpa$faa,target_year=faa_vector_year)
  if(is.null(saa_vector)) saa_vector <- apply_year_colum(res_vpa$faa,target_year=saa_vector_year)    
    
  # faa_vectorが何％のSPRにあたるか
  faa_perSPR <- calc_future_perSPR(fout    = res_future,
                                   res_vpa = res_vpa,
                                   Fvector = faa_vector,
                                   target.year = faa_bio_year,
                                   biopar = faa_bio)
  cat("%SPR in faa=", faa_perSPR,"\n")
  # saa_vectorがfaa_vectorに相当する%SPRになるためには何倍にしないといけないか
  saa_multiplier <- calc_future_perSPR(fout=res_future,
                                       res_vpa=res_vpa,
                                       Fvector=saa_vector,
                                       target.year=saa_bio_year, 
                                       SPRtarget=faa_perSPR,
                                       biopar=saa_bio)
  # faaの漁獲圧の大きさに相当するsaaの選択率を持ったF at age
  Fvector <- saa_vector/saa_multiplier$Fratio      
  return(lst(Fvector, faa_perSPR))
}


# 縦の行列の年によって足し算する; 関数の汎用版
#' @export

rowtapply2 <- function(a0,FUN.name){                                                  
    FUN <- get(FUN.name)
    yname <- floor(as.numeric(rownames(a0))) 
    res <- matrix(0,length(unique(yname)),ncol(a0))
    for(i in 1:ncol(a0)){                                                               
        res[,i] <- tapply(a0[,i],yname,FUN)                                               
    }
    dimnames(res) <- list(unique(yname),colnames(a0))                                   
    res                                                                                 
}    

#'
#' kobe.tableをさらにsummaryする
#' 
#' @export

summary_kobe_table <- function(kobe_data){
  tmpfunc_ <- function(x,header)  str_c(header,"_",x)
  arrange_table <- function(kobedata){
    kobedata <- kobedata[c("catch.mean","ssb.mean","prob.over.ssbtarget","catch.risk","overfishing.risk","redzone.risk","bban.risk","blimit.risk","bspecific.risk")]
    kobedata <- purrr::map(1:length(kobedata),
                           function(x) kobedata[[x]] %>% select(-stat_name) %>%
                                         rename_all(tmpfunc_, header=names(kobedata)[x]))
    bind_cols(kobedata)
  }
  
  final_table1 <- arrange_table(kobe_data) %>% 
    select(id:catch.mean_2031,ssb.mean_2023, ssb.mean_2026, ssb.mean_2031, prob.over.ssbtarget_2031, ends_with("risk_value")) # 必要な列を取り出す
  final_table1 <- final_table1  %>%
    mutate(catch21=final_table1$catch.mean_2021,
           catch2223=apply(select(final_table1,catch.mean_2022:catch.mean_2023),1,mean), # 必要な漁獲量を平均する
           catch2430=apply(select(final_table1,catch.mean_2024:catch.mean_2030),1,mean),
           catch2131=apply(select(final_table1,catch.mean_2021:catch.mean_2030),1,mean),
           HCR_name=catch.mean_HCR_name,
           beta=catch.mean_beta) %>%
    select(-starts_with("catch.mean")) # 平均しおわったcatchを削除する
#  final_table1 <- bind_cols(filter(final_table1,id==1) %>% rename_all(tmpfunc_,header="M1"),
#                            filter(final_table1,id==2) %>% rename_all(tmpfunc_,header="M2")) 
  final_table1 <-final_table1 %>%
    select(M1_HCR_name, M1_beta,              
           ends_with("prob.over.ssbtarget_2031"),
           ends_with("catch21"),ends_with("catch2223"),ends_with("catch2430"),ends_with("catch2131"),
           ends_with("ssb.mean_2023"),ends_with("ssb.mean_2026"),ends_with("ssb.mean_2031"),
           ends_with("bban.risk_value"),ends_with("blimit.risk_value"),ends_with("catch.risk_value"),
           ends_with("redzone.risk_value"),ends_with("overfishing.risk_value")) %>%
    arrange(desc(M1_catch21))
  
  return(final_table1)
}
