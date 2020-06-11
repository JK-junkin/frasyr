context("test for graphic functions (level 0)")

test_that("plot_futures",{
  g1 <- plot_futures(vpares=res_vpa,future.list=list(res_future_0.8HCR,res_future_current))
  g2 <- plot_futures(vpares=NULL,future.list=list(res_future_0.8HCR,res_future_current))
  expect_equal(class(g1)[1],"gg")
  expect_equal(class(g2)[1],"gg")    
})

test_that("plot_vpa",{
  g1 <- plot_vpa(res_vpa)
  g2 <- plot_vpa(list(res_vpa,res_vpa))
  expect_equal(class(g1)[1],"gg")
  expect_equal(class(g2)[1],"gg")    
})

test_that("compare_SRfit",{
  g1 <- compare_SRfit(list(res_sr_HSL1,res_sr_HSL2))
  expect_equal(class(g1)[1],"gg")
})

test_that("SRregime_plot",{
  data(res_vpa)
  SRdata <- get.SRdata(res_vpa)
  resSRregime <- fit.SRregime(SRdata, SR="HS", method="L2",
                              regime.year=c(1994,2003), regime.key=c(0,1,0),
                              regime.par = c("a","b","sd")[2:3])
  g1 <- SRregime_plot(resSRregime, regime.name=c("Low","High"))
  # 本当の意味でのテストにはなっていない
  expect_equal(class(g1)[1],"gg")  
})