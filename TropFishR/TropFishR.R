install.packages("remotes")
remotes::install_github("tokami/TropFishR")
install.packages("TropFishR")

library(TropFishR)
packageVersion("TropFishR")

library(openxlsx)
setwd("D:/R PRACTICE/FishR/TropFishR")

set.seed(1)
data <- read.xlsx("lengthcatch.xlsx")


dates <- as.Date(paste0("15-",01:08,"-2024"),format="%d-%m-%Y")

lfq <- list(dates = dates,midLengths = data$Lengthclass,catch = as.matrix(data[,2:ncol(data)]))
lfq
class(lfq) <- "lfq"

lfq_bin2 <- lfqModify(lfq, bin_size = 1)      
ma <- 7                       ## ma=moving average
lfq_bin2_res <- lfqRestructure(lfq_bin2, MA = ma, addl.sqrt = FALSE)
lfq_bin2_res
opar <- par(mfrow = c(1,1), mar = c(2,5,2,3), oma = c(2,0,0,0))
plot(lfq_bin2_res, Fname = "catch", date.axis = "modern")
plot(lfq_bin2_res, Fname = "rcounts", date.axis = "modern")
par(opar)


linf_guess <- max(lfq_bin2$midLengths) / 0.95

## lower search space bounds
low_par <- list(Linf = 0.8 * linf_guess,K = 0.01,t_anchor = 0,C = 0,ts = 0)

## upper search space bounds
up_par <- list(Linf = 1.2 * linf_guess,K = 2,t_anchor = 1,C = 1,ts = 1)



res_GA <- ELEFAN_GA(lfq_bin2, MA = ma, seasonalised = F,
                    maxiter = 1000, addl.sqrt = FALSE,
                    low_par = low_par,
                    up_par = up_par,
                    monitor = TRUE)




res_GA
par<-res_GA$par

par
res_GA$ncohort

res_GA$Rn_max


plot(lfq_bin2_res, Fname = "rcounts",date.axis = "modern", ylim=c(6,20))
par <-list(Linf=par$Linf, K=par$K, t_anchor= par$t_anchor, C=0, ts=0)

lt <- lfqFitCurves(lfq_bin2, par = par,
                   draw = TRUE, col = "grey", lty = 1, lwd=1.5)

lt <- lfqFitCurves(lfq_bin2, par = res_GA$par,
                   draw = TRUE, col = "darkgreen", lty = 1, lwd=1.5)

lfq_bin2 <- lfqModify(lfq_bin2, par = res_GA$par)


Ms <- M_empirical(Linf = 19.19, K_l = 0.94, method = "Then_growth")
Ms

lfq_bin2$par$M <- as.numeric(Ms)
Ms
plus_group <- lfq_bin2$midLengths[max(which(lfq_bin2$midLengths < lfq_bin2$par$Linf))]
lfq_catch_vec <- lfqModify(lfq_bin2, vectorise_catch = TRUE, plus_group = plus_group)

plot(catchCurve(lfq_catch_vec))

res_cc <- catchCurve(lfq_catch_vec, reg_int = c(7,12), calc_ogive = TRUE)
res_cc
lfq_catch_vec$par$Z <- res_cc$Z
lfq_catch_vec$par$Z 
lfq_catch_vec$par$FM <- as.numeric(lfq_catch_vec$par$Z - lfq_catch_vec$par$M)
lfq_catch_vec$par$FM 
lfq_catch_vec$par$E <- lfq_catch_vec$par$FM / lfq_catch_vec$par$Z
lfq_catch_vec$par$E
lfq_catch_vec$par$a <- 0.037
lfq_catch_vec$par$b <- 2.21


selectivity_list <- list(selecType = "trawl_ogive",L50 = res_cc$L50, L75 = res_cc$L75)
Lc<-res_cc$L50
Lc
