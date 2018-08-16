rm(list=ls())

library(reshape2); library(tidyr); library(matrixStats);
source("bhc_functions.R")

# Load Data ---------------------------------------------------------------

hist.data.fn = "historical_data.csv"
hist.df = read.csv(hist.data.fn, stringsAsFactors = FALSE, na.strings = c("","NA"))
hist.df[is.na(hist.df)] = 0
hist.df = hist.df[which(hist.df$Regulatory.Filer.Type == "Y9SP" | hist.df$Regulatory.Filer.Type == "Y9C" | hist.df$Regulatory.Filer.Type == "Y9LP"),]
row.names(hist.df) = NULL

# Formulas ----------------------------------------------------------------

# Small BHC Formulas

# # Bank Subs Operating Income / Total HC Operating Income
# hist.df$sp.bk_subs_op_inc_pct = (hist.df$BHSP0508 + hist.df$BHSP2111) / hist.df$BHSP4000
# hist.df$sp.bk_subs_op_inc_pct[which(is.nan(hist.df$sp.bk_subs_op_inc_pct))] = 0
# hist.df$sp.bk_subs_op_inc_pct[which(is.infinite(hist.df$sp.bk_subs_op_inc_pct))] = 0
# 
# # Nonbank Subs Operating Income / Total HC Operating Income
# hist.df$sp.nbk_subs_op_inc_pct = (hist.df$BHSP0523 + hist.df$BHSP0530) / hist.df$BHSP4000
# hist.df$sp.nbk_subs_op_inc_pct[which(is.nan(hist.df$sp.nbk_subs_op_inc_pct))] = 0
# hist.df$sp.nbk_subs_op_inc_pct[which(is.infinite(hist.df$sp.nbk_subs_op_inc_pct))] = 0
# 
# # HC Subs Operating Income / Total HC Operating Income
# hist.df$sp.hc_subs_op_inc_pct = (hist.df$BHSP0206 + hist.df$BHSP1283) / hist.df$BHSP4000
# hist.df$sp.hc_subs_op_inc_pct[which(is.nan(hist.df$sp.hc_subs_op_inc_pct))] = 0
# hist.df$sp.hc_subs_op_inc_pct[which(is.infinite(hist.df$sp.hc_subs_op_inc_pct))] = 0

# Debt Coverage Ratio
hist.df$sp.dscr = (hist.df$BHSP4000 - hist.df$BHSP4093) / hist.df$BHSP4073
hist.df$sp.dscr[which(is.nan(hist.df$sp.dscr))] = 0
hist.df$sp.dscr[which(is.infinite(hist.df$sp.dscr))] = 0

# # Total Bank Sub Income / Total HC Interest Expense
# hist.df$sp.bk_sub_inc_dscr = (hist.df$BHSP0508 + hist.df$BHSP2111 + hist.df$BHSP3156) / hist.df$BHSP4073
# hist.df$sp.bk_sub_inc_dscr[which(is.nan(hist.df$sp.bk_sub_inc_dscr))] = 0
# hist.df$sp.bk_sub_inc_dscr[which(is.infinite(hist.df$sp.bk_sub_inc_dscr))] = 0

# HC Debt / Equity
hist.df$sp.dbt_eq = (hist.df$BHSP2309 + hist.df$BHSP2724 + hist.df$BHSP3151) / hist.df$BHSP3210
hist.df$sp.dbt_eq[which(is.nan(hist.df$sp.dbt_eq))] = 0
hist.df$sp.dbt_eq[which(is.infinite(hist.df$sp.dbt_eq))] = 0

# HC Cash & Securities / Total Assets
hist.df$sp.csh_sec = (hist.df$BHSP0010 + hist.df$BHSP5993 + hist.df$BHSP0390) / (hist.df$BHSP2170)
hist.df$sp.csh_sec[which(is.nan(hist.df$sp.csh_sec))] = 0
hist.df$sp.csh_sec[which(is.infinite(hist.df$sp.csh_sec))] = 0

# Double Leverage Ratio
hist.df$sp.dbl_lev = (hist.df$BHSP3239 + hist.df$BHSP0088 + hist.df$BHSP0201) / hist.df$BHSP3210
hist.df$sp.dbl_lev[which(is.nan(hist.df$sp.dbl_lev))] = 0
hist.df$sp.dbl_lev[which(is.infinite(hist.df$sp.dbl_lev))] = 0

# Large BHC Formulas

# # Bank Subs Operating Income / Total HC Operating Income
# hist.df$lp.bk_subs_op_inc_pct = hist.df$BHCP0520 / hist.df$BHCP4000
# hist.df$lp.bk_subs_op_inc_pct[which(is.nan(hist.df$lp.bk_subs_op_inc_pct))] = 0
# hist.df$lp.bk_subs_op_inc_pct[which(is.infinite(hist.df$lp.bk_subs_op_inc_pct))] = 0
# 
# # Nonbank Subs Operating Income / Total HC Operating Income
# hist.df$lp.nbk_subs_op_inc_pct = hist.df$BHCP1279 / hist.df$BHCP4000
# hist.df$lp.nbk_subs_op_inc_pct[which(is.nan(hist.df$lp.nbk_subs_op_inc_pct))] = 0
# hist.df$lp.nbk_subs_op_inc_pct[which(is.infinite(hist.df$lp.nbk_subs_op_inc_pct))] = 0
# 
# # HC Subs Operating Income / Total HC Operating Income
# hist.df$lp.hc_subs_op_inc_pct = hist.df$BHCP0210 / hist.df$BHCP4000
# hist.df$lp.hc_subs_op_inc_pct[which(is.nan(hist.df$lp.hc_subs_op_inc_pct))] = 0
# hist.df$lp.hc_subs_op_inc_pct[which(is.infinite(hist.df$lp.hc_subs_op_inc_pct))] = 0

# Debt Coverage Ratio
hist.df$lp.dscr = (hist.df$BHCP4000 - hist.df$BHCP4130 + hist.df$BHCP4073) / hist.df$BHCP4073
hist.df$lp.dscr[which(is.nan(hist.df$lp.dscr))] = 0
hist.df$lp.dscr[which(is.infinite(hist.df$lp.dscr))] = 0

# # Total Bank Sub Income / Total HC Interest Expense
# hist.df$lp.bk_sub_inc_dscr = (hist.df$BHCP0520 + hist.df$BHCP3156) / hist.df$BHCP4073
# hist.df$lp.bk_sub_inc_dscr[which(is.nan(hist.df$lp.bk_sub_inc_dscr))] = 0
# hist.df$lp.bk_sub_inc_dscr[which(is.infinite(hist.df$lp.bk_sub_inc_dscr))] = 0

# HC Debt / Equity
hist.df$lp.dbt_eq = (hist.df$BHCP2332 + hist.df$BHCP0368 + hist.df$BHCP4062) / hist.df$BHCP3210
hist.df$lp.dbt_eq[which(is.nan(hist.df$lp.dbt_eq))] = 0
hist.df$lp.dbt_eq[which(is.infinite(hist.df$lp.dbt_eq))] = 0

# HC Cash & Securities / Total HC Assets
hist.df$lp.csh_sec = (hist.df$BHCP0010 + hist.df$BHCP5993 + hist.df$BHCP0400 + hist.df$BHCP6791 + hist.df$BHCP1299) / hist.df$BHCP2170
hist.df$lp.csh_sec[which(is.nan(hist.df$lp.csh_sec))] = 0
hist.df$lp.csh_sec[which(is.infinite(hist.df$lp.csh_sec))] = 0

# Double Leverage Ratio
hist.df$lp.dbl_lev = (hist.df$BHCP3239 + hist.df$BHCP1273 + hist.df$BHCP0201) / hist.df$BHCP3210
hist.df$lp.dbl_lev[which(is.nan(hist.df$lp.dbl_lev))] = 0
hist.df$lp.dbl_lev[which(is.infinite(hist.df$lp.dbl_lev))] = 0

# Combined Formulas

co.form = as.data.frame(as.matrix(hist.df[,96:103]) + as.matrix(hist.df[,104:111]))
names(co.form) = c("bk_subs_op_inc_pct","nbk_subs_op_inc_pct","hc_subs_op_inc_pct","dscr","bk_sub_inc_dscr","dbt_eq","csh_sec_dbt","dbl_lev")
hist.df = cbind(hist.df,co.form)


# Data Manipulation -------------------------------------------------------

# translate data to tall structure
# hist.df.long = hist.df[,c(1:7,112:119)]
# hist.df.long = gather(hist.df.long, metric, value, bk_subs_op_inc_pct:dbl_lev, factor_key = TRUE)

dscr = spread(hist.df[,c("Period","Company.Name","SNL.Institution.Key","dscr")], Period, dscr)
dscr[dscr == 0] = NA
debt_equity = spread(hist.df[,c("Period","Company.Name","SNL.Institution.Key","dbt_eq")], Period, dbt_eq)
debt_equity[debt_equity == 0] = NA
dbl_lvg = spread(hist.df[,c("Period","Company.Name","SNL.Institution.Key","dbl_lev")], Period, dbl_lev)

# hist.df[which(hist.df$nbk_subs_op_inc_pct>1),c(1,2,3,7,113)]

dscr_pct = as.data.frame.matrix(colQuantiles(as.matrix(dscr[3:17]), probs = c(10,25,50,75,90)/100,na.rm = TRUE))
dscr_pct$year = row.names(dscr_pct)
row.names(dscr_pct$year) = NULL

debt_equity_pct = as.data.frame.matrix(colQuantiles(as.matrix(debt_equity[3:17]), probs = c(10,25,50,75,90)/100,na.rm = TRUE))
debt_equity_pct$year = row.names(debt_equity_pct)
row.names(debt_equity_pct$year) = NULL

dbl_lvg_pct = as.data.frame.matrix(colQuantiles(as.matrix(dbl_lvg[3:17]), probs = c(10,25,50,75,90)/100,na.rm = TRUE))
dbl_lvg_pct$year = row.names(dbl_lvg_pct)
row.names(dbl_lvg_pct$year) = NULL



# Scoring Matrix ----------------------------------------------------------

# Names for the different labels
class.all = c("very low","low","medium","elevated","high")

# Double leverage
dbl_lev_levels = c(-1000,0.90,1.00,1.20,1.50)
dbl_lev_score = c(-12,0,6,18,36)
dbl_lev_sm = data.frame(levels = dbl_lev_levels, score = dbl_lev_score, class = class.all)
dbl_lev_matrix = do.call(rbind, lapply(hist.df[,"dbl_lev"], function(x) score.low(x, dbl_lev_sm)))
names(dbl_lev_matrix) = c("dbl_lev_score","dbl_lev_class")

# Debt / Equity
dbt_eq_levels = c(-1000,0.05,0.15,0.25,0.35)
dbt_eq_score = c(0,6,12,18,36)
dbt_eq_sm = data.frame(levels = dbt_eq_levels, score = dbt_eq_score, class = class.all)
dbt_eq_matrix = do.call(rbind, lapply(hist.df[,"dbt_eq"], function(x) score.low(x, dbt_eq_sm)))
names(dbt_eq_matrix) = c("dbt_eq_score","dbt_eq_class")

# DSCR
dscr_levels = c(1000000,30,15,5,2.5)
dscr_score = c(0,0,0,6,12)
dscr_sm = data.frame(levels = dscr_levels, score = dscr_score, class = class.all)
dscr_matrix = do.call(rbind, lapply(hist.df[,"dscr"], function(x) score.high(x, dscr_sm)))
names(dscr_matrix) = c("dscr_score","dscr_class")

hist.df.scores = cbind(hist.df, dbl_lev_matrix, dbt_eq_matrix, dscr_matrix, stringsAsFactors = FALSE)

# Graphs ------------------------------------------------------------------

dev.off()
boxplot(dscr[,3:17], ylim = c(-100,200))
dev.off()
boxplot(dbl_lvg[,3:17], ylim = c(-0.25,2))
dev.off()
boxplot(debt_equity[,3:17], ylim = c(-0.25,2))

dev.off()
plot(dbl_lvg_pct$`10%`, type="o", col="blue", axes = FALSE, ann = FALSE, ylim = c(0.85,1.5))
axis(1, at=1:15, labels = dbl_lvg_pct$year, las = 2)
axis(2, las=1)
lines(dbl_lvg_pct$`25%`, type="o", col="red")
lines(dbl_lvg_pct$`50%`, type="o", col="green")
lines(dbl_lvg_pct$`75%`, type="o", col="grey")
lines(dbl_lvg_pct$`90%`, type="o", col="purple")
title(main = "Double Leverage Percentiles Over Time")
legend(0.5,1.5, names(dbl_lvg_pct)[1:5], cex = 0.8, col = c("blue","red","green","grey","purple"), pch = 1, lty = 1)

dev.off()
plot(debt_equity_pct$`10%`, type="o", col="blue", axes = FALSE, ann = FALSE, ylim = c(-0.10,0.75))
axis(1, at=1:15, labels = debt_equity_pct$year, las = 2)
axis(2, las=1)
lines(debt_equity_pct$`25%`, type="o", col="red")
lines(debt_equity_pct$`50%`, type="o", col="green")
lines(debt_equity_pct$`75%`, type="o", col="grey")
lines(debt_equity_pct$`90%`, type="o", col="purple")
title(main = "Debt / Equity Percentiles Over Time")
legend(0.5,0.75, names(debt_equity_pct)[1:5], cex = 0.8, col = c("blue","red","green","grey","purple"), pch = 1, lty = 1)

dev.off()
plot(dscr_pct$`10%`, type="o", col="blue", axes = FALSE, ann = FALSE, ylim = c(-25,100))
axis(1, at=1:15, labels = dscr_pct$year, las = 2)
axis(2, las=1)
lines(dscr_pct$`25%`, type="o", col="red")
lines(dscr_pct$`50%`, type="o", col="green")
lines(dscr_pct$`75%`, type="o", col="grey")
lines(dscr_pct$`90%`, type="o", col="purple")
title(main = "Debt Service Coverage Ratio")
legend(0.5,100, names(dscr_pct)[1:5], cex = 0.8, col = c("blue","red","green","grey","purple"), pch = 1, lty = 1)
