setwd("C:/Users/gwill/Dropbox/Research/Dissertation/chapter2/replication_la2017")
library(haven)
library(dplyr)
library(tvcure)

la = read_dta("ISQ_FINAL_DATA.dta")
la = rename(la, "st" = "_st", "event" = "_d", "stop" = "_t", "start" = "_t0")
la = filter(la, type2 == 1)
la$lnbtpop = log(la$btpop)
la$mot = la$victimd + la$amnestyd + la$trial_griev
la$opp = la$trial_deter + la$exilefu + la$purged
la$lrgdpch = log((la$lrgdpch * 1000000))

lasubs = la %>% select(start, stop, event, mot, opp, victimd, amnestyd, trial_griev,
                       trial_deter, exilefu, purged, pko, victory, pagreement,
                       pws, lnbtpop, lndur, lrgdpch, lgdpgrowth, ldemdum2,
                       nstate5, cw, lnmilper, ethnic2)
lasubs2 = na.omit(lasubs)

################################################################################
# Kaplan-Meier Plot
################################################################################
km = ggsurvplot(
  survfit(Surv(start, stop, event) ~ 1,
          data = la),
  censor = F,
  legend = "none",
  ggtheme = theme_bw())
km = km$plot
ggsave("../figures/la2018_km.png", km)


events = la %>% group_by(start) %>% mutate(one = 1) %>% summarize(events = sum(event),
                                                                  remain = sum(one))
################################################################################
# Models
################################################################################
mc2 = coxph(Surv(start, stop, event) ~ mot + opp +
              pko + victory + pagreement + pws + lnbtpop + lndur +
              lrgdpch + lgdpgrowth + ldemdum2 + nstate5 +
              cw + lnmilper + ethnic2,
            data = la, x = T)
cox.zph(mc2)

cl <- makeCluster(4, "SOCK"); registerDoSNOW(cl)
set.seed(3894812)
m2 = tvcure(Surv(start, stop, event) ~ mot + opp +
              pws + pko +
              lgdpgrowth + lnmilper,
            cureform = ~  mot + opp +
              ldemdum2 + pws + nstate5 +
              lrgdpch + ethnic2 + cw +  lnbtpop +
              pko + victory + pagreement + lndur,
            data = la, brglm = T, var = T, nboot = 500)# link = "probit")
m2s = sch(m2)
plotsch(m2s, "mot", zeroline = F)
plotsch(m2s, "opp", zeroline = F)
plotsch(m2s, "ldemdum2", zeroline = F)
plotsch(m2s, "lgdpgrowth", zeroline = F)
plotsch(m2s, "lnmilper", zeroline = F)
plotsch(m2s, "nstate5", zeroline = F)

m_full = tvcure(Surv(start, stop, event) ~ mot + opp +
                  pws + pko +
                  lgdpgrowth + lnmilper +
                  ldemdum2 + nstate5 +
                  lrgdpch + ethnic2 + cw +  lnbtpop +
                  pko + victory + pagreement + lndur,
                cureform = ~ mot + opp +
                  pws + pko +
                  lgdpgrowth + lnmilper +
                  ldemdum2 + nstate5 +
                  lrgdpch + ethnic2 + cw +  lnbtpop +
                  pko + victory + pagreement + lndur,
                data = la, brglm = T, var = T, nboot = 500)

################################################################################
# Model fit
################################################################################
# AIC = -2(log-likelihood) + 2K
m2$loglik
-2 * (m2$loglik) + 2 * (length(m2$beta) + length(m2$gamma))
# BIC = kln(n) - 2loglik
(length(m2$beta) + length(m2$gamma)) * log(m2$nobs) - 2 * m2$loglik
AIC(mc2)

plot(survfit(Surv(start, stop, event) ~ 1, data = la))
lines(survfit(mc2), col = 2)
lines(m2$Survival[order(m2$Time)] ~ m2$Time[order(m2$Time)], col = 3)
-a = prediction4(m2, "basesurv", "mot", c(0, 1))

m1_pred = predict_tvcure_noci(m2)
survplot(m1_pred, type = "spop", "mot", c(0))


################################################################################
# Results table
################################################################################
varlist = list("Motivation Post-conflict Justice" = "mot",
               "Opportunity Post-conflict Justice" = "opp",
               "Reparations" = "victimd",
               "Amnesty" = "amnestyd",
               "Comprehensive Trials" = "trial_griev",
               "Opposition Trials" = "trial_deter",
               "Exile" = "exilefu",
               "Purges" = "purged",
               "Power Sharing" = "pws",
               "Peacekeeping" = "pko",
               "Military Personnel" = "lnmilper",
               "GDP Growth" = "lgdpgrowth",
               "ln GDP per Capita" = "lrgdpch",
               "Ethnic War" = "ethnic2",
               "Democracy" = "ldemdum2",
               "Conflict Duration" = "lndur",
               "ln Battle Deaths per Capita" = "log(btpop)",
               "Battle Deaths per Capita" = "lnbtpop",
               "Victory" = "victory",
               "Peace Agreement" = "pagreement",
               "Number of Rebel Groups" = "nstate5",
               "Post-Cold War" = "cw")

ltab = xtable(tvtable(mc2, m2, varlist = varlist, siglevel = c('*' = 0.05)))
print(ltab,
      booktabs = F,
      sanitize.text.function = identity,
      include.rownames = F,
      include.colnames = F)

################################################################################
# Motivation Plot
################################################################################

### cox plot *****************
# Predict survival
mot_nd = apply(mc2$x, 2, median)
mot_nd = as.data.frame(rbind(mot_nd, mot_nd, mot_nd, mot_nd))
mot_nd[, "mot"] = c(0, 1, 2, 3)


# Survival curve
mot_surv = survfit(mc2, newdata = mot_nd)
mot_time = mot_surv$time
mot_surv = as.data.frame(mot_surv$surv)


# Format for plotting
mot_mat = rbind(cbind(mot_surv[, 1], mot_time, 1),
                 cbind(mot_surv[, 4], mot_time, 4))
colnames(mot_mat) = c("surv", "time", "mot")
mot_mat = as.data.frame(mot_mat)
mot_mat$mot = as.factor(mot_mat$mot)


# Plot
mot_plot_cox = ggplot(mot_mat, mapping = aes(time,
                                               surv,
                                               col = mot,
                                               linetype = mot)) +
  geom_step(size = 1) +
  ylim(0, 1) +
  ggtitle("Cox Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c(0, 3), name = "Number of Motivation Post-conflict Justice Provisions") +
  scale_color_discrete(labels = c(0, 3), name = "Number of Motivation Post-conflict Justice Provisions")


### Cure Plot ***************
mot_ndx = apply(m2$X, 2, median)
mot_ndx = as.data.frame(rbind(mot_ndx, mot_ndx))
mot_ndx[1, "mot"] = 0
mot_ndx[2, "mot"] = 3

mot_ndz = apply(m2$Z, 2, median)
mot_ndz = as.data.frame(rbind(mot_ndz, mot_ndz))
mot_ndz[, '(Intercept)'] = 1.77

mot_plot_spop = prediction4(m2, "spop", newX = mot_ndx, newZ = mot_ndz, CI = T,
                             legendtitle = "Number of Motivation Post-conflict Justice Provisions",
                            xlab = "Time (years)") +
  ylim(0, 1) +
  ggtitle("Cure Model", "Population Survival Function") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c("0", "3")) +
  scale_color_discrete(labels = c("0", "3"))

mot_plot_suncure = prediction4(m2, "suncure", newX = mot_ndx, newZ = mot_ndz, CI = F,
                            legendtitle = "Number of Motivation Post-conflict Justice Provisions",
                            xlab = "Time (years)") +
  ggtitle("Cure Model", "Conditional Survival Function") +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c("0", "3")) +
  scale_color_discrete(labels = c("0", "3"))


### Combine plots ************
mot_plot_comb = ggarrange(mot_plot_cox, mot_plot_spop, common.legend = T)
ggsave("../figures/la2018_mot.png", mot_plot_comb, width = 6.5, height = 4, units = "in")

################################################################################
# Experiment
################################################################################
nmotz = apply(m2$Z, 2, median)
nmotz = rbind(nmotz, nmotz)

################################################################################
# Military personnel
################################################################################
# mil_breaks = c(seq(quantile(la$lnmilper, 0.05, na.rm = T), quantile(la$lnmilper, 0.95, na.rm = T), .5), 8.12)
mil_breaks = c(quantile(la$lnmilper, 0.05, na.rm = T),
               quantile(la$lnmilper, 0.95, na.rm = T))
mil_nd = apply(mc2$x, 2, median)
mil_nd = as.data.frame(rbind(mil_nd, mil_nd))
mil_nd[, "lnmilper"] = mil_breaks


# Survival curve
mil_surv = survfit(mc2, newdata = mil_nd)
mil_time = mil_surv$time
mil_surv = as.data.frame(mil_surv$surv)


# Format for plotting
mil_mat = rbind(cbind(mil_surv[, 1], mil_time, 1),
                cbind(mil_surv[, 2], mil_time, 2))
colnames(mil_mat) = c("surv", "time", "lnmilper")
mil_mat = as.data.frame(mil_mat)
mil_mat$lnmilper = as.factor(mil_mat$lnmilper)

# Plot
mil_cox_plot = ggplot(mil_mat, mapping = aes(time,
                                             surv,
                                             col = lnmilper,
                                             linetype = lnmilper)) +
  geom_step(size = 1) +
  ylim(0, 1) +
  ggtitle("Cox Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = 4.65, 8.12, name = "ln Military Personnel") +
  scale_color_discrete(labels = 4.65, 8.12, name = "ln Military Personnel")



##### Cure plot
mil_ndx = apply(m2$X, 2, median)
# mil_ndx = rbind(mil_ndx, mil_ndx, mil_ndx, mil_ndx,
                mil_ndx, mil_ndx, mil_ndx, mil_ndx)
mil_ndx = rbind(mil_ndx, mil_ndx)
mil_ndx[, "lnmilper"] = mil_breaks

mil_ndz = apply(m2$Z, 2, median)
mil_ndz = rbind(mil_ndz, mil_ndz)
mil_ndz[, "(Intercept)"] = 1.77

mil_pred = predict_tvcure_noci(m2, insamp = F, newX = mil_ndx, newZ = mil_ndz)
mil_cure_spop = survplot(mil_pred,
                         "spop") +
  ylim(0, 1) +
  theme_bw() +
  ggtitle("Cure Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c(4.65, 8.12), name = "ln Military Personnel") +
  scale_color_discrete(labels = c(4.65, 8.12), name = "ln Military Personnel")
mil_cure_spop = survplot(mil_pred,
                         "suncure") +
  ylim(0, 1) +
  theme_bw() +
  ggtitle("Cure Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = c(4.65, 8.12), name = "ln Military Personnel") +
  scale_color_discrete(labels = c(4.65, 8.12), name = "ln Military Personnel")

##### Combine
mil_plot_comb = ggarrange(mil_cox_plot, mil_cure_spop, common.legend = T)
ggsave("../figures/la2018_milper.png", mil_plot_comb, width = 6.5, height = 4, units = "in")

################################################################################
# GDP Plot
################################################################################

gdp_breaks = seq(quantile(la$lrgdpch, 0.05, na.rm = T),
                 quantile(la$lrgdpch, 0.95, na.rm = T), .5)
gdp_labs = round(gdp_breaks, 1)
gdp_ndx = apply(m2$X, 2, median)
gdp_ndx = as.data.frame(rbind(gdp_ndx, gdp_ndx, gdp_ndx, gdp_ndx,
                              gdp_ndx, gdp_ndx, gdp_ndx))

gdp_ndz = apply(m2$Z, 2, median)
gdp_ndz = rbind(gdp_ndz, gdp_ndz, gdp_ndz, gdp_ndz,
                gdp_ndz, gdp_ndz, gdp_ndz)
gdp_ndz[, "(Intercept)"] = 1.77
gdp_ndz[, "lrgdpch"] = gdp_breaks

gdp_prob_pred = predict_tvcure_noci(m2, insamp = F, newX = gdp_ndx, newZ = gdp_ndz)
gdp_prob_plot = plotprob(gdp_prob_pred,
                         plottype = "line",
                         variable = "lrgdpch",
                         xlab = "ln GDP per Capita") +
  theme_bw() +
  scale_x_continuous(breaks = gdp_breaks, labels = gdp_labs)
ggsave("../figures/la2018_gdp.png", gdp_prob_plot, width = 4, height = 3, units = "in")


#################################################################################
# Peacekeeping
#################################################################################

### cox plot *****************
# Predict survival
pko_nd = apply(mc2$x, 2, median)
pko_breaks = c(0, 1)
pko_nd = as.data.frame(rbind(pko_nd, pko_nd))
pko_nd[, "pko"] = pko_breaks

# Survival curve
pko_surv = survfit(mc2, newdata = pko_nd)
pko_time = pko_surv$time
pko_surv = as.data.frame(pko_surv$surv)

# Format for plotting
pko_mat = rbind(cbind(pko_surv[, 1], pko_time, 1),
              cbind(pko_surv[, 2], pko_time, 2))
colnames(pko_mat) = c("surv", "time", "pko")
pko_mat = as.data.frame(pko_mat)
pko_mat$pko = as.factor(pko_mat$pko)

# Plot
pko_plot_cox = ggplot(pko_mat, mapping = aes(time,
                                            surv,
                                            col = pko,
                                            linetype = pko)) +
  ylim(0, 1) +
  geom_step(size = 1) +
  ggtitle("Cox Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = round(pko_breaks, 1), name = "Peacekeeping") +
  scale_color_discrete(labels = round(pko_breaks, 1), name = "Peacekeeping")


ggsave("../figures/la2018_pko.png", pko_plot_cox, width = 4, height = 3, units = "in")

#################################################################################
# Victory plot
#################################################################################
### cox plot *****************
# Predict survival
vic_breaks = c(0, 1)
vic_nd = apply(mc2$x, 2, median)
vic_nd = as.data.frame(rbind(vic_nd, vic_nd))
vic_nd[, "victory"] = vic_breaks

# Survival curve
vic_surv = survfit(mc2, newdata = vic_nd)
vic_time = vic_surv$time
vic_surv = as.data.frame(vic_surv$surv)

# Format for plotting
vicmat = rbind(cbind(vic_surv[, 1], vic_time, 1),
              cbind(vic_surv[, 2], vic_time, 2))
colnames(vicmat) = c("surv", "time", "pko")
vicmat = as.data.frame(vicmat)
vicmat$pko = as.factor(vicmat$pko)

# Plot
vic_cox_plot = ggplot(vicmat, mapping = aes(time,
                                             surv,
                                             col = pko,
                                             linetype = pko)) +
  ylim(0, 1) +
  geom_step(size = 1) +
  ggtitle("Cox Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = round(vic_breaks, 1), name = "Victory") +
  scale_color_discrete(labels = round(vic_breaks, 1), name = "Victory") #+
  #theme(aspect.ratio = 1)

ggsave("../figures/la2018_victory.png", vic_cox_plot, width = 4, height = 3, units = "in")

### Cure prob plot ***************
vic_ndx = apply(m2$X, 2, median)
vic_ndx = as.data.frame(rbind(vic_ndx, vic_ndx))

vic_ndz = apply(m2$Z, 2, median)
vic_ndz = rbind(vic_ndz, vic_ndz)
vic_ndz[, "(Intercept)"] = 1.77
vic_ndz[, "victory"] = vic_breaks

vic_prob_pred = predict_tvcure_noci(m2, insamp = F, newX = vic_ndx, newZ = vic_ndz)

#################################################################################
# Number of rebel groups
#################################################################################

# Predict survival
reb_breaks = c(1, 2, 3, 4)
reb_nd = apply(mc2$x, 2, median)
reb_nd = as.data.frame(rbind(reb_nd, reb_nd, reb_nd, reb_nd))
reb_nd[, "nstate5"] = reb_breaks

# Survival curve
reb_surv = survfit(mc2, newdata = reb_nd)
reb_time = reb_surv$time
reb_surv = as.data.frame(reb_surv$surv)

# Format for plotting
rebmat = rbind(cbind(reb_surv[, 1], reb_time, 1),
               cbind(reb_surv[, 2], reb_time, 2),
               cbind(reb_surv[, 3], reb_time, 3),
               cbind(reb_surv[, 4], reb_time, 4))
colnames(rebmat) = c("surv", "time", "nstate5")
rebmat = as.data.frame(rebmat)
rebmat$nstate5 = as.factor(rebmat$nstate5)

# Plot
reb_cox_plot = ggplot(rebmat, mapping = aes(time,
                                            surv,
                                            col = nstate5,
                                            linetype = nstate5)) +
  ylim(0, 1) +
  geom_step(size = 1) +
  ggtitle("Cox Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = round(reb_breaks, 1), name = "Number of Rebel Groups") +
  scale_color_discrete(labels = round(reb_breaks, 1), name = "Number of Rebel Groups") #+
  #theme(aspect.ratio = 1)
ggsave("../figures/la2018_rebels.png", reb_cox_plot, width = 5, height = 3.5, units = "in")

### Cure prob ***************
reb_ndx = apply(m2$X, 2, median)
reb_ndx = as.data.frame(rbind(reb_ndx, reb_ndx, reb_ndx, reb_ndx, reb_ndx))

reb_ndz = apply(m2$Z, 2, median)
reb_ndz = rbind(reb_ndz, reb_ndz, reb_ndz, reb_ndz, reb_ndz)
reb_ndz[, "(Intercept)"] = 1.77
reb_ndz[, "nstate5"] = reb_breaks

reb_prob_pred = predict_tvcure_noci(m2, insamp = F, newX = reb_ndx, newZ = reb_ndz)


#################################################################################
# Cold War plot
#################################################################################
### cox plot *****************
# Predict survival
cw_breaks = c(0, 1)
cw_nd = apply(mc2$x, 2, median)
cw_nd = as.data.frame(rbind(cw_nd, cw_nd))
cw_nd[, "cw"] = cw_breaks

# Survival curve
cw_surv = survfit(mc2, newdata = cw_nd)
cw_time = cw_surv$time
cw_surv = as.data.frame(cw_surv$surv)

# Format for plotting
cwmat = rbind(cbind(cw_surv[, 1], cw_time, 1),
               cbind(cw_surv[, 2], cw_time, 2))
colnames(cwmat) = c("surv", "time", "cw")
cwmat = as.data.frame(cwmat)
cwmat$cw = as.factor(cwmat$cw)

# Plot
cw_cox_plot = ggplot(cwmat, mapping = aes(time,
                                            surv,
                                            col = cw,
                                            linetype = cw)) +
  ylim(0, 1) +
  geom_step(size = 1) +
  ggtitle("Cox Model", "Population Survival Function") +
  xlab("Time (years)") +
  ylab("Predicted Survival Probability") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  scale_linetype(labels = round(cw_breaks, 1), name = "Post-Cold War") +
  scale_color_discrete(labels = round(cw_breaks, 1), name = "Post-Cold War")
ggsave("../figures/la2018_cw.png", cw_cox_plot, width = 4, height = 3, units = "in")

### Cure prob plot ***************
cw_ndx = apply(m2$X, 2, median)
cw_ndx = as.data.frame(rbind(cw_ndx, cw_ndx))

cw_ndz = apply(m2$Z, 2, median)
cw_ndz = rbind(cw_ndz, cw_ndz)
cw_ndz[, "(Intercept)"] = 1.77
cw_ndz[, "cw"] = cw_breaks

cw_prob_pred = predict_tvcure_noci(m2, insamp = F, newX = cw_ndx, newZ = cw_ndz)


#################################################################################
# Descriptive Stats Table
#################################################################################

stargazer(as.data.frame(select(la, stop, event, mot, opp, pws, lnmilper, lgdpgrowth, lrgdpch, ethnic2, ldemdum2, lndur, lnbtpop, victory, pagreement, nstate5, pko, cw)),
          title = "Descriptive Statistics for Replication Analysis",
          label = "tab_desc",
          style = "ajps",
          digits = 2,
          covariate.labels = c("Duration", "Civil war Recurrence",
                               'Motivation-Decreasing PCJ', 'Opportunity-Decreasing PCJ', 'Power Sharing', 'Military Personnel', 'GDP/Capita Growth', 'GDP/Capita', 'Ethnic Conflict', 'Democracy', 'Conflict Duration', 'Battle Deaths/Capita', 'Victory', 'Peace Agreement', 'Number of Rebel Groups', 'Peacekeeping Operations', 'Post-Cold War'))
