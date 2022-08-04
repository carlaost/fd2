#SETUP ####
library(readr)
library(ggplot2)
library(car)
library(psych)
library(MASS)
library(sfsmisc)
library(broom)
library(interactions)
library(jtools)
library(reghelper)
library(robustbase)

filepath = 'Downloads/funding_dominance_full_final.csv'

df <- read_csv(filepath)
View(df)
str(df)

df$smile_dom = df$smile
df$smile_dom[df$smile=='dominant'] = 1
df$smile_dom[df$smile!='dominant'] = 0

df$smile_dom = as.factor(df$smile_dom)
df$smile = as.factor(df$smile)
str(df)

#DESCRIPTIVE####
df2 = df[df$tweet_count > 11,]
View(df2)

ggplot(df2, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth()

boxplot(deal_size~smile, data=df2)
boxplot(deal_size~smile_dom, data=df2)

quantile(df2$deal_size, probs = seq(0, 1, by= 0.1), na.rm=TRUE)

ggplot(df2, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth() +
  ylim(0,263)

summary(df2)
sd(df2$ebk_d, na.rm = TRUE)
sd(df2$network, na.rm = TRUE)

df2_num = data.frame(df2$deal_size, 
                    df2$ebk_v, df2$ebk_a, df2$ebk_d,
                    df2$network)
cor(df2_num, use='complete.obs')

corr.test(df2_num)

overall = data.frame(df$deal_size, df$tweet_count, df$network)
View(overall)
cor(overall, use='complete.obs')

.
#REGRESSION FULL DATA####
reg_full = lm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom*network, data=df2)
summary(reg_full)
confint(reg_full)

reg_verb = lm(deal_size ~ ebk_d + ebk_d*network, data=df2, na.action=na.omit)
summary(reg_verb)
confint(reg_verb)

reg_face = lm(deal_size ~ smile_dom + smile_dom*network, data=df2)
summary(reg_face)
confint(reg_face)

#ASSUMPTION TESTING####
dwt(reg_full)
vif(reg_full)
plot(reg_full)
#REGRESSION TRIMMED DATA####
q1 = quantile(df$deal_size, .25, na.rm=TRUE)
q3 = quantile(df$deal_size, .75, na.rm=TRUE)
iqr = IQR(df$deal_size, na.rm=TRUE)

trim = subset(df, df$deal_size > (q1-1.5*iqr) & df$deal_size < (q3+1.5*iqr))
boxplot(deal_size~smile_dom, data=trim)
ggplot(trim, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth()

reg_full2 = lm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom*network, data=trim)
summary(reg_full2)
confint(reg_full2)

reg_verb2 = lm(deal_size ~ ebk_d + ebk_d*network, data=trim, na.action=na.omit)
summary(reg_verb2)
confint(reg_verb2)

reg_face2 = lm(deal_size ~ smile_dom + smile_dom*network, data=trim)
summary(reg_face2)
confint(reg_face2)

pct = quantile(df$deal_size, c(.02, .98), type=1, na.rm=TRUE)
win = df
win$deal_size[df$deal_size <= pct[1]] = pct[1]
win$deal_size[df$deal_size >= pct[2]] = pct[2]
boxplot(deal_size~smile_dom, data=win)
ggplot(win, aes(ebk_d, deal_size)) +
  geom_point() +
  geom_smooth()

reg_full3 = lm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom*network, data=win)
summary(reg_full3)
confint(reg_full3)

reg_verb3 = lm(deal_size ~ ebk_d + ebk_d*network, data=win, na.action=na.omit)
summary(reg_verb3)
confint(reg_verb3)

reg_face3 = lm(deal_size ~ smile_dom + smile_dom*network, data=win)
summary(reg_face3)
confint(reg_face3)


#ROBUST REGRESSION FULL DATA####

#Huber Robust Regression on full model
rr_full = rlm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom:network, data=df, na.action=na.omit)
summary(rr_full)
tidy(rr_full, conf.int=TRUE)
#Wald tests (robust F-tests) to determine predictor significance
f.robftest(rr_full, var = 'ebk_d') #p=0.01413
f.robftest(rr_full, var = 'smile_dom1') #p=0.2161
f.robftest(rr_full, var = 'network') #p=0.008536
f.robftest(rr_full, var = 'ebk_d:network') #p=0.008139
f.robftest(rr_full, var = 'smile_dom1:network') #p=0.2035
#observations
nobs(rr_full)
df_rr = rr_full$model
View(df_rr)
summary(df_rr)
#separate models
rr_verb = rlm(deal_size ~ ebk_d + ebk_d*network, data=df, na.action=na.omit)
summary(rr_verb)
f.robftest(rr_verb, var = 'ebk_d')

rr_face = rlm(deal_size ~ smile_dom + smile_dom*network, data=df, na.action=na.omit)
summary(rr_face)
f.robftest(rr_face, var = 'smile_dom1') 

#ROBUST REGRESSION CRIT2 APPLIED####
#Huber Robust Regression on full model, Criterion 2 applied (min. 12 tweets)
rr_full2 = rlm(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom:network, data=df2, na.action=na.omit)
summary(rr_full2)

f.robftest(rr_full2, var = 'ebk_d') #p=0.009244 **
f.robftest(rr_full2, var = 'smile_dom1') #p=0.3985
f.robftest(rr_full2, var = 'network') #p=0.04376 *
f.robftest(rr_full2, var = 'ebk_d:network') #p=0.0426 *
f.robftest(rr_full2, var = 'smile_dom1:network') #p=0.3039

tidy(rr_full2, conf.int=TRUE)
nobs(rr_full2) #n=720
df2_rr = rr_full2$model
View(df2_rr)
summary(df2_rr)
quantile(df2_rr$deal_size, probs = seq(0, 1, by= 0.1), na.rm=TRUE)
sd(df2_rr$ebk_d, na.rm = TRUE)
quantile(df2_rr$network, probs = seq(0, 1, by= 0.1), na.rm=TRUE)
sd(df2_rr$network, na.rm = TRUE)

#Interaction 
johnson_neyman(model=rr_full2, pred=ebk_d, modx=network)

johnson_neyman(model = reg_full, pred = ebk_d, modx = network,
               mod.range=c(23350,23400))

interact_plot(model=rr_full2, pred=ebk_d, modx=network)

simple_slopes(rr_full2)

#LMROB####
lmr = lmrob(deal_size ~ ebk_d + smile_dom + ebk_d*network + smile_dom:network, data=df2)
summary(lmr)
johnson_neyman(model=lmr, pred=ebk_d, modx=network,
               mod.range=(c(0,20000)))
johnson_neyman(model=lmr, pred=ebk_d, modx=network,
               mod.range=(c(0,20000)), control.fdr = TRUE)
df_lmr = lmr$model
summary(df_lmr)
sd(df_lmr$ebk_d, na.rm = TRUE)
sd(df_lmr$network, na.rm = TRUE)
sd(df_lmr$deal_size, na.rm = TRUE)
c = data.frame(df_lmr$deal_size, df_lmr$ebk_d, df_lmr$network)
cor(c)
corr.test(c)
nobs(lmr)
confint(lmr)



dwt(lmr)
vif(lmr)
plot(lmr)

#summary(lmr): R2 = 0.09451; Adj. R2 = 0.08817

R2 = 0.09451
f2 = R2 / (1 - R2)
f2

R2a = 0.08817
f2a= R2a / (1-R2a)
f2a
