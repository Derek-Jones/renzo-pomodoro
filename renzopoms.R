#
# renzopoms.csv, 11 Dec 19
# Data from:
# Renzo Borgatti
# renzo.borgatti@droit.tech
#
# Example from:
# Evidence-based Software Engineering: based on the publicly available data
# Derek M. Jones
#
# TAG

source("ESEUR_config.r")


library("forecast")
library("gamlss")
library("gamlss.tr")
library("plyr")


pal_col=rainbow(2)


rm_cut_paste=function(tasks)
{
# Remove same day tasks appearing outside and inside of DONE
t=ddply(tasks, .(date, description),
			function(df)
   			   if (nrow(df) == 1) df else subset(df, DONE == 1))
return(t)
}


rm_dup_DONE=function(tasks)
{
   rm_succ_DONE=function(df)
   {
   done=subset(df, DONE == 1)
   not_done=subset(df, DONE != 1)
   # Add an entry so length==nrow(df), and we want to remove the second DONE
   day_diff=c(10, diff(done$day_num))
   # Both tasks are DONE and seperated by 1-day
   return(rbind(not_done, subset(done, day_diff > 1)))
   }

# Remove same tasks appearing in two successive DONE sections
t=ddply(tasks, .(description),
			function(df)
   			   if (nrow(df) == 1) df
			     else if (sum(df$DONE) <= 1) df
				else rm_succ_DONE(df))
return(t)
}


# A non-NA actual means effort occurred, but task was never moved to DONE
maybe_DONE=function(df)
{
if (sum(df$DONE) == 0)
   return(subset(df, !is.na(actual)))
return(NULL)
}


# NA actual, no effort and never DONE
never_DONE=function(df)
{
if (sum(df$DONE) == 0)
   return(subset(df, is.na(actual)))
return(NULL)
}

# Task appeared outside DONE, but eventually appeared in DONE
eventually_DONE=function(df)
{
if ((sum(df$DONE) == 0) | (sum(df$DONE) == nrow(df)))
   return(NULL)

df$num_pends=NA
df$days_diff=NA
df$orig_est=NA
df$final_act=NA

nd=subset(df, DONE == 0)
d=subset(df, DONE == 1)
d$num_pends=nrow(nd)
d$orig_est=d$estimate
d$final_act=d$actual

# Ignore complicated cases (for time being)
if (nrow(d) > 1)
   return(df)

if (d$day_num<nd$day_num[nrow(nd)])
   return(df)
d$days_diff=d$day_num-nd$day_num[nrow(nd)]

# Add in any task actual performed outside DONE
if (!is.na(nd$actual[nrow(nd)]))
   {
   d$orig_est=nd$estimate[nrow(nd)]
   d$final_act=d$actual+nd$actual[nrow(nd)]
   }

return(rbind(nd, d))
}


only_DONE=function(df)
{
if (sum(df$DONE) == nrow(df))
   return(df)
return(NULL)
}


daily_totals=function(df)
{
return(data.frame(est_total=sum(df$estimate),
		  act_total=sum(df$actual, na.rm=TRUE),
		  items=df$freq[1]))
}

plot_day_mean=function(df)
{
bins=count(df$act_total/df$items)
lines(bins, col=pal_col[df$items[1]])
}


# For each day: return a list of estimates made, total estimates and actuals
mk_day_tot=function(df)
{
all_est=sort(df$estimate)
est_str=paste(all_est, collapse=",")
return(data.frame(est_str, num_tasks=nrow(df),
			tasks_done=nrow(subset(df, DONE == 1)),
			day_est=sum(df$estimate, na.rm=TRUE),
				day_act=sum(df$actual, na.rm=TRUE)))
}


act_fixed_est=function(df)
{
cnt_act=count(df$actual)
points(cnt_act$x, cnt_act$freq, type="b", col=pal_col[df$estimate])
}


day_task_est=function(df)
{
t=count(df$day_est)
lines(t$x, t$freq, col=pal_col[df$num_tasks[1]])
text(t$x[1], t$freq[1], df$num_tasks[1], pos=2, offset=-0.1, cex=1.3)
}


# Return the top five sequence of daily estimates
daily_top_5=function(df)
{
t=count(df$est_str)
day_est_seq=t[order(t$freq, decreasing=TRUE), ]
return(head(day_est_seq, n=10))
}



# X.words,word_cnt,description,DONE,date,estimate,actual
all_items=read.csv(paste0(ESEUR_dir, "../pending/renzo-pomodoro/all-tasks.csv"), as.is=TRUE)

# Having a consecutive day number, simplifies check for previous/next work day
u_dates=unique(all_items$date)
all_items$day_num=as.numeric(mapvalues(all_items$date, u_dates, 1:length(u_dates)))

all_items$date=as.Date(all_items$date, format="%Y-%m-%d")

items=subset(all_items, X.words != "")
items=subset(items, estimate < 20)

items=rm_cut_paste(items)
items=rm_dup_DONE(items)


e_DONE=ddply(items, .(description), eventually_DONE)
m_DONE=ddply(items, .(description), maybe_DONE)
n_DONE=ddply(items, .(description), never_DONE)
o_DONE=ddply(items, .(description), only_DONE)


# 100*nrow(e_DONE)/nrow(items)
# [1] 19.85107
# 100*nrow(m_DONE)/nrow(items)
# [1] 16.42419
# 100*nrow(n_DONE)/nrow(items)
# [1] 26.47728
# 100*nrow(o_DONE)/nrow(items)
# [1] 37.24747

# 100*(nrow(e_DONE)+ nrow(m_DONE)+ nrow(n_DONE)+ nrow(o_DONE))/nrow(items)

# Never DONE tasks
pal_col=rainbow(4)

# On how many days does each description appear?
plot_wide()

o_desc=count(o_DONE$description)
oo_desc=count(o_desc$freq)

plot(oo_desc, log="xy", col=pal_col[1],
	xaxs="i",
	xlim=c(0.99, 15),
	xlab="Days appeared", ylab="Descriptions\n")

d_mod=glm(log(freq) ~ log(x), data=oo_desc, subset=(x<5))
# summary(d_mod)
pred=predict(d_mod)
lines(oo_desc$x[1:4], exp(pred), col=pal_col[1])

n_desc=count(n_DONE$description)
nn_desc=count(n_desc$freq)

points(nn_desc, col=pal_col[2])

d_mod=glm(log(freq) ~ log(x), data=nn_desc, subset=(x<12))
# summary(d_mod)
pred=predict(d_mod, newdata=data.frame(x=1:11))
lines(1:11, exp(pred), col=pal_col[2])

m_desc=count(m_DONE$description)
mm_desc=count(m_desc$freq)

points(mm_desc, col=pal_col[3])

d_mod=glm(log(freq) ~ log(x), data=mm_desc, subset=(x<6))
# summary(d_mod)
pred=predict(d_mod, newdata=data.frame(x=1:6))
lines(1:6, exp(pred), col=pal_col[3])

e_desc=count(e_DONE$description)
ee_desc=count(e_desc$freq)

points(ee_desc, col=pal_col[4])

d_mod=glm(log(freq) ~ log(x), data=ee_desc, subset=(x<11))
# summary(d_mod)
pred=predict(d_mod, newdata=data.frame(x=1:10))
lines(1:10, exp(pred), col=pal_col[4])

legend(x="topright", legend=c("Only DONE", "Never DONE", "Maybe DONE", "Eventually DONE"), bty="n", fill=pal_col, cex=1.2)


# Never DONE tasks
n_DONE=subset(n_DONE, actual > 0)
n_est=count(n_DONE$estimate)
n_act=count(n_DONE$actual)

plot(n_est, type="b", log="y", col=pal_col[1],
	xlim=c(1, 20),
	xlab="Pomodoro", ylab="Tasks\n")
points(n_act, col=pal_col[2])
n_mod=glm(log(freq) ~ x, data=n_act, subset=(x<15))
# summary(n_mod)
pred=predict(n_mod)
lines(n_act$x[1:14], exp(pred), col=pal_col[2])

legend(x="topright", legend=c("Estimate", "Actual"), bty="n", fill=pal_col, cex=1.2)


pal_col=rainbow(2)

# Only DONE tasks
o_DONE=subset(o_DONE, actual > 0)
o_est=count(o_DONE$estimate)
o_act=count(o_DONE$actual)

plot(o_est, type="b", log="y", col=pal_col[1],
	xlim=c(1, 17), ylim=c(1, 1900),
	xlab="Pomodoro", ylab="Tasks\n")
points(o_act, col=pal_col[2])
o_mod=glm(log(freq) ~ x, data=o_act, subset=(x<15))
# summary(o_mod)
pred=predict(o_mod)
lines(o_act$x[1:14], exp(pred), col=pal_col[2])

legend(x="topright", legend=c("Estimate", "Actual"), bty="n", fill=pal_col, cex=1.2)


# Maybe DONE tasks
m_DONE=subset(m_DONE, actual > 0)
m_est=count(m_DONE$estimate)
m_act=count(m_DONE$actual)

plot(m_est, type="b", log="y", col=pal_col[1],
        xlim=c(1, 17), ylim=c(1, 1900),
        xlab="Pomodoro", ylab="Tasks\n")
points(m_act, col=pal_col[2])
m_mod=glm(log(freq) ~ x, data=m_act, subset=(x<15))
# summary(m_mod)
pred=predict(m_mod)
lines(m_act$x[1:14], exp(pred), col=pal_col[2])

legend(x="topright", legend=c("Estimate", "Actual"), bty="n", fill=pal_col, cex=1.2)


# Eventually DONE tasks

plot(e_DONE$orig_est, e_DONE$final_act)



item_days=count(items$date)
item_day_freq=item_days$freq

# plot(item_days$freq,
# 	xlim=c(1, 300), ylim=c(0, 15))
# lines(loess.smooth(1:300, item_days$freq[1:300], span=0.1), col="red")

# pacf(item_day_freq)
# acf(diff(item_day_freq))
# auto.arima(item_day_freq)


items_pday=count(item_day_freq)

pal_col=rainbow(4)

plot(items_pday, col=pal_col[1],
	yaxs="i",
	xlim=c(1, 20), ylim=c(0, 400),
	xlab="Tasks ... per day", ylab="Days\n")

# We need to explicitly specify truncation at zero
# NBI is a very poor fit.  NBII is not bad.
gen.trun(par=0, family=NBII)

p_NBIItr=gamlss(item_day_freq ~ 1, family=NBIItr)

NBII.mu=exp(coef(p_NBIItr, "mu"))
NBII.sigma=exp(coef(p_NBIItr, "sigma"))

lines(1:20, dNBIItr(1:20, mu=NBII.mu, sigma=NBII.sigma)*length(item_day_freq),
        col=pal_col[2])

# # Poisson is a terrible fit
# gen.trun(par=0, family=PO(mu.link=identity))
# p_POtr=gamlss(item_day_freq ~ 1, family=POtr)
# 
# PO_mu=coef(p_POtr, "mu")
# 
# lines(1:20, dPOtr(1:20, mu=PO_mu)*length(item_day_freq),
#         col="green")

act_days=count(subset(items, !is.na(actual))$date)
act_day_freq=act_days$freq

# plot(act_days$freq,
# 	xlim=c(1, 300), ylim=c(0, 15))
# lines(loess.smooth(1:300, act_days$freq[1:300], span=0.1), col="red")

# pacf(act_day_freq)
# acf(diff(act_day_freq))
# auto.arima(act_day_freq)


acts_pday=count(act_day_freq)

points(acts_pday, col=pal_col[3])

# We need to explicitly specify truncation at zero
# NBI is a very poor fit.  NBII is not bad.
gen.trun(par=0, family=NBII)

p_NBIItr=gamlss(act_day_freq ~ 1, family=NBIItr)

NBII.mu=exp(coef(p_NBIItr, "mu"))
NBII.sigma=exp(coef(p_NBIItr, "sigma"))

lines(1:15, dNBIItr(1:15, mu=NBII.mu, sigma=NBII.sigma)*length(act_day_freq),
        col=pal_col[4])

legend(x="topright", legend=c("Tasks planned per day", "Tasks worked on per day"), bty="n", fill=pal_col, cex=1.2)


# Number of task actuals having a given value
pal_col=rainbow(3)

actuals=subset(count(items$actual), x >= 1)
estimates=subset(count(items$estimate), x >= 1)

plot(actuals$x, actuals$freq, log="y", col=pal_col[1],
      xlab="Pomodoro", ylab="Tasks\n")

points(estimates$x, estimates$freq, type="b", col=pal_col[3])

legend(x="topright", legend=c("Actual", "Fitted model", "Estimates"), bty="n", fill=pal_col, cex=1.2)

act_mod=glm(log(freq) ~ x, data=actuals)
# summary(act_mod)

pred=predict(act_mod, data.frame(x=1:15))
lines(1:15, exp(pred), col=pal_col[2])


# Total tasks estimated per day
day_tot=ddply(subset(items, !is.na(estimate) & (estimate != 0)), .(date), mk_day_tot)

pal_col=rainbow(2)

day_e_freq=count(day_tot$day_est)
day_e_freq=subset(day_e_freq, freq > 1)
day_a_freq=count(day_tot$day_act)
day_a_freq=subset(day_a_freq, freq > 1)
plot(day_a_freq, type="b", col=pal_col[1],
        yaxs="i",
        xlim=c(1, 37),
        xlab="Total Pomodoro on a day", ylab="Days\n")
points(day_e_freq, type="b", col=pal_col[2])

legend(x="topright", legend=c("Actual: daily total", "Estimate: daily total"), bty="n", fill=pal_col, cex=1.4)


day_num_tasks=count(day_tot$num_tasks)
day_done=count(day_tot$tasks_done)
plot(day_num_tasks, type="b", col=pal_col[1],
        yaxs="i",
        xlim=c(0, 18),
        xlab="Total tasks on a day", ylab="Days\n")
points(day_done, type="b", col=pal_col[2])

legend(x="topright", legend=c("Planned tasks", "Done tasks"), bty="n", fill=pal_col, cex=1.4)

# What are the most frequent sequences of daily task estimates?
t2=daily_top_5(subset(day_tot, num_tasks==2))
t3=daily_top_5(subset(day_tot, num_tasks==3))
t4=daily_top_5(subset(day_tot, num_tasks==4))

s_234=data.frame(Estimates=t2$x, Occurrence=t2$freq,
                 Estimates_3=t3$x, Occurrence_=t3$freq,
                 Estimates_4=t4$x, Occurrence__=t4$freq)
xt=xtable(s_234, caption="Top 10 most frequent daily task estimates.", label="top-10_est_seq")
names(xt)=c("2 estimates", "Occur", "3 estimates", "Occur", "4 estimates", "Occur")
print.xtable(xt, include.rownames=FALSE, align="lrlrlr")


# 
pal_col=rainbow(12)

plot(1, type="n", log="y",
	xlim=c(1, 17), ylim=c(1,180),
	xlab="Daily estimate total", ylab="Days\n")
d_ply(day_tot, .(num_tasks), day_task_est)


# Mean of total daily estimates, for days having a given number of tasks
plot(1, type="n",
	xlim=c(1, 12), ylim=c(1,3),
	xlab="Daily tasks", ylab="Mean estimate\n")
d_ply(day_tot, .(num_tasks),
		function(df) {points(df$num_tasks[1], mean(df$day_est)/df$num_tasks[1])})




#
a_means=sapply(2:10, function(X) mean(subset(items, estimate == X)$actual, na.rm=TRUE))
a_medians=sapply(2:10, function(X) median(subset(items, estimate == X)$actual, na.rm=TRUE))
plot(2:10, a_means, col=pal_col[1])
lines(c(1, 10), c(1, 10))
points(2:10, a_medians, col=pal_col[2])

pal_col=rainbow(6)

plot(1, type="n", log="y",
	xlim=c(1, 8), ylim=c(10, 900),
	xlab="Actual", ylab="Tasks\n")

d_ply(items, .(estimate), act_fixed_est)

legend(x="topright", legend=paste0("Estimate= ", 1:6), bty="n", fill=pal_col, cex=1.2)


# Fit model to estimate vs. actual

nz_items=subset(items, (estimate != 0) & (actual != 0))

# smoothScatter(log(nz_items$estimate), log(nz_items$actual))

plot(jitter(nz_items$estimate), jitter(nz_items$actual), log="xy", col=point_col,
	xlab="Estimate", ylab="Actual\n")
# lines(loess.smooth(nz_items$estimate, nz_items$actual, span=0.3), col=loess_col)
lines(c(1, 10), c(1, 10), col="green")

ea_mod=glm(log(actual) ~ log(estimate), data=nz_items)
# ea_mod=glm(log(actual) ~ log(estimate)+I(log(estimate)^2), data=nz_items)
summary(ea_mod)
pred=predict(ea_mod, newdata=data.frame(estimate=1:10))
lines(1:10, exp(pred), col="red")

# How many work items estimated each day?
item_days=count(subset(nz_items, !is.na(estimate))$date)
item_days$date=item_days$x
item_days$x=NULL

fnz_items=merge(nz_items, item_days)

day_totals=ddply(fnz_items, .(date), daily_totals)
fnz_items=merge(fnz_items, day_totals)

# Add more information to the model
ea_mod=glm(log(actual) ~ log(estimate)+freq+(log(estimate)+freq):log(est_total), data=fnz_items)
summary(ea_mod)

# Model the daily totals

plot(jitter(day_totals$est_total), jitter(day_totals$act_total), log="xy", col=point_col,
        xlab="Total daily estimate", ylab="Total daily actual\n")
lines(c(1, 30), c(1, 30), col="green")

# tea_mod=glm(log(act_total) ~ log(est_total)*log(items), data=day_totals)
tea_mod=glm(log(act_total) ~ log(est_total), data=day_totals)
summary(tea_mod)

pred=predict(tea_mod, newdata=data.frame(est_total=1:30))
lines(1:30, exp(pred), col="red")

# Daily average task duration

max_items=7
pal_col=rainbow(max_items)

plot(0.1, type="n", log="y",
	xaxs="i", yaxs="i",
	xlim=c(1, 7), ylim=c(1, 150),
	xlab="Mean task duration (pomodoro)", ylab="Days")

d_ply(subset(day_totals, items < 8), .(items), plot_day_mean)
legend(x="topright", legend=paste0(1:max_items, " tasks in day"), bty="n", fill=pal_col, cex=1.2)


acf(fnz_items$est_total)

# tsea_mod=gls(log(act_total) ~ log(est_total)*log(items), data=fnz_items,
# Error in `coef<-.corARMA`(`*tmp*`, value = value[parMap[, i]]) : 
#   Coefficient matrix not invertible
# 		correlation=corARMA(c(0.1, -0.9821),
# 					form=~1, 1, 1))
					# form=~items | date, 1, 1))
		# correlation=corAR1(0.1, form=~1))
# summary(tsea_mod)

# library("COMPoissonReg")
#
# Need a zero-truncated version!
# cmp_mod=glm.cmp(item_day_freq ~ 1, formula.nu=item_day_freq~1, beta.init=0.84)
# summary(cmp_mod)
#
# lines(1:15, length(item_day_freq)*dcmp(1:15, lambda=exp(cmp_mod$beta),
#                                 nu=exp(cmp_mod$gamma)))


