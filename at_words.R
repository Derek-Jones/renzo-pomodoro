#
# at_words.csv,  1 Dec 19
# Data from:
# Renzo
#
# Example from:
# Evidence-based Software Engineering: based on the publicly available data
# Derek M. Jones
#
# TAG

source("ESEUR_config.r")


library("arules")
library("lubridate")
library("plyr")
library("stringdist")


pal_col=rainbow(2)


clean_at_1_names=function(vec)
{
vec=sub("^@ad$", "@ads", vec)
vec=sub("^@addintsr$", "@addinstr", vec)
vec=sub("^@apilesupport$", "@agilesupport", vec)
vec=sub("^@agilesuppport$", "@agilesupport", vec)
vec=sub("^@applenew$", "@applenews", vec)
vec=sub("^@asl$", "@ask", vec) # one occurrence
vec=sub("^@axis$", "@axes", vec)
vec=sub("^@bugs$", "@bug", vec)
vec=sub("^@charts$", "@chart", vec)
vec=sub("^@clj_fe$", "@clj-fe", vec)
vec=sub("^@cljfe$", "@clj-fe", vec)
vec=sub("^@clojutre$", "@clojure", vec)
vec=sub("^@clojure-ug$", "@clojureug", vec)
vec=sub("^@cotract$", "@contract", vec)
vec=sub("^@cointains$", "@contains", vec)
vec=sub("^@contracts$", "@contract", vec)
vec=sub("^@courses$", "@course", vec)
vec=sub("^@emails$", "@email", vec)
vec=sub("^@filters$", "@filter", vec)
vec=sub("^@fonts$", "@font", vec)
vec=sub("^@hardning$", "@hardening", vec)
vec=sub("^@invitation$", "@invitations", vec)
vec=sub("^@Invitations$", "@invitations", vec)
vec=sub("^@interviews$", "@interview", vec)
vec=sub("^@job$", "@jobs", vec)
vec=sub("^@leftover$", "@leftovers", vec)
vec=sub("^@likelt$", "@likely", vec)
vec=sub("^@logistics$", "@logistic", vec)
vec=sub("^@meeeting$", "@meeting", vec)
vec=sub("^@meetings$", "@meeting", vec)
vec=sub("^@merckmanual$", "@merkmanual", vec)
vec=sub("^@moneymorgages$", "@moneymortgages", vec)
vec=sub("^@mortgages-api$", "@mortgagesapi", vec)
vec=sub("^@mturk$", "@turk", vec)
vec=sub("^@paralell$", "@parallel", vec)
vec=sub("^@payments$", "@payment", vec)
vec=sub("^@persona$", "@personal", vec)
vec=sub("^@persistece$", "@persistence", vec)
vec=sub("^@plannin$", "@planning", vec)
vec=sub("^@planing$", "@planning", vec)
vec=sub("^@plannig$", "@planning", vec)
vec=sub("^@plannng$", "@planning", vec)
vec=sub("^@pomodoros$", "@pomodoro", vec)
vec=sub("^@pomodori$", "@pomodoro", vec)
vec=sub("^@presets$", "@preset", vec)
vec=sub("^@prjsats$", "@prjstats", vec)
vec=sub("^@puechase$", "@purchase", vec)
vec=sub("^@questions$", "@question", vec)
vec=sub("^@sortedmap$", "@sorted-map", vec)
vec=sub("^@spikes$", "@spike", vec)
vec=sub("^@talks$", "@talk", vec)
vec=sub("^@tickets$", "@ticket", vec)
vec=sub("^@unexpectd$", "@unexpected", vec)
vec=sub("^@uplanned$", "@unplanned", vec)
vec=sub("^@users$", "@user", vec)
vec=sub("^@uswtich$", "@uswitch", vec)
vec=sub("^@wewkly$", "@weekly", vec)

return(vec)
}

clean_at_2_names=function(vec)
{
vec=sub("^@bugs$", "@bug", vec)
vec=sub("^@contains?$", "@contains", vec)
vec=sub("^@corportate$", "@corporate", vec)
vec=sub("^@cotract$", "@contract", vec)
vec=sub("^@courses$", "@course", vec)
vec=sub("^@docs$", "@doc", vec)
vec=sub("^@empty?$", "@empty", vec)
vec=sub("^@endquestioned$", "@endquestion", vec)
vec=sub("^@endquestions$", "@endquestion", vec)
vec=sub("^@estimates$", "@estimate", vec)
vec=sub("^@filter$", "@filters", vec)
vec=sub("^@fonts$", "@font", vec)
vec=sub("^@fraudd$", "@fraud", vec)
vec=sub("^@hardning$", "@hardening", vec)
vec=sub("^@hash-map$", "@hashmap", vec)
vec=sub("^@identical?$", "@identical", vec)
vec=sub("^@Invitations$", "@invitation", vec)
vec=sub("^@invitations$", "@invitation", vec)
vec=sub("^@labs$", "@lab", vec)
vec=sub("^@logistics$", "@logistic", vec)
vec=sub("^@managment$", "@management", vec)
vec=sub("^@managmenent$", "@managment", vec)
vec=sub("^@moneymorgages$", "@moneymortgages", vec)
vec=sub("^@mortgages-api$", "@mortgagesapi", vec)
vec=sub("^@others$", "@other", vec)
vec=sub("^@payments$", "@payment", vec)
vec=sub("^@perftests$", "@perftest", vec)
vec=sub("^@persistece$", "@persistence", vec)
vec=sub("^@picmatic$", "@picomatic", vec)
vec=sub("^@pill$", "@pills", vec)
vec=sub("^@plannning$", "@planning", vec)
vec=sub("^@pomodoros$", "@pomodoro", vec)
vec=sub("^@questions$", "@question", vec)
vec=sub("^@reducers$", "@reducer", vec)
vec=sub("^@rejections$", "@rejection", vec)
vec=sub("^@risklabels$", "@risklabel", vec)
vec=sub("^@sorted-map$", "@sortedmap", vec)
vec=sub("^@suppoer$", "@support", vec)
vec=sub("^@talks$", "@talk", vec)
vec=sub("^@ticket$", "@tickets", vec)
vec=sub("^@tests$", "@test", vec)
vec=sub("^@unexpectd$", "@unexpected", vec)
vec=sub("^@wewkly$", "@weekly", vec)

return(vec)
}


mk_day_at_list=function(df)
{
d_at=paste0(df$at_1, collapse=",")
return(data.frame(date=df$date[1], d_at,  stringsAsFactors=FALSE))
}


# Number each at word by number of previous occurrences (plus 1)
number_at=function(df)
{
df$at_order=1:nrow(df)
return(df)
}


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


all_items=read.csv(paste0(ESEUR_dir, "../pending/renzo-pomodoro/all-tasks.csv"), as.is=TRUE)

all_items$date=as.Date(all_items$date, format="%Y-%m-%d")

items=subset(all_items, X.words != "")
items=subset(items, estimate < 20)

items=rm_cut_paste(items)
items=rm_dup_DONE(items)

items=items[order(items$date), ]

at=strsplit(items$X.words, ",")
# Need to convert the list returned by strsplit
at_1=clean_at_1_names(unlist(lapply(at, function(l) l[1])))
items$at_1=at_1
at_2=clean_at_2_names(unlist(lapply(at, function(l) l[2])))
items$at_2=at_2

day_pomo=ddply(items, .(date), mk_day_at_list)
da=strsplit(day_pomo$d_at, ",")

d_trans=as(da, "transactions")
rules=apriori(d_trans, parameter=list(support=0.0001, confidence=0.1))

summary(rules)
inspect(head(rules, n=3, by = "confidence"))


u_at=unique(at_1)
pal_col=rainbow(length(u_at))

items$uat_1_num=as.integer(mapvalues(items$at_1, u_at, 1:length(u_at)))

plot(items$date, items$uat_1_num, col=pal_col[items$uat_1_num],
	xaxs="i", yaxs="i",
	xlab="Date", ylab="@word\n")

t=count(items$uat_1_num)
top5=subset(t, freq > 180)

legend(x="topleft", legend=rev(substring(u_at[top5$x], 2)), bty="n", fill=rev(pal_col[top5$x]), cex=1.3)

first_use=ddply(items, .(uat_1_num), function(df) min(df$date))
start_date=min(first_use$V1)
first_use$days=as.integer(first_use$V1-start_date)

# Pick changepoints by experimenting by eye
# points(start_date+1100, 100)
# points(start_date+2100, 140)

# fu_mod=nls(days ~ a*uat_1_num^b, data=first_use)
fu_mod=nls(uat_1_num ~ a*days^b, data=first_use)
# summary(fu_mod)

pred=predict(fu_mod)
lines(start_date+first_use$days, pred, col=point_col)

days_2=subset(first_use, (days < 1100))
fu_1_mod=nls(uat_1_num ~ a*(1-exp(b*days)), data=days_2,
		start=list(a=100, b=-0.05))
# summary(fu_1_mod)
pred=predict(fu_1_mod)
lines(start_date+days_2$days, pred)

days_2=subset(first_use, (days >= 1100) & (days <= 2100))
fu_2_mod=glm(uat_1_num ~ days, data=days_2)
# summary(fu_2_mod)
pred=predict(fu_2_mod)
lines(start_date+days_2$days[1:length(pred)], pred)

days_3=subset(first_use, (days > 2100))
fu_3_mod=nls(uat_1_num ~ c+a*(1-exp(b*(days-2100))), data=days_3,
		start=list(c=140, a=80, b=-0.05))
# summary(fu_3_mod)
pred=predict(fu_3_mod)
lines(start_date+days_3$days, pred)


q=table(at_1)
qv=as.vector(q)
plot(sort(qv), log="y", col=point_col)

u_at=unique(names(q))

sd=stringdistmatrix(u_at, u_at, "dl")
colnames(sd)=u_at
rownames(sd)=u_at
which(sd ==1, arr.ind=TRUE)

all_at=clean_at_names(unlist(at))
uw=sort(unique(all_at))
uw=uw[414:length(uw)]

sd=stringdistmatrix(uw, uw, "dl")
colnames(sd)=uw
rownames(sd)=uw
t=which(sd ==1, arr.ind=TRUE)

q=dimnames(t)

close_words=data.frame(r=uw[t[1:nrow(t), 1]],
			c=uw[t[1:nrow(t), 2]], stringsAsFactors=FALSE)

cw=adply(close_words, 1, function(df)
				{
				t=sort(c(df$r, df$c))
				data.frame(r=t[1], c=t[2])
				})
ucw=unique(cw)

# Include at_order sequence number in estimation model
nz_items=subset(items, (estimate != 0) & (actual != 0))
proj=ddply(nz_items, .(at_1), number_at)
proj$est_order=1:nrow(proj)

est_mod=glm(log(actual) ~ log(estimate)*log(at_order), data=proj)
summary(est_mod)

est_mod=glm(log(actual) ~ log(estimate)+log(est_order), data=proj)
summary(est_mod)

# est_mod=glm(log(actual) ~ log(estimate)+log(at_order)*log(est_order), data=proj)
est_mod=glm(actual ~ log(estimate)+log(est_order), data=proj,
				family=poisson)
summary(est_mod)

table(proj$actual - proj$estimate)

# Rate at which new @words appear
uat_bit=!duplicated(items$uat_1_num)
at_1use=subset(items, uat_bit)

at_1use$month=month(at_1use$date)
at_1use$year=year(at_1use$date)

at_month=ddply(at_1use, .(month, year), function(df) nrow(df))

every_month=data.frame(year=rep(min(at_month$year):max(at_month$year), each=12),
			month=rep(1:12, times=10))
every_month$V1=0
t=rbind(at_month, every_month)
all_months=subset(t, !duplicated(data.frame(t$year, t$month)))

all_months$date=all_months$year+(all_months$month-1)/12
all_months=all_months[order(all_months$date), ]
plot(all_months$date, all_months$V1, type="b", col=point_col,
	xaxs="i", yaxs="i",
	xlab="Date", ylab="New @words")

# Using 64 tasks creates 120 bins
task_bin=sapply(seq(1, nrow(items), by=64),
				function(X) length(which(uat_bit[X:(X+64)])))
plot(task_bin, type="b", col=point_col,
	xaxs="i", yaxs="i",
	xlab="Group 64 tasks", ylab="New @words")

# Are all at_2 words associated unique at_1 word?
pal_col=rainbow(2)

at_21=ddply(items, .(at_2), function(df) length(unique(df$at_1)))

at_freq=count(at_21$V1)
plot(at_freq, log="xy", col=pal_col[1],
	xlim=c(1, 50),
	xlab="Number of first @words", ylab="Occurrences\n")

at2_mod=glm(log(freq) ~ log(x), data=at_freq, subset=(x <= 10))
summary(at2_mod)

pred=predict(at2_mod, newdata=data.frame(x=1:10))
lines(1:10, exp(pred), col=pal_col[2])


#
plot_wide()

u_at=unique(at_2)
pal_col=rainbow(length(u_at))

items$uat_2_num=as.integer(mapvalues(items$at_2, u_at, 1:length(u_at)))

plot(items$date, items$uat_2_num, col=pal_col[items$uat_2_num],
        xaxs="i", yaxs="i",
        xlab="Date", ylab="@word\n")

t=count(items$uat_2_num)
top5=subset(t, freq > 180)

legend(x="topleft", legend=rev(substring(u_at[top5$x], 2)), bty="n", fill=rev(pal_col[top5$x]), cex=1.3)

