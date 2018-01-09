library(ggplot2)
library(plyr)
library(reshape)

ufo <- read.delim("/Users/jiuzhitianxia/test/R/test0/ufo_awesome.tsv", sep = "\t", stringsAsFactors = FALSE, header = FALSE, na.strings = "" )

head(ufo)

names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "LongDescription")

good.rows <- ifelse( nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8, FALSE, TRUE )

ufo <- ufo[good.rows,]

ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

get.location <- function(l) {
	split.location <- tryCatch(strsplit(l, ",")[[1]], error=function(e) return(c(NA,NA)))
	clean.location <- gsub("^ ","",split.location)
	if (length(clean.location) > 2) {
		return(c(NA,NA))
	} else {
		return(clean.location)
	}
}

city.state <- lapply(ufo$Location, get.location)

location.matrix <- do.call(rbind, city.state)
ufo <- transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]), stringsAsFactors=FALSE)

us.states <- c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il","in","ks","ky","la","ma","md","me","mi","mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy")

ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA

ufo.us <- subset(ufo, !is.na(USState))

# 生成统计各日期出现次数的直方图 （有几个参数和书本、github上有变化 - R v3.4.3) —— 分析数据分布情况
# quick.hist <- ggplot(ufo.us, aes(x=DateOccurred)) + geom_histogram(binwidth=30) + scale_x_date(date_breaks="50 years", date_labels="%Y")
# ggsave(plot=quick.hist, filename="/Users/jiuzhitianxia/test/R/test0/images/quick_hist.png", height=6, width=8)

# 缩小分析的日期范围，抛弃时间跨度长但报告次数少的数据（注意：缩小后的总数据量占比变化小)
ufo.us <- subset(ufo.us, DateOccurred >= as.Date("1990-01-01"))

# nrow(ufo.us) # 查看记录行数
# summary(ufo.us$DateOccurred) #查看一列数据的统计摘要

ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")


sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)

states.dates <- lapply(us.states, function(s) cbind(s, date.strings))
states.dates <- data.frame(do.call(rbind, states.dates), stringsAsFactors=FALSE)

all.sightings <- merge(states.dates, sightings.counts, by.x=c("s","date.strings"), by.y=c("USState", "YearMonth"), all=TRUE)

names(all.sightings) <- c("State", "YearMonth", "Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)] <- 0
all.sightings$YearMonth <- as.Date(rep(date.range, length(us.states)))
all.sightings$State <- as.factor(toupper(all.sightings$State))

state.plot <- ggplot(all.sightings, aes(x=YearMonth, y=Sightings)) + 
geom_line(aes(color="darkblue")) + 
facet_wrap(~State, nrow=10, ncol=5) + 
theme_bw() + 
scale_color_manual(values=c("darkblue"="darkblue"), guide="none") + 
scale_x_date(date_breaks="10 years", date_labels="%Y") + 
xlab("Time") + ylab("Number of Sightings") + ggtitle("Number of UFO sightings by Month-Year and U.S.State(1990-2010)") 
#ggsave(plot=state.plot, filename="/Users/jiuzhitianxia/test/R/test0/images/ufo_sightings.pdf", width=14, height=8.5)

