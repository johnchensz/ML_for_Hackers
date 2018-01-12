library('ggplot2')

#getwd() #查看当前工作区
#setwd("/Users/jiuzhitianxia/test/R/t2")

# 加载一个csv文本数据文件（逗号分隔）
heights.weights <- read.csv("01_heights_weights_genders.csv", header=TRUE, sep=',')

# 取出‘高’这一列来，准备进行数据摘要分析
heights <- with(heights.weights, Height)

summary(heights)

mean(heights) #均值

median(heights) #中位数

range(heights) # 数据范围（最小、最大值）

quantile(heights) # 几个关键位置的数值（基于排序的数据）

seq(0,1,0.2) # 生成从0至1的序列数值，每个数值间隔0.2

quantile(heights, probs=seq(0,1,0.20)) # 按指定的间隔取出相应位置的数值

quantaile(heights, probs=0.25) #取指定位置的数值

var(heights) # 方差，所有数和均值的平均距离（每个距离都平方了）

sd(heights) # 标准差，在方差基础上开平方

ggplot(heights.weights, aes(x=Height)) + geom_histogram(binwidth=1)  # 直方图，查看数据分布情况。多调试binwidth区间宽度发现数据特点

ggplot(heights.weights, aes(x=Height)) + geom_density() # 密度曲线图。正态分布曲线

ggplot(heights.weights, aes(x=Height,fill=Gender)) + geom_density() # 按第2个维度区分第1个维度的数据曲线

ggplot(heights.weights, aes(x=Weight,fill=Gender)) + geom_density() + facet_grid(Gender~.) # 按第2个维度区分2个曲线后，分开到2张图中

# 散点图（回归图）
ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point() 

# 增加显示关系特征曲线（线性回归） 作用：可以根据‘身高’预测‘体重’
ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point() + geom_smooth() 

#越大的数据范围，关系特征曲线越准确
ggplot(heights.weights[1:20,], aes(x=Height, y=Weight)) + geom_point() + geom_smooth() 
ggplot(heights.weights[1:2000,], aes(x=Height, y=Weight)) + geom_point() + geom_smooth()


# 散点图 用第3个维度，区分第1、2维度的关系散点 —— 分类模型 
ggplot(heights.weights, aes(x=Height, y=Weight, color=Gender)) + geom_point()

# 在分类模型基础上划出 分类超平面  作用：可以根据‘身高’和‘体重’预测‘性别’
heights.weights <- transform(heights.weights, Male=ifelse(Gender=='Male',1,0))
logit.model <- glm(Male~Weight+Height, data=heights.weights, family=binomial(link='logit'))
ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point(aes(color = Gender, alpha = 0.25)) + scale_alpha(guide = "none") + scale_color_manual(values = c("Male" = "black", "Female" = "gray")) + theme_bw() + geom_abline(intercept=-coef(logit.model)[1]/coef(logit.model)[2], slope=-coef(logit.model)[3]/coef(logit.model)[2],color='black')