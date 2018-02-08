# 以下代码不要一次性载入。适合逐行加入到命令行实验。

# library(ggplot2)

# 看下当前数据分布情况-用密度图
# ages <- read.csv('data/longevity.csv')
# ggplot(ages, aes(x=AgeAtDeath, fill=factor(Smokes)))+geom_density()+facet_grid(Smokes~.)

# 用寿命的平均值计算一下所有寿命与平均值的平方差的平均值（均方误差 - MSE）
# guess <- 73
# with(ages, mean((AgeAtDeath-guess)^2))

# 为了证明平均值是最好的预测数，从63到83的每个数字都计算一次均方误差，并用线点图显示出每个数字及其均方误差的对应情况。
# guess.accuracy <- data.frame()
# for(guess in seq(63, 83, by=1)) { 
# 	prediction.error <- with(ages, mean((AgeAtDeath-guess)^2))  
# 	guess.accuracy <- rbind(guess.accuracy, data.frame(Guess=guess, Error=prediction.error))
# }
# ggplot(guess.accuracy, aes(x=Guess, y=Error))+geom_point()+geom_line()
# 结论是，如果想知道预测效果是否好，可以和均值进行对比。

# 直接用平均寿命作为预测，和实际寿命比较，误差多少——均方根误差（RMSE）
# constant.guess <- with(ages, mean(AgeAtDeath))
# with(ages, sqrt(mean((AgeAtDeath-constant.guess)^2)))

# 根据是否吸烟分别计算平均寿命，以平均寿命为预测值，和实际寿命比较，计算结果（RMSE）和不考虑吸烟因素相比，误差更小。
# smokers.guess <- with(subset(ages, Smokes==1), mean(AgeAtDeath))
# non.smokers.guess <- with(subset(ages, Smokes==0), mean(AgeAtDeath))
# ages <- transform(ages, NewPrediction=ifelse(Smokes==0, non.smokers.guess, smokers.guess))
# with(ages, sqrt(mean((AgeAtDeath - NewPrediction)^2)))
# 结论是，有更多的特征参与预测计算，预测的结果更好。

# 在身高-体重关系的散点图上，画出线性回归模型的直线。基于这条直接，根据身高来预测体重
# heights.weights <- read.csv('data/01_heights_weights_genders.csv', header=TRUE, sep=',')
# ggplot(heights.weights, aes(x=Height, y=Weight)) + geom_point() + geom_smooth(method='lm') 
# 隐含回归公式 Weight~Height ，即以身高x预测体重y

# 构造一个线性回归模型？ 模型的公式：Weight~Height 即，以身高预测体重
# fitted.regression <- lm(Weight~Height, data=heights.weights)

# 查看线性回归模型的系数（lm算出来的线性函数的系数）
# coef(fitted.regression)

# 残值 = 实际体重 和 用线性模型预测的体重 的差
# true.values <- with(heights.weights, Weight)
# errors <- true.values - predict(fitted.regression)
# 直接用residuals()计算预测的残值
# errors.t2 <- residuals(fitted.regression)
# 图形化查看残值和实际值。which参数是指定要显示几个回归诊断点。which=1 - 拟合值 对 残值
# plot(fitted.regression, which=1)
# 用这个图来分析，当前的线性回归模型的效果，残值是否过大还是比较合适。

# 评价一个线性回归模型有多好，用 残值 的 均方误差 —— 常用评估机器学习算法的效果
# errors.t3 <- residuals(fitted.regression.t2)
# squared.errors <- errors.t3 ^ 2 #残值开平方
# mse <- mean(squared.errors)  # 取均值
# rmse <- sqrt(mse)  # 开平方
# 只看残值的RMSE，还是不好分辨出模型预测的效果如何

# 计算R2 = 1-（回归模型预测的残值RMSE / 目标字段的均值RMSE）
# mean.weights <- with(heights.weights, mean(Weight))
# rmse.mean <- with(heights.weights, sqrt(mean((Weight-mean.weights)^2)))
# rmse.model <- sqrt(mean(errors.t2 ^ 2))
# r2 <- 1-(rmse.model/rmse.mean)
# R2的结果在0-1之间，趋于0表示模型预测的效果不如均值好，趋于1表示模型预测的效果好于均值。


top.1000.sites <- read.csv('data/top_1000_sites.tsv', sep='\t', stringsAsFactors=FALSE)
ggplot(top.1000.sites, aes(x=PageViews, y=UniqueVisistors)) + geom_point()
# 散点图没有明显特征，数据范围太大，去ln缩小数值范围，再绘制散点图，看到有线性特征
ggplot(top.1000.sites, aes(x=log(PageViews), y=log(UniqueVisitors))) + geom_point()
# 增加显示线性回归模型
ggplot(top.1000.sites, aes(x=log(PageViews), y=log(UniqueVisitors))) + geom_point() + geom_smooth(method='lm', se=FALSE)

# 分析 UV和PV之间关系的线性回归模型（根据UV预测PV）- 建模
lm.fit <- lm(log(PageViews)~log(UniqueVisitors), data=top.1000.sites)
# 怎么分析？查看模型系数；查看模型的残值分析图；查看模型的分析数据-模型是否可靠、两个数据是否有联系
plot(lm.fit, which=1) #查看模型的残值分析图 红色线比较平直，说明模型比较有效
summary(lm.fit) #查看模型的分析数据


# 增加2个输入因子，看下模型的分析报告
lm.fit <- lm(log(PageViews)~HasAdvertising+log(UniqueVisitors)+InEnglish, data=top.1000.sites)
summary(lm.fit)

# 比较新增的2个因子的R2（R平方）值，对比每个因子单独的对预测结果的影响效果。
lm.fit <- lm(log(PageViews)~HasAdvertising, data=top.1000.sites)
summary(lm.fit)$r.squared
lm.fit <- lm(log(PageViews)~InEnglish, data=top.1000.sites)
summary(lm.fit)$r.squared
# 这个分析结果可以帮忙判断，如果有输入因子的影响效果小，而本身又难获取的话，就可以不用到模型中。但是都不难获取的话，尽可能都用到模型中。

# 用一个例子说明 2个变量之间的 相关性 —— 呈现直线的关系。是否存在相关性？》建模的好坏？
# 相关性趋于0，表示2个变量的相关性很弱，为0表示无相关性。趋于1（或-1）表示相关性强，为1或-1表示很强的相关性。
x <- 1:10
y <- x^2
# 图形分析相关性，x、y之间的相关性如何 （拟合直线没有通过所有的点，是不完美的线性）
ggplot(data.frame(X=x, Y=y), aes(x=X, y=Y)) + geom_point() + geom_smooth(method='lm', se=FALSE)

# 量化分析相关性 —— cor函数。
cor(x,y)
cor(top.1000.sites$UniqueVisitors, top.1000.sites$PageViews)
cor(log(top.1000.sites$UniqueVisitors), log(top.1000.sites$PageViews))















