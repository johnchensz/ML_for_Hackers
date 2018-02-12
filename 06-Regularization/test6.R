# library(ggplot2)

# set.seed(1)
# x <- seq(-10, 10, by=0.01)
# y <- 1-x^2+rnorm(length(x), 0, 5)

# ggplot(data.frame(X=x, Y=y), aes(x=X, y=Y)) + geom_point()

# ggplot(data.frame(X=x, Y=y), aes(x=X, y=Y)) + geom_point() + geom_smooth(method='lm', se=FALSE)

# ggplot(data.frame(X=x, Y=y), aes(x=X, y=Y)) + geom_point() + geom_smooth(se=FALSE)


# # 一个正旋曲线直接拟合的线性回归模型。
# set.seed(1)
# x <- seq(0, 1, by=0.01)
# y <- sin(2*pi*x) + rnorm(length(x), 0, 0.1)
# df <- data.frame(X=x, Y=y)
# ggplot(df, aes(x=X, y=Y))+geom_point()
# # 图形分析-拟合的线性模型效果差
# ggplot(df, aes(x=X, y=Y))+geom_point()+geom_smooth(method='lm', se=FALSE)
# # 数据分析- 查看R2的值，只有40%几。
# summary(lm(Y~X, data=df))

# # 增加x的平方和立方，变为多项式模型，数据分析结构R2提高到90%
# df <- transform(df, X2=X^2)
# df <- transform(df, X3=X^3)
# summary(lm(Y~X+X2+X3, data=df))

# # 用poly函数变化为多项式模型，解决出现奇异点问题（太多次方，使回归算法不能正常执行）
# summary(lm(Y~poly(X,degree=14),data=df))

# # 分别用1，3，5，25次方的多项式回归模型 图形分析，当次数达到一定时，会出现‘过拟合’的情况（回归模型变得不准确了）
# poly.fit <- lm(Y~poly(X,degree=1), data=df)
# df <- transform(df, PredictedY=predict(poly.fit))
# ggplot(df, aes(x=X, y=PredictedY))+geom_point()+geom_line()

# poly.fit <- lm(Y~poly(X,degree=3), data=df)
# df <- transform(df, PredictedY=predict(poly.fit))
# ggplot(df, aes(x=X, y=PredictedY))+geom_point()+geom_line()

# poly.fit <- lm(Y~poly(X,degree=5), data=df)
# df <- transform(df, PredictedY=predict(poly.fit))
# ggplot(df, aes(x=X, y=PredictedY))+geom_point()+geom_line()

# poly.fit <- lm(Y~poly(X,degree=25), data=df)
# df <- transform(df, PredictedY=predict(poly.fit))
# ggplot(df, aes(x=X, y=PredictedY))+geom_point()+geom_line()
# # 如何知道到什么程度就过拟合了呢？


# 一个验证模型有效性的方法：把数据分两部分，训练和测试，训练构造模型，测试验证模型的效果。分的比例建议训练占大部分，小部分数据测试。
set.seed(1)
x <- seq(0, 1, by=0.01)
y <- sin(2*pi*x)+rnorm(length(x), 0, 0.1)
n <- length(x)
indices <- sort(sample(1:n, round(0.5*n)))
training.x <- x[indices]
training.y <- y[indices]
test.x <- x[-indices]
test.y <- y[-indices]
training.df <- data.frame(X=training.x, Y=training.y)
test.df <- data.frame(X=test.x, Y=test.y)

# 用训练数据构造线性回归模型，用图形分析查看模型在训练数据上的拟合情况
poly.fit <- lm(Y~poly(X,degree=1), data=training.df)
training.df <- transform(training.df, PredictedY=predict(poly.fit))
ggplot(training.df, aes(x=X, y=PredictedY))+geom_point()+geom_line()
# 多项式线性模型
poly.fit <- lm(Y~poly(X,degree=3), data=training.df)
training.df <- transform(training.df, PredictedY=predict(poly.fit))
ggplot(training.df, aes(x=X, y=PredictedY))+geom_point()+geom_line()


# 计算1~12项线性回归模型的均方根误差（RMSE，同比越小越好）
rmse <- function(y, h) {
	return (sqrt(mean((y-h)^2)))
}

performance <- data.frame()

# 每次分别计算训练数据的 和 测试数据的
for (d in 1:12) {
	poly.fit <- lm(Y~poly(X, degree=d), data=training.df)
	performance <- rbind(performance, data.frame(Degree=d, Data='Training', RMSE=rmse(training.y, predict(poly.fit))))
	performance <- rbind(performance, data.frame(Degree=d, Data='Test', RMSE=rmse(test.y, predict(poly.fit, newdata=test.df))))
}

# 图形对比分析，训练数据上的RMSE和测试数据RMSE在不同的多项式模型下的拟合情况。
# RMSE越小越好。在训练和测试数据上都是最小的RMSE的对应项数就是最合适的多项式线性回归模型了。
# 如果训练和测试数据的RMSE都很大，说明拟合不够。如果两者的RMSE差别比其他项式大，说明过拟合了。模型要避免这两种情况。
ggplot(performance, aes(x=Degree, y=RMSE, linetype=Data)) + geom_point() + geom_line()
