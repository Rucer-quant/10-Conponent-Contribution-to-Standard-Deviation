#ETL函数 Extract-Transform-Load（抽取-转换-加载）

#getSymbols函数的解析，层层递进
getSymbols('AAPL',src="yahoo",from="2014-09-1",to="2014-10-1") #EDU股票代码、src数据来源（默认yahoo）、时间区间
get("AAPL")#加引号？
head(AAPL)#不加引号？
class(AAPL)#查看数据类型，为zoo or zool类型？

#环境（容器，里面放置各种对象）
my666environment<-new.env()#创建一个名为my666enviroment的新环境/作用：应对庞大数据的分类处理
getSymbols("AAPL",env=my666environment,scr="yahoo",from="2014-09-1",to="2014-10-1")#放置在新环境中
ls(envir=my666environment)#ls()可以罗列出该工作区间内所有的变量
getSymbols("GOOG",env=my666environment,scr="yahoo",from="2014-09-1",to="2014-10-1")
ls(envir=my666environment)#可以看到这个环境中有AAPL和GOOG两个数据
get("AAPL",envir=my666environment)

#quantmod获取国内d额股票数据,以茅台为例(为什么数字代码不行？)
getSymbols("600519.ss",env=my666environment,scr="yahoo",from="2019-09-1",to="2019-9-30")#上证代码是 ss，深证代码是 sz，港股代码是 hk
get("600519.ss",envir=my666environment)

getSymbols("GZMT",env=my666environment,scr="yahoo",from="2019-09-1",to="2019-9-30")
get("GZMT",envir=my666environment)

getSymbols("GZMT",env=my666environment,scr="google",from="2019-09-1",to="2019-9-30")#google能用？
get("GZMT",envir=my666environment)

#getDividends函数：获取上市公司的股息数据
getDividends("AAPL",env=my666environment,scr="yahoo",from="2010-01-01",to="2019-01-01")
getDividends("GZMT",env=my666environment,scr="yahoo",from="2010-01-01",to="2019-01-01")#GZMT为什么拉不出来,AAPL就可以拉出来

#getFinancials函数：获取上市公司的财务报表/Google Finance stopped providing data in March, 2018.
getFinancials("AAPL",env=my666environment,scr="yahoo",auto.assign = TRUE)
viewFinancials(BS, period=c('A','Q'),subset = NULL)
getFinancials("AAPL")

#getFX函数：获取汇率数据
getFX("HKD/USD",from="2019-09-01",env=my666environment)#被墙？
getFX("USD/JPY")

#getQuote函数：获取即时网络报价
getQuote("AAPL",src="yahoo",what=yahooQF(c("Bid","Ask")))#what:检索什么数据/c？

eurusd<-getSymbols("EUR/USD", src = "oanda", auto.assign = FALSE)



