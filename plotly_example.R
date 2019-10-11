#加载包
install.packages("plotly")
library(plotly)
packageVersion('plotly')

#颜色选择
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all(type = 'seq')
#seq连续型，共18组颜色，每组分为9个渐变颜色展示。使用渐变色往往能让图形看起来更美观


#数据查看
str(diamonds)  #使用diamonds数据集
d <- diamonds[sample(nrow(diamonds), 2000), ] #抽取2000个样本

#1.简单散点图（两个变量）
#1.1(只需要设定数据集、X和Y)
p <- plot_ly(data = d, x = ~carat, y = ~price)
p
#1.2（修改颜色+点的边框透明度+图名+坐标线显示）
p <- plot_ly(data = d, x = ~carat, y = ~price,
             marker = list(size = 10,
                           color = 'rgba(225,0,0,.6)', 
                           line = list(color = 'rgba(93,71,139, .8)',
                                       width = 2))
) %>%
  # rgb确定颜色，a是alpha,确定这个颜色的透明度，a为小数，
  # 当a=1时，为完全不透明，当a=0时，为完全透明
  # line = list(color)表示点边缘线的颜色和透明度,width表示点边缘线的宽度
  
  layout(title='Styled Scatter',#散点图命名
         yaxis = list(zeroline = FALSE),#去掉纵坐标轴的零线
         xaxis = list(zeroline = FALSE)) #去掉纵坐标轴的零线

p

#2.点线图
#数据：
trace_0 <- d$depth[1:100] 
trace_1 <- d$carat[1:100]
trace_2 <- d$x[1:100]
x <- c(1:100) #编码
data <- data.frame(x, trace_0, trace_1, trace_2)

#2.1简单点线图
p <- plot_ly(data, x = ~x) %>%
  add_trace(y = ~trace_0, 
            name = 'trace 0',
            mode = 'lines+markers') #模式设定为点+线
p
#2.2多条点线图(简单点线图的叠加)
p <- plot_ly(data, x = ~x) %>%
  add_trace(y = ~trace_1,
            name = 'trace 0',
            mode = 'lines',
            color = 'rgba(255,225,255)') %>%
  add_trace(y = ~trace_2, 
            name = 'trace 1',
            mode = 'lines+markers',
            color = 'rgba(93,71,139)')
p

#3.复杂散点图（三个变量）
#3.1当把类别变量作为填充颜色时：(cut为类别变量，颜色为默认)
p <- plot_ly(data = d, x = ~carat, y = ~price, color = ~cut)
p

#3.2更改默认的颜色类别
#3.2.1 colors = "YlOrRd"或 "Blues"等等
p <- plot_ly(data = d, x = ~carat, y = ~price, color = ~cut, colors = "Blues")
p

#3.2.2 对colors进行自定义
pal <- c("LavenderBlush1", "LavenderBlush2", "LavenderBlush3",
         "LavenderBlush4","MistyRose1")
p <- plot_ly(data = d, x = ~carat, y = ~price, color = ~cut,colors = pal)
p

#3.3修改点的形状
p <- plot_ly(data = d, x = ~carat, y = ~price, type = 'scatter',
             mode = 'markers', symbol = ~cut, 
             symbols = c('circle','x','o',"cross-dot","star-diamond"),
             color = I('black'), marker = list(size = 10))
p 

#3.4 三个变量，但是三个变量都是数值变量。
#如果颜色和大小的变量设定为数值变量，则颜色会渐变，大小也会不一样
p <- plot_ly(d, x = ~carat, y = ~price,
             color = ~depth, size = ~depth)
p

##折线图
#1.基础折线图
data = LakeHuron
x <- c(1875:1972)
data <- data.frame(x,Level= data)
p <- plot_ly(data, x = ~x, y = ~Level, 
             type = 'scatter', mode = 'lines')
p

#2.叠加点线图
data = LakeHuron
data_1 = LakeHuron-10
data_2 = LakeHuron-20
x <- c(1875:1972)
data <- data.frame(x,Level= data,
                   Level_1= data_1,
                   Level_2= data_2)

p <- plot_ly(data, x = ~x) %>%
  add_trace(y = ~Level, 
            name = 'Level',
            mode = 'lines') %>% #线图
  add_trace(y = ~Level_1, 
            name = 'Level_1', 
            mode = 'lines+markers') %>% #点线图
  add_trace(y = ~Level_2,
            name = 'Level_2', 
            mode = 'markers') #点图
p

#3.自定义点线图（时间序列用得比较多）
#数据：
data = LakeHuron[1:12]
data_1 = data-2
data_2 = data-4
data_3 = data-6
data_4 = data-8
data_5 = data-10

x <- c('January', 'February', 'March', 
       'April', 'May', 'June', 'July',
       'August', 'September', 'October',
       'November', 'December')
data <- data.frame(x,data_0 = data,data_1,data_2,data_3,data_4,data_5)
#此数据为胡编乱造。
data$x = factor(data$x, levels = data[["x"]])
str(data)

#通过dash来设定线的形状：并根据layout函数定义名称
p <- plot_ly(data, x = ~x, y = ~data_0, 
             name = 'data_0', type = 'scatter',
             mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)',
                         width = 4)) %>% #粗的直线
  add_trace(y = ~data_1, name = 'data_1', 
            line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% #直线
  add_trace(y = ~data_2, name = 'data_2', 
            line = list(color = 'rgb(205, 12, 24)', width = 4, 
                        dash = 'dash')) %>%
  add_trace(y = ~data_3, name = 'data_3', 
            line = list(color = 'rgb(22, 96, 167)', width = 4, 
                        dash = 'dash')) %>% #虚线
  add_trace(y = ~data_4, name = 'data_4', 
            line = list(color = 'rgb(205, 12, 24)', width = 4,
                        dash = 'dot')) %>% #点
  add_trace(y = ~data_5, name = 'data_5', 
            line = list(color = 'rgb(22, 96, 167)', width = 4, 
                        dash = 'dot')) %>%
  layout(title = "例子", #图名
         xaxis = list(title = "Months"), #横坐标名
         yaxis = list (title = "level")) #纵坐标名
p

##气泡图
#1.简单气泡图（三个定量变量）
#注：定性变量可调节气泡图颜色
#数据 
data<-mtcars
head(data)
data$disp1<-(data$disp/(pi*5)) 

#1.1默认颜色
p<-plot_ly(data,
           x=~wt,
           y=~mpg,
           text=~disp,
           type = 'scatter',
           marker = list(size = ~disp1, 
                         opacity = 0.5)
) %>%
  #opacity表示透明度设置。size放置第三个定量变量。
  # text表示显示的标签。
  layout(title = 'style bubble',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
p

#1.2改变颜色
p<-plot_ly(data,
           x=~wt,
           y=~mpg,
           text=~disp,
           type = 'scatter',
           mode = 'markers',
           color = 'rgb(255,174,185)',
           marker = list(size = ~disp1, 
                         opacity = 0.5)
)  %>%
  
  layout(title = 'style bubble',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE)
  )
p

#1.3设定渐进颜色
p <- plot_ly(data,x = ~wt, y = ~mpg, 
             text = ~disp, type = 'scatter',
             mode = 'markers', color = ~disp, 
             colors ="YlOrRd",
             marker = list(size = ~disp1, opacity = 0.5)) %>% 
  layout(title = '简单气泡图',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
p
# color = ~定量变量名
#colors的选项："YlOrRd"  "Blues" 'Greens'"Reds"

#1.4设定颜色（按照属性变量设定）
#颜色按照给定的类别（新增属性变量）来分配
str(data)
data$gear = as.factor(data$gear)

p <- plot_ly(data,x = ~wt, y = ~mpg, 
             text = ~disp,type = 'scatter', 
             mode = 'markers', size = ~disp1, 
             color = ~gear, colors = 'Paired',
             marker = list(opacity = 0.5,
                           sizemode = 'diameter')) %>%
  layout(title = '简单气泡图',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
p

#2.气泡图的缩放问题：
#调整气泡的大小，除去对定量变量进行数据变换调整气泡大小外，还可以设定气泡的范围变化
#比如上图的气泡太大了，影响图片整体美观，就需要对气泡进行缩放处理。

str(data)
data$gear = as.factor(data$gear)

p <- plot_ly(data,x = ~wt, y = ~mpg, text = ~disp,
             type = 'scatter', mode = 'markers',
             size = ~disp1, color = ~gear, colors = 'Paired',
             sizes = c(10, 50),#选择气泡图的大小范围。
             marker = list(opacity = 0.5, 
                           sizemode = 'diameter')) %>%
  layout(title = '简单气泡图',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
p

#3.给气泡图打标签
p <- plot_ly(data,x = ~wt, y = ~mpg, 
             type = 'scatter', mode = 'markers', 
             size = ~disp1, color = ~gear, colors = 'Paired',
             sizes = c(10, 50),
             marker = list(opacity = 0.5, sizemode = 'diameter'),
             hoverinfo = 'text',
             text = ~paste('Displacement:', 
                           disp, '<br>Weight:',
                           wt,'<br>Miles/(US) gallon:',
                           mpg,'<br>gear:',gear)) %>%
  layout(title = '简单气泡图',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE),
         showlegend = FALSE)
p
#<br>表示自动换行

##条形图
#1.基础条形图
p <- plot_ly(
  #分别设定X轴，y轴包含的数据内容
  x = c("giraffes", "orangutans", "monkeys"),
  y = c(20, 14, 23),
  name = "SF Zoo",
  #图表类型是条形图
  type = "bar"
)
p

#2.分组条形图
Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
#用data.frame()函数生成一个数据框，使各变量变成数据框的成分
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
  #使用add_trace函数往原图上叠加不同的数据组
  add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
  #barmode用来设置多个条形图的排布模式，可以是group——分组，stack——叠加，或者overlay——重叠等
  layout(yaxis = list(title = 'Count'), barmode = 'group')
p

#3.堆积条形图
Animals <- c("giraffes", "orangutans", "monkeys")
SF_Zoo <- c(20, 14, 23)
LA_Zoo <- c(12, 18, 29)
data <- data.frame(Animals, SF_Zoo, LA_Zoo)

p <- plot_ly(data, x = ~Animals, y = ~SF_Zoo, type = 'bar', name = 'SF Zoo') %>%
  add_trace(y = ~LA_Zoo, name = 'LA Zoo') %>%
  #这里的方法和分组条形图类似，只需要把barmode改成"stack"
  layout(yaxis = list(title = 'Count'), barmode = 'stack')
p

#4.加入悬停文本
#x轴表示产品的种类，y表示销量
x <- c('Product A', 'Product B', 'Product C')
y <- c(20, 14, 23)
#使用text()函数给图像添加文字，比如Product B在1月的销量是14，占总销售额的14%
text <- c('27% market share', '24% market share', '19% market share')
data <- data.frame(x, y, text)

p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = text,
             #设置填充的颜色以及边缘线的颜色和宽度
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
  #设置图表标题等基本参数
  layout(title = "January 2013 Sales Report",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

#5.加入悬停文本和直接标签
x <- c('Product A', 'Product B', 'Product C')
y <- c(20, 14, 23)
text <- c('27% market share', '24% market share', '19% market share')
data <- data.frame(x, y, text)

p <- plot_ly(data, x = ~x, y = ~y, type = 'bar', 
             #这时每个bar顶部会出现各自对应的销售额数字，直接方便地看到它对应的具体数值
             text = y, textposition = 'auto',
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
  layout(title = "January 2013 Sales Report",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p

##饼图
#1.基础饼图
USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]
#查看数据
data

p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         #showgrid=FALSE是在设定是否显示网格线，zeroline=FALSE 设定的是当数值为零的时候是否显示，showticklabels=FALSE设置是否显示刻度
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

#2.个性化饼图
USPersonalExpenditure <- data.frame("Categorie" = rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[, c('Categorie', 'X1960')]

#更改颜色的设置
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')

p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie',
             #文字的位置放在图像的内部
             textposition = 'inside',
             #设置文本标签的显示内容为标签名+百分比
             textinfo = 'label+percent',
             #设置内置文本的字体格式参数
             insidetextfont = list(color = '#FFFFFF'),
             #当鼠标与图标交互时，指针所显示的参数，这里显示的是text的内容
             hoverinfo = 'text',
             #tag内容
             text = ~paste('$', X1960, ' billions'),
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #showlegend=FALSE设置是否显示图例名称
             showlegend = FALSE) %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

##环形图
#查看数据
mtcars

# Get Manufacturer
#使用strsplit函数对mtcars数据集的rownames按照" ",即空格进行分割
#使用sapply函数提取分割后内容中的第1个位置的元素
mtcars$manuf <- sapply(strsplit(rownames(mtcars), " "), "[[", 1)

p <- mtcars %>%
  #把mtcars按照manuf分组并计数
  group_by(manuf) %>%
  summarize(count = n()) %>%
  plot_ly(labels = ~manuf, values = ~count) %>%
  #环形图的特征所在，加入一个半径为0.6单位的饼图，使得整幅图被挖去一部分，变成一个环形图
  add_pie(hole = 0.6) %>%
  #显示了图例名称showlegend = TRUE

