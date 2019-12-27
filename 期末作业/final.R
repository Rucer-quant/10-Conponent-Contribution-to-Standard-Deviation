library(RMySQL)
library(tidyverse)
library(ggplot2)
library(xts)
library(DBI)
library(RMySQL)
library(dplyr)
library(plotly)
library(purrr)
library(quantmod)
library(treemapify)
library(ggpubr)
library(httr)
library(xts)
library(zoo)
library(forecast)
library(Tushare)
library(RColorBrewer)

today = format(Sys.Date(),"%Y%m%d")
tushare <- Tushare::pro_api(token = "6c79c582657805374a1d2296a88e25f510bde4b5d34c0f4cadc29338")

###蜡烛图
candlestick <- function(stock_code="000001.SH",start_date = '20190101', end_date = today) {
  Index <- tushare(api_name = "index_daily", ts_code = stock_code , start_date = start_date, end_date = today,
                       fields='trade_date,open,high,low,close')
  Index$trade_date<-as.Date(Index$trade_date,"%Y%m%d")
  #设置涨跌颜色
  i <- list(line = list(color = '#f6416c'))
  d <- list(line = list(color = '#7bc0a3'))
  SSE_candlestick <- Index %>%
    plot_ly(x = ~trade_date, type="candlestick",
            open = ~open, close = ~close,
            high = ~high, low = ~low, 
            increasing=i,
            decreasing=d) %>%
    layout(title = "SSE Composite Index")
  SSE_candlestick
  
}
candlestick()

### 300行业指数柱状图


barchart <-function(start_date = as.character(as.integer(today)-5),
                    end_date =  as.character(as.integer(today)-4),
                    industy_index_code=c('000908.SH','000909.SH','000910.SH',
                                         '000912.SH','000913.SH','000914.SH',
                                         '000915.SH','000916.SH','000917.SH'),
                    row_names = c("Energy","Materials","Industrials",
                                  "Cons Staples","Health Care","Financials",
                                  "Info Tech","Telecom Svc","Utilities")
                    ){
  
  industy_r <- data.frame()
  #9个股票的收益率
  for (code in industy_index_code){
    ind_index <- tushare(api_name = "index_daily", ts_code = code, start_date='20180830',end_date =today)
    
    industy_r[code,"returns"] <- ind_index$close[2]/ind_index$close[1] - 1
    
  }
  row.names(industy_r)<- row_names
  
  color <- function (x){
    if (x > 0){
      return(sprintf("rgb(%d,0,0)",floor(255*(1-exp(-x*30)))));
    }
    else if (x<=0){
      return(sprintf("rgb(0,%d,0)",ceiling(255*(1-exp(x*30)))));
    }
  }
  
  industy_r["color"] = map_chr(industy_r$returns, color)
  industy_barchart <- plot_ly(y = reorder(row.names(industy_r), industy_r$returns), 
                                  x = abs(industy_r$returns),
                                  marker =list(color = industy_r$color),
                                  type = 'bar',orientation = 'h') %>%
  add_annotations(xref = 'x1', yref = 'y',
                    x = abs(industy_r$returns)+0.002 ,  y = row.names(industy_r),
                    text = paste(round(industy_r$returns*100, 2), '%'),
                    font = list(family = 'Arial', size = 12, color = 'rgb(0, 0, 0)'),
                    showarrow = FALSE)
  
  return(industy_barchart)
}
barchart()


#抽取SSE50指数成分股code
SSE50_constituents <- read.csv("//Mac/Home/Documents/中国人民大学/风险计量/R语言学习/R_final_project-master/000016closeweight.csv")
SSE50_codes <- paste0(SSE50_constituents$Constituent.Code,".SH")
SSE50_names <- SSE50_constituents$Constituent.Name
SSE50_basics <- data.frame()
SSE50 <- data.frame()

for (code in SSE50_codes){
  stk <- tushare(api_name = "daily", ts_code = code, trade_date = return_cal_dates[1])
  stk_mv <- tushare(api_name = "daily_basic", ts_code = code,  trade_date = return_cal_dates[1])
  SSE50[code,"returns"] <- stk$close/stk$pre_close -1 
  SSE50[code,"vol"] <- stk$vol
  SSE50[code,"mv"] <- stk_mv$total_mv
  SSE50_basics <- rbind(SSE50_basics,stk_mv)
}
#两位小数
SSE50$names <- paste0(SSE50_names,'\n',round(SSE50$returns*100,2),'%')
SSE50_basics_new <- data.frame(row.names = 1:50)
SSE50_basics_new$Code <- SSE50_codes
SSE50_basics_new$Company <- SSE50_names
SSE50_basics_new$Exchange <-           $Exchange
SSE50_basics_new["Market Cap"] <- SSE50_basics$total_mv
SSE50_basics_new["P/E"] <- SSE50_basics$pe
SSE50_basics_new$Price <- SSE50_basics$close
SSE50_basics_new$Change <- paste0(round(SSE50$returns*100,2),"%")
SSE50_basics_new$Volume <- SSE50$vol

### 树图
treemap <- function(data=SSE50){
  
  color <- function (x){
    if (x > 0){
      return(sprintf("rgb(%d,0,0)",floor(255*(1-exp(-x*30)))));
    }
    else if (x<=0){
      return(sprintf("rgb(0,%d,0)",ceiling(255*(1-exp(x*30)))));
    }
  }
  
  rgb2hex <- function(rgb){
    rgb <- strsplit(substr(rgb,5,nchar(rgb)-1),',')[[1]]
    rgb <- as.integer(rgb)
    rgb <- as.character(as.hexmode(rgb))
    hex <- "#"
    for (i in rgb){
      if (nchar(i) == 1){i <- paste0('0',i)} 
      hex <- paste0(hex, i)
    }
    return(hex)
  }
  
  
  data["color"] = map_chr(data$returns, color) %>% map_chr(rgb2hex)
  ggplot(data, aes(area = mv, label = names)) +
    geom_treemap(fill = data$color) +
    geom_treemap_text(fontface = "bold", colour = "white", place = "centre",
                      grow = TRUE) + 
    ggtitle("上证50成分股涨跌情况")
  
}

treemap()




###表格
chart<-function(data=SSE50_basics_new){
  data <- head(data,5)
  tbody.style = tbody_style(color = "black",
                            fill = c("#bdbdbd","#ededed"), hjust=1, x=0.9)
  ggtexttable(data,
              theme = ttheme(
                colnames.style = colnames_style(color = "white", fill = "#7d7d7d"),
                tbody.style = tbody.style
              ))
  
}
chart()

##趋势图

line_chart <- function(code='000002.SZ'){
  
  Wanke <- tushare(api_name = "daily", ts_code = '000002.SZ', start_date = '20190405', end_date = return_cal_dates[1])
  Wanke_return = Wanke$close[nrow(Wanke)]/Wanke$pre_close[nrow(Wanke)] - 1 
  
  Wanke_name <- list(
    xref = 'paper',
    yref = 'paper',
    x = 0.23,
    y = 0.99,
    xanchor = 'right',
    yanchor = 'middle',
    text = '万科A\n',
    font = list(family = '楷体',
                size = 20,
                color = '#efefef'),
    showarrow = FALSE)
  Wanke_price <- list(
    xref = 'paper',
    yref = 'paper',
    x = 0.20,
    y = 0.85,
    xanchor = 'right',
    yanchor = 'middle',
    text = ~as.character(Wanke$close[nrow(Wanke)]),
    font = list(family = 'Arial',
                size = 26,
                color = '#efefef'),
    showarrow = FALSE)
  
  Wanke_change <- list(  
    xref = 'paper',
    yref = 'paper',
    x = 0.84,
    y = 0.98,
    xanchor = 'right',
    yanchor = 'middle',
    text = ~paste0(round(Wanke_return*100,2),'%'),
    font = list(family = 'Arial',
                size = 20,
                color = '#efefef'),
    showarrow = FALSE)
  
  Wanke_HL <- list(  
    xref = 'paper',
    yref = 'paper',
    x = 0.84,
    y = 0.80,
    xanchor = 'right',
    yanchor = 'middle',
    text = ~paste0("H ",Wanke$high[nrow(Wanke)],"\nL ",Wanke$low[nrow(Wanke)]),
    font = list(family = 'Arial',
                size = 16,
                color = '#cfcfcf'),
    showarrow = FALSE)
  
  Wanke_plot <- plot_ly(Wanke, y=~close,x=~paste0(substr(trade_date,5,6),'-',substr(trade_date,7,8)),
                         type = 'scatter', mode = 'lines',
                         line = list(color = '#adadad')) %>%
    layout(paper_bgcolor=to_color(Wanke_return), plot_bgcolor=to_color(Wanke_return),
           xaxis = list(title = ""),
           yaxis = list(title = "", range=c(min(Wanke$close),max(Wanke$close)*1.5 - 0.5*min(Wanke$close))),
           margin =list(autoexpand = TRUE, r=10,l=10))%>%
    layout(annotations = Wanke_name)%>%
  layout(annotations = Wanke_price)%>%
    layout(annotations = Wanke_change)%>%
    layout(annotations = Wanke_HL)
  return(Wanke_plot)
}
line_chart()
