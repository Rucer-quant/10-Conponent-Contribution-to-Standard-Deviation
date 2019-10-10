
---
  title: "Returns Distribution"
runtime: shiny
output:
  flexdashboard::flex_dashboard:
  orientation: rows
source_code: embed
---
library(rmarkdown)
library(flexdashboard)
library(purrr) 
library(dplyr)
library(ggplot2)
library(tidyr)
library(DBI)
library(quantmod)
library(PerformanceAnalytics) 
library(tibble) #remove_rownames()
library(lubridate) #ymd
library(timetk)
library(highcharter)
library(DBI)
library(RMySQL)
library(stringi)
library(data.table)
library(shiny)
library(shinydashboard)



#creat sidebar
fluidRow(
  column(6,textInput("stock1","Stock 1","SPY")),
  column(5,numericInput("w1","Portf .%",25,min = 1,max = 100))
  ),
fluidRow(
  column(6,textInput("stock2","Stock 2","EFA")),
  column(5,numericInput("w2","Portf .%",25,min = 1,max = 100))
),
fluidRow(
  column(6,textInput("stock3","Stock 3","IJS")),
  column(5,numericInput("w3","Portf .%",20,min = 1,max = 100))
),
fluidRow(
  column(6,textInput("stock4","Stock 4","EEM")),
  column(5,numericInput("w4","Portf .%",20,min = 1,max = 100))
),
fluidRow(
  column(6,textInput("stock5","Stock 5","AGG")),
  column(5,numericInput("w5","Portf .%",10,min = 1,max = 100))
),

fluidRow(
  column(7,selectInput("rebalance","rebal freq",
                       c("Yearly"="years","Monthly"="months",
                         "Weekly"="weeks")))
),
actionButton("go","Submit")

#获得数据并转化成收益率
asset_returns_dplyr_byhand <- eventReactive(input$go,{
  symbols <- c(input$stock1,input$stock2,  #输入的股票代码
               input$stock3,input$stock4,input$stock5)
  prices <-getSymbols(symbols,src = 'yahoo',from = input$date_start,#输入始末时间带入                    
                      to = input$date_end,auto.assign = TRUE,warnings = FALSE)%>%
  map(~Ad(get(.)))%>%
  reduce(merge)%>%
  `colnames<-`(symbols)
asset_returns_dplyr_byhand <-
  prices%>%
  to.monthly(indexAt = "lastof",OHLC = FALSE)%>%
  tk_tbl(preserve_index = TRUE,rename_index = "date")%>%
  gather(asset,returns,-date)%>%
  group_by(asset)%>%
  mutate(returns = log(returns)- log(lag(returns) ))%>%
  spread(asset,returns)%>%
  select(date,symbols)%>%
  slice(-1)
})

#计算每个资产方差贡献的funtion
#calculate component contribution/percentages
component_contr_matrix_fun <- eventReactive(input$go,{
function(return,w){
  # create covariance matrix
  covariance_matrix = cov(return)
  # calculate portfolio standard deviation
  sd_portfolio<-sqrt(t(w)%*%covariance_matrix%*%w)
  # calculate marginal contribution of each asset
  margin_contribution<- w%*%covariance_matrix/sd_portfolio[1,1]
  # multiply marginal by weights vecoter
  conponent_contribution<-margin_contribution*w
  # divide by total standard deviation to get percentages
  conponent_percentages<-conponent_contribution/sd_portfolio[1,1]
  #convert the type
  conponent_percentages%>%
    as_tibble()%>%
    gather(asset,contribution)
}})

#计算窗口期间的资产贡献的function
##function for calculating interval sd
interval_sd_by_hand<- eventReactive(input$go,{
  function(return_df,start=1, window=24 ,weights){
  #funtion 参数赋值是默认值
  # start =1
  # window<-24
  # return_df<-asset_returns_dplyr_byhand
  start_date <-
    return_df$date[start]
  end_date <-
    return_df$date[c(start+window)]
  return_to_use <-
    filter(return_df,date>=start_date&
             date<end_date)%>%
    select(-date)
  w<-weights
  component_percentages <- component_contr_matrix_fun(return_to_use,w)
  results_with_date <-
    component_percentages%>%
    mutate(date = ymd(end_date))%>%
    select(date,everything())%>%
    spread(asset,contribution)%>%
    mutate_if(is.numeric,function(x) x*100)
}})


# 从整体的角度来看各个资产的标准差贡献
percentages_tibble_pre_built <- eventReactive(input$go,{
  asset_return_xts <-asset_returns_dplyr_byhand%>%
    tk_xts(date_col = date)
  w <- c(input$w1/100,input$w2/100, input$w3/100, input$w4/100, input$w5/100)
  port_vol_contr_total_builtin <-
  StdDev(asset_returns_xts, weights = w, portfolio_method = "component")
  percentages_tibble_pre_built <-
  port_vol_contr_total_builtin$pct_contrib_StdDev %>%
    tk_tbl(preserve_index = FALSE) %>% 
    mutate(asset = symbols) %>% 
    rename('risk contribution' = data) %>% 
  mutate(`risk contribution` =
          round(`risk contribution`, 4) * 100,w * 100) %>% 
  select(asset, everything()) #将asset换位到前面  最后生成3列 asset /contribution/weights
})


#滚动的标准差
portfolio_vol_components_xts <-eventReactive(input$go,{
  asset_returns_dplyr_byhand <- asset_returns_dplyr_byhand()
  w <- c(input$w1/100,input$w2/100, 
         input$w3/100, input$w4/100, input$w5/100)
  ##rolling function
  portfolio_vol_component_tigy_by_hand<-
    map_df(1:(nrow(asset_returns_dplyr_byhand)-window),
           interval_sd_by_hand,
           return_df = asset_returns_dplyr_byhand,
           weights=w,
           window= window)%>%
    tk_xts(date_col = date)
})

#画24个月为窗口的折线图
renderHighchart({
  portfolio_vol_components<-
    portfolio_vol_components_xts()
  highchart(type = "stock")%>%
    hc_title(text = "Volatility Contribution")%>%
    hc_add_series(portfolio_vol_components[,1],
                  name =names(portfolio_vol_components[,1]))%>%
    hc_add_series(portfolio_vol_components[,2],
                  name =names(portfolio_vol_components[,2]))%>%
    hc_add_series(portfolio_vol_components[,3],
                  name =names(portfolio_vol_components[,3]))%>%
    hc_add_series(portfolio_vol_components[,4],
                  name =names(portfolio_vol_components[,4]))%>%
    hc_add_series(portfolio_vol_components[,5],
                  name =names(portfolio_vol_components[,5]))%>%
    hc_add_theme(hc_theme_flat())%>%
    hc_yAxis(
      labels =list(format = "{value}%"),
      opposite = FALSE,
      min = min(portfolio_vol_components)-5,
      max = max(portfolio_vol_components)+5)%>%
    hc_navigator(enabled = FALSE)%>%
    hc_scrollbar(enabled = FALSE)
})

##堆积面积图
renderHighchart({
  portfolio_vol_components <-
    portfolio_vol_components_xts()
  
  highchart()%>%
    hc_chart(type = "area")%>%
    hc_title(text = "Stack Volatility Contribution")%>%
    hc_plotOptions(area = list(
      stacking ="percent",
      lineColor = "#ffffff",
      lineWidth = 1,
      marker = list(
        lineWidth =1,
        lineColor = "#ffffff"
      ))
                   )%>%
    hc_add_series(portfolio_vol_components[,1],
                  name=names(portfolio_vol_components[,1]))%>%
    hc_add_series(portfolio_vol_components[,2],
                  name=names(portfolio_vol_components[,2]))%>%
    hc_add_series(portfolio_vol_components[,3],
                  name=names(portfolio_vol_components[,3]))%>%
    hc_add_series(portfolio_vol_components[,4],
                  name=names(portfolio_vol_components[,4]))%>%
    hc_add_series(portfolio_vol_components[,5],
                  name=names(portfolio_vol_components[,5]))%>%
    hc_xAxis(type = "datetime")%>%
    hc_yAxis(labels = list(format = "{value}%"),
             opposite = FALSE)%>%
    hc_tooltip(
      "<span style=\"color:{series.color}\"> {series.name}</span>:<b>{point.percentage:.1f}%</b><br/>",
      shared =TRUE)%>%
    hc_navigator(enabled = FALSE)%>%
    hc_scrollbar(enabled =FALSE)%>%
    hc_add_theme(hc_theme_flat())%>%
    hc_exporting(enabled = TRUE)%>%
    hc_legend(enabled = TRUE)
})

#柱状图
renderPlot({
  percentages_tibble_pre_built()%>%
    gather(type,percent,-asset)%>%
    group_by(type)%>%
    mutate(percent = percent/100)%>%
    ggplot(aes(x = asset,y = percent,fill = type))+
    geom_col(position = 'dodge')+
    scale_y_continuous(labels = percent)+
    ggtitle("Percent Contribution to Volatility")+
    theme(plot.title = element_text(hjust = 0.5))
})


