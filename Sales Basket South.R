Sales_Basket_South <- function()
{
  # Determining the top 'k' itemset comprising of 'n' elements
  # Reporting Based on Transaction i.e. basket analysis for each transaction
  ## Creating a function that will generate entire data and reports
  
  #load dbms package
  library("RODBC", lib.loc="~/R/win-library/3.2")
  library("RODM", lib.loc="~/R/win-library/3.2")
  
  #load arule package
  library("arules", lib.loc="~/R/win-library/3.2")
  library("arulesViz", lib.loc="~/R/win-library/3.2")
  
  #loading rattle and other graphical packages
  library("ggplot2", lib.loc="C:/Program Files/R/R-3.2.0/library")
  library("reshape", lib.loc="C:/Program Files/R/R-3.2.0/library")
  library("rattle", lib.loc="C:/Program Files/R/R-3.2.0/library")
  
  #Connecting to database
  channel1 <- odbcConnect("Oracle", uid="system", pwd="011235")
  
  
  ##storing required column values from table into a matrix
  sales_week <- sqlQuery(channel1,"select ORDER_ID, PRODUCT_NAME from 
                         south_oltp where order_week = 16 group by PRODUCT_NAME,ORDER_ID order 
                         by ORDER_ID")
  
  #removing duplicate rows
  sales_week_unique <- sales_week[!duplicated(sales_week), ]
  
  #splitting dataframe into transaction and itemset
  split_sales_weekly <- split(sales_week_unique$PRODUCT_NAME,
                              sales_week_unique$ORDER_ID)
  
  #creating transaction table
  txns_daily <- as(split_sales_weekly,"transactions")
  
  #applying apriori
  rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.05,conf=0.9, minlen=3,
                                                       maxlen = 4))
  rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05, minlen=3, 
                                                              maxlen = 4))
  
  
  #inspect basket rules
  rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
  rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
  
  
  #Plotting lift vs support vs confidence
  rep_1_c <- plot(rule_txns_daily[1:10], method = "grouped",
                  control=list(main = "Highest Lift Itemset - Grouped Plot"),
                  measure=c("support","lift"), 
                  shading="confidence")
  rep_1_d <- plot(rule_txns_daily_elcat[1:10],
                  control=list(main="Highest Frequenct Itemset - Bubble Plot"))
  rep_1_e <- plot(rule_txns_daily_elcat[1:10], method="paracoord",
                  control=list(main="Highest Frequency Itemset - Parallel Coordinate Plot",reorder=TRUE))
  
  ## Extracting counts of order for top ten itemset per month
  
  
  y1 <- sqlQuery(channel1,"select sum(quantity) Aprilbonanza_Promo_4 from south_oltp
                 where order_month = 4 and product_id LIKE 'P2' and order_id
                 in(select order_id from south_oltp where product_id
                 LIKE 'P17') group by order_week order by order_week")
  
  y2 <- sqlQuery(channel1,"select sum(quantity) Aprilbonanza_Promo_5 from south_oltp
                 where order_month = 4 and product_id LIKE 'P7' and order_id
                 in(select order_id from south_oltp where product_id
                 LIKE 'P6') group by order_week order by order_week")
  
  y3 <- sqlQuery(channel1,"select sum(quantity) Aprilbonanza_Promo_7 from south_oltp
                 where order_month = 4 and product_id LIKE 'P19' and order_id
                 in(select order_id from south_oltp where product_id
                 LIKE 'P3') group by order_week order by order_week")
  
  
  
  z1 <- sqlQuery(channel1,"select count(customer_id) Aprilbonanza_Promo_4 from south_oltp
                 where order_month = 4 and product_id LIKE 'P2' and order_id
                 in(select order_id from south_oltp where product_id
                 LIKE 'P17') group by order_week order by order_week")
  
  z2 <- sqlQuery(channel1,"select count(customer_id) Aprilbonanza_Promo_5 from south_oltp
                 where order_month = 4 and product_id LIKE 'P7' and order_id
                 in(select order_id from south_oltp where product_id
                 LIKE 'P6') group by order_week order by order_week")
  
  
  z3 <- sqlQuery(channel1,"select count(customer_id) Aprilbonanza_Promo_7 from south_oltp
                 where order_month = 4 and product_id LIKE 'P19' and order_id
                 in(select order_id from south_oltp where product_id
                 LIKE 'P3') group by order_week order by order_week")
  
  
  
  ##creating month number through sequence
  
  x <- matrix(seq(14,18,1))
  
  ##Creating Dataframes and convering them into matrix form from list
  
  df1 <- data.frame(x,y1,y2,y3)
  
  df2 <- melt(df1, id.vars = "x")
  
  df3 <- data.frame(x,z1,z2,z3)
  
  df4 <- melt(df3, id.vars = "x")
  
  ##creating plots using ggplot2
  
  ##plots of top 5 itemset based on number of order per month
  y <- ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()
  # providing title to the plot
  y <- y + labs(title = "Quantity of purchase on promo items: A comparison")
  # providing axis names
  y <- y + labs(x = "Weeks", y="Quantity of purchase")
  #providing smoothness to curves
  y <- y + stat_smooth(size = 1.5)
  
  ##similar plotting for other criteria
  z <- ggplot(data = df4, aes(x = x, y = value, colour = variable)) + geom_line()
  z <- z + labs(title = "Number of orders on promo items: A comparison")
  z <- z + labs(x = "Month", y="Number of Order")
  z <- z + stat_smooth(size = 1.5)
  
  return(list(rep_1_a, rep_1_b, rep_1_c, rep_1_c, rep_1_e, y, z))
}
