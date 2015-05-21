Product_Behaviour_South <- function()
{
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
  
  
  ## Extracting counts of order for top ten itemset per month
  y1 <- sqlQuery(channel1,"select count(order_id) SUGAR_GHEE_CHICKEN
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Chicken'))
                 group by order_month order by order_month")
  
  y2 <- sqlQuery(channel1,"select count(order_id) SUGAR_GHEE_FISH
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Fish'))
                 group by order_month order by order_month")
  
  y3 <- sqlQuery(channel1,"select count(order_id) SUGAR_GHEE_BUTTER
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Butter'))
                 group by order_month order by order_month")
  
  
  y4 <- sqlQuery(channel1,"select count(order_id) CHICKEN_BUTTER_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Chicken' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Butter' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  
  y5 <- sqlQuery(channel1,"select count(order_id) FISH_BUTTER_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Fish' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Butter' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  y6 <- sqlQuery(channel1,"select count(order_id) CHICKEN_FISH_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Chicken' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Fish' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  
  z1 <- sqlQuery(channel1,"select sum(quantity) SUGAR_GHEE_CHICKEN
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Chicken'))
                 group by order_month order by order_month")
  
  z2 <- sqlQuery(channel1,"select sum(quantity) SUGAR_GHEE_FISH
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Fish'))
                 group by order_month order by order_month")
  
  z3 <- sqlQuery(channel1,"select sum(quantity) SUGAR_GHEE_BUTTER
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Butter'))
                 group by order_month order by order_month")
  
  z4 <- sqlQuery(channel1,"select sum(quantity) CHICKEN_BUTTER_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Chicken' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Butter' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  z5 <- sqlQuery(channel1,"select sum(quantity) FISH_BUTTER_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Fish' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Butter' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  z6 <- sqlQuery(channel1,"select sum(quantity) CHICKEN_FISH_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Chicken' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Fish' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  c1 <- sqlQuery(channel1,"select count(customer_id) SUGAR_GHEE_CHICKEN
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Chicken'))
                 group by order_month order by order_month")
  
  c2 <- sqlQuery(channel1,"select count(customer_id) SUGAR_GHEE_FISH
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Fish'))
                 group by order_month order by order_month")
  
  c3 <- sqlQuery(channel1,"select count(customer_id) SUGAR_GHEE_BUTTER
                 from south_oltp where product_name LIKE 
                 'Sugar' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 
                 'Ghee' and order_id in (select order_id 
                 from south_oltp where product_name LIKE 'Butter'))
                 group by order_month order by order_month")
  
  
  c4 <- sqlQuery(channel1,"select count(customer_id) CHICKEN_BUTTER_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Chicken' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Butter' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  
  c5 <- sqlQuery(channel1,"select count(customer_id) FISH_BUTTER_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Fish' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Butter' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  c6 <- sqlQuery(channel1,"select count(customer_id) CHICKEN_FISH_SUGAR_GHEE from south_oltp where 
                 product_name LIKE 'Chicken' and order_month != 12 and order_id in(select order_id 
                 from south_oltp where product_name LIKE 'Fish' and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Sugar'and order_id in 
                 (select order_id from south_oltp where product_name LIKE 'Ghee'))) 
                 group by order_month order by order_month")
  
  
  
  
  ##creating month number through sequence
  
  x <- matrix(seq(1,6,1))
  
  ##Creating Dataframes and convering them into matrix form from list
  
  df1 <- data.frame(x,y1,y2,y3,y4,y5,y6)
  
  df2 <- melt(df1, id.vars = "x")
  
  df3 <- data.frame(x,z1,z2,z3,z4,z5,z6)
  
  df4 <- melt(df3, id.vars = "x")
  
  df5 <- data.frame(x,c1,c2,c3,c4,c5,c6)
  
  df6 <- melt(df5, id.vars = "x")
  
  ##creating plots using ggplot2
  
  ##plots of top 5 itemset based on number of order per month
  y <- ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()
  # providing title to the plot
  y <- y + labs(title = "No. of orders for Popular itemsets per month")
  # providing axis names
  y <- y + labs(x = "Month", y="Number of orders")
  #providing smoothness to curves
  y <- y + stat_smooth(size = 1.5)
  
  ##similar plotting for other criteria
  z <- ggplot(data = df4, aes(x = x, y = value, colour = variable)) + geom_line()
  z <- z + labs(title = "Quantity of items purchased in Popular itemsets per month")
  z <- z + labs(x = "Month", y="quantity of items")
  z <- z + stat_smooth(size = 1.5)
  
  
  c <- ggplot(data = df6, aes(x = x, y = value, colour = variable)) + geom_line()
  c <- c + labs(title = "No. of customers buying Popular itemsets per month")
  c <- c + labs(x = "Month", y="Number of customers")
  c <- c + stat_smooth(size = 1.5)
  
  
  return(list(y,z,c))
}