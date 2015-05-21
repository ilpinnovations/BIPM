# Determining the top 'k' itemset comprising of 'n' elements
# Reporting Based on Transaction i.e. basket analysis for each transaction
## Creating a function that will generate entire data and reports
South_Transaction <- function()
{
  #load dbms package
  library("RODBC", lib.loc="~/R/win-library/3.2")
  library("RODM", lib.loc="~/R/win-library/3.2")
  
  #load arule package
  library("arules", lib.loc="~/R/win-library/3.2")
  
  #Connecting to database
  channel1 <- odbcConnect("Oracle", uid="system", pwd="011235")
  
  
  ##storing required column values from table into a matrix
  dataset_daily <- sqlQuery(channel1,"select ORDER_ID, PRODUCT_NAME from 
                            south_oltp group by PRODUCT_NAME,ORDER_ID order 
                            by ORDER_ID")
  
  #removing duplicate rows
  dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
  
  #splitting dataframe into transaction and itemset
  split_dataset <- split(dataset_daily_unique$PRODUCT_NAME,
                         dataset_daily_unique$ORDER_ID)
  
  #creating transaction table
  txns_daily <- as(split_dataset,"transactions")
  
  #applying apriori
  rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.05,conf=0.9, minlen=3,
                                                       maxlen = 4))
  rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05, minlen=3, 
                                                              maxlen = 4))
  
  
  #inspect basket rules
  rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
  rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
  
  #load arule vizualizaton package
  library("arulesViz", lib.loc="C:/Program Files/R/R-3.2.0/library")  
  
  #Plotting lift vs support vs confidence
  rep_1_c <- plot(rule_txns_daily[1:10], method = "grouped",
                  control=list(main = "Highest Lift Itemset - Grouped Plot"),
                  measure=c("support","lift"), 
                  shading="confidence")
  rep_1_d <- plot(rule_txns_daily_elcat[1:10],
                  control=list(main="Highest Frequenct Itemset - Bubble Plot"))
  rep_1_e <- plot(rule_txns_daily_elcat[1:10], method="paracoord",
                  control=list(main="Highest Frequency Itemset - Parallel Coordinate Plot",reorder=TRUE))
  rep_1_f <- plot(rule_txns_daily[1:10], method = "matrix",
                  control=list(main = "Highest Lift Itemset - Matrix Plot", type = "grid", reorder = TRUE),
                  measure=c("support","lift"), 
                  shading="confidence")
}
