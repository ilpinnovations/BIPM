South_Customer <- function()
{
  #loading arules and arulesviz
  library("arules")
  library(arulesViz)
  
  # loading RODM and RODBC package
  library(RODM)
  library(RODBC)
  
  #creating a function for rowmatch
  rowmatch <- function(A,B)
  { 
    # Rows in A that match the rows in B 
    f <- function(...) paste(..., sep=":") 
    if(!is.matrix(B)) B <- matrix(B, 1, length(B)) 
    a <- do.call("f", as.data.frame(A)) 
    b <- do.call("f", as.data.frame(B)) 
    match(b, a) 
  } 
  
  # Create a connection to the database called "channel"
  channel <- odbcConnect("ORACLE", uid="SYSTEM", pwd="011235")
  
  
  Customer_id <- sqlQuery(channel, "SELECT DISTINCT(CUSTOMER_ID) FROM SOUTH_OLTP
                          ORDER BY CUSTOMER_ID")
  m <- data.frame(Customer_id)
  x <- readline("Enter Customer_id(From C36 to C70) or press esc to cancel  :")
  y <- rowmatch(m,x)
  if (y == 1) 
  {
    ##splitting data into categories to identify basket
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C36' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    
    #removing duplicate rows
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    #splitting dataframe into transaction and itemset
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    
    #creating transaction table
    txns_daily <- as(split_dataset_daily,"transactions")
    
    #applying apriori
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05, minlen=3, 
                                                                maxlen = 4))
    
    #inspect basket rules
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    
    #load arule vizualizaton package
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    
    #Plotting lift vs support vs confidence
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C36 (Shashi) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord",
                    control=list(main="Highest Frequency Itemset For C36 (Shashi) - Parallel Coordinate Plot",reorder=TRUE))
    
  } else if (y == 2) 
  {
    ##splitting data into categories to identify basket
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C37' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    
    #removing duplicate rows
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    #splitting dataframe into transaction and itemset
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    
    #creating transaction table
    txns_daily <- as(split_dataset_daily,"transactions")
    
    #applying apriori
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    
    #inspect basket rules
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    
    #load arule vizualizaton package
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    
    #Plotting lift vs support vs confidence
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C37 (Jai) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord",
                    control=list(main="Highest Frequency Itemset for C37 (Jai) - Parallel Coordinate Plot",reorder=TRUE))
    
  } else if (y == 3) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C38' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C38(Riya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C38(Riya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 4) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C39' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C39(Manish) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C39(Manish) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 5) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C40' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C40(Savitri) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C40(Savitri) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 6) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C41' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.5, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05,minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C41(Jagdish) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C41(Jagdish) - Parallel Coordinate Plot",reorder=TRUE)[1:10])
  } else if (y == 7) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C42' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen = 3,
                                                         maxlen = 4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat[1:7])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset for C42(Mohan) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C42(Mohan) - Parallel Coordinate Plot",reorder=TRUE)[1:7])
  } else if (y == 8) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C43' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C43(Rickey) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C43(Rickey) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 9) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C44' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C44(Ashutosh) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset C44(Ashutosh) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 10) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C45' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C45(Keshav) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C45(Keshav) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 11) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C46' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C46(Ashish) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C46(Ashish) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 12) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C47' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C47(Nivedita) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C47(Nivedita) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 13) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C48' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C48(Sneha) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C48(Sneha) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 14) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C49' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C49(Yamini)- Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C49(Yamini) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 15) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C50' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C50(Shiuli) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C50(Shiuli) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 16) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C51' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C51(Alisha) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C51(Alisha) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 17) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C52' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C52(Pooja) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C52(Pooja) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 18) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C53' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C53(Ramya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C53(Ramya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 19) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C54' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C54(Aham) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C54(Aham) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 20) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C55' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C55(Mounika) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C55(Mounika) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 21) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C56' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C56(Shazid) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C56(Shazid) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 22) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C57' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C57(Mayank) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C57(Mayank) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 23) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C58' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C58(Himanshu) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C58(Himanshu) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 24) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C59' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C59(Deeksha) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C59(Deeksha) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 25) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C60' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C60(Swati) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C60(Swati) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 26) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C61' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C61(Mahesh) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C61(Mahesh) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 27) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C62' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C62(Rajnish) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C62(Rajnish) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 28) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C63' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C63(Amita) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C63(Amita) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 29) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C64' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C64(Kratika) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C64(Kratika) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 30) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C65' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C65(Tiya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C65(Tiya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 31) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C66' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C66(Priya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C66(Priya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 32) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C67' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C67(Sanjali) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C67(Sanjali) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 33) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C68' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C68(Prateek) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C68(Prateek) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 34) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C69' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3, maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, maxlen=4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C69(Sunil) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C69(Sunil) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 35) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from SOUTH_OLTP WHERE CUSTOMER_ID='C70' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C70(Swapna) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C70(Swapna) - Parallel Coordinate Plot",reorder=TRUE))
  } else {"Error: Invalid Entry"}
  South_Customer()
}