North_Customer <- function()
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
  
  
  Customer_id <- sqlQuery(channel, "SELECT DISTINCT(CUSTOMER_ID) FROM NORTH_OLTP
                          ORDER BY CUSTOMER_ID")
  m <- data.frame(Customer_id)
  x <- readline("Enter Customer_id(From C1 to C35) or press esc to cancel  :")
  y <- rowmatch(m,x)
  if (y == 1) 
  {
    ##splitting data into categories to identify basket
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C1' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    
    #removing duplicate rows
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    #splitting dataframe into transaction and itemset
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    
    #creating transaction table
    txns_daily <- as(split_dataset_daily,"transactions")
    
    #applying apriori
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=4,
                                                         maxlen=7))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05, minlen=3, 
                                                                maxlen = 4))
    
    #inspect basket rules
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    
    #load arule vizualizaton package
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    
    #Plotting lift vs support vs confidence
    rep_1_c <- plot(rule_txns_daily_elcat[1:7],
                    control=list(main="Highest Frequency Itemset For C1 (Divya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:7], method="paracoord",
                    control=list(main="Highest Frequency Itemset For C1 (Divya) - Parallel Coordinate Plot",reorder=TRUE))
    
  } else if (y == 2) 
  {
    ##splitting data into categories to identify basket
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C10' group by 
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
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:7])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:7])
    
    #load arule vizualizaton package
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    
    #Plotting lift vs support vs confidence
    rep_1_c <- plot(rule_txns_daily_elcat[1:7],
                    control=list(main="Highest Frequency Itemset for C10 (Niral) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:7], method="paracoord",
                    control=list(main="Highest Frequency Itemset for C10 (Niral) - Parallel Coordinate Plot",reorder=TRUE))
    
  } else if (y == 3) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C11' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05, minlen=3))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C11(Thomas) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C11(Thomas) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 4) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C12' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:7])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:7])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:7],
                    control=list(main="Highest Frequency Itemset for C12(Ali) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:7], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C12(Ali) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 5) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C13' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen=3,
                                                         maxlen=4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1, minlen=3, 
                                                                maxlen = 4))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:7])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:7])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:7],
                    control=list(main="Highest Frequency Itemset for C13(Riya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:7], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C13(Riya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 6) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C14' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.5))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C14(Ajay) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C14(Ajay) - Parallel Coordinate Plot",reorder=TRUE)[1:10])
  } else if (y == 7) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C15' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95, minlen = 3,
                                                         maxlen = 4))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat[1:7])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:7],
                    control=list(main="Highest Frequency Itemset for C15(Shubham) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C15(Shubham) - Parallel Coordinate Plot",reorder=TRUE)[1:7])
  } else if (y == 8) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C16' group by 
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
                    control=list(main="Highest Frequency Itemset for C16(Monika) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C16(Monika) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 9) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C17' group by 
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
                    control=list(main="Highest Frequency Itemset for C17(Christiine) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset C17(Christiine) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 10) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C18' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C18(Robin) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C18(Robin) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 11) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C19' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift")[1:10])
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset for C19(Priyanka) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset for C19(Priyanka) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 12) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C2' group by 
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
                    control=list(main="Highest Frequency Itemset For C2(Neeraj) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C2(Neeraj) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 13) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C20' group by 
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
                    control=list(main="Highest Frequency Itemset For C20(Joseph) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C20(Joseph) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 14) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C21' group by 
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
                    control=list(main="Highest Frequency Itemset For C21(David)- Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C21(David) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 15) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C22' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C22(Sophie) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C22(Sophie) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 16) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C23' group by 
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
                    control=list(main="Highest Frequency Itemset For C23(Mohit) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C23(Mohit) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 17) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C24' group by 
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
                    control=list(main="Highest Frequency Itemset For C24(Rohan) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C24(Rohan) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 18) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C25' group by 
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
                    control=list(main="Highest Frequency Itemset For C25(Smrite) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C25(Smrite) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 19) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C26' group by 
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
                    control=list(main="Highest Frequency Itemset For C26(Tushar) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C26(Tushar) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 20) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C27' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.05))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C27(Deeksha) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C27(Deeksha) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 21) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C28' group by 
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
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C28(Daksh) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C28(Daksh) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 22) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C29' group by 
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
                    control=list(main="Highest Frequency Itemset For C29(Nikki) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C29(Nikki) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 23) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C3' group by 
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
                    control=list(main="Highest Frequency Itemset For C3(Ayushi) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C3(Ayushi) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 24) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C30' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C30(Stephen) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C30(Stephen) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 25) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C31' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat[1:10])
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat[1:10],
                    control=list(main="Highest Frequency Itemset For C31(Ashu) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C31(Ashu) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 26) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C32' group by 
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
                    control=list(main="Highest Frequency Itemset For C32(Jyoti) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C32(Jyoti) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 27) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C33' group by 
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
                    control=list(main="Highest Frequency Itemset For C33(Daljeet) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C33(Daljeet) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 28) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C34' group by 
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
                    control=list(main="Highest Frequency Itemset For C34(Himanshu) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C34(Himanshu) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 29) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C35' group by 
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
                    control=list(main="Highest Frequency Itemset For C35(Sonali) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C35(Sonali) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 30) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C4' group by 
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
                    control=list(main="Highest Frequency Itemset For C4(Khyati) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C4(Khyati) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 31) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C5' group by 
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
                    control=list(main="Highest Frequency Itemset For C5(Shaurya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C5(Shaurya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 32) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C6' group by 
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
                    control=list(main="Highest Frequency Itemset For C6(Ayush) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C6(Ayush) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 33) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C7' group by 
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
                    control=list(main="Highest Frequency Itemset For C7(Kanak) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C7(Kanak) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 34) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C8' group by 
                              PRODUCT_NAME,ORDER_DATE order by ORDER_DATE")
    dataset_daily_unique <- dataset_daily[!duplicated(dataset_daily), ]
    
    split_dataset_daily <- split(dataset_daily_unique$PRODUCT_NAME,
                                 dataset_daily_unique$ORDER_DATE)
    txns_daily <- as(split_dataset_daily,"transactions")
    rule_txns_daily <- apriori(txns_daily,parameter=list(sup=0.1,conf=0.95))
    rule_txns_daily_elcat <- eclat(txns_daily, parameter = list(support=0.1))
    rep_1_a <- inspect(sort(rule_txns_daily, by="lift"))
    rep_1_b <- inspect(rule_txns_daily_elcat)
    library("arulesViz", lib.loc="~/R/win-library/3.2")
    rep_1_c <- plot(rule_txns_daily_elcat,
                    control=list(main="Highest Frequency Itemset For C8(Ananya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat, method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C8(Ananya) - Parallel Coordinate Plot",reorder=TRUE))
  } else if (y == 35) 
  {
    dataset_daily <- sqlQuery(channel,"select ORDER_DATE, PRODUCT_NAME 
                              from north_oltp WHERE CUSTOMER_ID='C9' group by 
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
                    control=list(main="Highest Frequency Itemset For C9(Shreya) - Bubble Plot"))
    rep_1_d <- plot(rule_txns_daily_elcat[1:10], method="paracoord", 
                    control=list(main="Highest Frequency Itemset For C9(Shreya) - Parallel Coordinate Plot",reorder=TRUE))
  } else {"Error: Invalid Entry"}
  North_Customer()
}