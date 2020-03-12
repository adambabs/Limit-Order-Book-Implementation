book.total_volumes <- function(book) {
    #DONE
    
    # find the sum of asks and bids
    totalAsk <- sum(book$ask$size)
    totalBid <- sum(book$bid$size)
    
    
    # turn the sum of asks and bids into a dataframe
    totalVolumes <- data.frame("bid" = totalBid, "ask" = totalAsk)
}

book.best_prices <- function(book) {
    #DONE
    
    bestAsk <- min(book$ask$price)
    bestBid <- max(book$bid$price)
    
    bestPrices <- data.frame("bid"= bestBid, "ask"=bestAsk)
}

book.midprice <- function(book) {
    #DONE
    
    bestAsk <- min(book$ask$price)
    bestBid <- max(book$bid$price)
    
    book$midprice = (bestAsk+bestBid)/2
}

book.spread <- function(book) {
    # DONE
    
    bestAsk <- min(book$ask$price)
    bestBid <- max(book$bid$price)
    
    book$spread = bestAsk-bestBid
}


book.add <- function(book, message) {
    
    
    bestAsk <- min(book$ask$price)
    bestBid <- max(book$bid$price)
    
    book <- book.sort(book)
    
    ### ADD TO THE BID SIDE IF MESSAGE IS BUY
    if(message$side == "B"){
        
        i <-1
        available <- sum(book$ask$size)
        toTrade <- min(message$size, available)
        
        ### IF MESSAGE TO BUY IS HIGHER THAN OR EQUAL TO THE HIGHEST SELLING PRICE 
        ### SPREAD IS CROSSED AND WE NEED TO DELETE MATCHING SELLING ORDERS
        if(message$price >= bestAsk){
          
            
            traded <- 0
            
            while(toTrade >0 && i <= nrow(book$ask)){
               
                if(book$ask$size[i] > toTrade){
                    
                    traded <- traded + book$ask$size[i]
                    tmp <- book$ask$size[i]
                    book$ask$size[i] <- book$ask$size[i] - toTrade
                    toTrade <- toTrade - tmp
                   
                } else if(book$ask$size[i] <= toTrade ){
                    
                    traded <- traded + book$ask$size[i]
                    tmp <- book$ask$size[i]
                    book$ask$size[i] <-0
                    toTrade <- toTrade - tmp
                    i <- i + 1
                } 
                
                if(message$price < book$ask$price[i]){
                    i = nrow(book$ask) + 1000   #end the while loop
                    
                }
            }
            
            # delete rows with size values which are less than 1
            book$ask <- book$ask[book$ask$size > 0, ]
            book$bid <- book$bid[book$bid$size > 0, ]
           
            flag1 <-0
            #if all available asks were bought, add the rest of the request to the bid book
            if(available<message$size){
                 
                
                toAdd <- message$size - available
                if(toTrade >0){
                    flag1 <-1
                    toAdd <- toAdd + toTrade
                }
                
                listAdd <- list(message$oid, as.integer(message$price), toAdd)
                book$bid <-  rbind(book$bid, listAdd)
                
            }
            if(flag1 == 0 && toTrade>0){
                listAdd <- list(message$oid, as.integer(message$price), toTrade)
                book$bid <-  rbind(book$bid, listAdd)
            }
    
        }
        
        ##### IF IT IS NOT, THEN JUST ADD IT TO THE BUYING BID SIDE
        ## if there are no buying orders in the book 
        if(nrow(book$bid) == 0 && message$price < bestAsk){
            
            listAddBid <- list(message$oid, as.integer(message$price), as.integer(message$size))
            book$bid[1, ] <- listAddBid
            
        } else if (message$price < bestAsk) {
            
            listAddBid <- list(message$oid, as.integer(message$price), as.integer(message$size))
            book$bid <- rbind(book$bid, listAddBid)
        }
    }
##############################################################################################################################################################
    
    ### ADD TO THE ASK SIDE IF MESSAGE IS SELL
    if(message$side == "S"){

        i <- 1
        available <- sum(book$bid$size)
        toTrade <- min(message$size, available)
        
        ### IF MESSAGE TO SELL IS LOWER THAN OR EQUAL TO THE HIGHEST BUYING PRICE 
        ### SPREAD IS CROSSED AND WE NEED TO DELETE MATCHING BUYING ORDERS
        if(message$price <= bestBid){
            
            while(toTrade > 0 && i < nrow(book$bid)){
                
                if(book$bid$size[i] > toTrade){
                    
                    tmp <- book$bid$size[i]
                    book$bid$size[i] <- book$bid$size[i] - toTrade
                    toTrade <- toTrade - tmp
                   
                    
                } else if(book$bid$size[i] <= toTrade){
                    
                    tmp <- book$bid$size[i]
                    book$bid$size[i] <-0
                    toTrade <- toTrade - tmp
                    i <- i + 1

                } 
                if(message$price > book$bid$price[i]){
                    i = nrow(book$ask) +1000     #end the while loop
                    
                }
                
            }
            
            ### end of while
            book$ask <- book$ask[book$ask$size > 0, ]
            book$bid <- book$bid[book$bid$size > 0, ]
            
            
            # if all available ASKS were bought, add the rest of the request to the bid book
            flag2 <-0
            if(available<message$size){
                
                
                toAdd <- message$size - available
                if(toTrade>0){
                    flag2 <- 1
                    toAdd <- toAdd + toTrade
                
                listAdd <- list(message$oid, as.integer(message$price), toAdd)
                book$ask <- rbind(book$ask, listAdd)
                
                }
            }
            
            if(flag2 == 0 && toTrade > 0){
                listAdd <- list(message$oid, as.integer(message$price), toTrade)
                book$ask <-  rbind(book$ask, listAdd)
            }
            
        }
        
        
        ### IF IT IS NOT, THEN JUST ADD IT TO THE SELLING ASK SIDE
        
        ## if there are no selling orders in the book 
        if(nrow(book$ask) == 0 && message$price > bestBid){
            
            listAddAsk <- list(message$oid, as.integer(message$price), as.integer(message$size))
            book$ask[1, ] <- listAddAsk
            
        } else if(message$price > bestBid) {
            
            listAddSell <-  list(message$oid, as.integer(message$price), as.integer(message$size))
            book$ask <-  rbind(book$ask, listAddSell)
        }
    }
    
    
    return(book)
}

book.reduce <- function(book, message) {

    book <- book.sort(book)
    valueToReduceAsk <- book$ask$size[match(message$oid, book$ask$oid)] - message$amount
    valueToReduceBid <- book$bid$size[match(message$oid, book$bid$oid)] - message$amount

    # insert a new value
    book$ask$size[match(message$oid, book$ask$oid)] <- valueToReduceAsk
    book$bid$size[match(message$oid, book$bid$oid)] <- valueToReduceBid
    
    # delete rows with sizes that are less than or equal to 0
    # by keeping only positive values
    book$ask <- book$ask[book$ask$size > 0, ]
    book$bid <- book$bid[book$bid$size > 0, ]
    
    book <- book.sort(book)

    
    return(book)
}

book.handle <- function(book, row) {
    if (row$type == 'A')
        return(book.add(book, list(
            oid=row$oid,
            side=row$side,
            price=as.numeric(row$price),
            size=as.numeric(row$size)
        )))
    else if (row$type == 'R')
        return(book.reduce(book, list(
            oid=row$oid,
            amount=as.numeric(row$size)
        )))
    else {
        warn("Unknown row type.")

        return(book)
    }
}

book.load <- function(path) {
    df <- read.table(
        path, fill=NA, stringsAsFactors=FALSE, header=TRUE, sep=','
    )

    book.sort(list(
        ask=df[df$side == "S", c("oid", "price", "size")],
        bid=df[df$side == "B", c("oid", "price", "size")]
    ))
}

book.summarise <- function(book, with_stats=T) {
    if (nrow(book$ask) > 0)
        book$ask <- book$ask[nrow(book$ask):1,]

    print(book)

    if (with_stats) {
        clean <- function(x) { ifelse(is.infinite(x), NA, x) }

        total_volumes <- book.total_volumes(book)
        best_prices <- lapply(book.best_prices(book), clean)
        midprice <- clean(book.midprice(book))
        spread <- clean(book.spread(book))

        cat("Total volume:", total_volumes$bid, total_volumes$ask, "\n")
        cat("Best prices:", best_prices$bid, best_prices$ask, "\n")
        cat("Mid-price:", midprice, "\n")
        cat("Spread:", spread, "\n")
    }
}

book.sort <- function(book, sort_bid=T, sort_ask=T) {
    if (sort_ask && nrow(book$ask) >= 1) {
        book$ask <- book$ask[order(book$ask$price,
                                   nchar(book$ask$oid),
                                   book$ask$oid,
                                   decreasing=F),]
        row.names(book$ask) <- 1:nrow(book$ask)
    }

    if (sort_bid && nrow(book$bid) >= 1) {
        book$bid <- book$bid[order(-book$bid$price,
                                   nchar(book$bid$oid),
                                   book$bid$oid,
                                   decreasing=F),]
        row.names(book$bid) <- 1:nrow(book$bid)
    }
    book
}

book.reconstruct <- function(data, init=NULL, log=F) {

    if (nrow(data) == 0) return(book)
    if (is.null(init)) init <- book.init()

    book <- Reduce(
         function(b, i) {
             new_book <- book.handle(b, data[i,])
             if (log) {
                 cat("Step", i, "\n\n")
                 book.summarise(new_book, with_stats=F)
                 cat("====================\n\n")
             }
             new_book
         },
         1:nrow(data), init,
    )
    book.sort(book)
}

data.load <- function(data_path, n_rows=-1) {
    data <- read.table(
        data_path,
        fill=NA,
        stringsAsFactors=FALSE,
        col.names=c("type", "oid", "side", "price", "size"),
        nrows=n_rows,
    )

    data[data$type == 'R', "size"] <- data[data$type == 'R', "side"]
    data[data$type == 'R', "side"] <- NA

    data
}

if (!interactive()) {
    options(warn=-1)

    args <- commandArgs(trailingOnly = TRUE)

    if (length(args) != 2) {
        stop("Must provide two arguments: <path_to_book> <path_to_messages>")
    }
    book_path <- args[1]; data_path <- args[2]

    if (!file.exists(data_path) || !file.exists(book_path)) {
        stop("File does not exist at path provided.")
    }

    book <- book.load(book_path)
    book <- book.reconstruct(data.load(data_path), init=book)

    book.summarise(book)
}
