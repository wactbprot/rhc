post <- function(host, path, data, referer, contenttype,
                 port=5984) {
    
    maxlen <- 131063L
    
    if(!missing(data))
        dL <- length(charToRaw(data))
    else {
        data<-character(0)
        dL <- 0
    }
    
    head <- c(paste0("POST ", path, " HTTP/1.1\n"))
    head <- c(head, paste0("Host: ", host, "\n"))
    head <- c(head,"Accept: application/json\n\n")
    head <- c(head, paste0("Content-Length: ",dL , "\n"))
    head <- c(head, "Connection: Keep-Alive\n\n")
    
    head <- paste(c(head, data ,"\n"), collapse="")
    
    sock <- make.socket(host=host, port=port, server=FALSE)
    write.socket(sock, head)
    out <- character(0)
                                        # read as long as maxlen bytes have been returned
    repeat {
        ss <- read.socket(sock, maxlen, loop=FALSE)
        out <- paste(out, ss, sep="")
        
        if (length(charToRaw(ss)) < maxlen) break
    }
    close.socket(sock)
    return(out)
}
