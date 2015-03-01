get <- function(host,path,port=5984){
    if(missing(path))
        path <- "/"

    header <- c(paste0("GET ",path," HTTP/1.1\n"))
    header <- c(header,paste0("Host: ",host,"\n"))
    header <- c(header,"Connection: close\n")
    header <- c(header, paste0("User-Agent: ", getOption("HTTPUserAgent"), "\n"))
    header <- c(header,"Accept: application/json\n\n")

    print(header)
    scon   <- socketConnection(host=host,port=port,open="a+b",blocking=TRUE)
    writeBin(charToRaw(paste(header,collapse="")), scon, size=1)
    output <- character(0)
    
    repeat{
        ss <- rawToChar(readBin(scon, "raw", 2048))
        output <- paste(output,ss,sep="")
        if(regexpr("\r\n0\r\n\r\n",ss)>-1) break
        if(ss == "") break
    }

    close(scon)

    return(output)
}
