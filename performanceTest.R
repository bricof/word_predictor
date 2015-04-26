library(rjson)

datafolders <- c('models/model_500_all',
                 'models/model_1000_all',
                 'models/model_1500_all',
                 'models/model_500_75p',
                 'models/model_1000_75p',
                 'models/model_1500_75p',
                 'models/model_500_50p',
                 'models/model_1000_50p',
                 'models/model_1500_50p'
                 )

performanceMetrics <- list()


testsample <- readLines("en_US.test.sample.txt")

splitBySpaces <- function(str) {
    regex = "[^[:space:]]+"
    regmatches(tolower(str), gregexpr(regex, tolower(str)))
}

wordsSeqsTestsample <- sapply(testsample,splitBySpaces,USE.NAMES=FALSE)



for (datafolder in datafolders) {
    
    ngrams2 <- fromJSON(file=paste(datafolder,"/ngrams2.json",sep=""))
    ngrams3 <- fromJSON(file=paste(datafolder,"/ngrams3.json",sep=""))
    ngrams4 <- fromJSON(file=paste(datafolder,"/ngrams4.json",sep=""))
    ngrams5 <- fromJSON(file=paste(datafolder,"/ngrams5.json",sep=""))
    
    wordList <- function(str) {
        str = gsub("[.!?;]", "  ##s## ", str)
        str = paste("##s##",str)
        str = gsub("##s##[ ]+##s##", "##s##", str)
        regex = "[^[:space:],:=<>/\\)\\(\"]+"
        regmatches(tolower(str), gregexpr(regex, tolower(str)))
    }
    
    predictNextWord5 <- function(inputText){
        
        # construct ngrams from input
        l <- wordList(inputText)
        ll <- length(l[[1]])
        prefix_ngrams2 = ""
        prefix_ngrams3 = ""
        prefix_ngrams4 = ""
        prefix_ngrams5 = ""
        if (ll > 0){
            prefix_ngrams2 = paste(as.character(l[[1]][(ll-0):ll]),collapse=" ")
        }
        if (ll > 1){
            prefix_ngrams3 = paste(as.character(l[[1]][(ll-1):ll]),collapse=" ")
        }
        if (ll > 2){
            prefix_ngrams4 = paste(as.character(l[[1]][(ll-2):ll]),collapse=" ")
        }
        if (ll > 3){
            prefix_ngrams5 = paste(as.character(l[[1]][(ll-3):ll]),collapse=" ")
        }
        
        # pull predictions from ngram objects (from json files)
        out_ngrams2 <- c()
        out_ngrams2p <- c()
        for (i in 1:5) {
            ng <- ngrams2[[prefix_ngrams2]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams2[i] <- w
                    out_ngrams2p[i] <- p
                } else {
                    if (p > out_ngrams2p[(i-1)]) {
                        w2 <- out_ngrams2[(i-1)]
                        p2 <- out_ngrams2p[(i-1)]
                        out_ngrams2[(i-1)] <- w
                        out_ngrams2p[(i-1)] <- p
                        out_ngrams2[i] <- w2
                        out_ngrams2p[i] <- p2
                    }
                    out_ngrams2[i] <- w
                    out_ngrams2p[i] <- p
                }
                
            } else {
                out_ngrams2[i] <- ""
                out_ngrams2p[i] <- ""
            }
        }
        
        out_ngrams3 <- c()
        out_ngrams3p <- c()
        for (i in 1:5) {
            ng <- ngrams3[[prefix_ngrams3]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams3[i] <- w
                    out_ngrams3p[i] <- p
                } else {
                    if (p > out_ngrams3p[(i-1)]) {
                        w2 <- out_ngrams3[(i-1)]
                        p2 <- out_ngrams3p[(i-1)]
                        out_ngrams3[(i-1)] <- w
                        out_ngrams3p[(i-1)] <- p
                        out_ngrams3[i] <- w2
                        out_ngrams3p[i] <- p2
                    }
                    out_ngrams3[i] <- w
                    out_ngrams3p[i] <- p
                }
            } else {
                out_ngrams3[i] <- ""
                out_ngrams3p[i] <- ""
            }
        }
        
        out_ngrams4 <- c()
        out_ngrams4p <- c()
        for (i in 1:5) {
            ng <- ngrams4[[prefix_ngrams4]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams4[i] <- w
                    out_ngrams4p[i] <- p
                } else {
                    if (p > out_ngrams4p[(i-1)]) {
                        w2 <- out_ngrams4[(i-1)]
                        p2 <- out_ngrams4p[(i-1)]
                        out_ngrams4[(i-1)] <- w
                        out_ngrams4p[(i-1)] <- p
                        out_ngrams4[i] <- w2
                        out_ngrams4p[i] <- p2
                    }
                    out_ngrams4[i] <- w
                    out_ngrams4p[i] <- p
                }
            } else {
                out_ngrams4[i] <- ""
                out_ngrams4p[i] <- ""
            }
        }
        
        out_ngrams5 <- c()
        out_ngrams5p <- c()
        for (i in 1:5) {
            ng <- ngrams5[[prefix_ngrams5]]
            if (length(ng) > (i)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams5[i] <- w
                    out_ngrams5p[i] <- p
                } else {
                    if (p > out_ngrams5p[(i-1)]) {
                        w2 <- out_ngrams5[(i-1)]
                        p2 <- out_ngrams5p[(i-1)]
                        out_ngrams5[(i-1)] <- w
                        out_ngrams5p[(i-1)] <- p
                        out_ngrams5[i] <- w2
                        out_ngrams5p[i] <- p2
                    }
                    out_ngrams5[i] <- w
                    out_ngrams5p[i] <- p
                } 
            } else {
                out_ngrams5[i] <- ""
                out_ngrams5p[i] <- ""
            }
        }
        
        
        top5 <- c(" -- "," -- "," -- "," -- "," -- ")
        current <- 1
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams5[[i]] != "") && (!(out_ngrams5[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams5[[i]]
                    current <- current + 1
                }
            }
        }
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams4[[i]] != "") && (!(out_ngrams4[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams4[[i]]
                    current <- current + 1
                }
            }
        }
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams3[[i]] != "") && (!(out_ngrams3[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams3[[i]]
                    current <- current + 1
                }
            }
        }
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams2[[i]] != "") && (!(out_ngrams2[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams2[[i]]
                    current <- current + 1
                }
            }
        }
        
        top5
        
    }
    
    
    
    predictNextWord4 <- function(inputText){
        
        # construct ngrams from input
        l <- wordList(inputText)
        ll <- length(l[[1]])
        prefix_ngrams2 = ""
        prefix_ngrams3 = ""
        prefix_ngrams4 = ""
        if (ll > 0){
            prefix_ngrams2 = paste(as.character(l[[1]][(ll-0):ll]),collapse=" ")
        }
        if (ll > 1){
            prefix_ngrams3 = paste(as.character(l[[1]][(ll-1):ll]),collapse=" ")
        }
        if (ll > 2){
            prefix_ngrams4 = paste(as.character(l[[1]][(ll-2):ll]),collapse=" ")
        }
        
        # pull predictions from ngram objects (from json files)
        out_ngrams2 <- c()
        out_ngrams2p <- c()
        for (i in 1:5) {
            ng <- ngrams2[[prefix_ngrams2]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams2[i] <- w
                    out_ngrams2p[i] <- p
                } else {
                    if (p > out_ngrams2p[(i-1)]) {
                        w2 <- out_ngrams2[(i-1)]
                        p2 <- out_ngrams2p[(i-1)]
                        out_ngrams2[(i-1)] <- w
                        out_ngrams2p[(i-1)] <- p
                        out_ngrams2[i] <- w2
                        out_ngrams2p[i] <- p2
                    }
                    out_ngrams2[i] <- w
                    out_ngrams2p[i] <- p
                }
                
            } else {
                out_ngrams2[i] <- ""
                out_ngrams2p[i] <- ""
            }
        }
        
        out_ngrams3 <- c()
        out_ngrams3p <- c()
        for (i in 1:5) {
            ng <- ngrams3[[prefix_ngrams3]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams3[i] <- w
                    out_ngrams3p[i] <- p
                } else {
                    if (p > out_ngrams3p[(i-1)]) {
                        w2 <- out_ngrams3[(i-1)]
                        p2 <- out_ngrams3p[(i-1)]
                        out_ngrams3[(i-1)] <- w
                        out_ngrams3p[(i-1)] <- p
                        out_ngrams3[i] <- w2
                        out_ngrams3p[i] <- p2
                    }
                    out_ngrams3[i] <- w
                    out_ngrams3p[i] <- p
                }
            } else {
                out_ngrams3[i] <- ""
                out_ngrams3p[i] <- ""
            }
        }
        
        out_ngrams4 <- c()
        out_ngrams4p <- c()
        for (i in 1:5) {
            ng <- ngrams4[[prefix_ngrams4]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams4[i] <- w
                    out_ngrams4p[i] <- p
                } else {
                    if (p > out_ngrams4p[(i-1)]) {
                        w2 <- out_ngrams4[(i-1)]
                        p2 <- out_ngrams4p[(i-1)]
                        out_ngrams4[(i-1)] <- w
                        out_ngrams4p[(i-1)] <- p
                        out_ngrams4[i] <- w2
                        out_ngrams4p[i] <- p2
                    }
                    out_ngrams4[i] <- w
                    out_ngrams4p[i] <- p
                }
            } else {
                out_ngrams4[i] <- ""
                out_ngrams4p[i] <- ""
            }
        }
        
        
        
        top5 <- c(" -- "," -- "," -- "," -- "," -- ")
        current <- 1
        
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams4[[i]] != "") && (!(out_ngrams4[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams4[[i]]
                    current <- current + 1
                }
            }
        }
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams3[[i]] != "") && (!(out_ngrams3[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams3[[i]]
                    current <- current + 1
                }
            }
        }
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams2[[i]] != "") && (!(out_ngrams2[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams2[[i]]
                    current <- current + 1
                }
            }
        }
        
        top5
        
    }
    
    
    
    predictNextWord3 <- function(inputText){
        
        # construct ngrams from input
        l <- wordList(inputText)
        ll <- length(l[[1]])
        prefix_ngrams2 = ""
        prefix_ngrams3 = ""
        if (ll > 0){
            prefix_ngrams2 = paste(as.character(l[[1]][(ll-0):ll]),collapse=" ")
        }
        if (ll > 1){
            prefix_ngrams3 = paste(as.character(l[[1]][(ll-1):ll]),collapse=" ")
        }
        
        # pull predictions from ngram objects (from json files)
        out_ngrams2 <- c()
        out_ngrams2p <- c()
        for (i in 1:5) {
            ng <- ngrams2[[prefix_ngrams2]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams2[i] <- w
                    out_ngrams2p[i] <- p
                } else {
                    if (p > out_ngrams2p[(i-1)]) {
                        w2 <- out_ngrams2[(i-1)]
                        p2 <- out_ngrams2p[(i-1)]
                        out_ngrams2[(i-1)] <- w
                        out_ngrams2p[(i-1)] <- p
                        out_ngrams2[i] <- w2
                        out_ngrams2p[i] <- p2
                    }
                    out_ngrams2[i] <- w
                    out_ngrams2p[i] <- p
                }
                
            } else {
                out_ngrams2[i] <- ""
                out_ngrams2p[i] <- ""
            }
        }
        
        out_ngrams3 <- c()
        out_ngrams3p <- c()
        for (i in 1:5) {
            ng <- ngrams3[[prefix_ngrams3]]
            if (length(ng) > (i-1)) {
                w <- ng[[i]][[1]]
                p <- ng[[i]][[2]]
                if (i == 1) {
                    out_ngrams3[i] <- w
                    out_ngrams3p[i] <- p
                } else {
                    if (p > out_ngrams3p[(i-1)]) {
                        w2 <- out_ngrams3[(i-1)]
                        p2 <- out_ngrams3p[(i-1)]
                        out_ngrams3[(i-1)] <- w
                        out_ngrams3p[(i-1)] <- p
                        out_ngrams3[i] <- w2
                        out_ngrams3p[i] <- p2
                    }
                    out_ngrams3[i] <- w
                    out_ngrams3p[i] <- p
                }
            } else {
                out_ngrams3[i] <- ""
                out_ngrams3p[i] <- ""
            }
        }
        
        
        top5 <- c(" -- "," -- "," -- "," -- "," -- ")
        current <- 1
        
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams3[[i]] != "") && (!(out_ngrams3[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams3[[i]]
                    current <- current + 1
                }
            }
        }
        for (i in 1:5) {
            if (current > 5) {
                break
            } else {
                if ((out_ngrams2[[i]] != "") && (!(out_ngrams2[[i]] %in% top5) )) {
                    top5[current] <- out_ngrams2[[i]]
                    current <- current + 1
                }
            }
        }
        
        top5
        
    }
    

    
    for (bigN in 3:5) {
        
        totalTests <- 0
        inTop5 <- 0
        predictionCorrect <- 0
        
        # start clock
        ptm <- proc.time()
        
        for (line in wordsSeqsTestsample) { 
            l <- length(line)
            for (i in 1:(l-1)){
                prefix <- paste(line[1:i],collapse=" ")
                regex <- "[[:space:].!?;,:=<>/\\)\\(\"]+"
                actualWord <- gsub(regex, "", line[(i+1)])
                if (!is.na(actualWord)) {
                    if (bigN == 5) { top5 <- predictNextWord5(prefix) }
                    if (bigN == 4) { top5 <- predictNextWord4(prefix) }
                    if (bigN == 3) { top5 <- predictNextWord3(prefix) }
                    totalTests <- totalTests + 1
                    if (actualWord %in% top5) { inTop5 <- inTop5 + 1 }
                    if (actualWord == top5[1]) { predictionCorrect <- predictionCorrect + 1 }
                }
            }
        }
        
        # stop clock
        elapsedTime <- proc.time() - ptm
        
        print(paste("bigN = ",bigN))
        print(totalTests)
        print(inTop5 / totalTests)
        print(predictionCorrect / totalTests)
        print(elapsedTime)
        print(elapsedTime[[1]] / totalTests)

        thisMetrics <- list()
        thisMetrics[["totalTests"]] <- totalTests
        thisMetrics[["inTop5P"]] <- inTop5 / totalTests
        thisMetrics[["predictionP"]] <- predictionCorrect / totalTests
        thisMetrics[["lookupTime"]] <- elapsedTime[[1]] / totalTests
        performanceMetrics[[paste(datafolder,bigN,sep="x")]] <- thisMetrics
        
    }
    
}
