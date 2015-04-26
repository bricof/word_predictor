
filenames <- c('blogs','news','twitter')
selectRatio <- 0.0001
set.seed(42)
conOut <- file("en_US.test.sample.txt", "w")
for (f in filenames) {
        conIn <- file(paste("../data/en_US/en_US.",f,".txt",sep=""), "r")
        for(i in 1:2360148) {
            if (rbinom(1,1,selectRatio) > 0 ) {
                l <- readLines(conIn, 1)
                if (length(l) > 0) {
                    writeLines(l,con=conOut)
                }
            } else {
                readLines(conIn, 1)
            }
        }
        close(conIn)
}
close(conOut)
