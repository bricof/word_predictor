
pfDF <- do.call(rbind.data.frame, performanceMetrics)
pfDF$vocabSize <- sapply(row.names(pfDF), function(x) { as.numeric(strsplit(x,"_")[[1]][2]) } )
pfDF$maxN <- sapply(row.names(pfDF), function(x) { as.numeric(strsplit(x,"x")[[1]][2]) } )


pfDF$trainingDataSize <- sapply(row.names(pfDF), function(x) { 
    s <- strsplit(x,"[_x]")[[1]][3]
    if (s == "all") {
        o <- 1
    } else if (s == "75p") {
        o <- 0.75
    } else {
        o <- 0.5
    }
    o
} )

pfDF$modelMB <- c(2.241,8.741,16.741,3.682,11.982,19.882,4.724,13.824,21.124,3.024,7.524,10.624,2.382,6.682,10.182,3.024,7.524,10.624,1.941,7.041,12.941,3.082,9.382,14.982,3.924,10.724,15.824)


EMRcomputeTimes <- read.csv("computeTimes/EMR_runtimes.csv")
EMRcomputeTimes <- EMRcomputeTimes[EMRcomputeTimes$maxN != 2,]
EMRcomputeTimes$trainingDataSize <- EMRcomputeTimes$trainingDataSize * 0.01

df <- merge(pfDF,EMRcomputeTimes)

rm(df$totalTests)

drops <- c("totalTests")
df <- df[,!(names(df) %in% drops)]

