### Hierarchical clustering with minimum and maximum cluster size

min_max_size_clustering <- function(data, max_cluster_size = 59, min_cluster_size = 30) {
  start_time <- Sys.time()
  i <- 2
  loopnum <- 1
  redo_clusters <- data
  dat_size <- nrow(redo_clusters)
  saved_clusters <- data.frame(matrix(ncol = (ncol(data) + 2), nrow = 0))
  colnames(saved_clusters) <- c(colnames(data), "labels", "loopnum")
  
  while(dat_size > 0){
    treemodel <- hclust(dist(redo_clusters))
    repeat{
      clusterlabels <- cutree(treemodel, i)
      clustersizes <- cbind(redo_clusters, labels = clusterlabels, loopn = rep(loopnum, nrow(redo_clusters)))
      clustersums <- clustersizes %>% group_by(labels) %>% summarise(size = n()) 
      maxsize <- max(clustersums$size)
      thirdlargestsize <- ifelse(is.na(nth(clustersums$size, 3)), 0, min_cluster_size)
      accurate_clusters <- (clustersizes %>% 
                              group_by(labels) %>% 
                              summarise(size = n()) %>% 
                              filter(size >= thirdlargestsize) %>% 
                              select(labels)
                            )[[1]]
      if(maxsize <= max_cluster_size){
        break
      }
      i = i + 1
    }
    saved_clusters <- rbind(saved_clusters, (clustersizes %>% filter(labels %in% accurate_clusters)))
    redo_clusters <- clustersizes %>% filter(!(labels %in% accurate_clusters))
    redo_clusters <- redo_clusters[,1:2]
    dat_size <- nrow(redo_clusters)
    print(paste("Number of clusters in this round: ", i))
    print(paste("Round ", loopnum, " is over"))
    i <- 2
    loopnum <- loopnum + 1
  }
  running_time <- Sys.time() - start_time
  print(paste("Total running time was: ", running_time))
}


