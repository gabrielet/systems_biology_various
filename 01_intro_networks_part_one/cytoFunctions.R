#read a sif file
loadSif <- function(filename){
	#read the file
	sifFile <- read.table(filename)

	#create a temp network using the first and last column of the sif file I just created
	tmpNet <- graph.data.frame(sifFile[,c(1,3)],directed=FALSE)

	#remove loops and multiple edges
	network <- simplify(tmpNet, remove.multiple=TRUE, remove.loops=TRUE)
	
	#verify the number of connected components
	if (length(decompose(network))>1) {
		print("verify that this network does not contain any disconnected component. if it does try spCluster or connectIsolated");
		#get the biggest component
		biggest <- decompose(network)[1]
		#and return it
		return(biggest)
	} else {
		return(network)
	}
}
