
# Initialisation de la matrice test
Table1 = matrix (c(0,2,4,6,6,8,
                   2,0,4,6,6,8,
                   4,4,0,6,6,8,
                   6,6,6,0,4,8,
                   6,6,6,4,0,8,
                   8,8,8,8,8,0),6,6)

colnames(Table1)=LETTERS[1:6]  
rownames(Table1) = colnames(Table1)

# -------------------------------------



# Calcul des Ui : fonctions sumDist & sumDist.all
sumDist = function(s, D){
  somme <- 0
  somme <- rowSums(D)
  return( somme[s] )
}

sumDist.all = function(D){
  res=c()
  S=colnames(D)
  for (i in S){
    res = c(res, sumDist(i,D)/(ncol(D)-2))
  }
  names(res)=S #what is the point of this line?
  return(res)
}

QijCalc = function (D,c1,c2){
  ar.Ui <- sumDist.all(D) # Calcul des Ui
  return (matrix ( c(D[c1,c2] - ar.Ui[c1] - ar.Ui[c2]),1,1 ))
}

minQij = function (D){
  
  combi = combn(colnames(Table1),2) #retourne toutes les combinaisons de 2 éléments (couple)
  ar.Qij = c()
  i=1
  while (i<ncol(combi)){ # on parcoure toutes les combinaison
    cluster1 = combi[1,i] # cluster 1 de la combinaison i 
    cluster2 = combi[2,i] # cluster 2 de la combinaison i
    Qij <- QijCalc(D,cluster1,cluster2) # calcul du Qij des deux cluster
    colnames(Qij) <- paste(cluster1,cluster2,sep=",") # on affecter à Qij le label des 2 cluster
    ar.Qij = cbind(ar.Qij , Qij) # on remplie la matrice ar.Qij avec un nouveau Qij des combinaisons
    i = i+1
  }
  NomClustersMin <- matrix(c(combi[,which.min(ar.Qij)]),1,2) # on récupère les labels des cluster dont le Qij est le minimum
  Ind <- which (D == D[NomClustersMin],arr.ind = TRUE)[1,]
  return (matrix(c(Ind),1,2)) # retourne les indices du cluster dont le Qij est le min
}

cluster.dist = function(D, c1, c2){
  dist = 0
  c1_split <- strsplit(c1, ",")[[1]]
  c2_split <- strsplit(c2, ",")[[1]]
  for (i in c1_split){
    for (j in c2_split){
      dist <- dist + D[i,j]
    }
  }
  dist <- (dist/ 2)

  return (dist)
}

newDist.mat = function(DD, D, i, j){
  clust.names=colnames(D) # the names of the current clusters
  c1 = clust.names[i] # the name of the i-th cluster #-----------------------------------------------------
  c2 = clust.names[j] # the name of the j-th cluster #-----------------------------------------------------
  res = as.matrix(D[-c(i,j),-c(i,j)]) # we delete from D the i-th and j-th row/col
  # the as.matrix function is useful only when D is a 3x3 matrix.
  # In this case what are the dimensions of res?
  # why did we use the as.matrix function ?
  # In this case we also need to name the rows/cols
  colnames(res) = colnames(D)[-c(i,j)] 
  rownames(res) = colnames(D)[-c(i,j)] #-------------------------------------------------------------------
  new.clust=paste(c1,c2,sep=",") # concatenation of the 2 clusters name with ","
  newClust.vect = c()
  other.clust = setdiff(colnames(D),c(c1,c2)) # what does it do?
  for (k in other.clust){
    newClust.vect = c(newClust.vect, cluster.dist(DD,k,new.clust) ) #--------------------------

  }
  # describe what each of the following lines do:
  newClust.vect = c(newClust.vect,Inf)
  res = cbind(res,rep(0,dim(res)[2]))
  res = rbind(res , newClust.vect)
  res[,dim(res)[2]] = newClust.vect
  colnames(res)[dim(res)[1]] = new.clust
  rownames(res)[dim(res)[1]] = new.clust
  return(res)
}


if (TRUE){
NJ = function (D){
  tmp = diagToInf(D)
  leaves = colnames(D)
  
  From = c()
  To = c()
  Length= c()
  Heights = data.frame(node = leaves, 
                       height=as.numeric(rep(0,length(leaves))), # ligne de 0 de longeur nombre de feuilles
                       stringsAsFactors = F) 

   while(dim(tmp)[1]>1){
	 clust.names = colnames(tmp)
	 # on va déterminer les indices des cluster à regrouper à partir du Qi,i min
	 min.ind = minQij(D) #indice du min de tmp
	 min.dist = tmp[min.ind]
	 desc1 = colnames(tmp)[min.ind[1]]
	 desc2 = colnames(tmp)[min.ind[2]]
	 tmp = newDist.mat(D, tmp, min.ind[1], min.ind[2]) # ---------------------------------------------
	 new.clust = colnames(tmp)[dim(tmp)[2]]

	 
	   new.clust = (paste(sort(unlist(strsplit(new.clust, ","))), collapse = ",")) # ------------
     ### tree construction :
     From = c(From, rep (new.clust,2))
	   desc1 = (paste(sort(unlist(strsplit(desc1, ","))), collapse = ",")) # ----------------
	   desc2 = (paste(sort(unlist(strsplit(desc2, ","))), collapse = ",")) # ----------------
	   
     To = c(To, desc1, desc2)
     
     
     br1.lgth = cluster.dist(D, desc1, desc2)
     br2.lgth = br1.lgth
     desc1.height = Heights$height[which(Heights$node==desc1)]
     desc2.height = Heights$height[which(Heights$node==desc2)]
     Length=c(Length, br1.lgth-desc1.height, br2.lgth-desc2.height)

     #new node height
     new.height = newNode.height(Heights, new.clust, desc1, desc2, 
                                 br1.lgth-desc1.height, 
                                 br2.lgth-desc2.height)
     Heights = rbind(Heights, 
                     data.frame(node=new.clust,
                                height=new.height))
   }
   root = paste(colnames(D),collapse=",")
   edges = data.frame(From,To,Length, stringsAsFactors = F)
   edges <- edges[nrow(edges):1,] # ----------------------- pas obligatoire
   tree = list(edges = edges, leaves=leaves, root= root)
   return(tree)
  
}
}
Table1
tree = NJ(Table1)
print (tree)
#phy.plot(tree)
