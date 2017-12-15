source("/Users/andre/Documents/UPMC/M1\ BIM\ S1/AAGB/LaurentDavid/phyPlot.R")

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
  tmp1 = D
  tmp1[which(!is.finite(tmp1))] <- 0
  ar.Ui <- sumDist.all(tmp1) # Calcul des Ui
  return (matrix ( c(tmp1[c1,c2] - ar.Ui[c1] - ar.Ui[c2]),1,1 ))
}

UiCalc = function (D,col){
  tmp1 = D

  tmp1[which(!is.finite(tmp1))] <- 0
  ar.Ui <- sumDist(col,tmp1) # Calcul des Ui

  ar.Ui <- ar.Ui/(ncol(tmp1)-2)

  return (ar.Ui)
}

minQij = function (D){
  combi = combn(colnames(D),2) #retourne toutes les combinaisons de 2 éléments (couple)
  ar.Qij = c()
  i=1
  while (i<=ncol(combi)){ # on parcoure toutes les combinaison
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
    distNewClust = (D[k,c1]+ D[k,c2] -D[c1,c2])/2
    

    newClust.vect = c(newClust.vect, distNewClust  ) #--------------------------
    
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

   while(dim(tmp)[1]>2){

	 clust.names = colnames(tmp)
	 # on va déterminer les indices des cluster à regrouper à partir du Qi,i min

	 min.ind = minQij(tmp) #indice du min de tmp
	 min.dist = tmp[min.ind]
	 desc1 = colnames(tmp)[min.ind[1]]
	 desc2 = colnames(tmp)[min.ind[2]]
	 Ddesc1_2 = tmp[desc1,desc2]
	 br1.lgth = (Ddesc1_2 + UiCalc(tmp,min.ind[1]) - UiCalc(tmp,min.ind[2]))/2
	 br2.lgth = (Ddesc1_2 + UiCalc(tmp,min.ind[2]) - UiCalc(tmp,min.ind[1]))/2
	 
	 
	 
	 tmp = newDist.mat(D, tmp, min.ind[1], min.ind[2]) # ---------------------------------------------
	 new.clust = colnames(tmp)[dim(tmp)[2]]

	 
	   new.clust = (paste(sort(unlist(strsplit(new.clust, ","))), collapse = ",")) # ------------
     ### tree construction :
     From = c(From, rep (new.clust,2))
	   desc1 = (paste(sort(unlist(strsplit(desc1, ","))), collapse = ",")) # ----------------
	   desc2 = (paste(sort(unlist(strsplit(desc2, ","))), collapse = ",")) # ----------------
	   
     To = c(To, desc1, desc2)

     desc1.height = Heights$height[which(Heights$node==desc1)]
     desc2.height = Heights$height[which(Heights$node==desc2)]
     Length=c(Length, br1.lgth, br2.lgth)

     #new node height
     new.height = newNode.height(Heights, new.clust, desc1, desc2, 
                                 br1.lgth, 
                                 br2.lgth)
     Heights = rbind(Heights, 
                     data.frame(node=new.clust,
                                height=new.height))
   }
   # Terminaison
    new.clust = (paste(sort(unlist(strsplit(colnames(tmp), ","))), collapse = ",")) # ------------
    print (new.clust)
    desc1 = colnames(tmp)[1]
    desc2 = colnames(tmp)[2]
    Ddesc1_2 = tmp[desc1,desc2]
    
    br1.lgth = Ddesc1_2
    br2.lgth = Ddesc1_2
  
    ### tree construction :
    From = c(From, rep (new.clust,2))
    desc1 = (paste(sort(unlist(strsplit(desc1, ","))), collapse = ",")) # ----------------
    desc2 = (paste(sort(unlist(strsplit(desc2, ","))), collapse = ",")) # ----------------
    
    To = c(To, desc1, desc2)
    
    desc1.height = Heights$height[which(Heights$node==desc1)]
    desc2.height = Heights$height[which(Heights$node==desc2)]
    Length=c(Length, br1.lgth, br2.lgth)
    
    #new node height
    new.height = newNode.height(Heights, new.clust, desc1, desc2, 
                                br1.lgth, 
                                br2.lgth)
    Heights = rbind(Heights, 
                    data.frame(node=new.clust,
                               height=new.height))
    

    
  
   # ------------
   root = paste(colnames(D),collapse=",")
   edges = data.frame(From,To,Length, stringsAsFactors = F)
   edges <- edges[nrow(edges):1,] # ----------------------- pas obligatoire
   tree = list(edges = edges, leaves=leaves, root= root)
   return(tree)
  
}
}
tree = NJ(Table1)
print(tree)
phy.plot(tree)
