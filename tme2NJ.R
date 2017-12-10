

Table1 = matrix (c(0,2,4,6,6,8,
                   2,0,4,6,6,8,
                   4,4,0,6,6,8,
                   6,6,6,0,4,8,
                   6,6,6,4,0,8,
                   8,8,8,8,8,0),6,6)



colnames(Table1)=LETTERS[1:6]  
rownames(Table1) = colnames(Table1)
Table1

########### Erratum?

### There was a 'tmp4' in the is.additive function, which is to be replaced by 'tmp2'

#is.additive = function (D){
#  tmp = combn(colnames(D),4) #return all combinations of 4 element from colnames(D)
#  res = T
#  i=1
#  while (res && i<=ncol(tmp)){
#    tmp1 = D[tmp[1,i],tmp[2,i]] + D[tmp[3,i],tmp[4,i]]
#    tmp2 = D[tmp[1,i],tmp[3,i]] + D[tmp[2,i],tmp[4,i]]
#    tmp3 = D[tmp[1,i],tmp[4,i]] + D[tmp[3,i],tmp[2,i]]
#    res = (tmp1 < tmp2 && tmp2 == tmp3) || 
#      (tmp1 == tmp2 && tmp3 < tmp1) ||
#      (tmp1 == tmp3 && tmp2 < tmp1)
#    i = i+1
#  }
#  return (res)
#}

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


ar.Ui <- sumDist.all(Table1)

### Following the same strategy as in UPGMA, implement the NJ algorithm, then plot the results for the different tables


##### Optional questions
# 1. For a rooted binary tree with n leaves, how many internal nodes do we have? How many edges?
# 2. Prove (by induction) that for n leaves, there are (2n-3)!! = 1*3*..*(2n-3) possible rooted binary trees.
