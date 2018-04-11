choice <- as.list(as.data.frame(matrix(rep(c("A", "B", "C", "D"), 10), ncol=10, byrow=FALSE)))
perm <-expand.grid(choice)

# remove Q2 incompatible
c.q2 <- c("C","D","A","B")
names(c.q2) <- c("A","B","C","D")
ix <- which(c.q2[perm[,2]] == perm[,5])
perm <- perm[ix,]
#262,144 left


# remove Q3 incompatible : level of 2,3,4,6 should be 3 vs. 1
ix <- c(
    which(perm[,3]=="A" & perm [,2]!="A" & perm [,4]!="A" & perm [,6]!="A"),
    which(perm[,3]=="B" & perm [,2]=="B" & perm [,4]=="B" & perm [,6]!="B"),
    which(perm[,3]=="C" & perm [,2]!="C" & perm [,4]=="C" & perm [,6]=="C"),
    which(perm[,3]=="D" & perm [,2]=="D" & perm [,4]!="D" & perm [,6]=="D")
)
perm <- perm[ix,]
#36,864 left


# remove Q4 incompatible:
ix <- c(
    which(perm[,4]=="A" & perm[,1]==perm[,5]),
    which(perm[,4]=="B" & perm[,2]==perm[,7]),
    which(perm[,4]=="C" & perm[,1]==perm[,9]),
    which(perm[,4]=="D" & perm[,6]==perm[,10])
)

perm <- perm[ix,]
#9,216 left

# remove Q5 incompatible:
ix <- c(
    which(perm[,5]=="A" & perm[,8]=="A"),
    which(perm[,5]=="B" & perm[,4]=="B"),
    which(perm[,5]=="C" & perm[,9]=="C"),
    which(perm[,5]=="D" & perm[,7]=="D")
)
perm <-perm[ix,]
#2,112 left


# remove Q6 incompatible:
ix <- c(
    which(perm[,6]=="A" & perm[,2]==perm[,4] & perm[,2]==perm[,8]),
    which(perm[,6]=="B" & perm[,1]==perm[,6] & perm[,1]==perm[,8]),
    which(perm[,6]=="C" & perm[,3]==perm[,10] & perm[,3]==perm[,8]),
    which(perm[,6]=="D" & perm[,5]==perm[,9] & perm[,5]==perm[,8])
)
perm <-perm[ix,]
#172 left

# remove Q7 incompatible:
p.q7 <- apply(perm, 1, FUN=function(x) sort(table(as.character(x))))
c.q7 <- c("C","B","A","D")
names(c.q7)  <- c("A", "B", "C", "D")

ix <- which(c.q7[perm[,7]] == sapply(p.q7, FUN=function(x) names(x)[1]))
perm <- perm[ix,]
#24 left


# remove Q8 incompatible:
ix <- c(
    which(perm[,8]=="A" & abs(as.numeric(perm[,7])-as.numeric(perm[,1])) > 1),
    which(perm[,8]=="B" & abs(as.numeric(perm[,5])-as.numeric(perm[,1])) > 1),
    which(perm[,8]=="C" & abs(as.numeric(perm[,2])-as.numeric(perm[,1])) > 1),
    which(perm[,8]=="D" & abs(as.numeric(perm[,10])-as.numeric(perm[,1])) > 1)
)

perm <- perm[ix,]
#8 left



# remove Q9 incompatible:
state.1 <- perm[,1]==perm[,6]
ix <- c(
    which(perm[,9]=="A" & (perm[,6]==perm[,5]) != state.1),
    which(perm[,9]=="B" & (perm[,10]==perm[,5]) != state.1),
    which(perm[,9]=="C" & (perm[,2]==perm[,5]) != state.1),
    which(perm[,9]=="D" & (perm[,9]==perm[,5]) != state.1)
)
perm <- perm[ix,]
#5 left


# remove Q10 incompatible:
p.q10 <- apply(perm, 1, FUN=function(x) sort(table(as.character(x))))
c.q10 <- c(3,2,4,1)
names(c.q10) <- c("A", "B", "C", "D")
ix <- which(c.q10[perm[,10]] == sapply(p.q10, FUN=function(x) x[length(x)]-x[1]))
perm <- perm[ix,]







