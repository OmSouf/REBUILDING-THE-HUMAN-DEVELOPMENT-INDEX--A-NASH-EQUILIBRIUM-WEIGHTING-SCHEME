
### libraries 
library(Rcmdr)

  library(xlsx)

  library(lpSolve)

  library(utils)

#### identify each column by its name to bind them into a datamatrix



dataHDI <- readXL("C:/Users/soufiane/Desktop/efficiency/HDI.xls", rownames=TRUE, 
  header=TRUE, na="", sheet="Sheet1", stringsAsFactors=TRUE)

Gross.domestic.product.per.capita=dataHDI[,1]
Life.expectancy.at.birth=dataHDI[,2]
Knowlegde=dataHDI[,3] 
Gross.national.income.per.capita=dataHDI[,4]
	

##### bend all column into data matrix with input first sceeded by the output 

   datamatrix=cbind(Gross.domestic.product.per.capita, Life.expectancy.at.birth, Knowlegde, Gross.national.income.per.capita) # data matrix

##### add row names companies or DMUs number

   rownames(datamatrix)=rownames(dataHDI)

#### check the data into a new value 

   data_dea =datamatrix

   N = dim(data_dea)[1]  # number of DMU

   s = 1 # number of inputs

   m = 3 # number of outputs

   inputs = as.matrix(data_dea[,c(1:s)])

   outputs = as.matrix(data_dea[,c((s+1):(s+m))])

#### start of the algorithm 

    crosseff = matrix(0,nrow=N,ncol=N) # initialize cross efficiency matrix

    f.rhs = c(rep(0,N),1) # RHS constraints

    f.dir = c(rep("<=",N),"=") # directions of the constraints

    aux = cbind(-1*inputs,outputs) # matrix of constraint coefficients in (CCR)

    for (i in 1:N) {

         f.obj = c(rep(0,s),t(outputs[i,])) # objective function coefficients

         f.con = rbind(aux ,c(inputs[i,(1:s)], rep(0,m))) # add LHS

         results = lp("max",f.obj,f.con,f.dir,f.rhs,scale=1,compute.sens=TRUE) # solve LPP

         multipliers = results$solution # input and output weights

         efficiency = results$objval # efficiency score

         duals = results$duals # shadow prices

#### keep weight and final efficiency (CCR) values

         if (i==1) {

                weights = c(multipliers[seq(1,s+m)])

                effcrs = efficiency

                lambdas = duals [seq(1,N)]

          } else {

                weights = rbind(weights,c(multipliers[seq(1,s+m)]))

                effcrs = rbind(effcrs , efficiency)

                lambdas = rbind(lambdas,duals[seq(1,N)])

                }
##### fill in the cross efficiency matrix

         for (j in 1:N) {

               crosseff[i,j] = multipliers[(s+1):(m+s)]%*%(outputs[j,])/(multipliers[1:s]%*%(inputs[j,]))

               }

               }

#### create a matrix with CCR Efficiencies and relevant weights, we can add the lamdas (shadow prices) 

       matrix_results = cbind(effcrs,weights)

       rownames(matrix_results) = rownames(data_dea)

       colnames(matrix_results) = c("efficiency",colnames(data_dea)[1:(s+m)])

#### Compute the mean Cross Efficiences

      rankingb = (N*apply(crosseff,2,mean))/(N) #mean CrossEff including self apparaisal

      rankinga = (N*apply(crosseff,2,mean)-diag(crosseff))/(N-1)  #mean CrossEff without self apparaisal

      maverick = (effcrs-rankingb)/rankingb # index developed by Green et al with self apparaisal


      mavericka = (effcrs-rankinga)/rankinga # index developed by Green et al without self apparaisal

      Table = t(rbind(as.numeric(effcrs),round(rankingb,4),t(maverick))) # Table with CCR, meanCrosseff and the maverick index with self apparaisal this will later be used for the game cross effeciency 

      colnames(Table) = c('CCR','cross_eff','Maverick')

      rownames(Table) = rownames(data_dea)  

     Tablef = t(rbind(as.numeric(effcrs),round(rankingb,4),t(maverick))) # Table with CCR, meanCrosseff and the maverick index with self apparaisal this will later be used for final comparaison  

      colnames(Tablef) = c('CCR','cross_eff','Maverick')

      rownames(Tablef) = rownames(data_dea)
    
      Tabl = t(rbind(as.numeric(effcrs),rankinga,t(mavericka)))# Table with CCR, meanCrosseff and the maverick index without self apparaisal

      colnames(Tabl) = c('CCR','cross_eff','Maverick')

      rownames(Tabl) = rownames(data_dea)

Table





#########Game cross efficiency######################

weighmatrix = matrix(0,nrow=N,ncol=4) 

eps=0.001 ## small value to be chosen by the author


Table = t(rbind(as.numeric(effcrs),rankingb,t(maverick)))

eff = matrix(0,nrow=N,ncol=N) # initialize cross efficiency matrix

Table_2 = matrix(0,nrow=N,ncol=3) # initialize cross efficiency matrix


for (x in 1:N) {

    while  (abs(Table[x,2]-Table_2[x,2])>=eps) { 

         for (i in 1:N) {

            f.rhs = c(rep(0,(N+1)),1) # RHS constraints

            f.dir = c(rep("<=",(N+1)),"=") # directions of the constraints

            aux = cbind(-1*inputs,outputs) # matrix of constraint coefficients in (6)

            f.obj = c(rep(0,s),t(outputs[i,])) # objective function coefficients

                for (u in 1:N) {

                   alpha=Table[u,2]

                   aux1 = rbind(aux ,c(alpha*inputs[u,],-1*outputs[u,])) # add LHS

                   f.con = rbind(aux1 ,c(inputs[i,], rep(0,m))) # add LHS

                   results = lp("max",f.obj,f.con,f.dir,f.rhs,scale=1,compute.sens=TRUE) # solve LPP
  
                   Gmultipliers = results$solution # input and output weights

                   gameff = results$objval # game efficiency score

                   eff[u,i]=results$objval
				   
				   weighmatrix[u,]=t(as.matrix(Gmultipliers))
				   
				   multipliers = results$solution # input and output weights


                  Gduals = results$duals # shadow prices
                   }


             }

    Table_2=Table

    rankingg = (N*apply(eff,2,mean))/(N)

    Table[,2]=t(round(rankingg,4))

    colnames(Table) = c('CCR','cross_eff','Maverick')

    rownames(Table) = rownames(data_dea)

    colnames(Table_2) = c('CCR','game_cross_eff','Maverick')

    rownames(Table_2) = rownames(data_dea)


    }
	
}

      CCR=Table[,1]

      Game_cross_eff=Table[,2]

      Cross_eff=Tablef[,2]
         
      Final_Table=cbind(CCR, Cross_eff, Game_cross_eff ) # final results
      
      rownames(Final_Table) = rownames(data_dea)
      
      colnames(crosseff)=utility

      colnames(eff)=utility

      rownames(crosseff)=utility

      rownames(eff)=utility
     
      Cross_efficiency_matrix=crosseff

      Game_Cross_efficiency_matrix=eff

      Cross_efficiency_matrix

      Game_Cross_efficiency_matrix

      Final_Table
	  
	  Final_Table








 
     
      




