# SD Tool v.3 and WKBIOPTIM SimTool v.1  were taken into account for the developement of the present tool.
# SD tool has been developed in MARE/2014/19 project and updated in MARE/2016/22 project (STREAM)

# The tool is documented in Deliverable D3.3 "Upgrade the methodological framework and tools for sampling optimization, implement and report case studies" (January 2019)

# Authors of the first version of SD Tool: F. Gontrand, T. Rouyer, N. Billet, 2016
# IFREMER, UMR MARBEC, Avenue Jean Monnet, BP 171, 34203 S` ete, France 

# Authors of the first version of WKBIOPTIM scripts: Nuno Prista (SLU, Sweden) from a subgroup work carried out during a local Workshop on Sampling Design and Optimization (Lysekil, 31 October to 4 November 2016, unpublished)
# SLU Aqua, Institute of Marine Research, Department of Acquatic Resources - Swedish University of Agricultural Sciences

# Authors of this new tool: M.T. Facchini, I. Bitetto, 2017
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 

# In case of use of the tool, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail addresses: facchini@coispa.it, bitetto@coispa.it
# SD Tool is believed to be reliable. However, we disclaim any implied warranty or representation 
# about its accuracy, completeness or appropriateness for any particular purpose.


setVar123 = function(availSamp_loca) {
  
  col_var1 <-  as.character(table_strat_res[1,colnames(table_strat_res) == elements[1] ]) 
  col_var2 <-  as.character(table_strat_res[1,colnames(table_strat_res) == elements[2] ])  
  col_var3 <-  as.character(table_strat_res[1,colnames(table_strat_res) == elements[3] ]) 
#  col_var4 <-  as.character(table_strat_res[1,colnames(table_strat_res) == elements[4] ]) 
  
  switch(col_ele_var1,
         GSA={ 
           availSamp_loca$Var1 <- availSamp_loca$area
         },
         Country={
           availSamp_loca$Var1 <- availSamp_loca$vslFlgCtry
         },
		     ALL = { 
           availSamp_loca$Var1 <- "ALL"
         },
         stop("Enter something that switches me!")
  )
  
  
  switch(col_ele_var2,
         Q = { 
           availSamp_loca$Var2 <- availSamp_loca$SampQuarter
         },
         S = { 
           availSamp_loca$Var2 <- availSamp_loca$SampSemester
         },
         Y = { 
           availSamp_loca$Var2 <- 1
         },
         stop("Enter something that switches me!")
  )
  
  
  switch(col_ele_var3,    
         lev6 = { 
           availSamp_loca$Var3 <- availSamp_loca$foCatEu6
         },
         lev4 = { 
           availSamp_loca$Var3 <- availSamp_loca$foCatEu5
         },
         NONE = {  
           if (col_ele_var4 == "N") {
             availSamp_loca$Var3 <- "1"
           } else {
             print("Sampling by commercial category")
           }
         },
         stop("Enter something that switches me!")
  )
  
  
  
#  switch(col_ele_var4,    
#         Y = { 
#           availSamp_loca$Var3 <- availSamp_loca$commCat
#         },
#         N = { print(paste("Sampling by", col_ele_var3) )
#         },
#         stop("Enter something that switches me!")
#  )
  
  
  
  
  return(availSamp_loca)
}
                      
