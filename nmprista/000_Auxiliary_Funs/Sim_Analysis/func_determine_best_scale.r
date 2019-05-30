# ====================	
# determine_best_scale
# ====================	
	
	# Nuno Prista, SLU Aqua, Sweden @ ICES WKBIOPTIM
	
	# 2018-09-10: created as faz_determine_best_scale
	# 2019-05-26: renamed from faz_determine_best_scale to determine_best_scale
	# 2019-05-26: annotations improved


		determine_best_scale<-function(x = ls_DT_compiled, variables = c("lenCls","age"), stats = c("cv","MWCV"), zero_is_lowest = FALSE)
			{
			
			# this function determines a suitable ylim ranges for plotting "stats" of a set of "variables"
			# x is the output from 003_sim_data (res_sims)
			# variables is a string of variables (e.g., lenCls)
			# stats is a string of stats (e.g., MWCV)
			# zero_is_lowest controls the origin of y-axis. use zero_is_lowest == TRUE to set origin to 0.
			
			
			out<-sapply(variables, function(x) NULL)
			
			for (variable in variables)
				{
					for (stat in stats)
					{
					out[[variable]][[stat]]<-range(do.call("rbind",lapply(x, function(x, var1 = variable, stat1 = stat){
							if (!zero_is_lowest) y <- range(x[[var1]][[stat1]], na.rm=T) else  y <- c(0,max(x[[var1]][[stat1]], na.rm=T)); y})))
					#print(do.call("rbind",lapply(x, function(y, variable = "age", stat = "cv"){range(y[[variable]][[stat]])})))
					}
				}
			print(out)
			out		
			}