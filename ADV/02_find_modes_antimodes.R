### This script determines modes and antimodes of data set
### Inputs and parameters: data in RDB format (data); smoothed - if FALSE, then only all modes/antimodes without smoothing, if TRUE, then all modes/antimodes without smoothing + all modes/antimodes with smoothing; delta - smoothing level; important - important length classes, where robust modes/antimodes should be determined
### Outputs: all modes/antimodes/amplitudes; smoothed modes/antimodes/amplitudes; robust modes/antimodes/amplitudes


library(FSA)

find_modes_antimodes <- function(data, smoothed=FALSE, delta=1, important = c(0:5000)) 

{

library(FSA)

data <- subset(data, LengthClass_cm %in% important)

	u <- lencat(data$LengthClass_cm, w=1, startcat=0);
	u <- data.frame(u);  
	names(u) <- paste("LengthClass_cm_",1);
	z <- cbind(data,u); 
	x <- data.frame(table(factor(z[,names(u)], levels=seq(0, max(z[,names(u)]), by=1))));
	names(x) <- c("LengthClass_cm", "freq");

	ind_modes <- NULL
	ind_dips <- NULL


## all modes
    	for ( i in 2:(length(x$freq)-1) )
    	{
    	if ( (x$freq[i] > x$freq[i-1]) & x$freq[i] >= x$freq[i+1] ) {
    	ind_modes <- c(ind_modes,i)}
  	}
## all dips
    	for ( i in 2:(length(x$freq)-1) )
    	{
    	if ( x$freq[i] < x$freq[i-1] & (x$freq[i] <= x$freq[i+1])) {
    	ind_dips <- c(ind_dips,i)}
  	}


if ( length(ind_modes) == 0 ) modes = 'Sorry, you have no modes' else modes <- as.numeric(as.character(x[c(ind_modes),]$LengthClass_cm));
if ( length(ind_dips) == 0 ) antimodes = 'Sorry, you have no antimodes' else antimodes <- as.numeric(as.character(x[c(ind_dips),]$LengthClass_cm));



max_freq_value <- max(x[c(ind_modes),]$freq);

modes_signif <- intersect(as.numeric(as.character(x[which(x$freq >= max_freq_value*0.01),]$LengthClass_cm)), modes);
modes_signif <- modes_signif[which(modes_signif %in% important)];

antimodes_signif <- antimodes[which(antimodes %in% c(min(modes_signif):max(modes_signif)))];
antimodes_signif <- antimodes_signif[which(antimodes_signif %in% important)];

if(length(antimodes_signif)==0 & length(modes_signif)==1) antimodes_signif <- c(0);


ld <- sort(c(as.numeric(as.character(modes_signif)), as.numeric(as.character(antimodes_signif))));
xld <- as.numeric(as.character(subset(x, LengthClass_cm %in% ld)$freq));
amp <- abs(diff(xld));


if (smoothed==FALSE & delta==1) 

{

### Difference between significant critical points (modes_signif, antimodes_signif) and robust critical points (modes_robust, antimodes_robust): significant ones include
### only those modes/antimodes, that are located within important;  robust ones are subset of significat ones, include those modes/antimodes, that are located 
### within important AND are still revealed if a bin width (defined by delta) is increased.

### I would prevent from unsing minimal ratio in data (like a length class is a mode/antimode, if its proportion>0.1), to reveal significant critical points. 
###This can lead to situation, where f.ex. two modes are included and an antimode between them is excluded.

modes_robust <- modes_signif;
antimodes_robust <- antimodes_signif;

	return(list(modes_robust = as.numeric(as.character(modes_signif)), antimodes_robust=as.numeric(as.character(antimodes_signif)), amplitudes_robust = amp));

} else

{

	uk <- lencat(data$LengthClass_cm, w=delta, startcat=0);
	uk <- data.frame(uk);  
	names(uk) <- paste("LengthClass_cm_", delta);
	zk <- cbind(data,uk); 
	xk <- data.frame(table(factor(zk[,names(uk)], levels=seq(0, max(zk[,names(uk)]), by=delta))));
	names(xk) <- c("LengthClass_cm", "freq");

	ind_modes_k <- NULL
	ind_dips_k <- NULL

	for ( i in 2:(length(xk$freq)-1) )
    	{
    	if ( (xk$freq[i] > xk$freq[i-1]) & xk$freq[i] >= xk$freq[i+1]) {
    	ind_modes_k <- c(ind_modes_k, i)}
  	}

	for ( i in 2:(length(xk$freq)-1) )
    	{
    	if ( (xk$freq[i] < xk$freq[i-1]) & xk$freq[i] <= xk$freq[i+1]) {
    	ind_dips_k <- c(ind_dips_k, i)}
  	}

modes_k <- as.numeric(as.character(xk[c(ind_modes_k),]$LengthClass_cm));
antimodes_k <- as.numeric(as.character(xk[c(ind_dips_k),]$LengthClass_cm));

intervals <- seq(0, max(data$LengthClass_cm), by = delta);

modes_robust <- NULL;
antimodes_robust <- NULL;

for (j in 2:length(intervals))
{
h_all <- modes_signif[which(modes_signif < intervals[j] & modes_signif >= intervals[j-1])] ;
h_smoothed <- modes_k[which(modes_k < intervals[j] & modes_k >= intervals[j-1])] ;
	if (length(h_all)>0 & length(h_smoothed)>0) 
	{
	Mm <- max(xtabs( ~ LengthClass_cm, data=subset(data, LengthClass_cm %in% h_all)));
	modes_robust <- c(modes_robust, max(as.numeric(as.character(subset(x, freq==Mm & LengthClass_cm %in% important & LengthClass_cm %in% h_all)$LengthClass_cm))))
	}

g_all <- antimodes_signif[which(antimodes_signif < intervals[j] & antimodes_signif >= intervals[j-1])] ;

g_smoothed <- antimodes_k[which(antimodes_k < intervals[j] & antimodes_k >= intervals[j-1])] ;
	if (length(g_all)>0 & length(g_smoothed)>0) 
	{
	Set.d <- subset(data, LengthClass_cm %in% g_all); ### since could be, that Set.d is empty
	Md <- ifelse(nrow(Set.d)>0, min(xtabs( ~ LengthClass_cm, data=subset(data, LengthClass_cm %in% g_all))), g_all[1]);	
	antimodes_robust <- c(antimodes_robust, ifelse(nrow(Set.d)>0, as.numeric(as.character(subset(x, freq==Md & LengthClass_cm %in% important & LengthClass_cm %in% g_all)$LengthClass_cm)), g_all[1]));	
	}

#cat("******************",  fill=TRUE)

}



modes_smoothed <- modes_k;
antimodes_smoothed <- antimodes_k;

if(length(antimodes_smoothed )==0 & length(modes_smoothed)==1) antimodes_smoothed <- c(0);

ld_k <- sort(c(as.numeric(as.character(modes_smoothed)), as.numeric(as.character(antimodes_smoothed))));
xld_k <- as.numeric(as.character(subset(xk, LengthClass_cm %in% ld_k)$freq));
amp_k <- abs(diff(xld_k));

	

	modes_robust <- intersect(modes_robust, modes_signif);
	antimodes_robust <- intersect(antimodes_robust,antimodes_signif);

if(length(antimodes_robust)==0 & length(modes_robust)==1) antimodes_robust <- c(0);

	ld_r <- sort(c(as.numeric(as.character(modes_robust)), as.numeric(as.character(antimodes_robust))));
	xld_r <- as.numeric(as.character(subset(x, LengthClass_cm %in% ld_r)$freq));
	amp_r <- abs(diff(xld_r));

	
	return(list(modes = as.numeric(as.character(modes_signif)),antimodes=as.numeric(as.character(antimodes_signif)), amplitudes = amp, 
      modes_smoothed = as.numeric(as.character(modes_smoothed)), antimodes_smoothed = as.numeric(as.character(antimodes_smoothed)),
	modes_robust = as.numeric(as.character(modes_robust)), antimodes_robust = as.numeric(as.character(antimodes_robust)), amplitudes_robust = amp_r));  

}

}

