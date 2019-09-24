#### This script describes iterative subsampling procedure for reference subsample construction
#### Inputs and parameters: data - input data set; delta - parameter defining smoothing level; theta - parameter defining amplitude ratio; epsion - parameter defining tolerated critical poits shift;
#### gamma - parameter defining minimally permitted number per length class; important - defined important length classes
#### Ouputs: data set representing reference subsample + 2 histogram plots comparing original data set and reference subsample (with bin width = 1 cm and bin width = delta > 1 cm)

subsampling <- function(data, delta=1, theta=0.9, gamma=0.3, epsilon = 0, important=c(0:5000))
{

M <- max(data$LengthClass_cm);
cl <- seq(0, M, by=1);


if (missing(important)) important <- c(0:M);



if (gamma < 1)
{
min_per_length_class <- c();

for (j in cl)
{
n_in_j <- nrow(subset(data, LengthClass_cm==j));
if(!(j %in% important)) n.cl <- n_in_j else n.cl <- gamma*n_in_j;
min_per_length_class <- c(min_per_length_class, round(n.cl));
}

U <- data.frame(table(factor(data$LengthClass_cm, levels=seq(0, M, by=1))));
U <- cbind(U, min_per_length_class);

names(U) <- c("LengthClass_cm", "freq", "min_num_lc");

U$LengthClass_cm <- as.numeric(as.character(U$LengthClass_cm));
U$freq <- as.numeric(as.character(U$freq));
U$min_num_lc <- as.numeric(as.character(U$min_num_lc));

} else

{
min_per_length_class <- c();

for (j in cl)
{
if(!(j %in% important)) n.cl <- n_in_j else n.cl <- gamma;
min_per_length_class <- c(min_per_length_class, round(n.cl));
}

U <- data.frame(table(factor(data$LengthClass_cm, levels=seq(0, M, by=1))));
U <- cbind(U, min_per_length_class);

names(U) <- c("LengthClass_cm", "freq", "min_num_lc");

U$min_num_lc <- ifelse(U$min_num_lc>U$freq, U$freq, U$min_num_lc)

U$LengthClass_cm <- as.numeric(as.character(U$LengthClass_cm));
U$freq <- as.numeric(as.character(U$freq));
U$min_num_lc <- as.numeric(as.character(U$min_num_lc));
}


#### for plot #####
LengthClass_cm_delta <- lencat(data$LengthClass_cm, w=5, startcat=0);
LengthClass_cm_delta <- data.frame(LengthClass_cm_delta);  
data <- cbind(data,LengthClass_cm_delta);

Udelta <- data.frame(table(factor(data$LengthClass_cm_delta, levels=seq(0, M, by=delta))));
names(Udelta) <- c("LengthClass_cm", "freq");
###################

r <- max(U$freq);
rdelta <- max(Udelta$freq);

########################

data <- subset(data, LengthClass_cm %in% important);


if (delta>1)
{
u <- find_modes_antimodes(data, smoothed=TRUE, delta=delta, important = important)$modes_robust;
v <- find_modes_antimodes(data, smoothed=TRUE, delta=delta, important = important)$antimodes_robust;
} else
{
u <- find_modes_antimodes(data, smoothed=FALSE, delta=delta, important = important)$modes_robust;
v <- find_modes_antimodes(data, smoothed=FALSE, delta=delta, important = important)$antimodes_robust;
}

critical_points_robust <- sort(c(u,v))

cat("CP original: ", critical_points_robust, fill=TRUE)

if (delta>1)
y.diff <- find_modes_antimodes(data, smoothed=TRUE, delta=delta, important = important)$amplitudes_robust else 
y.diff <- find_modes_antimodes(data, smoothed=FALSE, delta=delta, important = important)$amplitudes_robust;


subsample <- data;
U_sub <- U;

critical_points_robust_sub <- critical_points_robust;
y.diff_sub <- y.diff;

cat("Ratio original: ", y.diff_sub/y.diff, fill=TRUE)

contrsubsample <- c();

t <- 1;

cat("**********",fill=TRUE);


min_number_LC <- U_sub$min_num_lc;


repeat 
{

if(length(critical_points_robust) == length(critical_points_robust_sub))
{

if(all(abs(critical_points_robust - critical_points_robust_sub)<=epsilon)  & all(y.diff_sub/y.diff >=theta) & any(U_sub$freq > min_number_LC))

{

cat("t=",t,fill=TRUE)
subsample_previous <- subsample;
U_sub_previous <- U_sub;

n <- which(U_sub$freq > min_number_LC & U_sub$freq > 0 & U_sub$LengthClass_cm %in% important);

s <- U_sub[n,]$LengthClass_cm;

fun_sub <- function(u) { nn <- which(subsample$LengthClass_cm == u); if(length(nn) > 1) rows <- sample(nn, 1, replace=FALSE) else { if (length(nn)==1)  rows <- nn } ;  return(rows)};


z <- unlist(lapply(s, fun_sub));

contrsubsample <- data.frame(rbind(contrsubsample, subsample[z,]));
subsample <- subsample[-z,];


J <- data.frame(table(factor(subsample$LengthClass_cm, levels=seq(0, M, by=1))));

U_sub <- J

names(U_sub) <- c("LengthClass_cm", "freq");

U_sub$LengthClass_cm <- as.numeric(as.character(U_sub$LengthClass_cm));
U_sub$freq <- as.numeric(as.character(U_sub$freq));

if (delta>1)
{
u_sub <- find_modes_antimodes(subsample, smoothed=TRUE, delta=delta, important = important)$modes_robust;
v_sub <- find_modes_antimodes(subsample, smoothed=TRUE, delta=delta, important = important)$antimodes_robust;
} else
{
u_sub <- find_modes_antimodes(subsample, smoothed=FALSE, delta=delta, important = important)$modes_robust;
v_sub <- find_modes_antimodes(subsample, smoothed=FALSE, delta=delta, important = important)$antimodes_robust;
}

critical_points_robust_sub <- sort(c(u_sub, v_sub));

cat("modes iterative: ", u_sub, fill=TRUE)
cat("antimodes iterative: ", v_sub, fill=TRUE)

if (delta>1)
y.diff_sub <- find_modes_antimodes(subsample, smoothed=TRUE, delta=delta, important = important)$amplitudes_robust else 
y.diff_sub <- find_modes_antimodes(subsample, smoothed=FALSE, delta=delta, important = important)$amplitudes_robust;


if (length(critical_points_robust) == length(critical_points_robust_sub))
cat("Ratio iterative: ", y.diff_sub/y.diff, fill=TRUE);


if (delta>1)
{
u_sub_previous <- find_modes_antimodes(subsample_previous, smoothed=TRUE, delta=delta, important = important)$modes_robust;
v_sub_previous <- find_modes_antimodes(subsample_previous, smoothed=TRUE, delta=delta, important = important)$antimodes_robust;
} else
{
u_sub_previous <- find_modes_antimodes(subsample_previous, smoothed=FALSE, delta=delta)$modes_robust;
v_sub_previous <- find_modes_antimodes(subsample_previous, smoothed=FALSE, delta=delta)$antimodes_robust;
}


critical_points_robust_sub_previous <- sort(c(u_sub_previous, v_sub_previous));


if (delta>1)
y.diff_sub_previous <- find_modes_antimodes(subsample_previous, smoothed=TRUE, delta=delta, important = important)$amplitudes_robust else 
y.diff_sub_previous <- find_modes_antimodes(subsample_previous, smoothed=FALSE, delta=delta, important = important)$amplitudes_robust;

cat("**********",fill=TRUE);

t <- t + 1;

} else 

{
cat("########################################", fill=TRUE);
cat("########## STOPPING RULE: ##############", fill=TRUE);
cat("########################################", fill=TRUE);
cat("",fill=TRUE);
### 1
if (any(abs(critical_points_robust - critical_points_robust_sub)>epsilon)) 
{
cat("########################################", fill=TRUE);
cat("CP: ", critical_points_robust_sub,fill=TRUE);
cat("CP difference > epsilon: ", abs(critical_points_robust - critical_points_robust_sub),fill=TRUE);
}

### 2
if (all(U_sub$freq <= min_number_LC)) 
{
cat("########################################", fill=TRUE);
cat("LC number < gamma: ", fill=TRUE);
print(U_sub[which(U_sub$freq <= min_number_LC),])
cat("########################################", fill=TRUE);
}

### 3

if (any(y.diff_sub/y.diff<theta))
{
cat("########################################", fill=TRUE);
cat("Amplitudes ratio < theta: ", y.diff_sub/y.diff, fill=TRUE);
cat("########################################", fill=TRUE);
}


break; 
} 

} else 

{

## 4
cat("########################################", fill=TRUE);
cat("########## STOPPING RULE: ##############", fill=TRUE);
cat("########################################", fill=TRUE);
cat("",fill=TRUE);
cat("########################################", fill=TRUE);
cat("Number critical points changed!", fill=TRUE); 
cat("CP: ", critical_points_robust_sub,fill=TRUE);
cat("Number of CP: ", length(critical_points_robust_sub),fill=TRUE);
cat("########################################", fill=TRUE);

break; 

}


}

final_subsample_unsuitable <- subsample;
subsample <- subsample_previous;

dev.new();

p1 <- ggplot(data, aes(x=LengthClass_cm)) + 
geom_histogram(data = data, aes(x = LengthClass_cm), binwidth = 1, boundary=0, closed="left", colour="black",fill="turquoise") + 
xlim(0,M)+
ylim(0,r)+
annotate("text", label = paste("n =", nrow(data)), x = M-20, y = 7/8*r, size = 13, colour = "black", fontface = 4) +
labs(title="ORIGINAL SAMPLE", x="LENGTH", y = "COUNT") +
theme(title = element_text(face="bold",size=19), axis.title = element_text(face="bold",size = 17), axis.text = element_text(face="bold", size = 17), strip.text = element_text(size=20, face="bold.italic")) 


p2 <- ggplot(subsample, aes(x=LengthClass_cm)) + 
geom_histogram(data = subsample, aes(x = LengthClass_cm), binwidth = 1, boundary=0, closed="left", colour="black",fill="lightskyblue1") + 
xlim(0,M)+
ylim(0,r)+
annotate("text", label = paste("n =", nrow(subsample)), x = M-20, y = 7/8*r, size = 13, colour = "black", fontface = 4) +
labs(title="REFERENCE SUBSAMPLE", x="LENGTH", y = "COUNT") +
theme(title = element_text(face="bold",size=19), axis.title = element_text(face="bold",size = 17), axis.text = element_text(face="bold", size = 17), strip.text = element_text(size=20, face="bold.italic")) 

library(gridGraphics)

grid.arrange(p1, p2, ncol=2, 
top=textGrob(paste("LFD (raised to whole catch):", paste(unique(data$Species_code)), ", ", 
"LC = ", min(important), ":", max(important), ", ", "EU STATE: ", paste(unique(data$Vessel_flag_country), collapse="/"), 
", Area ",  paste(unique(data$Area), collapse="/"), ", Quarter ", unique(data$Quarter),  ", Year", paste(unique(data$Year),collapse="/" ),"\n"), gp=gpar(fontsize=27, font=2)));



dev.new();

p3 <- ggplot(data, aes(x=LengthClass_cm)) + 
geom_histogram(data = data, aes(x = LengthClass_cm), binwidth = delta, boundary=0, closed="left", colour="black",fill="turquoise") + 
xlim(0,M)+
ylim(0,rdelta)+
annotate("text", label = paste("n =", nrow(data)), x = M-20, y = 7/8*rdelta, size = 13, colour = "black", fontface = 4) +
annotate(geom="text", x=M-30, y=7/9*rdelta, label=as.character(paste("bandwidth = ", delta)), color="blue",size=15) +
labs(title="ORIGINAL SAMPLE", x="LENGTH", y = "COUNT") +
theme(title = element_text(face="bold",size=19), axis.title = element_text(face="bold",size = 17), axis.text = element_text(face="bold", size = 17), strip.text = element_text(size=20, face="bold.italic")) 


p4 <- ggplot(subsample, aes(x=LengthClass_cm)) + 
geom_histogram(data = subsample, aes(x = LengthClass_cm), binwidth = delta, boundary=0, closed="left", colour="black",fill="lightskyblue1") + 
xlim(0,M)+
ylim(0,rdelta)+
annotate("text", label = paste("n =", nrow(subsample)), x = M-20, y = 7/8*rdelta, size = 13, colour = "black", fontface = 4) +
annotate(geom="text", x=M-30, y=7/9*rdelta, label=as.character(paste("bandwidth = ", delta)), color="blue",size=15) +
labs(title="REFERENCE SUBSAMPLE", x="LENGTH", y = "COUNT") +
theme(title = element_text(face="bold",size=19), axis.title = element_text(face="bold",size = 17), axis.text = element_text(face="bold", size = 17), strip.text = element_text(size=20, face="bold.italic")) 

library(gridGraphics)

grid.arrange(p3, p4, ncol=2, 
top=textGrob(paste("LFD (raised to whole catch):", paste(unique(data$Species_code)), ", ", 
"LC = ", min(important), ":", max(important), ", ", "EU STATE: ", paste(unique(data$Vessel_flag_country), collapse="/"), 
", Area ",  paste(unique(data$Area), collapse="/"), ", Quarter ", unique(data$Quarter),  ", Year", paste(unique(data$Year),collapse="/" ),"\n"), gp=gpar(fontsize=27, font=2)));


return(subsample);

}