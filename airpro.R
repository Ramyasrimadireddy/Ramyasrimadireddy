p=read.csv("pp3.csv")
p
colnames(p)=c("frequency","angle_of_attack","chord_length","free_stream_velocity","section_side_displacement_thickness","scaled_sound_pressure_level")
print(p)
#automatically
i<-1
lst <- c()
for(colname in colnames(p)){
  if(class(p[[colname]])=="numeric"){
    lst[i] = colname
    i=i+1
  }
}
cor_mat <- cor(p[lst])

# Scatter plot
pairs(p[,1:6])
#simplebARPLOT

c=table(p$frequency)
barplot(c, xlab = "X-axis",main ="Bar-Chart")

#horizontalbarplot

counts <- table(p$frequency)
barplot(counts, main="frequency", horiz=TRUE)

# Stacked Bar Plot with Colors and Legend
counts <- table(p$frequency, p$angle_of_attack)
barplot(counts, main="Distribution by f and a",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts))


View(p)
#boxplot

boxplot(frequency~angle_of_attack,data=p,main="boxplot",xlab="x",ylab="y")

#groupedbarplot
counts <- table(p$frequency, p$angle_of_attack)
barplot(counts, main="Distribution by f and a",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)
#hist prob dist
hist(p$frequency, main="Histogram", 
     xlab="fre", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=5, 
     prob = TRUE)
lines(density(p$frequency))

#piechart
library(dplyr)
p_pivot <- summarize(group_by(p,chord_length),frequency=sum(frequency))
slices <- p_pivot[["frequency"]]
pie(slices, labels=p[["chord_length"]], main="Pie Chart")




