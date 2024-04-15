DATA <- read.csv("DATA.csv")

#visualisation of slope vs region
grouped_data <- split(DATA$slope, DATA$Region)
# Increase the size of the plot and adjust bottom margin
par(mar = c(8, 6, 4, 2) + 0.1)  # Adjust margins
boxplot(grouped_data,horizontal = FALSE, xlab= "", ylab="slope", las=2)
# Add x-axis label above the x-axis labels
mtext("Region", side = 1, line = 5)

#visualisation of slope vs species

grouped_data2 <- split(DATA$slope, DATA$Species)
par(mar = c(8, 6, 4, 2) + 0.1)  # Adjust margins
boxplot(grouped_data2,horizontal = FALSE, xlab= "", ylab="slope", las=2)
mtext("Species", side = 1, line = 6)

#species per region
# Assuming `Species` and `Region` are factors
barplot(table(DATA$Species, DATA$Region), legend = TRUE, col= rainbow(7))

#install package for meta-analysis 
install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")
library(metafor)

#running a Chi-square test for multicollinearity between predictors "Species" and "Region"

chi_square_result <- chisq.test(table(DATA$Species, DATA$Region))

# Print the chi-square test result
print(chi_square_result)  # p-value >0.05, not significant correlation - predictors can be used in the function

# Assessing the degree of multicollinearity
# Load the 'car' package for the vif() function
library(car)

modeltets<- glm(slope~Species + Region+ Species*Region, data=DATA )
summary(modeltets)

vif(modeltets, type= "predictor")

#Running the meta-analysis

# Convert Species and Region variables to factors
DATA$Species <- factor(DATA$Species)
DATA$Region <- factor(DATA$Region)

# Change the reference level of the Species variable to "Atlantic Salmon"
DATA$Species <- relevel(DATA$Species, ref = "Atlantic Salmon")
# Change the reference level of the Region variable to "Juneau"
DATA$Region <- relevel(DATA$Region, ref = "Juneau")

#run the model 

meta1 <-rma(yi=slope,sei=SE,mods=~DATA$Species,data=DATA)
meta1

meta2 <-rma(yi=slope,sei=SE,mods=~DATA$Region,data=DATA)
meta2

#there is a significant variation between species and regions 

#forest plot 

quartz()
par(mfrow=c(1,2))
forest(meta1,header=TRUE,slab=DATA$Study,cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", main = "Forest plot",xlab="Observed outcome with Species as predictor", width = 0.7, cex = 0.5)
forest(meta2,header=TRUE,slab=DATA$Study,cex.lab = 0.8, cex.axis = 0.8, addfit = TRUE, shade = "zebra", main = "Forest plot",xlab="Observed outcome with Regions as predictor", width = 0.7, cex = 0.5)
?forest

#add pooled effect?????
x<- rma(yi=mean(meta1$yi), sei=mean(meta1$se))
x
addpoly(x, row=-2, level=x$level)

#Publication bias
#visualise data running funnel plot

par(mfrow=c(1,2))
plot(DATA$slope,DATA$sampleSize.Nyears.,xlab="Slope",ylab="Sample size")
plot(DATA$slope,(1/DATA$SE),xlab="Slope",ylab="Precision, (1/se)")

quartz()
sigslope<-which(DATA$pvalue<0.05)
par(mfrow=c(1,2))
plot(DATA$slope,DATA$sampleSize.Nyears.,xlab="Slope",ylab="Sample size")
points(DATA$slope[sigslope],DATA$sampleSize.Nyears.[sigslope],pch=16,col="red")
abline(v=slope,lty=2)
plot(DATA$slope,(1/DATA$SE),xlab="Slope",ylab="Precision, (1/se)")
points(DATA$slope[sigslope],(1/DATA$SE[sigslope]),pch=16,col="red")
abline(v=slope,lty=2)

#funnel plots of meta1 and meta2
funnel(meta1)
funnel(meta2)

#regression test for asymmetry detection 
regtest(x=slope, sei=SE, data=DATA,
        model="rma",predictor="sei", ret.fit=FALSE)

#significant asymmetry with p=0.0071, z= -2.69

funnel(meta1)
funnel(meta2)

#EXPERIMENT

library(metafor)
library(gridExtra)  # Load the gridExtra package

# Assuming 'meta1' is your meta-analysis object

# Create an empty list to store the indices of each study in 'meta1'
study_indices <- list()

# Loop through unique study names and find their corresponding indices in 'meta1'
for (study_name in unique(meta1$study)) {
  study_indices[[study_name]] <- which(meta1$study == study_name)
}

# Create the forest plot with effect sizes grouped by study
forest_list <- lapply(study_indices, function(indices) {
  forest(meta1, subset = indices, header = TRUE, addfit = TRUE, order = "obs", main = "")
})

# Combine forest plots into one
combined_forest <- do.call("grid.arrange", c(forest_list, nrow = length(forest_list)))
print(combined_forest)