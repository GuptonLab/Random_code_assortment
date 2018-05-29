#read in the data file. in the code:
#File = " ",
#replace what's in quitations with the path to the CSV file with your data in it.

tsn1 <- read.table(file="D:/Melissa/ANCOVA_data.csv",header=T, sep=",")


#Perform a MANOVA to see how tsnare1 affects the correlation and peak height values.
#Below, "corr" and "y" are the correlatio and peak height, and "tsn1" is the presence of tsnare1
#or not. Make sure the columns in the data table are named appropriately.
tsn1_manova <- manova(cbind(corr,y) ~tsn1, data = tsn1)

summary(tsn1_manova, test = "Wilks") #looks at MANOVA p-value, which is the "Pr (>F)" in the table.
