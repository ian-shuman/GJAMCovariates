library(corrplot)

#Version for 1:1 residual correlations
#mat.data1 <- c(1, 0.05, 0.05, 0.05, 1, 0.05, 0.05, 0.05, 1)

#Version for non 1:1 residual correlations
mat.data1 <- c(1, -0.99, 0.05, -0.99, 1, -0.99, 0.05, -0.99, 1)


mat1 <- matrix(mat.data1, nrow = 3, ncol = 3)
#View(mat1)
colnames(mat1) <- c("Prairie", "Savanna", "Forest")
rownames(mat1) <- c("Prairie", "Savanna", "Forest")
# Remove diagonals to improve visualization
mat1[mat1 == 1] <- NA

# Specify color palette
pal <- c('#364b9a', '#4a7bb7', '#6ea6cd', '#93cae1', '#cde4ef', 
         '#eaeccc', '#feda8b', '#fdb336', '#f67e4b', '#dd3d2d', '#a50026')
# Plot
corrplot(mat1, diag = F, type = 'upper', method = c("circle"), tl.col = 'black', col = rev(pal), cl.cex = 1, tl.cex =2.5, cl.align.text = 'l', addgrid.col = NA)

#Version for  1:1 residual correlations
mat.data2 <- c(0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)


mat2 <- matrix(mat.data2, nrow = 3, ncol = 3)
#View(mat1)
colnames(mat2) <- c("Prairie", "Savanna", "Forest")
rownames(mat2) <- c("Prairie", "Savanna", "Forest")
# Remove diagonals to improve visualization
mat2[mat2 == 1] <- NA

# Specify color palette

pal <- c('#364b9a', '#4a7bb7', '#6ea6cd', '#93cae1', '#cde4ef', 
         '#eaeccc', '#feda8b', '#fdb336', '#f67e4b', '#dd3d2d', '#a50026')
# Plot
corrplot(mat2, diag = F, type = 'upper', method = c("circle"), tl.col = 'black', col = rev(pal), cl.cex = 1, tl.cex =2.5, cl.align.text = 'l', addgrid.col = NA)





##Make conceptual corrplots for Taxon level 
pal <- c('#364b9a', '#4a7bb7', '#6ea6cd', '#93cae1', '#cde4ef', 
         '#eaeccc', '#feda8b', '#fdb336', '#a50026', '#dd3d2d', '#a50026') #Changed pal and circle size for aesthetics
tax_data <- c(0.05, -0.5, -0.5, 0.05, 0.05, -0.5, 0.05, 0.05, -0.5, -0.5, -0.5, 0.05, 0.05, -0.5, -0.5, 0.05, -0.5, -0.5, 0.05, 0.05, 0.05, -0.5, -0.5, 0.05, 0.05)
tax <- matrix(tax_data, nrow = 5, ncol = 5)
colnames(tax) <- c("No tree", "Oak", "Hickory", "Beech", "Maple")
rownames(tax) <- c("No tree", "Oak", "Hickory", "Beech", "Maple")
corrplot(tax, diag = F, type = 'upper', method = c("circle"), tl.col = 'black', col = rev(pal), cl.cex = 1, tl.cex =2.5, cl.align.text = 'l', addgrid.col = NA)
#Warning thrown bc of changes in pal

tax_data2 <- c(0.05, -0.05, -0.05, 0.05, 0.05, -0.05, 0.05, 0.05, -0.05, -0.05, -0.05, 0.05, 0.05, -0.05, -0.05, 0.05, -0.05, -0.05, 0.05, 0.05, 0.05, -0.05, -0.05, 0.05, 0.05)
tax2 <- matrix(tax_data2, nrow = 5, ncol = 5)
colnames(tax2) <- c("No tree", "Oak", "Hickory", "Beech", "Maple")
rownames(tax2) <- c("No tree", "Oak", "Hickory", "Beech", "Maple")
corrplot(tax2, diag = F, type = 'upper', method = c("circle"), tl.col = 'black', col = rev(pal), cl.cex = 1, tl.cex =2.5, cl.align.text = 'l', addgrid.col = NA)
#Warning thrown bc of changes in pal




