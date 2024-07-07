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










#x2 <- structure * rnorm(1, -.3, .2) + rnorm(1, .8, .2)
structure2 <- structure * .9 + .15
x3 <- x2 * .15 + .5
plot(c(structure + .9, structure2), c(x2, x3))
