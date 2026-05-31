# Code to verify element-wise sum that correctly handles not assigned values

mat1 <- matrix(c(NA, NA, 3.1, 2.1), nrow = 2, ncol = 2)
mat2 <- matrix(c(1.1, NA, NA, 4.3), nrow = 2, ncol = 2)
print("First matrix: ")
mat1
print("Second matrix: ")
mat2

print(paste0("sum() returns a single value, which is the sum of all non missing datas: ", sum(mat1, mat2, na.rm = TRUE)))

# Our method performs operations element-wise and gives NA as a result 
# only when both values are missing.
sumNA <- function(a, b){
  if(is.na(a) & is.na(b)){ return(NA) }
  sum(a, b, na.rm = TRUE)               # No need to use return (last line always returned)
}

res <- mapply(sumNA, mat1, mat2)
res <- matrix(res, nrow = nrow(mat1))
print("Element-wise sum with correct NAs handling!")
res