
# If is.pc then a value like "1" means 0.01
remap.cumsum <- function(z, is.pc = FALSE, base = 0) {
  z <- firstValueBase(z)
  for (i in 1:NCOL(z)) {
    tmp <- z[,i]
    if (is.pc) {
      tmp <- tmp/100#转换为百分制
    }
    if(NCOL(z) == 1){#base为基数
      z <- base + cumsum(tmp)
    } else {
      z[,i] <- base + cumsum(tmp)
    }
  }
  z#其实就是简单的累计求和
}

firstValueBase <- function(x){#令第一行全为0
  if(NCOL(x)==1){
   x[1] <- 0
 } else {
    x[1, ] <- 0
  }
  return(x)
}
