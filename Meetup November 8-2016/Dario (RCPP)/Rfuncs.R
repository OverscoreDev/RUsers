require(Rcpp)
sourceCpp("cppfuncs.cpp")

vecIndices_0 <- function(v,bks){
  1+rowSums(outer(v, bks, FUN = ">"))
}


vecIndices_1 <- function(v, bks)
{
  w <- rep(1,length(v))
  
  for(b in 1:length(bks)){
    for(i in 1:length(w)){
      if(v[i] > bks[b]){
        w[i] <- b+1
      }
    }
  }
  return(w)
}

vecIndices_2 <- function(v, bks){
  w <- rep(1,length(v))
  for(b in 1:length(bks)){
    w[v>bks[b]] <- b+1
  }
  return(w)
}

vecIndices_3 <- function(v, bks){
  vs <- order(v)
  vi <- order(vs)
  vsorted <- v[vs]

  w <- rep(1, length(v))

  curBreak <- 1
  for(curIdx in 1:length(v)){
        while(curBreak <= length(bks) && vsorted[curIdx] > bks[curBreak]){
          curBreak <- curBreak + 1
        }
        w[curIdx] <- curBreak
  }
  return(w[vi])
}


vecIndices_4 <- function(v, bks){
  vs <- order(v)
  vi <- order(vs)
  vsorted <- v[vs]
  w <- vecIndices_cxx(vsorted, bks)
  return(w[vi])
}


for(i in 1:10){
  v <- 80+20*rnorm(100000)
  buckets <- sort(160*runif(sample(20:500, 1)))
  df <- data.frame(v=v)
  df$bucket0 <- vecIndices_0(v, buckets)
#  df$bucket1 <- vecIndices_1(v, buckets)
  df$bucket2 <- vecIndices_2(v, buckets)
  df$bucket3 <- vecIndices_3(v, buckets)
  df$bucket4 <- vecIndices_4(v, buckets)
}














#' windowedQdist
#' Performs a windowed distance computation across a vector of quaternions [C++ function for speed]
#'
#' @param qx    x coordinate of quaternion vector
#' @param qy    y coordinate of quaternion vector
#' @param qz    z coordinate of quaternion vector
#' @param qw    w coordinate of quaternion vector
#' @param wnd   window for distances
#'
#' @return user with additional qVariation and accTotal columns in loc and mot tables
#' @export
#'
#' @examples
cppFunction("
NumericVector windowedQdist(NumericVector qx, NumericVector qy, NumericVector qz, NumericVector qw, int wnd)
{
  int L = qx.size();
  NumericVector out(L);
  for(int i=wnd; i<L; i++){
    double x = qx[i]*qx[i-wnd] + qy[i]*qy[i-wnd] + qz[i]*qz[i-wnd] + qw[i]*qw[i-wnd];
    x = x>0?x:-x;
    x = x>1?1:x;
    out[i] = 2*acos(x)/3.14159265;
  }
  return(out);
}")
