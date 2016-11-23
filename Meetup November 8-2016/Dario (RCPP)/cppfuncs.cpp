#include <Rcpp.h>
using namespace Rcpp;



// [[Rcpp::export]]
NumericVector vecIndices_cxx(NumericVector v, NumericVector bks)
{
      int nv = v.size();
      int nb = bks.size();
      NumericVector out(nv);
      if(nb == 0){
            for(int i=0; i<nv; i++)out[i]=1; 
            return(out);
      }

      int curBreak = 0;
      for(int curIdx = 0; curIdx < nv; curIdx++){
            while(curBreak < nb && v[curIdx] > bks[curBreak])curBreak++;
            out[curIdx] = curBreak+1;
      }
      return(out);
}


