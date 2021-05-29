#include "RandIx_R.h"
#include <Rcpp.h>

using std::vector;
using std::size_t;

RandIx_R::~RandIx_R() {
}

vector<size_t> RandIx_R::permute(size_t n) {
  static Rcpp::Function fsample("sample");
  static Rcpp::Function fseq("seq_len");
  Rcpp::RObject robj = fseq(n);
  vector<size_t> v = Rcpp::as<vector<size_t>>(fsample(robj));
  for (vector<size_t>::iterator it = v.begin();
       it != v.end(); it++) {
    *it -= 1;
  }
  
  return v;
}

vector<int> RandIx_R::permute(int n) {
  static Rcpp::Function fsample("sample");
  static Rcpp::Function fseq("seq_len");
  Rcpp::RObject robj = fseq(n);
  
  return Rcpp::as<vector<int>>(fsample(robj));
}

