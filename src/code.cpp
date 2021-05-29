#include <Rcpp.h>
#include "RandIx_R.h"
#include "intern.h"

using namespace Rcpp;

using std::vector;
using std::string;
using std::size_t;

RandIx_R rand_R;

// [[Rcpp::export]]
List alimena_square(CharacterVector labels) {
  vector<vector<int>> mx = alimena_square_intern(labels.size());
  vector<vector<string>> result(mx.size());
  for (size_t i = 0; i < mx.size(); i++) {
  }

  for (size_t i = 0; i < mx.size(); i++) {
    result[i] = vector<string>(mx[i].size());
    for (size_t j = 0; j < mx[i].size(); j++) {
      result[i][j] = labels[mx[i][j]];
    }
  }
  
  return wrap(result);
}

/*
int factorial(int n)
{
  return (n == 1 || n == 0) ? 1 : factorial(n - 1) * n;
}
*/

// [[Rcpp::export]]
List prs_factor(CharacterVector labels, int nreps, int maxrun = 0)
{
  std::vector<std::vector<std::string>> result =
    prs_factor_intern(as<std::vector<std::string>>(labels), nreps, maxrun,
		      (RandIx *) &rand_R);

  return wrap(result);
}
