#include <Rcpp.h>
#include "RandIx_R.h"
#include "intern.h"
#include "alimena_square.h"
#include "generic_square.h"
#include "digram_square.h"

using namespace Rcpp;

using std::vector;
using std::string;
using std::size_t;

// debugging
#include <iostream>
using std::cout;
using std::endl;

RandIx_R rand_R;

// [[Rcpp::export]]
CharacterMatrix generic_square(CharacterVector labels) {
  CharacterMatrix result(labels.size(), labels.size());
  Generic_Square gsqr(as<vector<string>>(labels));

  for (int i = 0; i < gsqr.dim(); i++) {
    for (int j = 0; j < gsqr.dim(); j++) {
      result(i, j) = gsqr.get()[i][j];
    }
  }

  return result;
}

// [[Rcpp::export]]
CharacterMatrix digram_square_even(CharacterVector labels) {
  CharacterMatrix result(labels.size(), labels.size());

  if (Digram_Square::is_even(labels.size())) {  
    Digram_Square dsqr(as<vector<string>>(labels));

    for (int i = 0; i < dsqr.dim(); i++) {
      for (int j = 0; j < dsqr.dim(); j++) {
	result(i, j) = dsqr.get()[i][j];
      }
    }
  }

  return result;
}

// [[Rcpp::export]]
List digram_square_odd(CharacterVector labels) {
  CharacterMatrix result1(labels.size(), labels.size());
  CharacterMatrix result2(labels.size(), labels.size());

  if (!Digram_Square::is_even(labels.size())) {  
    Digram_Square dsqr(as<vector<string>>(labels));

    for (int i = 0; i < dsqr.dim(); i++) {
      for (int j = 0; j < dsqr.dim(); j++) {
	result1(i, j) = dsqr.get()[i][j];
	// reflect columns
	result2(i, dsqr.dim() - j - 1) = dsqr.get()[i][j];
      }
    }
  }

  List result = List::create(result1, result2);
  
  return result;
}

// [[Rcpp::export]]
CharacterMatrix alimena_square(CharacterVector labels) {

  if (Alimena_Square::is_prime(labels.size() + 1)) {
    Alimena_Square asqr(as<vector<string>>(labels));
    CharacterMatrix result( asqr.dim(), asqr.dim() );

    for (int i = 0; i < asqr.dim(); i++) {
      for (int j = 0; j < asqr.dim(); j++) {
	result(i, j) = asqr.get()[i][j];
      }
    }

    return result;
  } else {
    CharacterMatrix badresult(0, 0);
    return badresult;
  }
}

// [[Rcpp::export]]
CharacterMatrix all_permutations(CharacterVector labels) {
  vector<string> labs = as<vector<string>>(labels);
  vector<vector<string>> res = all_perm(labs);
  CharacterMatrix result( factorial(labels.size()), labels.size() );

  for (size_t i = 0; i < res.size(); i++) {
    for (size_t j = 0; j < res[i].size(); j++) {
      result(i, j) = res[i][j];
    }
  }

  return result;
}

// [[Rcpp::export]]
List prs_factor(CharacterVector labels, int nreps, int maxrun = 0)
{
  std::vector<std::vector<std::string>> result =
    prs_factor_intern(as<std::vector<std::string>>(labels), nreps, maxrun,
		      (RandIx *) &rand_R);

  return wrap(result);
}
