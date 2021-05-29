#include <vector>
#include <algorithm>
#include <cmath>
#include "intern.h"

#include <iostream>
using std::cout;
using std::endl;

using std::vector;
using std::string;
using std::size_t;

bool is_prime(const int &n) {
  bool result = true;

  if (n >= 2) {
    for (int i = 2; i <= sqrt(n); i++) {
      if (!(n % i)) {
	result = false;
	break;
      }
    }
  } else {
    result = false;
  }

  return result;
}

// see Alimena, B. S. (1962)
// "A Method of Determining Unbiased Distribution in the Latin Square"
// Psychometrika, 27(2).
// counterbalance all immediate and remote effects
// only works if n + 1 is prime (e.g., 2, 4, 6, 10, 12, 16, 18)
vector<vector<int>> alimena_square_intern(const int &n)
{
  if (is_prime(n + 1)) {
    // allocate
    size_t nn = (size_t) n;
    vector<vector<int>> result(n);
    for (size_t i = 0; i < (size_t) n; i++) {
      result[i] = vector<int>(n);
    }

    // for each column...
    for (size_t j = 0; j < nn; j++) {
      size_t stepsize = j + 1;
      size_t offset = 0;
      for (size_t i = 0; i < nn; i++) {
	size_t thisrow = offset + (i == 0 ? j : stepsize);
	offset = (thisrow % nn) - (thisrow >= nn);
	//cout << offset << "/" <<
	//thisrow << ", j=" << j << " (i=" << i << "): " << i << endl;
	if (offset < nn) {
	  result[offset][j] = (int) i;
	} else {
	  // TODO: throw an error
	  cout << "ERROR! skipped" << endl;
	}
      }
    }
    
    return result;
  } else {
    return vector<vector<int>>{};
  }
}

// generate PseudoRandom Sequence of labels
vector<vector<string>> prs_factor_intern(const vector<string> &labels,
					 const int &nreps,
					 const int &maxrun,
					 RandIx * prand)
{
  // ERROR CHECKING
  if (labels.size() < 1) {
    // TODO: error checking
  }
  // TODO: make sure all labels are unique
  
  vector<size_t> seq(labels.size()),
    single(labels.size() * nreps);
  
  // first time
  seq = prand->permute(labels.size());
  for (size_t i = 0; i < seq.size(); i++) {
    single[i] = seq[i];
  }

  // each repetition
  for (int n = 1; n < nreps; n++) {
    size_t last_value = seq[labels.size() - 1];
    seq = prand->permute(labels.size());
    // prevent between-bucket runs
    if (seq[0] == last_value) {
      vector<size_t> next = prand->permute(labels.size() - 1);
      seq[0] = seq[next[0] + 1];
      seq[next[0] + 1] = last_value;
    }
    // copy over
    for (size_t i = 0; i < seq.size(); i++) {
      single[n * seq.size() + i] = seq[i];
    }
  }
  
  vector<vector<string>> result(labels.size());

  for (vector<size_t>::const_iterator it = single.cbegin();
       it != single.cend(); it++) {
    for (size_t i = 0; i < labels.size(); i++) {
      result[i].push_back(labels[(*it + i) % labels.size()]);
    }
  }
  
  return result;
}
