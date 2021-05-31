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

int factorial(int n) {
  return (n == 1 || n == 0) ? 1 : factorial(n - 1) * n;
}

vector<vector<string>> all_perm(vector<string> labels) {
  size_t nperms = (size_t) factorial(labels.size());
  vector<vector<string>> result(nperms);

  sort(labels.begin(), labels.end());
  result[0] = labels;
  for (size_t i = 1; i < nperms; i++) {
    next_permutation(labels.begin(), labels.end());
    result[i] = labels;
  }
  
  return result;
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

