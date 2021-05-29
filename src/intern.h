#ifndef INTERN_H_INCLUDED
#define INTERN_H_INCLUDED

#include "RandIx.h"
#include <vector>
#include <string>

std::vector<std::vector<std::string>>
prs_factor_intern(const std::vector<std::string> &labels,
		  const int &nreps,
		  const int &maxrun,
		  RandIx * prand);

std::vector<std::vector<int>> alimena_square_intern(const int &n);

#endif
