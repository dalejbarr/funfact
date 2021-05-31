#ifndef ALIMENA_SQUARE_H
#define ALIMENA_SQUARE_H

#include "latin_square.h"

// see Alimena, B. S. (1962)
// "A Method of Determining Unbiased Distribution in the Latin Square"
// Psychometrika, 27(2).
// counterbalance all immediate and remote effects
// only works if n + 1 is prime (e.g., 2, 4, 6, 10, 12, 16, 18)
class Alimena_Square : public Latin_Square {
 public:
  Alimena_Square(const std::vector<std::string> &labels);
  static bool is_prime(const int &n);
  virtual ~Alimena_Square();
};

#endif
