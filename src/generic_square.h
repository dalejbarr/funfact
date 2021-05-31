#ifndef GENERIC_SQUARE_H
#define GENERIC_SQUARE_H

#include "latin_square.h"

class Generic_Square : public Latin_Square {
public:
  Generic_Square(const std::vector<std::string> &labels);
  virtual ~Generic_Square();
};

#endif
