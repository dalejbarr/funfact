#include "latin_square.h"

class Digram_Square : public Latin_Square {
 public:
  Digram_Square(const std::vector<std::string> &labels);
  virtual ~Digram_Square();
  static inline bool is_even(const int &n) { return !(n % 2); };
};
