#ifndef LATIN_SQUARE_H
#define LATIN_SQUARE_H

#include <vector>
#include <string>

class Latin_Square {
 protected:
  int m_dim;
  std::vector<std::string> m_labels;
  int ** ppimx;
  std::string ** ppsmx;
  void build_string_matrix();
 public:
  Latin_Square(const std::vector<std::string> &labels);
  virtual ~Latin_Square();
  std::string ** get() { return ppsmx; };
  int dim() { return m_dim; };
};

#endif
