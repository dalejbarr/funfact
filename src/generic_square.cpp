#include "generic_square.h"

using std::vector;
using std::string;

Generic_Square::Generic_Square(const vector<string> &labels) :
  Latin_Square(labels) {

  for (int i = 0; i < m_dim; i++) {
    int offset = i;
    for (int j = 0; j < m_dim; j++) { 
      int jx = (offset + j) % m_dim;
      ppimx[i][jx] = j;
    }
  }

  build_string_matrix();
}

Generic_Square::~Generic_Square() {
}
