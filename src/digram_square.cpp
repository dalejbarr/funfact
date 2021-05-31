#include "digram_square.h"
#include <cmath>

using std::vector;
using std::string;

Digram_Square::Digram_Square(const vector<string> &labels) :
  Latin_Square(labels) {

  // fill in first row
  if (Digram_Square::is_even(m_dim)) {
    for (int j = 0; j < (m_dim / 2); j++) {
      int j_fwd = j * 2;
      int j_bwd = m_dim - 1 - j * 2;
      ppimx[0][j_fwd] = j;
      ppimx[0][j_bwd] = j + m_dim / 2;
    }
  } else {
    // for squares with odd rank
    for (int j = 0; j <= (int) ceil(m_dim / 2); j++) {
      ppimx[0][j * 2] = j;
    }
    for (int j = 0; j < (int) floor(m_dim / 2); j++) {
      ppimx[0][m_dim - 2 - j * 2] = j + m_dim / 2 + 1;
    }
  }

  // fill in columns
  for (int j = 0; j < m_dim; j++) {
    int offset = ppimx[0][j];
    for (int i = 1; i < m_dim; i++) {
      ppimx[i][j] = (offset + i) % m_dim;
    }
  }

  build_string_matrix();
}

Digram_Square::~Digram_Square() {
}
