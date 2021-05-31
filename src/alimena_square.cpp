#include "alimena_square.h"
#include <cmath>

using std::vector;
using std::string;

Alimena_Square::Alimena_Square(const vector<string> &labels) :
  Latin_Square(labels) {

  if (Alimena_Square::is_prime(m_dim + 1)) {
    for (int j = 0; j < m_dim; j++) { // for each column
      int stepsize = j + 1;
      int offset = 0;
      for (int i = 0; i < m_dim; i++) { // for each row
	int thisrow = offset + (i == 0 ? j : stepsize);
	offset = (thisrow % m_dim) - (thisrow >= m_dim);
	ppimx[offset][j] = i;
      }
    }
    build_string_matrix();
  } else {
    // TODO: error; do something
  }
}

Alimena_Square::~Alimena_Square() {
}

bool Alimena_Square::is_prime(const int &n) {
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
