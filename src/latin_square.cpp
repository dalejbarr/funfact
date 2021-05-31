#include "latin_square.h"

using std::string;
using std::vector;

Latin_Square::Latin_Square(const vector<string> &labels) :
  ppimx(nullptr), ppsmx(nullptr) {
  m_dim = labels.size();
  m_labels = labels;

  // TODO: check that m_dim and m_labels are reasonable
  // e.g., ensure all values of m_labels are unique

  ppimx = new int *[m_dim];
  for (int i = 0; i < m_dim; i++) {
    ppimx[i] = new int[m_dim];
  }
}

Latin_Square::~Latin_Square() {
  if (ppimx != nullptr) {
    for (int i = 0; i < m_dim; i++) {
      if (ppimx[i] != nullptr) {
	delete [] ppimx[i];
	ppimx[i] = nullptr;
      }
    }
    delete [] ppimx;
    ppimx = nullptr;
  }

  if (ppsmx != nullptr) {
    for (int i = 0; i < m_dim; i++) {
      if (ppsmx[i] != nullptr) {
	delete [] ppsmx[i];
	ppsmx[i] = nullptr;
      }
    }
    delete [] ppsmx;
    ppsmx = nullptr;
  }
}

void Latin_Square::build_string_matrix() {
  ppsmx = new string *[m_dim];
  for (int i = 0; i < m_dim; i++) {
    ppsmx[i] = new string[m_dim];
  }

  for (int i = 0; i < m_dim; i++) {
    for (int j = 0; j < m_dim; j++) {
      ppsmx[i][j] = m_labels[ppimx[i][j]];
    }
  }
}
