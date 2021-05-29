#ifndef RandIx_R_h
#define RandIx_R_h

#include "RandIx.h"

class RandIx_R : public RandIx {
public:
  RandIx_R() {}
  virtual ~RandIx_R();
  virtual std::vector<std::size_t> permute(std::size_t n);
  virtual std::vector<int> permute(int n);
};

#endif
