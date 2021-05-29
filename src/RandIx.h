#ifndef GUARD_RandIx_h
#define GUARD_RandIx_h

#include <vector>

// abstract class for randomly permuting indices
class RandIx {
public:
  RandIx() {}
  virtual ~RandIx();
  virtual std::vector<std::size_t> permute(std::size_t n) = 0;
  virtual std::vector<int> permute(int n) = 0;
};

#endif
