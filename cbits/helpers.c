#include "helpers.h"

OniVersion* helper_oniGetVersion() {
  // Use static variable (it's only 4 ints)
  static OniVersion v;
  v = oniGetVersion();
  return &v;
}
