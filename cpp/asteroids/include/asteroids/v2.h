#pragma once

#include <cmath>

/// 2D vector of floats
struct v2f {
  float x;
  float y;

  void rotate(float angle) {
    auto x1 = x * std::cos(angle) - y * std::sin(angle);
    auto y1 = x * std::sin(angle) + y * std::cos(angle);
    x = x1;
    y = y1;
  }

  void scale(float scalar) {
    x *= scalar;
    y *= scalar;
  }

  float dist_squared(const v2f &v2) const {
    auto x1 = v2.x - x;
    auto y1 = v2.y - y;
    return x1*x1 + y1*y1;
  }

  v2f &operator+=(const v2f &rhs) {
    x += rhs.x;
    y += rhs.y;
    return *this;
  }
};
