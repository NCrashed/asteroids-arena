module asteroids.math;

extern (C) void sincosf(float a, float* sina, float* cosb) @nogc pure; // from libm

/// Calculate sin and cos at once. Greately reduce calculation time in tight loops.
void sincos(float a, out float sina, out float cosb) @nogc pure {
  sincosf(a, &sina, &cosb);
}
