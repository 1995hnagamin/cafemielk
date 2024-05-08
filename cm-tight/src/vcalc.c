#include "vcalc.h"

double fddot(int dim, double u[], double v[]) {
  double sum = 0;
  for (int i = 0; i < dim; ++i) {
    sum += u[i] * v[i];
  }
  return sum;
}

void fdwrite_cross3d(double out[3], double u[3], double v[3]) {
  out[0] = u[1] * v[2] - u[2] * v[1];
  out[1] = u[2] * v[0] - u[0] * v[2];
  out[2] = u[0] * v[1] - u[1] * v[0];
}
