#include "trig2d.h"

#include "vcalc.h"

double fdtrig2d_prod(fdtrig2d_t *t, double cov[3], double vec[3]) {
  double a[3];
  fdwrite_cross3d(a, t->cd.x, t->cd.y);
  double b[3] = {
      t->cd.y[2] - t->cd.y[0],
      t->cd.y[0] - t->cd.y[1],
      t->cd.y[1] - t->cd.y[2],
  };
  double c[3] = {
      t->cd.x[2] - t->cd.x[1],
      t->cd.x[0] - t->cd.x[2],
      t->cd.x[1] - t->cd.x[0],
  };
  return fddot(3, cov, a) * vec[0] + fddot(3, cov, b) * vec[1] +
         fddot(3, cov, c) * vec[2];
}
