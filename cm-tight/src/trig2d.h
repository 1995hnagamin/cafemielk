#ifndef TRIG2D_H
#define TRIG2D_H

typedef union fdtrig2d {
	double data[6];
	struct {
		double x[3];
		double y[3];
	} cd;
} fdtrig2d_t;

double fdtrig2d_prod(fdtrig2d_t t, double cov[3], double vec[3]);

#endif /* TRIG2D_H */
