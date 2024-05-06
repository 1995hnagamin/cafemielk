/*
 * cafemielk.c
 */

#include "cafemielk.h"

ScmObj square_point_vec(ScmObj xvec, ScmObj yvec)
{
  int nx = SCM_VECTOR_SIZE(xvec), ny = SCM_VECTOR_SIZE(yvec);
  ScmObj rvec = Scm_MakeVector(2 * nx * ny, SCM_NIL);
  ScmObj *prv = SCM_VECTOR_ELEMENTS(rvec);
  for (int j = 0; j < ny; ++j) {
    for (int i = 0; i < nx; ++i) {
      *(prv++) = SCM_VECTOR_ELEMENT(xvec, i);
      *(prv++) = SCM_VECTOR_ELEMENT(yvec, j);
    }
  }
  return rvec;
}

ScmObj square_triangle_vec(int nx, int ny)
{
  ScmObj rvec = Scm_MakeVector(6 * (nx - 1) * (ny - 1), SCM_NIL);
  ScmObj *prv = SCM_VECTOR_ELEMENTS(rvec);
  for (int j = 0; j < ny - 1; ++j) {
    int offset = nx * j;
    for (int i = 0; i < nx - 1; ++i) {
      /*
       * a+nx a+nx+1
       *  #---#
       *  |  /|
       *  | / |
       *  |/  |
       *  #---#
       *  a  a+1
       */
      int a = offset + i;
      *(prv++) = SCM_MAKE_INT(a + nx);
      *(prv++) = SCM_MAKE_INT(a);
      *(prv++) = SCM_MAKE_INT(a + nx + 1);
      *(prv++) = SCM_MAKE_INT(a + 1);
      *(prv++) = SCM_MAKE_INT(a + nx + 1);
      *(prv++) = SCM_MAKE_INT(a);
    }
  }
  return rvec;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_cafemielklib(ScmModule*);

void Scm_Init_cafemielk(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(cafemielk);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("cafemielk", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_cafemielklib(mod);
}
