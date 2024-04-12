/*
 * cafemielk.c
 */

#include "cafemielk.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_cafemielk(void)
{
    return SCM_MAKE_STR("cafemielk is working");
}

ScmObj nil_vector(void)
{
  return Scm_MakeVector(3, SCM_NIL);
}

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
