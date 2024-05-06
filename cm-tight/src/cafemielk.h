/*
 * cafemielk.h
 */

/* Prologue */
#ifndef GAUCHE_CAFEMIELK_H
#define GAUCHE_CAFEMIELK_H

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

extern ScmObj square_point_vec(ScmObj xvec, ScmObj yvec);
extern ScmObj square_triangle_vec(int nx, int ny);


/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_CAFEMIELK_H */
