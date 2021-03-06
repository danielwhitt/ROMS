#include "cppdefs.h"
      MODULE tl_biology_mod
#if defined TANGENT && defined BIOLOGY
!
!svn $Id: tl_biology.F 751 2015-01-07 22:56:36Z arango $
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the tangent linear source and sink terms for  !
!  the selected biology model.                                         !
!                                                                      !
!=======================================================================
!
      implicit none

      PRIVATE
      PUBLIC  :: tl_biology

      CONTAINS

# if defined BIO_FENNEL
!!#  include <tl_fennel.h>
# elif defined NEMURO
!!#  include <tl_nemuro.h>
# elif defined NPZD_FRANKS
#  include <tl_npzd_Franks.h>
# elif defined NPZD_IRON
#  include <tl_npzd_iron.h>
# elif defined NPZD_POWELL
#  include <tl_npzd_Powell.h>
# endif

#endif
      END MODULE tl_biology_mod
