#include "cppdefs.h"
#ifdef SEDIMENT
!!
!!svn $Id: read_sedpar.F 751 2015-01-07 22:56:36Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!======================================================================
!!                                                                     !
!!  This routine reads and reports sediment model input parameters.    !
!!                                                                     !
!!======================================================================
!!
# include <sediment_inp.h>
#else
      SUBROUTINE read_SedPar
      END SUBROUTINE read_SedPar
#endif
