#include "cppdefs.h"
#ifdef BIOLOGY
!!
!!svn $Id: read_biopar.F 751 2015-01-07 22:56:36Z arango $
!!================================================= Hernan G. Arango ===
!! Copyright (c) 2002-2015 The ROMS/TOMS Group                         !
!!   Licensed under a MIT/X style license                              !
!!   See License_ROMS.txt                                              !
!!======================================================================
!!                                                                     !
!!  This routine reads and reports selected biological model input     !
!!  parameters.                                                        !
!!                                                                     !
!!======================================================================
!!
# if defined BIO_FENNEL
#  include <fennel_inp.h>
# elif defined ECOSIM
#  include <ecosim_inp.h>
# elif defined NEMURO
#  include <nemuro_inp.h>
# elif defined NPZD_FRANKS
#  include <npzd_Franks_inp.h>
# elif defined NPZD_IRON
#  include <npzd_iron_inp.h>
# elif defined NPZD_POWELL
#  include <npzd_Powell_inp.h>
# endif
#else
      SUBROUTINE read_BioPar
      END SUBROUTINE read_BioPar
#endif
