/*
** svn $Id: geostrophic.h 585 2012-01-03 18:44:28Z arango $
*******************************************************************************
** Copyright (c) 2002-2012 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for FRONTAL_ZONE 
**
** Application flag:  FRONTAL_ZONE 
*/

#define UV_ADV
#define UV_COR
#define UV_VIS2
#define MIX_S_UV
#define UV_C2ADVECTION 
#define TS_A4HADVECTION
#define TS_A4VADVECTION
#undef TS_MPDATA
#define DJ_GRADPS
#define UV_QDRAG
#define TS_DIF2
#define MIX_S_TS

/*#define SALINITY*/
#undef SALINITY
#define SOLVE3D
#define SPLINES
#define AVERAGES
#define DIAGNOSTICS_TS
#define DIAGNOSTICS_UV

#define ANA_GRID
#define T_PASSIVE
#define ANA_PASSIVE
#define ANA_SPFLUX
#define ANA_BPFLUX
/*#define ANA_INITIAL*/
#undef  ANA_INITIAL
#define ANA_SMFLUX
/* to use wind forcing netcdf file*/
/*#undef ANA_SMFLUX*/
#define ANA_STFLUX
#define ANA_SRFLUX
#define ANA_SSFLUX
#define ANA_BTFLUX
#define ANA_BSFLUX


#define BC_ZONE
#define FOXKEMPER
/* NOTE: viscous transport of the background momentum only works with splines on right now*/
#define BC_ZONE_MOMFLX

#define LMD_MIXING
/*#define RI_HORAVG*/
/*#define RI_VERAVG*/
#ifdef LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
#define LMD_SKPP
/*#define LMD_BKPP*/
#define LMD_NONLOCAL
#endif

/*#define FLOATS*/
/*#define FLOAT_VWALK*/
#define NPZD_POWELL
#define SPITZ
#define CONST_PAR
/*#undef MP78GRAZING*/
#if defined BIO_FENNEL  || defined ECOSIM || \
    defined NPZD_POWELL || defined NEMURO
/*# define ANA_BIOLOGY*/
# undef ANA_BIOLOGY
# define ANA_SPFLUX
# define ANA_BPFLUX
#endif

#ifdef PERFECT_RESTART
# undef  AVERAGES
# undef  DIAGNOSTICS_BIO
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# define OUT_DOUBLE
#endif
