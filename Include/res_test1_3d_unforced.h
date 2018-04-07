/*
** svn $Id: geostrophic.h 585 2012-01-03 18:44:28Z arango $
*******************************************************************************
** Copyright (c) 2002-2012 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for GEOSTROPHIC3D 
**
** Application flag:  GEOSTROPHIC3D 
*/

#define UV_ADV
#define UV_COR
#define UV_VIS2
#define MIX_S_UV
#undef TS_U3HADVECTION
#undef TS_C4VADVECTION
#define TS_MPDATA
#define DJ_GRADPS
#define UV_QDRAG
#define TS_DIF2
#define MIX_ISO_TS

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
/*#define ANA_INITIAL*/
#undef  ANA_INITIAL
/* to use wind forcing netcdf file*/
#define ANA_SMFLUX
#define ANA_STFLUX
#define ANA_SRFLUX
#define ANA_SSFLUX
#define ANA_BTFLUX
#define ANA_BSFLUX
/*#define BC_ZONE*/
#define ANA_VMIX
#undef LMD_MIXING
#ifdef LMD_MIXING
#define LMD_RIMIX
#define LMD_CONVEC
/*#define LMD_SKPP*/
/*#define LMD_BKPP*/
/*#define LMD_NONLOCAL*/
/*#define LMD_SHAPIRO*/
#endif

/*#if defined GLS_MIXING || defined MY25_MIXING
**# define KANTHA_CLAYSON
**# define N2S2_HORAVG
**#else
**# define ANA_VMIX
**#endif*/
/*#define FLOATS*/
/*#define FLOAT_VWALK*/
#define NPZD_POWELL
#define SPITZ
/*#undef CONST_PAR*/
/*#undef MP78GRAZING*/
#if defined BIO_FENNEL  || defined ECOSIM || \
    defined NPZD_POWELL || defined NEMURO
/*# define ANA_BIOLOGY*/
# undef ANA_BIOLOGY
# define ANA_SPFLUX
# define ANA_BPFLUX
#endif

#if defined NEMURO
# define HOLLING_GRAZING
# undef  IVLEV_EXPLICIT
#endif

#ifdef BIO_FENNEL
# define CARBON
# define DENITRIFICATION
# define BIO_SEDIMENT
# define DIAGNOSTICS_BIO
#endif

#ifdef PERFECT_RESTART
# undef  AVERAGES
# undef  DIAGNOSTICS_BIO
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# define OUT_DOUBLE
#endif
