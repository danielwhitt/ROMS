/*
** svn $Id: channel.h 751 2015-01-07 22:56:36Z arango $
*******************************************************************************
** Copyright (c) 2002-2015 The ROMS/TOMS Group                               **
**   Licensed under a MIT/X style license                                    **
**   See License_ROMS.txt                                                    **
*******************************************************************************
**
** Options for periodic uniform channel
**
** Application flag:   CHANNEL
** Input script:       ocean_channel.in
**
**
** Available Driver options:  choose only one and activate it in the
**                            build.sh script (MY_CPP_FLAGS definition)
**
** AD_SENSITIVITY             Adjoint Sensitivity
** AFT_EIGENMODES             Adjoint Finite Time Eigenmodes
** CORRELATION                Background-error Correlation Check
** GRADIENT_CHECK             TLM/ADM Gradient Check
** FORCING_SV                 Forcing Singular Vectors
** FT_EIGENMODES              Finite Time Eigenmodes
** IS4DVAR_OLD                Old Incremental, strong constraint 4DVAR
** IS4DVAR                    Incremental, strong constraint 4DVAR
** NLM_DRIVER                 Nonlinear Basic State trajectory
** OPT_PERTURBATION           Optimal perturbations
** PICARD_TEST                Picard Iterations Test
** R_SYMMETRY                 Representer Matrix Symmetry Test
** S4DVAR                     Strong constraint 4DVAR
** SANITY_CHECK               Sanity Check
** SO_SEMI                    Stochastic Optimals: Semi-norm
** TLM_CHECK                  Tangent Linear Model Check
** W4DPSAS                    Weak constraint 4D-PSAS
** W4DVAR                     Weak constraint 4DVAR
*/


/*
**-----------------------------------------------------------------------------
**  Nonlinear basic state settings.
**-----------------------------------------------------------------------------
*/

#define UV_ADV
#define UV_COR
#define UV_LDRAG
#undef UV_VIS2
#define UV_VIS4

#define ANA_GRID
#undef ANA_INITIAL
#define SOLVE3D
#undef SALINITY
#define ANA_SPONGE

#ifdef SOLVE3D
# define DJ_GRADPS
# undef TS_DIF2
# define TS_DIF4
# define MIX_S_TS
# define MIX_S_UV
# define ANA_SMFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_BTFLUX
#define DIFF_3DCOEF
#define VISC_3DCOEF
#define HMIX_BL
#endif

#ifdef NLM_DRIVER
# define AVERAGES
#endif
#undef AVERAGES
#undef DIAGNOSTICS_UV
#undef DIAGNOSTICS_TS

#define LMD_MIXING

#ifdef LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
#define LMD_SKPP
/*#define LMD_BKPP*/
#define LMD_NONLOCAL
#define LMD_SHAPIRO
#endif
#define INLINE_2DIO
