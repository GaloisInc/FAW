/* V */
/* This file contains structures to support the NITF 2.0 file format. */

/*****************************************************************************
* This software and any accompanying documentation is released "as is."  The
* government makes no warranty of any kind, express or implied,
* concerning this software and any accompanying documentation, including,
* without limitation, any warranties of merchantability or fitness for a
* particular purpose.  In no event will the U.S. government be liable for any
* damages, including any lost profits, lost savings or other incidental or
* consequential damages arising out of the use, or inability to use, this
* software or any accompanying documentation, even if informed in advance of
* the possibility of such damages.
*****************************************************************************/

/*****************************************************************************
*
*       Software developed at AFRL/SNAS for the DDB Project
*
* File Author:       Jim Stadler, Veridian Engineering
* Creation Date:
* Version:
*
* File Description:
*       This file contains structures to support the NITF 2.0 file format.
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/

/*******************************************************************************
 * Structure Name: nitf_20_main_type
 *
 * Description:
 *
 ******************************************************************************/

#include "nitf_s21.h"



typedef struct {

   char fhdr[9];

   char clevel[2];

   char stype[4];

   char ostaid[10];

   char fdt[14];

   char ftitle[80];

   char fsclas[1];

   char fscode[40];

   char fsctlh[40];

   char fsrel[40];

   char fscaut[20];

   char fsctln[20];

   char fsdwng[6];

    char fsdevt[40];

    char fscop[5];

    char fscpys[5];

    char encryp[1];

    char oname[27];

    char ophone[18];

    char fl[12];

    char hl[6];

    } nitf_20_main_type;


typedef struct {
    char im[2];
    char iid[10];
    char idatim[14];
    char tgtid[17];
    char ititle[80];
    char isclass[1];

    char iscode[40];
    char isctlh[40];
    char isrel[40];

    char iscaut[20];

    char isctln[20];

    char isdwng[6];
    char isdevt[40];

    char encryp[1];
    char isorce[42];
    char nrows[8];
    char ncols[8];
    char pvtype[3];
    char irep[8];
    char icat[8];
    char abpp[2];
    char pjust[1];
    char icords[1];
    char igeolo[60]; /* c */
    char nicom[1];
    char *pIcomn;  /* ptr to image comments of length 80 */
    char ic[2];
    char comrat[4]; /* c */
    char nbands[1];

    image_band_data_type *pBand_data;
    char isync[1];
    char imode[1];
    char nbpr[4];
    char nbpc[4];
    char nppbh[4];
    char nppbv[4];
    char nbpp[2];
    char idlvl[3];
    char ialvl[3];
    char iloc[10];
    char imag[4];
    char udidl[5];
    char udofl[3]; /*c */
    char *pUdid;    /*c pointer to user defined image data */
    char ixshdl[5];
    char ixsofl[3]; /*c */
    char *pIxshd;   /*c pointer to extended subheader data */

/* image data mask table */

    long imdatoff; /*c */
    unsigned short bmrlnth; /*c */
    unsigned short tmrlnth; /*c */
    unsigned short tpxcdlnth; /*c */
    long tpxcd;     /* c pad output pixel code */
    /*char BMRnBNDm */
    } nitf_20_imagesub_type;


/* Defined in nitf_s21.h */
#ifdef REMOVED
typedef struct {
    char IREPBANDnn[2];
    char ISUBCATnn[6];
    char IFCnn[1];
    char IMFLTnn[3];
    char NLUTSnn[1];
    char NELUTnn[5]; /*c */
    char *pLUTDnnm;  /* ptr to nnth band data of the mth LUT.  NULL if nlutsnn=0 */
} image_band_data_type;
#endif


typedef struct {
    char sy[2];
    char sid[10];
    char sname[20];
    char ssclas[1];
    char sscode[40];
    char ssctlh[40];
    char ssrel[40];
    char sscaut[20];
    char ssctln[20];
    char ssdwng[6];
    char ssdevt[40];
    char encryp[1];
    char stype[1];
    char nlips[4];
    char npixpl[4];
    char nwdth[4];
    char nbpp[1];
    char sdlvl[3]; /* c */
    char salvl[3];
    char sloc[10];
    char sloc2[10];
    char scolor[1];
    char snum[6];
    char srot[3];

    char nelut[3];
    char *pDlut;

    char sxshdl[5];

    char sxsofl[3];
    char *pSxshd;
    } nitf_20_symbolsub_type;

typedef struct {
    char la[2];
    char lid[10];
    char lsclas[1];
    char lscode[40];
    char lsctlh[40];
    char lsrel[40];
    char lscaut[20];
    char lsctln[20];
    char lsdwng[6];
    char lsdevt[40];
    char encryp[1];

    char lfs[1];
    char lcw[2];
    char lch[2];
    char ldlvl[3]; /* c */
    char lalvl[3];
    char lloc[10];
    char ltc[3];
    char lbc[3];

    char lxshdl[5];

    char lxsofl[3];
    char *pLxshd;
    } nitf_20_labelsub_type;



typedef struct {
    char te[2];
    char textid[10];

    char txtdt[14];
    char txtitl[80];

    char tsclas[1];
    char tscode[40];
    char tsctlh[40];
    char tsrel[40];
    char tscaut[20];
    char tsctln[20];
    char tsdwng[6];
    char tsdevt[40];
    char encryp[1];

    char txtfmt[3];
    char txshdl[5];

    char txsofl[3];
    char *pTxshd;
    } nitf_20_textsub_type;

typedef struct {
    char de[2];
    char destag[25];
    char desver[2];

    char desclas[1];
    char descode[40];
    char desctlh[40];
    char desrel[40];
    char descaut[20];
    char desctln[20];
    char desdwng[6];
    char desdevt[40];

    char desoflw[6];
    char desitem[3];
    char desshl[4];
    char *pDesshf;
    char *pDesdata;
    } nitf_20_dessub_type;


typedef struct {
    char p[1];
    } nitf_20_ressub_type;


