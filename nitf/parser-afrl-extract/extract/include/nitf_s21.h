/* V */
/* NITF 2.1 Structures */

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
*       This file contains structures to support the NITF file format.
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/


/*******************************************************************************
 * Structure Name: nitf_21_main_type
 *
 * Description:
 *
 ******************************************************************************/

typedef struct {
   char fhdr[9];    /* R */
   char clevel[2];  /* R */
   char stype[4];   /* R */
   char ostaid[10]; /* R */
   char fdt[14];    /* R */
   char ftitle[80]; /* <R> */
   char fsclas[1];  /* R */
   char fsclsy[2];  /* <R> */
   char fscode[11]; /* <R> */
   char fsctlh[2];  /* <R> */
   char fsrel[20];  /* <R> */
   char fsdctp[2];  /* <R> */
   char fsdcdt[8];  /* <R> */
   char fsdcxm[4];  /* <R> */
   char fsdg[1];    /* <R> */
   char fsdgdt[8];  /* <R> */
   char fscltx[43]; /* <R> */
   char fscatp[1];  /* <R> */
   char fscaut[40]; /* <R> */
   char fscrsn[1];  /* <R> */
   char fssrdt[8];  /* <R> */
   char fsctln[15]; /* <R> */
   char fscop[5];   /* R */
   char fscpys[5];  /* R */
   char encryp[1];  /* R */
   char fbkgc[3];   /* R */
   char oname[24];  /* <R> */
   char ophone[18]; /* <R> */

   char fl[12];     /* R */
   char hl[6];      /* R */

   } nitf_21_main_type;


typedef struct {
   char IREPBANDnn[2]; /* <-C> */
   char ISUBCATnn[6];  /* <-R> */
   char IFCnn[1];      /* R */
   char IMFLTnn[3];    /* <-R> */
   char NLUTSnn[1];    /* <R> */
   char NELUTnn[5];    /* <C> */
   char *LUTDnnm;/* <C> ptr to nnth band data of the mth LUT.  NULL if nlutsnn=0 */
} image_band_data_type;


typedef struct {
   char im[2];      /* R */
   char iid1[10];   /* R */
   char idatim[14]; /* R */
   char tgtid[17];  /* <R> */
   char iid2[80];   /* <R> */
   char isclass[1]; /* R */
   char isclsy[2];  /* <R> */
   char iscode[11]; /* <R> */
   char isctlh[2];  /* <R> */
   char isrel[20];  /* <R> */
   char isdctp[2];  /* <R> */
   char isdcdt[8];  /* <R> */
   char isdcxm[4];  /* <R> */
   char isdg[1];    /* <R> */
   char isdgdt[8];  /* <R> */
   char iscltx[43]; /* <R> */
   char iscatp[1];  /* <R> */
   char iscaut[40]; /* <R> */
   char iscrsn[1];  /* <R> */
   char issrdt[8];  /* <R> */
   char isctln[15]; /* <R> */
   char encryp[1];  /* R */
   char isorce[42]; /* <R> */
   char nrows[8];   /* R */
   char ncols[8];   /* R */
   char pvtype[3];  /* R */
   char irep[8];    /* R */
   char icat[8];    /* R */
   char abpp[2];    /* R */
   char pjust[1];   /* R */
   char icords[1];  /* <R> */
   char igeolo[60]; /* c */
   char nicom[1];   /* R */
   char *pIcomn;     /* C  ptr to image comments of length 80 */
   char ic[2];      /* R */
   char comrat[4];  /* c */
   char nbands[1];  /* R */
   char xbands[5];  /* c */
   image_band_data_type *pBand_data; /* C */
   char isync[1];   /* R */
   char imode[1];   /* R */
   char nbpr[4];    /* R */
   char nbpc[4];    /* R */
   char nppbh[4];   /* R */
   char nppbv[4];   /* R */
   char nbpp[2];    /* R */
   char idlvl[3];   /* R */
   char ialvl[3];   /* R */
   char iloc[10];   /* R */
   char imag[4];    /* R */

   char udidl[5];   /* R */
   char udofl[3];   /* C */
   char *pUdid;     /* C pointer to user defined image data */

   char ixshdl[5];  /* R */
   char ixsofl[3];  /* C */
   char *pIxshd;        /* C pointer to extended subheader data */

/* image data mask table */

   long imdatoff;   /*c */
   unsigned short bmrlnth;   /*c */
   unsigned short tmrlnth;   /*c */
   unsigned short tpxcdlnth; /*c */
   long tpxcd;      /* c pad output pixel code */

   /*char BMRnBNDm */
   } nitf_21_imagesub_type;


typedef struct {
    char sy[2];      /* R */
    char sid[10];    /* R */
    char sname[20];  /* <R> */
    char ssclas[1];  /* R */

    char ssclsy[2];  /* <R> */

    char sscode[11]; /* <R> */
    char ssctlh[2];  /* <R> */
    char ssrel[20];  /* <R> */

    char ssdctp[2];  /* <R> */
    char ssdcdt[8];  /* R */
    char ssdcxm[4];  /* <R> */
    char ssdg[1];    /* <R> */
    char ssdgdt[8];  /* R */
    char sscltx[43]; /* <R> */
    char sscatp[1];  /* <R> */

    char sscaut[40]; /* <R> */

    char sscrsn[1];  /* <R> */
    char sssrdt[8];  /* R */
    char ssctln[15]; /* <R> */

    char encryp[1];  /* R */
    char stype[1];   /* R */

    char sres1[13];  /* R */

    char sdlvl[3];   /* R */
    char salvl[3];   /* R */
    char sloc[10];   /* R */

    char sbnd1[10];  /* R */
    char scolor[1];  /* R */
    char sbnd2[10];  /* R */
    char sres2[2];   /* R */

    char sxshdl[5];  /* R */

    char sxsofl[3];  /* C */
    char *pSxshd;    /* C */
    } nitf_21_graphicsub_type;


typedef struct {
    char te[2];      /* R */
    char textid[7];  /* R */

    char txtalvl[3]; /* R */

    char txtdt[14];  /* R */
    char txtitl[80]; /* <R> */

    char tsclas[1];  /* R */

    char tsclsy[2];  /* <R> */

    char tscode[11]; /* <R> */
    char tsctlh[2];  /* <R> */
    char tsrel[20];  /* <R> */

    char tsdctp[2];  /* <R> */
    char tsdcdt[8];  /* <R> */
    char tsdcxm[4];  /* <R> */
    char tsdg[1];    /* <R> */
    char tsdgdt[8];  /* R */
    char tscltx[43]; /* <R> */
    char tscatp[1];  /* <R> */
    char tscaut[40]; /* <R> */

    char tscrsn[1];  /* <R> */
    char tssrdt[8];  /* R */

    char tsctln[15]; /* <R> */
    char encryp[1];  /* R */

    char txtfmt[3];  /* R */
    char txshdl[5];  /* R */

    char txsofl[3];  /* C */
    char *pTxshd;    /* C */
    } nitf_21_textsub_type;

typedef struct {
    char de[2];       /* R */
    char desid[25];  /* R */
    char desver[2];   /* R */
    char declas[1];   /* R */
    char desclsy[2];  /* <R> */
    char descode[11]; /* <R> */
    char desctlh[2];  /* <R> */
    char desrel[20];  /* <R> */
    char desdctp[2];  /* <R> */
    char desdcdt[8];  /* <R> */
    char desdcxm[4];  /* <R> */
    char desdg[1]; /* <R> */
    char desdgdt[8];  /* <R> */
    char descltx[43]; /* <R> */
    char descatp[1];  /* <R> */
    char descaut[40]; /* <R> */
    char descrsn[1];  /* <R> */
    char dessrdt[8];  /* <R> */
    char desctln[15]; /* <R> */
    char desoflw[6];  /* C */
    char desitem[3];  /* C */
    char desshl[4];  /* R */
    char *pDesdata;  /* R */
    } nitf_21_dessub_type;
