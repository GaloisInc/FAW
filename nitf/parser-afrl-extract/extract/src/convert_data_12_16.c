/*-------------------------------------------------------------------------
 *                        Routine: convert_data_12_16.c (Version 1.1)
 *                         Author: Jim Stadler, Veridian Inc./Veda
 *                           Date: 9 September, 1998
 *
 * What's new:
 *
 * v1.1: Fixed conversion problem on Suns (small amount of values weren't
 *       converted correctly)  6/1/2000
 *
 *       Added big-endian or little-endian output capability for the 
 *       12 bit big-endian input format.  Made this possible on big or
 *       little endian CPUs. 6/1/2000
 *
 *-------------------------------------------------------------------------
 *
 * Purpose: This routine performs image data conversion
 *
 *-------------------------------------------------------------------------
 *
 * [Calls]:
 *
 *     char *read_switch()   -- Utility to extract arguments from
 *                              input switches.  This was extracted
 *                              from code developed by Otto Milvang,
 *                              U. of Oslo and is part of the B-LAB
 *                              XITE software.
 *
 *                              Copyright 1990, Blab, UiO
 *                              Image processing lab,
 *                              Department of Informatics
 *                              University of Oslo
 *
 *     float
 *     byteswap_SR_IR()      -- Does big-endian to little-endian float
 *                              byteswap..this is specifically for the
 *                              case of Sun big-endian to PC-Intel
 *                              little-endian data.
 *
 *     unsigned short
 *     byteswap_SUS_IUS()    -- Does big-endian to little-endian swap for
 *                              unsigned short (16-bit) numbers. This is
 *                              specifically for the case of Sun big-
 *                              endian to PC-Intel little-endian data.
 *
 *     int
 *     CheckByteOrder()      -- This checks the byte order for the CPU that
 *                              this routine is compiled run on. If the
 *                              CPU is little-endian, it will return a 0
 *                              value (LSB_FIRST); else, it will return a 1
 *                              (MSB_FIRST).
 *
 *                              Taken from:
 *
 *                                Encyclopedia of Graphic File
 *                                Formats, Murray & Van Ryper,
 *                                O'Reilly & Associates, 1994,
 *                                pp. 114-115.
 *
 *-------------------------------------------------------------------------
 *
 * [Methodology]:
 *
 *     This routine converts ONLY the MAGNITUDE data portion of MSTAR
 *     files (i.e. it ignores the PHASE data).  The output JPEG file
 *     represents the compressed 8-bit value of the original MAGNITUDE
 *     data portion of the MSTAR files:
 *
 *          [int16 IMAGES]:     Unsigned 16-bit data linearly scaled to
 *                              unsigned 8-bit.
 *
 *          [float32 IMAGES]:   32-bit floating point linearly scaled to
 *                              unsigned 8-bit.
 *
 *     Scaling Method:
 *     --------------
 *
 *          Given: OP     = Output pixel
 *                 IP     = Input pixel
 *                 RANGE  = Range of gray values in input image (MAX-MIN)
 *                 MIN    = Minimum pixel value in input image
 *                 T      = Temporary value
 *
 *          Then:  T   =   ((FLOAT) (IP - MIN) / RANGE) * 255.0
 *                 OP  = (unsigned char) (T + 0.5);
 *
 *-------------------------------------------------------------------------
 *
 *-----------------------------------------------------------------------
 *
 * [INFORMATION]:
 *
 *    This routine includes a switch option (-v) that turns on the
 *    verbose mode.  The code normally runs in quiet mode, but will,
 *    if the -v switch is entered, echo what processes are being run.
 *
 *-----------------------------------------------------------------------
 *
 * [Contacts]:
 *
 *   Jim Stadler
 *   Veridian Inc., Veda Operations (Dayton Group)
 *   5200 Springfield Pike, Dayton, OH 45431
 *   email: jstadler@dytn.veridian.com
 *
 *   Veridian/Veda Contractor Area
 *   Area B  Bldg 23  Rm 115?
 *   2010 Fifth Street
 *   Wright Patterson AFB, OH  45433-7001
 *   Work : (937) 255-1116, Ext 2601
 *   email: jstadler@mbvlab.wpafb.af.mil
 *
 *-------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>


#define LSB_FIRST    0             /* Implies little-endian CPU... */
#define MSB_FIRST    1             /* Implies big-endian CPU...    */

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE  1
#endif

#define MAX(x,y)   (((x) > (y)) ? (x) : (y))
#define MIN(x,y)   (((x) < (y)) ? (x) : (y))
#define square(a)  (a * a)

//extern char *read_switch();
char *read_switch(int *argc, char **argv, char *name, int args, char *defreturn);
void iQuit(int x);
static int CheckByteOrder(void);
static unsigned short byteswap_SUS_IUS(unsigned short int *p);
static float byteswap_SR_IR(float *p);
double compress(double in);


unsigned char read_byte(FILE *fp);
void write_byte(FILE *fp, unsigned char c);


static float          byteswap_SR_IR();
static unsigned short byteswap_SUS_IUS();
static int            CheckByteOrder();
void make_data_file(char *filename);

char *dataname=NULL;
char *scaledname=NULL;
FILE *datafp=NULL;
FILE *scaledfp=NULL;

long   w, h;

double old_calibration = 1;
double new_calibration = 1;

int   bINT16=TRUE;
int   bFLOAT32=FALSE;
int   BIG_ENDIAN_DATA_OUT = TRUE;
int   BIG_ENDIAN_CPU = TRUE;
int   endian_convert = FALSE;
int   no_compress = FALSE;


main(int argc, char *argv[])
{
  int   VERBOSEflag=FALSE;
  char  *VERBopt;

  typedef struct {
      char d[3];
  } int12_type;

  float               float32_data[1024];
  int12_type          int12_data[1024];
  unsigned short int  int16_data[1024];
  unsigned short int  uint16_out_data[1024][2];
  long nchunks;
  float          fscale, f1, f2, fmax, fmin, frange;
  float          fmin_sq, fmax_sq;    // min/max of value taken to cubed root
  float          old_midpoint;

  int            filter_spikes = FALSE;

  unsigned char sBuffer[1024];
  char *s;
  int band_num;
  long chunks_to_do;
  long do_chunks;
  long rc;
  long x;
  long tot_count;
  long tot_over_half;  // # values larger than 1/2 way between min & max
  double floatval;   /* LittleEndian float value  */
  int  bytes_per_pixel = 3;
  short int ushort;
  int  byte_swap = FALSE;


/************************ B E G I N  C O D E ****************************/

    if (argc < 2) {
         fprintf(stderr,
                "\n\nUsage: convert_data_12_16  -i <data input file>  -o <scaled output file>");
         fprintf(stderr,"\n      -w <width in pixels>  -h <height in pixels>");
         fprintf(stderr,"\n      [-band <band #>]"); //
         fprintf(stderr,"\n      [-lendian]  [-bendian]  "); //
         fprintf(stderr,"\n Options: [-band #] -- Band to convert (default 1)");
//         fprintf(stderr,"\n          [-lendian] -- little endian format data");
//         fprintf(stderr,"\n          [-bendian] -- big endian format data");
         fprintf(stderr,"\n          [-v] -- verbose mode");
         fprintf(stderr,"\n");
         iQuit(1);
    }

    VERBopt = read_switch(&argc, argv, "-v", 0, NULL);
    if (VERBopt != (char *) NULL) {
        VERBOSEflag = TRUE;
    }

#ifdef REMOVED
    s = read_switch(&argc, argv, "-old_calibration", 1, NULL);
    if (!s) {
        fprintf(stderr,"\n Error - No old calibration value found\n");
        iQuit(5);
    }
    old_calibration = atof(s);

    s = read_switch(&argc, argv, "-new_calibration", 1, NULL);
    if (!s) {
        fprintf(stderr,"\n Error - No new calibration value found\n");
        iQuit(5);
    }
    new_calibration = atof(s);
#endif

    dataname = read_switch(&argc, argv, "-i", 1, NULL);
    if (!dataname) {
        fprintf(stderr,"\n Error - No input data file (-i filename) found\n");
        iQuit(5);
    }

    scaledname  = read_switch(&argc, argv, "-o", 1, NULL);
    if (!scaledname) {
        fprintf(stderr,"\n Error - No scaled output filename (-o filename) found\n");
        iQuit(5);
    }

    s = read_switch(&argc, argv, "-w", 1, NULL);
    if (!s) {
        fprintf(stderr,"\n Error - No image width parameter (-w) found\n");
        iQuit(5);
    }
    w = atol(s);
    if (w > 65535 || w < 5) {
        fprintf(stderr,"\n Error - width must be between 5 and 65535\n");
        iQuit(5);
    }

    s = read_switch(&argc, argv, "-h", 1, NULL);
    if (!s) {
        fprintf(stderr,"\n Error - No image height parameter (-w) found\n");
        iQuit(5);
    }
    h = atol(s);
    if (h > 65535 || h < 5) {
        fprintf(stderr,"\n Error - height must be between 5 and 65535\n");
        iQuit(5);
    }

    /* Extract optional arguments (if any) */

    s = read_switch(&argc, argv, "-no_compress", 0, NULL);
    if (s) {
        no_compress = TRUE;
        if (VERBOSEflag == TRUE) {
            printf("option: -no_compress\n");
        }
    }

    s = read_switch(&argc, argv, "-band", 1, NULL);
    if (s) {
        band_num = atoi(s);
        if (VERBOSEflag == TRUE) {
            printf("option: band %d\n", band_num);
        }
    } else {
        band_num = 1;
    }

    s = read_switch(&argc, argv, "-lendian", 0, NULL);
    if (s) {
        BIG_ENDIAN_DATA_OUT = FALSE;

        if (VERBOSEflag == TRUE) {
            printf("option: Little Endian Data Output\n");
        }
    }

    s = read_switch(&argc, argv, "-bendian", 0, NULL);
    if (s) {
        BIG_ENDIAN_DATA_OUT = TRUE;
        if (VERBOSEflag == TRUE) {
            printf("option: Big Endian Data Out\n");
        }
    }


    BIG_ENDIAN_CPU = CheckByteOrder();

    byte_swap = FALSE;

    if (BIG_ENDIAN_CPU != BIG_ENDIAN_DATA_OUT ) {
        byte_swap = TRUE;
    }

    printf("\nConvert 12 bit big-endian data to 16 bit ");
    if (BIG_ENDIAN_DATA_OUT) {
        printf("big-endian data\n\n");
    }
    else
    {
        printf("little--endian data\n\n");
    }

    printf("BIG_ENDIAN_CPU = %d\n", BIG_ENDIAN_CPU);

    /***************** Open Input & Output Files ********************/

    datafp = fopen(dataname,"rb");
    if (datafp == NULL) {
        fprintf(stderr, "\n\nError: Unable to open [%s] for reading!\n\n", dataname);
        iQuit(1);
    }

    scaledfp = fopen(scaledname,"wb");
    if (scaledfp == NULL) {
        fprintf(stderr, "\n\nError: Unable to open [%s] for writing!\n\n", scaledname);
        iQuit(1);
    }

    /* Skip over bands if necessary */

    rc = fseek(datafp, w * h * bytes_per_pixel * (band_num - 1), SEEK_SET);
    if (rc) {
        fprintf(stderr, "\n\nError repositioning to band %d\n\n", band_num);
        iQuit(1);
    }


    /******************************************************
     * Check input data types, input & CPU byte orders,   *
     * swap bytes if necessary.                           *
     ******************************************************/

    tot_count = 0;

    nchunks = (w * h)/2;
    chunks_to_do = nchunks;


    while (chunks_to_do > 0) {
        do_chunks = chunks_to_do;
        if (do_chunks > 1024) do_chunks = 1024;

        /* read do_chunks */
        rc = fread(int12_data, sizeof(int12_type), do_chunks, datafp);
        if (rc != do_chunks) {
            fprintf(stderr, "Error reading int12 data (read/write phase)\n");
            iQuit(1);
        }

        for (x = 0; x < do_chunks; x++) {
            /* first 12 bits */
            ushort = 0;
            ushort = (unsigned short) ((unsigned char) (int12_data[x].d[0]));
            ushort = ushort << 4;
            uint16_out_data[x][0] = ushort;

            ushort = (unsigned short) ((unsigned char) (int12_data[x].d[1]));
            ushort = ushort & 0x00F0;
            ushort = ushort >> 4;
            uint16_out_data[x][0] += ushort;


            /* second 12 bits */

            ushort = 0;
            ushort = (unsigned short) ((unsigned char) (int12_data[x].d[1])) & 0x000F;
            ushort = ushort << 8;
            ushort += (unsigned short) ((unsigned char) (int12_data[x].d[2]));
            uint16_out_data[x][1] = ushort;

            if (byte_swap) {
                uint16_out_data[x][0] = byteswap_SUS_IUS(&uint16_out_data[x][0]);
                uint16_out_data[x][1] = byteswap_SUS_IUS(&uint16_out_data[x][1]);
            }
        }

        /* write do_chunks */
        rc = fwrite(uint16_out_data, sizeof(short) * 2, do_chunks, scaledfp);
        if (rc != do_chunks) {
            fprintf(stderr, "Error writing float data\n");
            iQuit(1);
        }

        chunks_to_do -= do_chunks;
    }

    fclose(datafp);
    fclose(scaledfp);
    return 0;
}


/************************************************
 * Function:    byteswap_SR_IR                  *
 *   Author:    Dave Hascher (Veridian Inc.)    *
 *     Date:    06/05/97                        *
 *    Email:    dhascher@dytn.veridian.com      *
 ************************************************
 * 'SR' --> Sun 32-bit float value              *
 * 'IR' --> PC-Intel 32-bit float value         *
 ************************************************/

static float byteswap_SR_IR(float *p)
{
  unsigned char *pointer;
  float *temp;
  unsigned char iarray[4], *charptr;
  pointer = (unsigned char *) p;

  iarray[0] = *(pointer + 3);
  iarray[1] = *(pointer + 2);
  iarray[2] = *(pointer + 1);
  iarray[3] = *(pointer );
  charptr = iarray;
  temp    = (float *) charptr;
  return *(temp);
}


/************************************************
 * Function:    byteswap_SUS_IUS                *
 *   Author:    John Querns (Veridian Inc.)     *
 *     Date:    06/05/97                        *
 *    Email:    jquerns@dytn.veridian.com       *
 ************************************************
 * 'SUS' --> Sun 16-bit uns short value         *
 * 'IUS' --> PC-Intel 16-bit uns short value    *
 ************************************************/

static unsigned short byteswap_SUS_IUS(unsigned short int *p)
{
  unsigned char *pointer;
  unsigned short *temp;
  unsigned char iarray[2], *charptr;

  pointer = (unsigned char *) p;

  iarray[0] = *(pointer + 1);
  iarray[1] = *(pointer );
  charptr = iarray;
  temp    = (unsigned short *) charptr;
  return *(temp);
}



/**********************************
 *   checkByteOrder()             *
 **********************************
 * Taken from:                    *
 *                                *
 *   Encyclopedia of Graphic File *
 *   Formats, Murray & Van Ryper, *
 *   O'Reilly & Associates, 1994, *
 *   pp. 114-115.                 *
 *                                *
 * Desc: Checks byte-order of CPU.*
 **********************************/

static int CheckByteOrder(void)

{
  short   w = 1;
  char   *b = (char *) &w;

/*  printf("short 1, [0] = %xh, [1] = %xh\n", (int) b[0], (int) b[1]); */
  return(b[0] ? LSB_FIRST : MSB_FIRST);
}



/*******************************************************************************
 * Name:    iQuit
 *
 * Description: Clean up and exit
 *
 * Parameters:
 *      x       Exit code
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void iQuit(int x)
{
    if (datafp) fclose(datafp);
    if (scaledfp) fclose(scaledfp);
    exit(x);
}


