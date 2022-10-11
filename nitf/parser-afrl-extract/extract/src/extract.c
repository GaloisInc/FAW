/* V */
/* extract.c - extract the contents of an NITF file */

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
* Program Author:       Jim Stadler, Veridian Engineering
* Creation Date:
* Version:
*
* Program Description:
*       This program extracts the image data files, JPEG files, and most text
* segments from the NITF input file.  It can create Phoenix files from NITF
* files created at the Model Based Vision Lab, WPAFB.  It works on NITF 2.0
* and 2.1 files.
*
* Program Usage:
*     Run program to see usage
*
* Program Arguments:
*       Run program to see usage
*
* MAKING THE EXTRACT EXECUTABLE
*
* UNIX
*       Use the included makefile.  You might have to change the CC= line to gcc,
* g++, CC, cc, or whatever compiler you have.  This program should compile
* using a C or C++ compiler.
*
* PC
*       Make sure "PC" is defined.  This is necessary for byte swapping, correct
* include files, etc.
*
*       Set up your development environment to use Win32 and
* console output.  Next, add all the C files, compile, and link.
*
*       If you want to compile from the command line, modify the included
* makefile.
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/


#include <stdio.h>

#ifndef PC
#include <strings.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#else
#include <string.h>
#include <io.h>
#include <dos.h>
#endif

#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"

char Gstr[1024];
char sBuffer[1000];
char sNITFfilename[255];
char sOutputPath[260];
char sDFFpath[260] = "";
char ssummary_filename[256];
int  gTreVersion;

int hNITF;  /* handle to NITF file */

/*nitf_20_main_type_a nitf_20_main_a; */
nitf_20_main_type nitf_20_main;
nitf_21_main_type nitf_21_main;

nitf_20_imagesub_type *i20hdr;
nitf_21_imagesub_type *i21hdr;

nitf_20_symbolsub_type *s20hdr;

nitf_20_labelsub_type *l20hdr;

nitf_21_graphicsub_type *g21hdr;

nitf_20_textsub_type *t20hdr;
nitf_21_textsub_type *t21hdr;

nitf_20_dessub_type *d20hdr;
nitf_21_dessub_type *d21hdr;

/* V2_0, V2_1 */
int number_of_images;
image_info_type *image_info;

/* V2_0 */
int number_of_symbols;
segment_info_type *symbol_info;

/* V2_1 */
int number_of_graphics;
segment_info_type *graphics_info;

/* V2_0 */
int number_of_labels;
segment_info_type *label_info;

/* V2_0, V2_1 */
int number_of_text_files;
segment_info_type *text_info;

/* V2_0, V2_1 */
int number_of_DESs;
segment_info_type *DES_info;

/* V2_0, V2_1 */
int number_of_res;
segment_info_type *res_info;

int iNITF_version = -1;

long main_user_def_header_data_len;
char *main_user_def_data;

long main_extended_header_data_len;
char *main_extended_hdr_data;

/*int bCatalog = FALSE;*/
int bExtractAllTextFiles = FALSE;
jpeg_info_type jpeg_info;
options_type gOptions;
/* float bandsa_version = -1; */
int gTreIllegalNameCount = 0;

/* defines */

long skip_read(int fh, char *destination, long length, const char *sErrorMessage);

#define FIXED_HEADER_LENGTH_2_0A 286 /* field fhdr..fsdwng */
#define FIXED_HEADER_LENGTH_2_0B (sizeof (nitf_20_main_type) - \
        FIXED_HEADER_LENGTH_2_0A)


int main(int argc, char *argv[])
{
    long  rc;
    int   x;
    int   argnum;
    char *temp;
    int   non_dash_arg_num;
    char  str[255];
    char *header_ptr;
    char *s;
    char  fillername[256] = "dataout.phx";
    char  fname[256];
    int   doMenu = 0;
/*    int   number_list[7] = {0,0,0,0,0,0,0}; */
    //DFParseType *pt;
    long  file_position;
    long  len;
    char  dump_filename[1024];
    char  dff_filename[1024];

    sOutputPath[0] = '\0';
    sDFFpath[0] = '\0';
    //sAirborne9aDFFpath[0] = '\0';

    s = getenv("DFFPATH");
    if (s) {
        /*printf("getenv(DFFPATH) returned '%s'\n", s); */
        trim(s);

        strcpy(sDFFpath, s);

        s = strchr(s, 0);
        if (s[-1] != '\\' && s[-1] != '/') {
            strcat(sDFFpath, "/");
        }
    }
    else
    {

        printf("Note: DFFPATH environment variable is not set.  Please set it to the path where the extract xxxxxx.dff files are kept.\n");
#ifdef BORLAND
        printf("Please wait...");
        sleep(10);
        printf("\n");
#endif
        iQuit(1);
    }

#ifdef REMOVED
    s = getenv("AIRBORNE9A_DFFPATH");
    if (s) {
        /*printf("getenv(AIRBORNE9A_DFFPATH) returned '%s'\n", s); */
        trim(s);

        strcpy(sAirborne9aDFFpath, s);

        s = strchr(s, 0);
        if (s[-1] != '\\' && s[-1] != '/') {
            strcat(sAirborne9aDFFpath, "/");
        }
    }
    else
    {

        printf("Note: AIRBORNE9A_DFFPATH environment variable is not set.  Please set it to the path where the extract Airborne 0.9A xxxxxx.dff files are kept.\n");
#ifdef BORLAND
        printf("Please wait...");
        sleep(10);
        printf("\n");
#endif
        iQuit(1);
    }
#endif

    gOptions.bWriteImageInfoFile = FALSE;
    gOptions.bVeridianNitfFile = FALSE;
    gOptions.extract_mode = 1;
    gOptions.dump_tres = FALSE;
    gOptions.mag_only = FALSE;
    gOptions.dump_ssummary = FALSE;
    gOptions.dump_geo = FALSE;
    gOptions.dump_qpm = FALSE;
    gOptions.dump_text_only = FALSE;
    gOptions.dump_jpeg_only = FALSE;
    gOptions.dump_raw_only = FALSE;
    gOptions.dump_tre_only = FALSE;
    gOptions.dump_phoenix_tre = FALSE;
    gOptions.added_pad_pixels = 0;
    gOptions.gui_mode = FALSE;
    gOptions.dump_standard_filenames = 0;
    gOptions.debug = FALSE;

    /* gOptions.gui_extension = 'a'; */


/* Process Input arguments */
    non_dash_arg_num = 1;
    for (argnum = 1; argnum < argc; argnum++) {
        temp = argv[argnum];
        if (temp[0] != '-') {
            switch(non_dash_arg_num) {
            case 1:
                  strcpy(sNITFfilename, temp);
                  break;
            case 2:
                  strcpy(sOutputPath, temp);
                  s = strchr(sOutputPath, '\0');
#ifdef PC
                  if (s[-1] != '\\') {
                      strcpy(s, "\\");
                  }
#else
                  if (s[-1] != '/') {
                      strcpy(s, "/");
                  }
#endif
                  break;
            default:
                  print_usage();
                  iQuit(1);
                  break;
            }
            non_dash_arg_num++;
        }
        else
        {
            /* process command line switches / options */

            temp++; /* Skip over "-" */
            switch (*(temp++)) {


            case 'a': /* Top Level */
                switch (*(temp++)) {
                case 'l':
                    switch (*(temp++)) {
                    case 'l':
                        bExtractAllTextFiles = TRUE;
                        printf("Option: extract all text files\n");
                        break;
                    default:
                        sprintf(str, "Error: Unknown switch %s%20s\n",
                                                              argv[argnum]," ");
                        errmessage(str);
                        print_usage();
                        iQuit(1);
                        break;
                    }
                    break;
                default:
                    sprintf(str, "Error: Unknown switch %s%20s\n",
                                                          argv[argnum]," ");
                    errmessage(str);
                    print_usage();
                    iQuit(1);
                    break;
                }
                break;
                /* End of top-level "a" */

#ifdef REMOVED
            case 'c':  /* Top Level */
                switch (*(temp++)) {
                case 'a':
                      bCatalog = TRUE;
                      printf("Option: catalog\n");
                      break;
                default:
                    sprintf(str, "Error: Unknown switch %s%20s\n",
                                                          argv[argnum]," ");
                    errmessage(str);
                    print_usage();
                    iQuit(1);
                    break;
                }
                break;
                /* End of top-level "c" */
#endif

            case 'd':
                switch (*(temp++)) {
                case 'e':
                    gOptions.debug = TRUE;
                    break;
                case 'i':
                    switch (*(temp++)) {
                    case 'i':
                        gOptions.bWriteImageInfoFile = TRUE;
                        printf("Option: Dump Image Information\n");
                        break;
                    default:
                        sprintf(str, "Error: Unknown switch %s%20s\n",
                                                              argv[argnum]," ");
                        errmessage(str);
                        print_usage();
                        iQuit(1);
                        break;
                    }
                    break;
                default:
                    sprintf(str, "Error: Unknown switch %s%20s\n",
                                                          argv[argnum]," ");
                    errmessage(str);
                    print_usage();
                    iQuit(1);
                    break;
                }
                break;
                /* End of top-level "d" */

            case 'f':
                  if (argc < argnum+2) {
                      errmessage("Error: -f must have an argument\n");
                      iQuit(1);
                  }
                  temp = argv[(argnum+1)];
                  strcpy(fillername, temp);
                  gOptions.extract_mode = 0;         /* write everything to one file */
/*        printf("%s\n", fname); */
/* to advance the counter past the filename to write to */
                  argnum++;

                  printf("Option: One Metafile named '%s'\n", fillername);

                  break;
                  /* END of top-level "f" */

            case 'g' :
                  switch (*(temp++)) {
                  case 'u':
                      gOptions.gui_mode = TRUE;
                      break;
                  default:
                      gOptions.dump_geo = TRUE;
                      printf("Option: Create .geo file(s)\n");
                      break;
                  }
                  break;

            case 'h' :
            case 'H' :
            case '?' :
                  print_usage();
                  iQuit(1);
                  break;

            case 'j' :
                  gOptions.dump_jpeg_only = TRUE;
                  printf("Option: JPEGs Only\n");
                  break;

            case 'm':
                switch (*(temp++)) {
                case 'a':
                  gOptions.mag_only = TRUE;         /* write a phoenix file */
                  printf("Option: Magnitude only\n");
                  break;
                default:
                  doMenu = 1;
                  printf("Option: text menu\n");
                  break;
                }
                break;

            case 'p':
            case 'P' :
                switch (*(temp++)) {
                case 'n':
                  gOptions.extract_mode = 2;         /* write a phoenix file */
                  if (argc < argnum+2) {
                      errmessage("Error: -pn must have an argument\n");
                      iQuit(1);
                  }
                  temp = argv[(argnum+1)];
                  strcpy(fillername, temp);
                  argnum++;

                  printf("Option: Raw, text segments to phoenix file named '%s'\n", fillername);
                  break;

                case '_': /* p_tre */
                  gOptions.dump_phoenix_tre = TRUE;
                  gOptions.extract_mode = 2;         /* write a phoenix file */

                  if (argc < argnum+2) {
                      errmessage("Error: -p_tre must have an argument\n");
                      iQuit(1);
                  }
                  temp = argv[(argnum+1)];
                  strcpy(fillername, temp);
                  argnum++;

                  printf("Option: Raw, TRE to Phoenix file named '%s'\n", fillername);
                  break;

                case '\0':
                  gOptions.extract_mode = 2;         /* write a phoenix file */
                  printf("Option: Raw, Text segments to Phoenix file named '%s'\n", fillername);
                  break;

                default:
                  sprintf(str, "Error: Unknown switch %s%20s\n",
                                                              argv[argnum]," ");
                  break;
                }
                break;
                /* END of top-level "p","P" */


            case 'q' :
                  gOptions.dump_qpm = TRUE;
                  printf("Option: Quarter Power Magnitude\n");
                  break;


            case 'r' :
                  gOptions.dump_raw_only = TRUE;
                  printf("Option: Dump Raw Only\n");
                  break;


            case 's':
                switch (*(temp++)) {
                case 's':
                    switch (*(temp++)) {
                    case 'u':    /* -ssummary filename */
                        gOptions.dump_ssummary = TRUE;

                        temp = argv[(argnum+1)];
                        strcpy(ssummary_filename, temp);
                        argnum++;

                        printf("extract: dump summary to filename %s\n", ssummary_filename);
                        break;
                    default:
                        sprintf(str, "Error: Unknown switch %s%20s\n",
                                                              argv[argnum]," ");
                        errmessage(str);
                        print_usage();
                        iQuit(1);
                        break;
                    }
                    break;

                case 't':   /* -standard */
                    switch (*(temp++)) {
                    case 'a':
                        gOptions.dump_standard_filenames = TRUE;
                        break;
                    default:
                        sprintf(str, "Error: Unknown switch %s%20s\n",
                                                              argv[argnum]," ");
                        errmessage(str);
                        print_usage();
                        iQuit(1);
                        break;
                    }
                    break;

                default:
                    sprintf(str, "Error: Unknown switch %s%20s\n",
                                                          argv[argnum]," ");
                    errmessage(str);
                    print_usage();
                    iQuit(1);
                    break;
                }
                break;
                /* End of top-level "s" */

            case 't':
                switch (*(temp++)) {
                case 'e':   /* -text_only */
                    gOptions.dump_text_only = TRUE;
                    printf("Option: Dump Text Only\n");
                    break;

                case 'r':   /* -tre */
                    if (strcmp(temp, "e") == 0) {
                        gOptions.dump_tres = TRUE;
                        gOptions.bWriteImageInfoFile = TRUE;
                        printf("Option: Dump TREs\n");
                    }
                    if (strcmp(temp, "es_only") == 0) {
                        gOptions.dump_tre_only = TRUE;
                        printf("Option: Dump TREs only\n");
                    }
                    break;

                default:
                    sprintf(str, "Error: Unknown switch %s%20s\n",
                                                          argv[argnum]," ");
                    errmessage(str);
                    print_usage();
                    iQuit(1);
                    break;
                }
                break;

            default:
                sprintf(str, "Error: Unknown switch %s%20s\n",
                                                      argv[argnum]," ");
                errmessage(str);
                print_usage();
                iQuit(1);
                break;
            }
        }
    } /* end for argnum = 1..#args */

    if (non_dash_arg_num < 2 || non_dash_arg_num > 3) {
        print_usage();
        iQuit(1);
    }

    /* dump_text is true unless text only = FALSE and (JPEG only or RAW only or TRE Only = TRUE) */
    gOptions.dump_text = !(!gOptions.dump_text_only && (gOptions.dump_jpeg_only || gOptions.dump_raw_only || gOptions.dump_tre_only));

    /* dump_raw is only true when raw only is false and (JPEG only or text only or TRE Only = TRUE) */
    gOptions.dump_raw = !(!gOptions.dump_raw_only && (gOptions.dump_jpeg_only || gOptions.dump_text_only || gOptions.dump_tre_only));

    /* dump_jpeg is only true when JPEG only is false and (raw only or text only or TRE Only = TRUE) */
    gOptions.dump_jpeg = !(!gOptions.dump_jpeg_only && (gOptions.dump_raw_only || gOptions.dump_text_only || gOptions.dump_tre_only));

    /* dump_jpeg is only true when JPEG only is false and (raw only or text only or JPEG Only = TRUE) */
    gOptions.dump_tre = !(!gOptions.dump_tre_only && (gOptions.dump_raw_only || gOptions.dump_text_only || gOptions.dump_jpeg_only));

/*
    printf("dump_text = %s, dump_jpeg = %s, dump_raw = %s, dump_tre = %s\n",
            gOptions.dump_text ? "True":"False",
            gOptions.dump_jpeg ? "True":"False",
            gOptions.dump_raw ? "True":"False",
            gOptions.dump_tre ? "True":"False");
*/


#ifdef PC
    rc = open(sNITFfilename, O_RDONLY| O_BINARY);
#else
    rc = open(sNITFfilename, O_RDONLY);
#endif
   if (rc < 0) {
        printf("Error opening input file %s\n", sNITFfilename);
        iQuit(1);
    }

    sprintf(sBuffer, "*** Extract V%s %s ***\n", EXTRACT_VERSION, sNITFfilename);
    errmessage(sBuffer);

    hNITF = rc;


    /*printf("size of nitf_21_main = %d\n", sizeof(nitf_21_main)); */


    /* Read start of header */
    read_verify(hNITF, (char *) sBuffer, 9, "Error reading NITF header");

    /* Check the header type, file version */

    sBuffer[9] = '\0';
    if (strncmp(sBuffer, "NITF0", 5) != 0) {
        sprintf(Gstr, "File %s is not an NITF file\n", sNITFfilename);
        errmessage(Gstr);
        iQuit(1);
    }
    if (strncmp(sBuffer, "NITF02.", 7) != 0) {
        sprintf(Gstr, "File %s is not a version 2.x NITF file\n", sNITFfilename);
        errmessage(Gstr);
        iQuit(1);
    }
    if (strncmp(sBuffer, "NITF02.00", 9) == 0) {
        iNITF_version = V2_0;  /* Version 2.0 */
        printf("Reading NITF 2.0 file.  Please wait...");
        fflush(stdout);
    }
    else if (strncmp(sBuffer, "NITF02.10", 9) == 0) {
        iNITF_version = V2_1;  /* Version 2.1 */
        printf("Reading NITF 2.1 file.  Please wait...");
        fflush(stdout);
    }
    else {
        sprintf(Gstr, "File %s is not a version 2.0 or 2.1 NITF file\n", sNITFfilename);
        errmessage(Gstr);
        iQuit(1);
    }

    switch(iNITF_version) {
    case V2_0:
          strncpy(nitf_20_main.fhdr, sBuffer, 9);
          header_ptr = (char *) &nitf_20_main;

          /* Read portion A of header */
          read_verify(hNITF, (char *) (header_ptr + 9),
                FIXED_HEADER_LENGTH_2_0A - 9, "Error reading header");

          /* If conditional field fsdwng exists, read it, otherwise skip it. */
          if (strncmp(nitf_20_main.fsdwng, "999998", 6) == 0) {

              /* Read fsdevt & rest of header B */
              read_verify(hNITF, nitf_20_main.fsdevt,
                        FIXED_HEADER_LENGTH_2_0B, "Error reading header");
          }
          else
          {
              /* Skip fsdevt & read rest of header B */

              memset(nitf_20_main.fsdevt, ' ', 40);

              read_verify(hNITF, nitf_20_main.fscop,
                   FIXED_HEADER_LENGTH_2_0B - 40, "Error reading NITF header");
          }

          if (strncmp_caseless(nitf_20_main.ostaid, "Veridian", 8) == 0) {
              gOptions.bVeridianNitfFile = TRUE;
          }

          break;

    case V2_1:
          strncpy(nitf_21_main.fhdr, sBuffer, 9);
          header_ptr = (char *) &nitf_21_main;

          /* Read rest of fixed portion of header */
          read_verify(hNITF, (char *) (header_ptr + 9), 2+4+10+14+80+
              /* Security fields */
              1+2+11+2+20+2+8+4+1+8+43+1+40+1+8+15+5+5+1

              /* General info FBKGC..HL*/
              +3+24+18+12+6, "Error reading header");

          if (strncmp_caseless(nitf_21_main.ostaid, "Veridian", 8) == 0) {
              gOptions.bVeridianNitfFile = TRUE;
          }
          break;

    default:
           errmessage("Error - unknown NITF format (shouldn't get here!\n");
           iQuit(1);
           break;

    }

    /*x = lseek(hNITF, 0, SEEK_CUR); */
    /*printf("position in file = %ld\n", x); */

    /*************************/
    /* Read Number of images */
    /*************************/

    read_verify(hNITF, sBuffer, 3, "Error reading header");

    sBuffer[3] = '\0';
    number_of_images = atoi(sBuffer);

    if (number_of_images > 0) {
        /* Allocate Space for image information arrays */
        image_info = (image_info_type *)
                 malloc(sizeof(image_info_type) * number_of_images);
        if (image_info == NULL) {
            errmessage("Error allocating memory for image_info");
            iQuit(1);
        }

        /* Read Image subheader / data lengths */

        read_verify(hNITF, sBuffer, 16 * number_of_images,
                    "Error reading header");

        temp = sBuffer;

        for (x = 0; x < number_of_images; x++) {
            strncpy(Gstr, temp, 6);
            Gstr[6] = '\0';
            image_info[x].length_of_subheader = atol(Gstr);
            temp += 6;

            strncpy(Gstr, temp, 10);
            Gstr[10] = '\0';
            image_info[x].length_of_data = atol(Gstr);
            temp += 10;

            image_info[x].bFile_written = FALSE;
            /*image_info[x].pData = NULL; */
        }
    }
/*    number_list[0] = number_of_images; */


    if (iNITF_version == V2_0) {
        /***************************/
        /* Read Number of symbols */
        /***************************/

        read_verify(hNITF, (char *) sBuffer, 3, "Error reading header (# symbols)");
        sBuffer[3] = '\0';
        number_of_symbols = atoi(sBuffer);

        if (number_of_symbols > 0) {
            /* Allocate Space for symbols information arrays */
            symbol_info = (segment_info_type *)
                     malloc(sizeof(segment_info_type) * number_of_symbols);
            if (symbol_info == NULL) {
                errmessage("Error allocating memory for symbol_info");
                iQuit(1);
            }

            /* Read Image subheader / data lengths */

            read_verify(hNITF, sBuffer, 10 * number_of_symbols,
                "Error reading header (symbols subheader data lengths)");

            temp = sBuffer;

            for (x = 0; x < number_of_symbols; x++) {
                strncpy(Gstr, temp, 4);
                Gstr[4] = '\0';
                symbol_info[x].length_of_subheader = atol(Gstr);
                temp += 4;

                strncpy(Gstr, temp, 6);
                Gstr[6] = '\0';
                symbol_info[x].length_of_data = atol(Gstr);
                temp += 6;
                symbol_info[x].pData = NULL;
                symbol_info[x].bFile_written = FALSE;
            }
        }
/*        number_list[1] = number_of_symbols; */
    }

    if (iNITF_version == V2_1) {
        /***************************/
        /* Read Number of graphics */
        /***************************/

        read_verify(hNITF, (char *) sBuffer, 3, "Error reading header (# graphics)");
        sBuffer[3] = '\0';
        number_of_graphics = atoi(sBuffer);

        if (number_of_graphics > 0) {
            /* Allocate Space for graphics information arrays */
            graphics_info = (segment_info_type *)
                     malloc(sizeof(segment_info_type) * number_of_graphics);
            if (graphics_info == NULL) {
                errmessage("Error allocating memory for graphics_info");
                iQuit(1);
            }

            /* Read graphic subheader / data lengths */

            read_verify(hNITF, sBuffer, 10 * number_of_graphics, "Error reading header (graphics subheader data lengths)");

            temp = sBuffer;

            for (x = 0; x < number_of_graphics; x++) {
                strncpy(Gstr, temp, 4);
                Gstr[4] = '\0';
                graphics_info[x].length_of_subheader = atol(Gstr);
                temp += 4;

                strncpy(Gstr, temp, 6);
                Gstr[6] = '\0';
                graphics_info[x].length_of_data = atol(Gstr);
                temp += 6;

                graphics_info[x].pData = NULL;
                graphics_info[x].bFile_written = FALSE;
            }
        }

/*       number_list[2] = number_of_graphics; */
    }



    if (iNITF_version == V2_0) {
        /* Read numl(2.0) */
        read_verify(hNITF, (char *) sBuffer, 3, "Error reading header # labels");

        /* Decode label information */
        sBuffer[3] = '\0';
        number_of_labels = atoi(sBuffer);

        if (number_of_labels > 0) {
            /* Allocate Space for label information arrays */
            label_info = (segment_info_type *)
                 malloc(sizeof(segment_info_type) * number_of_labels);
            if (label_info == NULL) {
                errmessage("Error allocating memory for label_info");
                iQuit(1);
            }

            /* Read label subheaders / data lengths */

            read_verify(hNITF, sBuffer, 7 * number_of_labels,
                    "Error reading header label subheader");

            temp = sBuffer;

            for (x = 0; x < number_of_labels; x++) {
                strncpy(Gstr, temp, 4);
                Gstr[4] = '\0';
                label_info[x].length_of_subheader = atol(Gstr);
                temp += 4;

                strncpy(Gstr, temp, 3);
                Gstr[3] = '\0';
                label_info[x].length_of_data = atol(Gstr);
                temp += 3;

                label_info[x].pData = NULL;
                label_info[x].bFile_written = FALSE;
            }
        }

/*        number_list[3] = number_of_labels; */
    }

    if (iNITF_version == V2_1) {
        /* Read numx (2.1) */
        read_verify(hNITF, (char *) sBuffer, 3, "Error reading header numx");
    }

    /**************************************/
    /* Read Number of text files/segments */
    /**************************************/

    read_verify(hNITF, (char *) sBuffer, 3, "Error reading header # text files");
    sBuffer[3] = '\0';
    number_of_text_files = atoi(sBuffer);

    if (number_of_text_files > 0) {
        /* Allocate Space for text_files information arrays */
        text_info = (segment_info_type *)
                 malloc(sizeof(segment_info_type) * number_of_text_files);
        if (text_info == NULL) {
            errmessage("Error allocating memory for text_info");
            iQuit(1);
        }

        /* Read Image subheader / data lengths */

        read_verify(hNITF, sBuffer, 9 * number_of_text_files,
                        "Error reading header text files data lengths");

        temp = sBuffer;

        for (x = 0; x < number_of_text_files; x++) {
            strncpy(Gstr, temp, 4);
            Gstr[4] = '\0';
            text_info[x].length_of_subheader = atol(Gstr);
            temp += 4;

            strncpy(Gstr, temp, 5);
            Gstr[5] = '\0';
            text_info[x].length_of_data = atol(Gstr);
            temp += 5;

            text_info[x].pData = NULL;
            text_info[x].bFile_written = FALSE;
        }
    }

    /* number_list[4] = number_of_text_files-2; */


    /***********************/
    /* Read Number of DESs */
    /***********************/

    read_verify(hNITF, (char *) sBuffer, 3,
                "error reading header (# extension segs");
    sBuffer[3] = '\0';
    number_of_DESs = atoi(sBuffer);

    if (number_of_DESs > 0) {
        /* Allocate Space for extension segs information arrays */
        DES_info = (segment_info_type *)
                 malloc(sizeof(segment_info_type) * number_of_DESs);
        if (DES_info == NULL) {
            errmessage("Error allocating memory for DES_info");
            iQuit(1);
        }

        /* Read Image subheader / data lengths */

        read_verify(hNITF, sBuffer, 13 * number_of_DESs,
            "Error reading header / image subheader data lengths");

        temp = sBuffer;

        for (x = 0; x < number_of_DESs; x++) {
            strncpy(Gstr, temp, 4);
            Gstr[4] = '\0';
            DES_info[x].length_of_subheader = atol(Gstr);
            temp += 4;

            strncpy(Gstr, temp, 9);
            Gstr[9] = '\0';
            DES_info[x].length_of_data = atol(Gstr);
            temp += 9;

            DES_info[x].pData = NULL;
            DES_info[x].bFile_written = FALSE;
        }
    }

/*    number_list[5] = number_of_DESs; */


    /**********************************************/
    /* Read Number of Reserved Extension Segments */
    /**********************************************/

    rc = read_verify(hNITF, (char *) sBuffer, 3, "Error reading header # RESs");
    sBuffer[3] = '\0';
    number_of_res = atoi(sBuffer);

    if (number_of_res > 0) {
        /* Allocate Space for RES information arrays */
        res_info = (segment_info_type *)
                 malloc(sizeof(segment_info_type) * number_of_res);
        if (res_info == NULL) {
            errmessage("Error allocating memory for res_info");
            iQuit(1);
        }

        /* Read Image subheader / data lengths */

        rc = read_verify(hNITF, sBuffer, 11 * number_of_res,
            "Error reading header / RES data lengths");

        temp = sBuffer;

        for (x = 0; x < number_of_res; x++) {
            strncpy(Gstr, temp, 4);
            Gstr[4] = '\0';
            res_info[x].length_of_subheader = atol(Gstr);
            temp += 4;

            strncpy(Gstr, temp, 7);
            Gstr[7] = '\0';
            res_info[x].length_of_data = atol(Gstr);
            temp += 7;

            res_info[x].pData = NULL;
            res_info[x].bFile_written = FALSE;
        }
    }

/*    number_list[6] = number_of_res; */

    /**********************************************/
    /* Read User Data, Extended Data Segments     */
    /**********************************************/


    rc = read_verify(hNITF, (char *) sBuffer, 5,
                "Error reading main file header user def. header data length ");
    sBuffer[5] = '\0';
    main_user_def_header_data_len = atoi(sBuffer);
    if (main_user_def_header_data_len > 0) {
        main_user_def_header_data_len -= 3;

        /* read UDOFL */
        rc = read_verify(hNITF, (char *) sBuffer, 3,
                "Error reading main file header user def. header overflow");
        sBuffer[3] = '\0';
        rc = atol(sBuffer);
        if (rc > 0) {
            printf("NOTE: NITF Main header TREs stored in a DES.  Extraction not implemented at this time.\n");
        }
        else
        {
            main_user_def_data = (char *) malloc(main_user_def_header_data_len);
            if (main_user_def_data == NULL) {
                printf("extract: Error allocating memory for main file header user defined data\n");
                iQuit(1);
            }
            rc = read_verify(hNITF, main_user_def_data,
                  main_user_def_header_data_len,
                  "Error reading main file header extended header data length");
        }
    }

    rc = read_verify(hNITF, (char *) sBuffer, 5,
                "Error reading main file header extended header data length");
    sBuffer[5] = '\0';
    main_extended_header_data_len = atoi(sBuffer);
    if (main_extended_header_data_len > 0) {
        main_extended_header_data_len -= 3;

        /* read UDOFL */
        rc = read_verify(hNITF, (char *) sBuffer, 3,
                "Error reading main file header extended subheader overflow");
        sBuffer[3] = '\0';
        rc = atol(sBuffer);
        if (rc > 0) {
            printf("NOTE: NITF Main file header TREs stored in a DES.  Extraction not implemented at this time.\n");
        }
        else
        {
            main_extended_hdr_data = (char *) malloc(main_extended_header_data_len);
            if (main_extended_hdr_data == NULL) {
                printf("extract: Error allocating memory for main file header extended data\n");
                iQuit(1);
            }
        }
    }



/*
rc = lseek(hNITF, 0, SEEK_CUR);
printf("Header length (read) = %ld\n", rc);
*/



/* Dump main header */

    /* Store current file position */
    file_position = lseek(hNITF, 0, SEEK_CUR);

    /* Get NITF file header length */
    if (iNITF_version == V2_0) {
        len = get_long(nitf_20_main.hl, 6);
    }
    else if (iNITF_version == V2_1) {
        len = get_long(nitf_21_main.hl, 6);
    }

    /* Allocate memory for NITF file header */
    s = (char *) malloc(len);
    if (s == NULL) {
        errmessage("dump main header: error allocating memory\n");
        iQuit(1);
    }

    /* Rewind to start of file */
    rc = lseek(hNITF, 0, SEEK_SET);
    if (rc != 0) {
        errmessage("Error positioning NITF file\n");
        iQuit(1);
    }

    /* Read NITF File header */
    read_verify(hNITF, s, len, "Error reading main header");

    /* Dump NITF file header */
    if (iNITF_version == V2_0) {
        //sprintf(dump_filename, "%sNITF_20_main.txt", sOutputPath, write_nitf_headers);
        sprintf(dump_filename, "%sNITF_20_main.txt", sOutputPath);
        sprintf(dff_filename, "%smain20.dff", sDFFpath);
        DFPExportData(dump_filename, dff_filename, s, FALSE, 50);
    }
    else if (iNITF_version == V2_1) {
        //sprintf(dump_filename, "%sNITF_21_main.txt", sOutputPath, write_nitf_headers);
        sprintf(dump_filename, "%sNITF_21_main.txt", sOutputPath);
        sprintf(dff_filename, "%smain21.dff", sDFFpath);
        DFPExportData(dump_filename, dff_filename, s, FALSE, 50);
    }

    /* Reposition file pointer */
    len = lseek(hNITF, file_position, SEEK_SET);
    if (len != file_position) {
        errmessage("dump main header: error repositioning file pointer\n");
        iQuit(1);
    }
    free(s);
    s = NULL;



/* Read remainder of NITF File */

    read_image_data(number_of_images, image_info);

    if (gOptions.dump_geo) {
        dump_geo();
    }

    if (gOptions.dump_ssummary) {
        dump_ssummary(ssummary_filename);
        close(hNITF);
        goto cleanup_and_exit;
    }

    if (iNITF_version == V2_0) {
        read_symbol_data(number_of_symbols, symbol_info);
        read_label_data(number_of_labels, label_info);
    }

    if (iNITF_version == V2_1) {
        read_graphic_data(number_of_graphics, graphics_info);
    }
    read_text_data(number_of_text_files, text_info);

    read_DES_data(number_of_DESs, DES_info);

/*******************************************************/

    if ( doMenu ) {
        gOptions.extract_mode = menu2(fillername, /* number_list,*/ fname);
        strcpy(fillername, fname);
        /*       printf("After menu <%d>\n", gOptions.extract_mode); */

    }
    else {
        /*      printf("No menu <%d>\n", gOptions.extract_mode); */
        printf("\n");
    }

    if (gOptions.gui_mode) {
        write_nitf_inventory_file();
        goto cleanup_and_exit;
    }

    if (gOptions.bWriteImageInfoFile) {
        dump_image_information(number_of_images);
    }

    if (gOptions.dump_tres) {
        dump_tres(NULL);
    }
    write_output_files(fillername, gOptions.extract_mode);

cleanup_and_exit:
    close(hNITF);
    exit( 0);
} /* End of main */



/**************************************************************************
 *
 * iQuit        This function does any required cleanup, and exits.
 *            If a controlling process exists, it is informed of the
 *            error/quit condition.
 *
 * Parameters:
 *      num     # to pass to exit()
 **************************************************************************/

void iQuit(int num)
{
    printf("Thanks for using extract!\n");
    fflush(stdout);
    fflush(stderr);

    close(hNITF);
    exit(num);
}



/**************************************************************************
 *
 * print_usage  This function prints the usage message to stderr.
 *
 **************************************************************************/

void print_usage(void)
{
    fprintf(stderr, "*** Extract V%s ***\n\n", EXTRACT_VERSION);
    errmessage(sBuffer);
    fprintf(stderr, "\
usage: extract## [-m] [-p] [-pn phoenix-filename] [-f metadata-filename]\n\
       [-mag_only] [-qpm] \n\
       [-jpeg_only] [-text_only] \n\
       [-tre] [-p_tre] [-dii] [-ssummary output_name] [-g]\n\
       nitf_file [output-path]\n\n");
    fprintf(stderr, "\
[-m] Present a menu for gOptions\n");
    fprintf(stderr, "\
[-p] Create one Phoenix file (dataout.phx) holding data and meta-data .\n");
    fprintf(stderr, "\
[-p_tre] Create one Phoenix file (dataout.phx) holding data and TRE meta-data.\n");
    fprintf(stderr, "\
[-pn filename] One Phoenix file with the name you give it.\n");
    fprintf(stderr, "\
[-f filename] Create one file holding meta-data.\n");
    fprintf(stderr, "\
[-dii] Dump image subheader information to a file(s).\n");
    fprintf(stderr, "\
[-g] Dump IGEOLO information to a .geo file (DDB format).\n");
    fprintf(stderr, "\
[-mag_only] Magnitude data only (for mag/phase data)\n");
    fprintf(stderr, "\
[-qpm] dump the image data (magnitude data) as quarter power magnitude.\n");
    fprintf(stderr, "\
       (square root of magnitude data).\n");
    fprintf(stderr, "\
[-ssummary fname] Open file, append a line of information about the NITF file\n");
    fprintf(stderr, "\
                  (See output file for column format)\n");
    fprintf(stderr, "\
[-standard] Extract files with standard filenames (text_seg#.txt, image#.raw,\n");
    fprintf(stderr, "\
            image#.jpg\n");
    fprintf(stderr, "\
[-tre] Dump TRE information to individual files.\n");
    fprintf(stderr, "\
[-jpeg_only] Dump JPEG files only.  Can be combined with other -only flags\n");
    fprintf(stderr, "\
[-raw_only] Dump raw data files only.  Can be combined with other -only flags\n");
    fprintf(stderr, "\
[-text_only] Dump text files only.  Can be combined with other -only flags\n");
    fprintf(stderr, "\
[-tre_only] Dump TREs only.  Can be combined with other -only flags\n");

#ifdef BORLAND
   printf("Please wait...");
   sleep(10);
   printf("\n");
#endif
}




