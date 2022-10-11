/* V */
/* menu.c - GUI, TUI (Text User Interface) for command line haters */

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
*       Routines to implement TUI (Text User Interface) for command line haters
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

#endif

#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"


/* defines */

/*******************************************************************************
 * Name:  menu2
 *
 * Description: pops up a menu of options for the user if -cat -m given on command line.
 *
 * Parameters:
 *    fname        filename entered on command line (if it exists)
 *    new_fname    Filename as given by 1st) the user, or 2nd) the command line,
 *                 or last) the default filename.
 *
 * Returns:
 *    <fname>   name of the file to write all text files to
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * meb                Created
 *
 ******************************************************************************/

int menu2(char* fname, char *new_fname)
{
   char choice = ' ';
   int repeat = 1;
   char newfile[256];
   char rename[256];
   int retValue = 1;
   char sBuffer[100];

   write_screen_lines(FALSE, 24, "\n");

   sprintf(sBuffer, "******************* NITF EXTRACTOR V%s ********************************\n", EXTRACT_VERSION);
   write_screen_lines(FALSE, 24, sBuffer);

   print_inventory();
   write_screen_lines(FALSE, 24, "************************************************************************\n");
   write_screen_lines(FALSE, 24, "\nOptions:\n\n");

   write_screen_lines(FALSE, 24, "1. Write separate metadata files - metadata files will have original filenames.\n");

   write_screen_lines(FALSE, 24, "2. Write one metadata file - prompts for filename\n");

   write_screen_lines(FALSE, 24, "3. Write Phoenix-format file - write one file containing all metadata files \n");
   write_screen_lines(FALSE, 24, "   and the image data (not the JPEG).\n");

   write_screen_lines(FALSE, 24, "4. Write TREs to separate files (TRENAME_IMAGE#_COUNT#.txt).\n");

   write_screen_lines(FALSE, 24, "0. Quit.\n\n");

   write_screen_lines(FALSE, 24, "Options 1, 2, and 4 write the image files separately.\n");
   while (repeat) {
      printf("\nPlease make your choice now: ");
      scanf("%c", &choice);
      if ( (choice >= '0') && (choice <= '6')) {
         repeat = 0;
      }
   }

    strcpy(newfile, fname);
    switch (choice) {
         case '0': iQuit(0);
                 break;
         case '1': printf("Proceeding...\n");
                 retValue = 1;
                 repeat = 0;
                 break;
         case '2': printf("The current filename is: %s\n", newfile);
                 printf("If you wish to change it type in the new filename, ");
                 printf("otherwise enter 'q'.\nFilename:");
                 scanf("%s", rename);
                 if ( strcmp(rename,"q") ) /*bass ackwards 0 = true!!! */
                 {
                    strcpy(newfile, (char *)rename);
                 }
                 printf("Metadata will be written to file '%s'\n", newfile);
                 printf("Proceeding...\n");
                 retValue = 0;
                 repeat = 0;
                 break;
         case '3': printf("The current filename is: %s\n", newfile);
                 printf("If you wish to change it type in the new filename, ");
                 printf("otherwise enter 'q'.\nFilename:");
                 scanf("%s", rename);
                 if ( strcmp(rename,"q") ) /*bass ackwards 0 = true!!! */
                 {
                    strcpy(newfile, (char *)rename);
                 }
                 printf("Writing to Phoenix file: '%s'...\n", newfile);
                 retValue = 2;
                 repeat = 0;
                 break;
         case '4': printf("Proceeding...\n");
                 retValue = NITF_AND_IMAGES;
                 gOptions.bWriteImageInfoFile = TRUE;
                 repeat = 0;
                 break;
         case '5': printf("Proceeding...\n");
                 retValue = DUMP_ALL;
                 repeat = 0;
                 break;
         case '6': printf("Proceeding...\n");
                 retValue = DUMP_TRES;
                 repeat = 0;
                 break;
         default:printf("Invalid choice entered.  Please type a number corresponding ");
                 printf("to a menu choice.\n");
                 break;
    }
    strcpy(new_fname, newfile);
    return retValue;
}


/*******************************************************************************
 * Name:        print_inventory
 *
 * Description: This function prints an inventory of the contents of the current
 *              NITF file.
 *
 * Parameters:
 *
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * JLS                Created
 *
 ******************************************************************************/

void print_inventory(void)
{
    char *s;
    char *n;
    char name[81];
    int image_num;
    char sBuffer[100];

    write_screen_lines(FALSE, 24, "NITF File Header:\n");
    loop_through_TREs(NULL, -1, DO_ALL | TRE_NAMES, "");

    for (image_num = 0; image_num < number_of_images; image_num++) {
        sprintf(sBuffer, "Image %d: ", image_num+1);
        write_screen_lines(FALSE, 24, sBuffer);
        if (iNITF_version == V2_0) {
            s = i20hdr[image_num].ic;
            n = i20hdr[image_num].ititle;
        }
        else if (iNITF_version == V2_1) {
            s = i21hdr[image_num].ic;
            n = i21hdr[image_num].iid2;
        }
        else {
            errmessage("print_inventory: unsupported NITF version\n");
            iQuit(1);
        }
        strncpy(name, n, 80);
        name[80] = '\0';
        trim(name);

        if (s[0] == 'C' && s[1] == '3') { /* JPEG */
            sprintf(sBuffer, "JPEG image '%s'\n", name);
            write_screen_lines(FALSE, 24, sBuffer);
        }
        else {
            sprintf(sBuffer, "Data image '%s'\n", name);
            write_screen_lines(FALSE, 24, sBuffer);
        }
        write_screen_lines(FALSE, 24, "TREs: ");
        loop_through_TREs(NULL, image_num, DO_ALL | TRE_NAMES, "");
        /*loop_through_TREs(image_num, WRITE_FILES | DO_ALL | TRE_NAMES, "");*/
    }
    sprintf(sBuffer, "%d symbols\n", number_of_symbols);
    write_screen_lines(FALSE, 24, sBuffer);

    sprintf(sBuffer, "%d graphics\n", number_of_graphics);
    write_screen_lines(FALSE, 24, sBuffer);

    sprintf(sBuffer, "%d labels\n", number_of_labels);
    write_screen_lines(FALSE, 24, sBuffer);

    sprintf(sBuffer, "%d DESs (Data Extension Segments)\n", number_of_DESs);
    write_screen_lines(FALSE, 24, sBuffer);

    sprintf(sBuffer, "%d RESs (Reserved Extension Segments)\n", number_of_res);
    write_screen_lines(FALSE, 24, sBuffer);

    sprintf(sBuffer, "%d Metadata and/or text segments (some internally used segments aren't written)\n", number_of_text_files);
    write_screen_lines(FALSE, 24, sBuffer);

/*    printf("%d data extension segments\n", numbers[5]); */
/*    printf("%d reserved extension segments\n", numbers[6]); */

}


/*******************************************************************************
 * Name:        write_nitf_inventory_file
 *
 * Description: This function writes an inventory of the contents of the current
 *              NITF file.  This is to support the -gui filename flag
 *
 * Parameters:
 *
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * JLS                Created
 *
 ******************************************************************************/

void write_nitf_inventory_file(void)
{
    char *s;
    char *n;
    char name[81];
    char pvtype[10];
    char imode[20];
    char icat[20];
    char classification[10];
    char datetime[20];
    char igeolo[90];
    int rows;
    int cols;
    int image_num;
    char sDateTime[100];
    char sOutputPath[256];
    char sOstaid[20];
    FILE *fp;
    int  nbpp;
    int  nbpr;
    int  nbpc;
    int  bands;
    int  number_of_igeolos = 0;

    s = getenv("nitf_extract_path");
    if (s) {
        trim(s);
        strcpy(sOutputPath, s);
        s = strchr(sOutputPath, '\0');
        s--;
        if (s[0] == '/' || s[0] == '\\') {
        }
        else
        {
            strcat(sOutputPath, "/");
        }
        local_filename(sOutputPath);  /* fix output pathname slashes */

        strcat(sOutputPath, "nitfinfo.txt");
    }
    else
    {
        printf("write_inventory: Unknown environment variable nitf_extract_path\n");
        iQuit(1);
    }

    fp = open_write_file(sOutputPath);
    if (iNITF_version == V2_0) {
        get_string(classification, nitf_20_main.fsclas, 1);
        get_string(sOstaid, nitf_20_main.ostaid, 10);
    }
    else if (iNITF_version == V2_1) {
        get_string(classification, nitf_21_main.fsclas, 1);
        get_string(sOstaid, nitf_21_main.ostaid, 10);
    }
    fprintf(fp, "File Classification: ");
    switch(classification[0]) {
        case 'U':
            fprintf(fp, "Unclassified\n");
            break;
        case 'R':
            fprintf(fp, "** Restricted **\n");
            break;
        case 'C':
            fprintf(fp, "** Confidential **\n");
            break;
        case 'S':
            fprintf(fp, "**** Secret ****\n");
            break;
        case 'T':
            fprintf(fp, "***** Top Secret *****\n");
            break;
        default:
            break;
            fprintf(fp, "Unknown classification %c\n", classification[0]);
    }
    fprintf(fp, "\nNITF File Header:\n");
    fprintf(fp, "  TREs: ");
    loop_through_TREs(fp, -1, DO_ALL | TRE_NAMES, "");
    fprintf(fp, "\n\n");

    for (image_num = 0; image_num < number_of_images; image_num++) {
        fprintf(fp, "Image %d: ", image_num+1);
        if (iNITF_version == V2_0) {
            s = i20hdr[image_num].ic;
            n = i20hdr[image_num].ititle;
            rows = get_long(i20hdr[image_num].nrows, 8);
            cols = get_long(i20hdr[image_num].ncols, 8);
            nbpr = get_long(i20hdr[image_num].nbpr, 4);
            nbpc = get_long(i20hdr[image_num].nbpc, 4);

            nbpp = get_long(i20hdr[image_num].nbpp, 2);
            bands = get_long(i20hdr[image_num].nbands, 1);

            get_string(pvtype, i20hdr[image_num].pvtype, 3);
            get_string(imode, i20hdr[image_num].imode, 1);
            get_string(icat, i20hdr[image_num].icat, 8);
            get_string(datetime, i20hdr[image_num].idatim, 14);

            get_string(igeolo, i20hdr[image_num].igeolo, 60);
            trim(igeolo);
            if (strlen(igeolo) > 0) number_of_igeolos++;

            /* format date, time string */
            nitf20dt_2_nitf21dt(datetime);
            format_nitf21datetime(datetime, sDateTime);
        }
        else if (iNITF_version == V2_1) {
            s = i21hdr[image_num].ic;
            n = i21hdr[image_num].iid2;
            rows = get_long(i21hdr[image_num].nrows, 8);
            cols = get_long(i21hdr[image_num].ncols, 8);
            nbpr = get_long(i21hdr[image_num].nbpr, 4);
            nbpc = get_long(i21hdr[image_num].nbpc, 4);
            get_string(classification, i21hdr[image_num].isclass, 1);

            nbpp = get_long(i21hdr[image_num].nbpp, 2);
            bands = get_long(i21hdr[image_num].nbands, 1);
            if (bands == 0) {
                bands = get_long(i21hdr[image_num].xbands, 3);
            }

            get_string(pvtype, i21hdr[image_num].pvtype, 3);
            get_string(imode, i21hdr[image_num].imode, 1);
            get_string(icat, i21hdr[image_num].icat, 8);
            get_string(datetime, i21hdr[image_num].idatim, 14);

            get_string(igeolo, i21hdr[image_num].igeolo, 60);
            trim(igeolo);
            if (strlen(igeolo) > 0) number_of_igeolos++;

            /* format date, time string */
            format_nitf21datetime(datetime, sDateTime);
        }
        else {
            errmessage("print_inventory: unsupported NITF version\n");
            iQuit(1);
        }
        strncpy(name, n, 80);
        name[80] = '\0';
        trim(name);

        if (s[0] == 'C' && s[1] == '3') { /* JPEG */
            fprintf(fp, "JPEG image '%s'\n", name);

            if (nbpp != 12) {
                fprintf(fp, "  Columns (width): %d, Rows: %d pixels\n", cols, rows);
            }
        }
        else {
            fprintf(fp, "Raw image '%s'\n", name);

            trim(pvtype);

            if (nbpp != 12) {
                fprintf(fp, "  Columns (width): %d, Rows: %d pixels\n", cols, rows);
            }

            if (strcmp_caseless(pvtype, "int") == 0) {
                fprintf(fp, "  %d bit unsigned integer data\n", nbpp);
            }
            else if (strcmp_caseless(pvtype, "si") == 0) {
                fprintf(fp, "  %d bit signed integer data\n", nbpp);
            }
            else if (strcmp_caseless(pvtype, "r") == 0) {
                fprintf(fp, "  %d bit floating point data\n", nbpp);
            }
            else if (strcmp_caseless(pvtype, "b") == 0) {
                fprintf(fp, "  bi-level (binary) data\n");
            }
            else if (strcmp_caseless(pvtype, "c") == 0) {
                fprintf(fp, "  %d bit complex data\n", nbpp);
            }

            fprintf(fp, "  %d bands\n", bands);
            if (bands > 1) {
                /* print image mode */

                if (nbpr == 1 && nbpc == 1 && imode[0] == 'B') {
                    /* If image is only 1 block, interleaved by block is */
                    /* band sequential (less confusing) */
                    fprintf(fp, "  Image Mode: Band Sequential\n");
                }
                else
                {
                    switch (imode[0]) {
                    case 'B':
                        fprintf(fp, "  Image Mode: Band Interleaved by block\n");
                        break;
                    case 'P':
                        fprintf(fp, "  Image Mode: Band Interleaved by pixel\n");
                        break;
                    case 'R':
                        fprintf(fp, "  Image Mode: Band Interleaved by row\n");
                        break;
                    case 'S':
                        fprintf(fp, "  Image Mode: Band Sequential\n");
                        break;
                    default:
                        fprintf(fp, "  Unknown imode image mode '%c'\n", imode[0]);
                        break;
                    }
                }
            }
            fprintf(fp, "  Image Category: ");
            trim(icat);
            if (strcmp(icat, "MS") == 0) {
                fprintf(fp, "Multi-Spectral");
            }
            else if (strcmp(icat, "HS") == 0) {
                fprintf(fp, "Hyper-Spectral");
            }
            else if (strcmp(icat, "VIS") == 0) {
                fprintf(fp, "Visible");
            }
            else if (strcmp(icat, "SL") == 0) {
                fprintf(fp, "Side Looking Radar");
            }
            else if (strcmp(icat, "TI") == 0) {
                fprintf(fp, "Thermal Infrared");
            }
            else if (strcmp(icat, "FL") == 0) {
                fprintf(fp, "Forward looking Infrared");
            }
            else if (strcmp(icat, "RD") == 0) {
                fprintf(fp, "Radar");
            }
            else if (strcmp(icat, "HR") == 0) {
                fprintf(fp, "High Resolution radar");
            }
            else if (strcmp(icat, "SARIQ") == 0) {
                fprintf(fp, "SAR radio hologram");
            }
            else {
                fprintf(fp, "%s", icat);
            }
            fprintf(fp, "\n  Dated: %s\n", sDateTime);
        }


        fprintf(fp, "  TREs: ");
        loop_through_TREs(fp, image_num, DO_ALL | TRE_NAMES, "");
        /*loop_through_TREs(image_num, WRITE_FILES | DO_ALL | TRE_NAMES, "");*/
        fprintf(fp, "\n\n");
    }
    fprintf(fp, "%d symbols\n", number_of_symbols);

    fprintf(fp, "%d graphics\n", number_of_graphics);

    fprintf(fp, "%d labels\n", number_of_labels);

    fprintf(fp, "%d DESs (Data Extension Segments)\n", number_of_DESs);

    fprintf(fp, "%d RESs (Reserved Extension Segments)\n", number_of_res);

    fprintf(fp, "%d Metadata and/or text segments (some internally used segments aren't written)\n", number_of_text_files);

    fprintf(fp, "<END REPORT>\n");


/***** SOFTWARE READABLE REPORT SECTION *****/

    trim(sOstaid);
    fprintf(fp, "-ostaid %s\n", sOstaid);
    for (image_num = 0; image_num < number_of_images; image_num++) {
        fprintf(fp, "-image %d", image_num+1);
        if (iNITF_version == V2_0) {
            s = i20hdr[image_num].ic;
            n = i20hdr[image_num].ititle;
            rows = get_long(i20hdr[image_num].nrows, 8);
            cols = get_long(i20hdr[image_num].ncols, 8);
            nbpr = get_long(i20hdr[image_num].nbpr, 4);
            nbpc = get_long(i20hdr[image_num].nbpc, 4);

            nbpp = get_long(i20hdr[image_num].nbpp, 2);
            bands = get_long(i20hdr[image_num].nbands, 1);

            get_string(pvtype, i20hdr[image_num].pvtype, 3);
            get_string(imode, i20hdr[image_num].imode, 1);
            get_string(icat, i20hdr[image_num].icat, 8);
            get_string(datetime, i20hdr[image_num].idatim, 14);

            /* format date, time string */
            nitf20dt_2_nitf21dt(datetime);
            format_nitf21datetime(datetime, sDateTime);
        }
        else if (iNITF_version == V2_1) {
            s = i21hdr[image_num].ic;
            n = i21hdr[image_num].iid2;
            rows = get_long(i21hdr[image_num].nrows, 8);
            cols = get_long(i21hdr[image_num].ncols, 8);
            nbpr = get_long(i21hdr[image_num].nbpr, 4);
            nbpc = get_long(i21hdr[image_num].nbpc, 4);
            get_string(classification, i21hdr[image_num].isclass, 1);

            nbpp = get_long(i21hdr[image_num].nbpp, 2);
            bands = get_long(i21hdr[image_num].nbands, 1);
            if (bands == 0) {
                bands = get_long(i21hdr[image_num].xbands, 3);
            }

            get_string(pvtype, i21hdr[image_num].pvtype, 3);
            get_string(imode, i21hdr[image_num].imode, 1);
            get_string(icat, i21hdr[image_num].icat, 8);
            get_string(datetime, i21hdr[image_num].idatim, 14);

            /* format date, time string */
            format_nitf21datetime(datetime, sDateTime);
        }
        else {
            errmessage("print_inventory: unsupported NITF version\n");
            iQuit(1);
        }
        strncpy(name, n, 80);
        name[80] = '\0';
        trim(name);

        if (s[0] == 'C' && s[1] == '3') { /* JPEG */
            fprintf(fp, ",jpeg,%s", name);
        }
        else {
            fprintf(fp, ",raw,%s", name);

            trim(pvtype);

            if (strcmp_caseless(pvtype, "int") == 0) {
                fprintf(fp, ",nbpp %d,type uint", nbpp);
            }
            else if (strcmp_caseless(pvtype, "si") == 0) {
                fprintf(fp, ",nbpp %d,type sint", nbpp);
            }
            else if (strcmp_caseless(pvtype, "r") == 0) {
                fprintf(fp, ",nbpp %d,type float", nbpp);
            }
            else if (strcmp_caseless(pvtype, "b") == 0) {
                fprintf(fp, ",nbpp %d,type binary", nbpp);
            }
            else if (strcmp_caseless(pvtype, "c") == 0) {
                fprintf(fp, ",nbpp %d,type complex", nbpp);
            }

            fprintf(fp, ",bands %d", bands);
        }

        fprintf(fp, ",TREs ");
        loop_through_TREs(fp, image_num, DO_ALL | TRE_NAMES, "");
        /*loop_through_TREs(image_num, WRITE_FILES | DO_ALL | TRE_NAMES, "");*/
        fprintf(fp, "\n");
    }
    fprintf(fp, "-symbols %d\n", number_of_symbols);

    fprintf(fp, "-graphics %d\n", number_of_graphics);

    fprintf(fp, "-labels %d\n", number_of_labels);

    fprintf(fp, "-text %d\n", number_of_text_files);

    fprintf(fp, "-DESs %d\n", number_of_DESs);

    fprintf(fp, "-RESs %d\n", number_of_res);

    fprintf(fp, "-IGEOLOs %d\n", number_of_igeolos);


/*    printf("%d data extension segments\n", numbers[5]); */
/*    printf("%d reserved extension segments\n", numbers[6]); */

    fclose(fp);
}

