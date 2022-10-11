/* Version  */
/* tre.c - Hold functions that deal with Tagged Record Extensions (TREs) ******/

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
*       Functions that deal with NITF TREs (Tagged record Extensions)
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

#else /* PC */
#include <string.h>
#include <io.h>

#endif

#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"

int discoverTreVersion(char *sTreData);

/*******************************************************************************
 * Name:        tre_verify_extract_version
 *
 * Description: This function verifies that this extract version is new
 *              enough to extract this data.
 *
 * Parameters:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * JLS                Created
 *
 ******************************************************************************/

void tre_verify_extract_version(void)
{
    //char *s;
    //char *n;
    //char name[81];
    int image_num;
    //char sBuffer[100];

    /*printf("dump_tres....dumping the TREs...\n"); */

    loop_through_TREs(NULL, -1, CHECK_MINIMUM_EXTRACT_VERSION | 
                                         DO_ALL, "");  /* NITF Header */

    for (image_num = 0; image_num < number_of_images; image_num++) {
        loop_through_TREs(NULL, image_num, CHECK_MINIMUM_EXTRACT_VERSION | 
                                         DO_ALL, "");
    }
}



/*******************************************************************************
 * Name:        dump_tres
 *
 * Description: This function dumps the TREs to files.  The TREs are parsed,
 *              using the .dff or .pff files.
 *
 * Parameters:
 *     dest_fp      File to dump output to.
 *                  If NULL: dumps to screen or individual files
 *                           Uses .dff files
 *                  Otherwise: dumps to open file
 *                             Uses .pff files (for Phoenix file format)
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * JLS                Created
 *
 ******************************************************************************/

void dump_tres(FILE *dest_fp)
{
    //char *s;
    //char *n;
    //char name[81];
    int image_num;
    //char sBuffer[100];

    /*printf("dump_tres....dumping the TREs...\n"); */

    loop_through_TREs(dest_fp, -1, WRITE_FILES | DO_ALL, "");  /* NITF Header */

    for (image_num = 0; image_num < number_of_images; image_num++) {
        loop_through_TREs(dest_fp, image_num, WRITE_FILES | DO_ALL, "");
    }
}



/*******************************************************************************
 * Name:        loop_through_TREs
 *
 * Description: This function performs operations on the TREs
 *              attached to the NITF File header or image passed.
 *
 * Parameters:
 *   dest_fp      File to dump output to.  If NULL, dumps to screen or
 *                individual files
 *
 *   image_num    (-1 = NITF File header -or- Image Num to scan for the TREs of)
 *
 *   options      Bit-or (|) these options together when calling with
 *                multiple options:
 *
 *                TRE_NAMES             Print TRE Names (comma separated)
 *                WRITE_FILES
 *                DO_1_TRE              Use with WRITE_FILES
 *                DO_ALL
 *                DO_ALL_PROMPT         Prompt for which TREs to write.  Use
 *                                      with WRITE_FILES
 *                PROMPT_FILENAMES      Prompt for filenames to write TREs Use
 *                                      with WRITE_FILES
 *
 *   tre_to_dump  Name of TRE to dump.  Use if dump_options contain
 *                DUMP_1_TRE.
 *
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void loop_through_TREs(FILE *dest_fp, int image_num, int options, const char *tre_to_dump)
{
    char *s;
    //char *s_retag;
    //char *s_rel;
    char s1[10];
    //long l;
    long total_length;
    //int  bDumpThisTRE = FALSE;
    //char sBuffer[100];
    //char InFilename[256];
    //char OutFilename[256];
    //char sTemp[20];
    int  iTRE_found = FALSE;    /* Used to decide how to terminate the line */

    /*****************************************/
    /***** TREs attached to NITF File Header */
    /*****************************************/
    if (image_num < 0) {
        /***** NITF File header TREs */
        total_length = main_user_def_header_data_len;

        if (total_length <= 0) {
        }
        else
        {
            s = main_user_def_data;
            if (s == NULL) {
                printf("Error - internal error - main_user_def_header_data_len > 0, main_user_def_data == NULL\n");
                iQuit(1);
            }

            iTRE_found = TRUE;
            do_loop_through_TREs(dest_fp, image_num, s, total_length, options, tre_to_dump);
        }

        /***** EXTENDED DATA (TREs) */

        total_length = main_extended_header_data_len;

        if (total_length <= 0) {
        }
        else
        {
            s = main_extended_hdr_data;
            if (s == NULL) {
                printf("Internal error - main_extended_header_data_len > 0 but main_extended_hdr_data == NULL\n");
                iQuit(1);
            }

            iTRE_found = TRUE;
            do_loop_through_TREs(dest_fp, image_num, s, total_length, options, tre_to_dump);
        }
    }
    else    /*******************************/
            /* TREs attached to image data */
            /*******************************/
    {
        /***** USER-DEFINED DATA (TREs) */
        if (iNITF_version == V2_0) {
            s = i20hdr[image_num].pUdid;
            strncpy(s1, i20hdr[image_num].udidl, 5);
            s1[5] = '\0';
            total_length = atol(s1);
        }
        else if (iNITF_version == V2_1) {
            s = i21hdr[image_num].pUdid;
            strncpy(s1, i21hdr[image_num].udidl, 5);
            s1[5] = '\0';
            total_length = atol(s1);
        }
        else {
            errmessage("Error: unsupported NITF version.");
            iQuit(2);
        }

        if (total_length <= 0) {
        }
        else
        {
            if (s == NULL) {
                printf("internal error - thought I found a User-defined TRE in image %d, but pointer was NULL.\n", image_num);
                iQuit(1);
            }

            total_length -= 3;  /* Compensate for size of UDOFL field */
            iTRE_found = TRUE;
            do_loop_through_TREs(dest_fp, image_num, s, total_length, options, tre_to_dump);
        }

        /***** EXTENDED DATA (TREs) */
        if (iNITF_version == V2_0) {
            s = i20hdr[image_num].pIxshd;
            strncpy(s1, i20hdr[image_num].ixshdl, 5);
            s1[5] = '\0';
            total_length = atol(s1);
        }
        else if (iNITF_version == V2_1) {
            s = i21hdr[image_num].pIxshd;
            strncpy(s1, i21hdr[image_num].ixshdl, 5);
            s1[5] = '\0';
            total_length = atol(s1);
        }
        else {
            errmessage("Error: unsupported NITF version.");
            iQuit(2);
        }

        if (total_length <= 0) {
        }
        else
        {
            if (s == NULL) {
                printf("internal error - thought I found an extended TRE in image %d\n", image_num);
                iQuit(1);
            }

            total_length -= 3;  /* Compensate for size of IXSOFL field */
            iTRE_found = TRUE;
            do_loop_through_TREs(dest_fp, image_num, s, total_length, options, tre_to_dump);
        }
    }

    /*******************************/
    /* Terminate list of TREs      */
    /* if we're to print TRE names */
    /*******************************/

    if (options & TRE_NAMES) {
        if (iTRE_found == FALSE) {
            if (dest_fp) {
                fprintf(dest_fp, "No TREs");
            }
            else
            {
                write_screen_lines(FALSE, 24, "No TREs\n");
            }
        }
        else
        {
            if (dest_fp == NULL) {
                write_screen_lines(FALSE, 24, "\n");
            }
        }
    }
}



/*******************************************************************************
 * Name:        do_loop_through_TREs
 *
 * Description: This function loops through the user data passed,
 *              performing operations on the user data passed (see
 *              options parameter)
 *
 * Parameters:
 *   image_num
 *   sData        Pointer to the user data
 *
 *   total_length Length of the user data
 *
 *   options      Bit-or (|) these options together when calling with
 *                multiple options:
 *
 *                TRE_NAMES             Print TRE Names (comma separated)
 *                WRITE_FILES
 *                DO_1_TRE              Use with WRITE_FILES
 *                DO_ALL
 *                DO_ALL_PROMPT         Prompt for which TREs to write.  Use
 *                                      with WRITE_FILES
 *                PROMPT_FILENAMES      Prompt for filenames to write TREs Use
 *                                      with WRITE_FILES
 *                CHECK_MINIMUM_EXTRACT_VERSION
 *                                      Whine if this extractor doesn't meet
 *                                      the minimum version #
 *
 *   tre_to_dump  Name of TRE to dump.  Use if dump_options contain
 *                DUMP_1_TRE.
 *
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void do_loop_through_TREs(FILE *dest_fp, int image_num, char *sData, long total_length,
                            int options, const char *tre_to_dump)
{
	char *s;// , *s2;
    char *s_retag;
	char *s_rel;
    char s1[10];
    long l;
    int  bDumpThisTRE = FALSE;
    char sBuffer[1024];
    char InFilename[1024];
    char OutFilename[1024];
    char sTemp[20];
	int  iTRE_found = FALSE;
    //int  num_versions;
    //static int imgdta_read = FALSE;
    char c;
    char sExtra[100];


    s = sData;

    if (s == NULL) {
        printf("do_loop_through_tres: NULL sData pointer\n");
        iQuit(1);
    }

    iTRE_found = TRUE;

    while (total_length > 0) {
        s_retag = s;
        sExtra[0] = '\0';

        gTreVersion = discoverTreVersion(s);
        if (gTreVersion == TRE_AIRBORNE_0_9A) strcpy(sExtra, " (v0.9)");

        s += 6; /* advance to REL or CEL */
        s_rel = s;

        strncpy(s1, s, 5);
        s1[5] = '\0';
        l = atol(s1);

        bDumpThisTRE = options & (DO_ALL | DO_ALL_PROMPT);

        if (options & DO_1_TRE) {
            if (strncmp_caseless(s_retag, tre_to_dump, 6) == 0) {
                bDumpThisTRE = TRUE;
            }
            else {
                bDumpThisTRE = FALSE;
            }
        }

        sprintf(sTemp, "%6.6s", s_retag);
        trim(sTemp);

        if ((options & TRE_NAMES) && bDumpThisTRE ) {
            if (dest_fp) {
                fprintf(dest_fp, "'%s'%s", sTemp, sExtra);
            }
            else
            {
                sprintf(sBuffer, "'%s'%s", sTemp, sExtra);
                write_screen_lines(FALSE, 24, sBuffer);
            }
        }

        if ((options & WRITE_FILES) && bDumpThisTRE ) {
            strsetlower(sTemp);
            if (strcmp(sTemp, "bandsa") == 0) {
                c = s_retag[6+5+7];
                if (c == 'f' || c == 'm' || c == 'r') {
                    strcpy(sTemp, "bandsa1");
                }
                else
                {
                    strcpy(sTemp, "bandsa0");
                }
            }
            if (gTreVersion == TRE_AIRBORNE_0_9A) {
                strcat(sTemp, ".0.9a");
            }

            if (dest_fp) {
                sprintf(InFilename, "%s%s.pff", sDFFpath, sTemp);
                if (gOptions.debug) printf("using pff file: '%s'\n", InFilename);
                DFPExportDataFp(dest_fp, InFilename, s_retag+11, TRUE, l);
            }
            else
            {
                sprintf(InFilename, "%s%s.dff", sDFFpath, sTemp);
                if (gOptions.debug) printf("using dff file: '%s'\n", InFilename);
                if (image_num >= 0) {
                    sprintf(OutFilename, "%s%s_%d_0.txt", sOutputPath, sTemp, image_num);  /* path\TRENAME_IMAGE#_COUNT#.txt */
                }
                else    /* Attached to NITF File header */
                {
                    sprintf(OutFilename, "%s%s_0.txt", sOutputPath, sTemp);       /* path\TRENAME_COUNT#.txt */
                }
                if (gTreVersion == TRE_ILLEGAL_NAME) {
                    sprintf(OutFilename, "%sillegalname%d.txt", sOutputPath,
                            ++gTreIllegalNameCount);
                    printf("illegal filename.  Writing to file '%s'\n", OutFilename);
                }

                DFPExportData(OutFilename, InFilename, s_retag+11, TRUE, l);
            }
        }

        s += l + 5;
        total_length -= (l + 11);
        if (options & TRE_NAMES) {
            if (dest_fp) {
                if (total_length > 0) fprintf(dest_fp, ", ");
            }
            else
            {
                if (total_length > 0) write_screen_lines(FALSE, 24, ", ");
            }
        }
    }
    if (total_length < 0) {
        printf("do_loop_through_TREs: Error - TRE length processing overrun\n");
    }
	s_rel = s_rel;
	iTRE_found = iTRE_found;
}



/*******************************************************************************
 * Name:        discoverTreVersion
 *
 * Description: This function discovers the version of the TRE data passed
 *              to it, using length or other means to differentiate between
 *              possible different versions.
 *
 * Parameters:
 *    sTreData    Text of the TRE data, starting from the TRE name field
 *                (CETAG or RETAG)
 *
 * Returns:
 *    an enumerated value.  See defines.h  Normally, TRE_STANDARD is returned
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int discoverTreVersion(char *sTreData)
{
    char *s;
    char s1[10];
    char *s_retag;
    char *s_rel;
    char sTemp[20];
    long l;
    int  bTreVersion;

    bTreVersion = TRE_STANDARD;

    s = sTreData;
    s_retag = s;

    s += 6; /* advance to REL or CEL */
    s_rel = s;

    strncpy(s1, s, 5);
    s1[5] = '\0';
    l = atol(s1);

    sprintf(sTemp, "%6.6s", s_retag);
    trim(sTemp);

    if (strcmp_caseless(sTemp, "ACFTA") == 0) {
        if (l == 154) bTreVersion = TRE_AIRBORNE_0_9A;
        else if (l == 132) bTreVersion = TRE_STANDARD;
        else {
            printf("discoverTreVersion: Unknown ACFTA version (length = %ld)\n", l);
            bTreVersion = TRE_STANDARD;
        }
    }
    else if (strcmp_caseless(sTemp, "AIMIDA") == 0) {
        if (l == 73) bTreVersion = TRE_AIRBORNE_0_9A;
        else if (l == 69) bTreVersion = TRE_STANDARD;
        else {
            printf("discoverTreVersion: Unknown AIMIDA version (length = %ld)\n", l);
            bTreVersion = TRE_STANDARD;
        }
    }

    else if (strcmp_caseless(sTemp, "MENSRA") == 0) {
        if (l == 174) bTreVersion = TRE_AIRBORNE_0_9A;
        else if (l == 155) bTreVersion = TRE_STANDARD;
        else {
            printf("discoverTreVersion: Unknown MENSRA version (length = %ld)\n", l);
            bTreVersion = TRE_STANDARD;
        }
    }
    else if (strcmp_caseless(sTemp, "PATCHA") == 0) {
        if (l == 74) bTreVersion = TRE_AIRBORNE_0_9A;
        else if (l == 115) bTreVersion = TRE_STANDARD;
        else {
            printf("discoverTreVersion: Unknown PATCHA version (length = %ld)\n", l);
            bTreVersion = TRE_STANDARD;
        }
        /* printf("Found patcha with length %d\n", l); */
    }
    else if (strchr(sTemp, '?') != 0) {
        bTreVersion = TRE_ILLEGAL_NAME;
    }
    else if (strchr(sTemp, '*') != 0) {
        bTreVersion = TRE_ILLEGAL_NAME;
    }
/*
    else if (strcmp_caseless(sTemp, "CLCTNA") == 0) {
        bTreVersion = TRE_ILLEGAL_NAME;
    }
*/
	s_rel = s_rel;
    return bTreVersion;
}
