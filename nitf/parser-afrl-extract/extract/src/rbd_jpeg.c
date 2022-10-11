/* Version  */
/* rbd_jpeg.c - Functions to rebuild a NITF JPEG image segment back */
/*              into a commercial JPEG file using the original info */

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
*       Functions to rebuild a NITF JPEG image segment back into a commercial
* JPEG file using the original info
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/

#include <stdio.h>

#ifndef PC
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

void copy_edit_ffc0(FILE *fp_in, FILE *fp_out);
void copy_edit_ffda(FILE *fp_in, FILE *fp_out);
void copy_edit_ffdb(FILE *fp_in, FILE *fp_out);
void copy_edit_ffe0(FILE *fp_in, FILE *fp_out);

static long jpeg_bytes_read;

/*************************************************************************/
/*                                                                       */
/* rebuild_jpeg                                                          */
/* This function Converts NITF JPEG Files to JFIF commercial JPEG files. */
/* (Open temp jpeg and output jpeg files, copy & edit as we copy)        */
/*                                                                       */
/*************************************************************************/

void rebuild_jpeg(int image_num)
{
    FILE *in_fp;                /* File Ptr to temp. image file */
    FILE *out_fp;               /* File ptr to output JPEG file */
    char in_filename[1024];
    char out_filename[1024];
    unsigned char sBuffer[1024];
    char sBuffer1[2048];
    int last_was_ff;
    int quantization_table;
    long rc;
    char *s;

    memset(sBuffer1, 0, 255);

    in_fp = fopen(sNITFfilename, "rb");
    if (in_fp == NULL) {
        fprintf(stderr, "rebuild_jpeg Error: unable to open NITF file [%s]\n",
                sNITFfilename);
        iQuit(1);
    }

    rc = fseek(in_fp, image_info[image_num].nitf_file_offset, SEEK_SET);
    if (rc != 0) {
        fprintf(stderr, "rebuild_jpeg Error positioning NITF file [%s]\n", sNITFfilename);
        iQuit(1);
    }

    /* Create output JPEG filename and open */

    if (gOptions.dump_standard_filenames) {
            sprintf(sBuffer1, "image_seg%d.jpg", image_num);
    }
    else
    {
        if (iNITF_version == V2_0) {
            strncpy(sBuffer1, i20hdr[image_num].ititle, 80);
        }
        if (iNITF_version == V2_1) {
            strncpy(sBuffer1, i21hdr[image_num].iid2, 80);
        }
    }
    
    strcpy(out_filename, sOutputPath);
    remove_trailing_spaces(sBuffer1);
    strcat(out_filename, sBuffer1);

    out_fp = fopen(out_filename, "wb");
    if (out_fp == NULL) {

        sprintf(sBuffer1, "Error: unable to open output JPEG file '%s', trying ",
                out_filename);

        sprintf(out_filename, "%simage_seg%d.jpg", sOutputPath, image_num);

        s = strchr(sBuffer1, 0);
        sprintf(s, "%s\n", out_filename); /* Concatenate rest of error message */
        errmessage(sBuffer1);

        out_fp = fopen(out_filename, "wb");
        if (out_fp == NULL) {
            fprintf(stderr, "Error: unable to open output JPEG file [%s]\n",
                    out_filename);
            iQuit(1);
        }
    }

    /* Copy & edit as we copy */

    last_was_ff = FALSE;
    quantization_table = 0;

    if (jpeg_info.ffdb_remove < 1) quantization_table = -100; /* don't alter */

    jpeg_bytes_read = 0;

    while (jpeg_bytes_read < image_info[image_num].length_of_data) {
        rc = fread(sBuffer, 1, 1, in_fp);
        if (rc < 1) {
            if (rc == 0) break;

            fprintf(stderr, "Error: unable to read input JPEG file [%s]\n",
                                                    in_filename);
            iQuit(1);
        }
        jpeg_bytes_read++;

        if (last_was_ff) {
            switch(sBuffer[0]) {
                case 0xe6:   /* FFE6 */
                    copy_edit_ffe0(in_fp, out_fp);
                    break;

                case 0xc0:   /* FFC0 */
                    copy_edit_ffc0(in_fp, out_fp);
                    break;


                case 0xda:   /* FFDA */
                    copy_edit_ffda(in_fp, out_fp);
                    break;

                case 0xdb:   /* FFDB */
                    quantization_table++;
                    if (quantization_table == 2) {
                        copy_edit_ffdb(in_fp, out_fp);
                    }
                    else
                    {
                        rc = fwrite(sBuffer, 1, 1, out_fp);
                        if (rc < 1) {
                            fprintf(stderr, "Error: unable to write to output JPEG file [%s]\n",
                                                            out_filename);
                            iQuit(1);
                        }
                    }
                    break;

                default:
                    rc = fwrite(sBuffer, 1, 1, out_fp);
                    if (rc < 1) {
                        fprintf(stderr, "Error: unable to write to output JPEG file [%s]\n",
                                                            out_filename);
                        iQuit(1);
                    }
                    break;
            }
            last_was_ff = FALSE;
        }
        else
        {
            if (sBuffer[0] == (unsigned) 0xff) {
                /* Make sure we have information to rebuild JPEG */
                if (jpeg_info.ffc0_remove > 0) last_was_ff = TRUE;
            }
            else
            {
                last_was_ff = FALSE;
            }

            rc = fwrite(sBuffer, 1, 1, out_fp);
            if (rc < 1) {
                fprintf(stderr, "Error: unable to write to output JPEG file [%s]\n",
                                                                out_filename);
                iQuit(1);
            }
        }
    }
    fclose(out_fp);
    fclose(in_fp);
}




/*******************************************************************************
 * Name:    copy_edit_ffc0
 *
 * Description: FFC0 - SOFv0 Frame Header - Baseline DCT
 *              This function sucks in the NITF ffc0 JPEG section, and writes
 *              out the original JPEG section.
 *
 * Parameters:
 *      fp_in       File pointer to FIF JPEG input file
 *      fp_out      File pointer to NITF JPEG output file
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void copy_edit_ffc0(FILE *fp_in, FILE *fp_out)
{
    //long rc;
    char *s;
    int x;
    unsigned long c;

    /* Strip out the rest of the NITF JPEG section */
    for (x = 0; x < jpeg_info.ffc0_remove - 2 ; x++) {
        read_byte(fp_in);
        jpeg_bytes_read++;
    }

    /* Write out the rest of the original commercial JPEG section */
    s = jpeg_info.ffc0_data;

    /* Skip over 1st item, which has already been written */
    while (s[0] == ' ' && s[0] != '\0') s++;
    s = strchr(s, ' ');

    while (s) {
        while (s[0] == ' ' && s[0] != '\0') s++;
        sscanf(s, "%02x", (unsigned int*) &c);
        write_byte(fp_out, (unsigned char) c);
        s = strchr(s, ' ');
    }
}



/*******************************************************************************
 * Name:    copy_edit_ffda
 *
 * Description: SOS - Start of Scan header - FF DA
 *
 *              This function sucks in the NITF ffda JPEG
 *              section, and writesout the original JPEG section.
 *
 * Parameters:
 *      fp_in       File pointer to FIF JPEG input file
 *      fp_out      File pointer to NITF JPEG output file
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void copy_edit_ffda(FILE *fp_in, FILE *fp_out)
{
    //long rc;
    char *s;
    int x;
    unsigned long c;

    /* Strip out the rest of the NITF JPEG section */
    for (x = 0; x < jpeg_info.ffda_remove - 2 ; x++) {
        read_byte(fp_in);
        jpeg_bytes_read++;
    }

    /* Write out the rest of the original commercial JPEG section */
    s = jpeg_info.ffda_data;

    /* Skip over 1st item, which has already been written */
    while (s[0] == ' ' && s[0] != '\0') s++;
    s = strchr(s, ' ');

    while (s) {
        while (s[0] == ' ' && s[0] != '\0') s++;
        sscanf(s, "%02x", (unsigned int*) &c);
        write_byte(fp_out, (unsigned char) c);
        s = strchr(s, ' ');
    }
}


/*******************************************************************************
 * Name:    copy_edit_ffdb
 *
 * Description: Quantization Table - FF DB
 *
 *              This function sucks in the NITF ffdb JPEG
 *              section, and writesout the original JPEG section.
 *
 * Parameters:
 *      fp_in       File pointer to FIF JPEG input file
 *      fp_out      File pointer to NITF JPEG output file
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void copy_edit_ffdb(FILE *fp_in, FILE *fp_out)
{
    //long rc;
    char *s;
    int x;
    unsigned long c;

    /* Strip out the rest of the NITF JPEG section */
    for (x = 0; x < jpeg_info.ffdb_remove - 2 ; x++) {
        read_byte(fp_in);
        jpeg_bytes_read++;
    }

    /* Write out the rest of the original commercial JPEG section */
    s = jpeg_info.ffdb_data;

    /* Skip over 1st item, which has already been written */
    while (s[0] == ' ' && s[0] != '\0') s++;
    s = strchr(s, ' ');

    while (s) {
        while (s[0] == ' ' && s[0] != '\0') s++;
        sscanf(s, "%02x", (unsigned int*) &c);
        write_byte(fp_out, (unsigned char) c);
        s = strchr(s, ' ');
    }
}


/*******************************************************************************
 * Name:    copy_edit_ffe0
 *
 * Description: This function sucks in the NITF ffe0 JPEG section, and writes
 *              out the original JPEG section.
 *
 * Parameters:
 *      fp_in       File pointer to FIF JPEG input file
 *      fp_out      File pointer to NITF JPEG output file
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void copy_edit_ffe0(FILE *fp_in, FILE *fp_out)
{
    //long rc;
    char *s;
    int x;
    unsigned long c;

    /* Strip out the rest of the NITF JPEG section */
    for (x = 0; x < jpeg_info.ffe6_remove - 2 ; x++) {
        read_byte(fp_in);
        jpeg_bytes_read++;
    }

    /* Write out the rest of the original commercial JPEG section */
    s = jpeg_info.ffe0_data;

    /* Skip over 1st item, which has already been written */
    while (s[0] == ' ' && s[0] != '\0') s++;
    s = strchr(s, ' ');

    while (s) {
        while (s[0] == ' ' && s[0] != '\0') s++;
        sscanf(s, "%02x", (unsigned int*) &c);
        write_byte(fp_out, (unsigned char) c);
        s = strchr(s, ' ');
    }
}

