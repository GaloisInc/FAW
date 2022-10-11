/* V */
/* write_fs.c - Write output files to disk */

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
*       Functions to write image and metadata files in different output formats
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
#else
#include <string.h>
#include <io.h>
#include <sys\stat.h>
#endif

#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>
#include <math.h>

#include "defines.h"


/*******************************************************************************
 * Name:    Write output files
 *
 * Description:     This function writes the output files to the output path,
 *                  if present.
 *
 * Parameters:
 *      fname       Name of the single output file (depending on write_flag)
 *
 *      write_flag  0 = write all meta-data to 1 output file
 *                  1 = write separate output files
 *                  2 = make phoenix file (meta-data, image data in 1 file)
 *                  DUMP_ALL = Dump files, NITF headers, and TREs
 *                  NITF = Dump NITF headers, TREs, and images
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
void write_output_files(char* fname, int write_flag)
{
    char sFilename[260];
    //char *s;

    int fh;
    //int graphic_num;
    int text_num;

    /*************************************************************************/
    /* If there are any files that need special handling when written, do it */
    /* and mark in the segment structure the segments that have been written */
    /*************************************************************************/
    write_special_files();

    /*******************************************************************
     *
     * Write remaining output, format depends on write_flag
     *
     *   write_flag  0 = write all meta-data to 1 output file
     *               1 = write separate output files
     *               2 = make phoenix file (meta-data, image data in 1 file)
     *               DUMP_ALL = Dump files, NITF headers, and TREs
     *               NITF = Dump NITF headers, TREs, and images
     *               DUMP_TRE = Dump TREs only
     ************************************************************************/

    /****************************************/
    /* Write all meta-data to 1 output file */
    /****************************************/

    if (write_flag == 0) {

        /* Create filename */
        strcpy(sFilename, sOutputPath);
        strcat(sFilename, fname);
        remove_trailing_spaces(sFilename);
        fh = open_write_clear_ufile(sFilename);

        /* Write text files */
        for (text_num = 0; text_num < number_of_text_files; text_num++) {
            if (text_info[text_num].bFile_written
                            && bExtractAllTextFiles == FALSE) continue;

            write_meta_data(fh, text_num);
        } /* end loop write text files */
        close(fh);

        /* Write image files */
        if (gOptions.dump_jpeg) {
            write_image_files(JPEG_FILES);
        }

        if (gOptions.dump_raw) {
            write_image_files(IMAGE_DATA_FILES);
        }
#ifdef REMOVED
        if (gOptions.dump_tres) {
            dump_tres(NULL);
        }
        gOptions.dump_tres = FALSE;
#endif
    }  /* if write meta-data to one output file */



    /****************************************/
    /* Write separate output files          */
    /****************************************/

    if (write_flag == 1 ) {    /* Write Separate output files */
        if (gOptions.dump_text) {
            write_text_files_separately();
        }

        if (gOptions.dump_jpeg) {
            write_image_files(JPEG_FILES);
        }

        if (gOptions.dump_raw) {
            write_image_files(IMAGE_DATA_FILES);
        }
#ifdef REMOVED
        if (gOptions.dump_tres) {
            dump_tres(NULL);
        }
        gOptions.dump_tres = FALSE;
#endif
    } /* if (write separate output files) */



    /**********************/
    /* Write PHOENIX file */
    /**********************/

    if (write_flag == 2) {
        if (gOptions.dump_jpeg) {
            write_image_files(JPEG_FILES);
        }

#ifdef REMOVED
        if (gOptions.dump_tres) {
            dump_tres(NULL);
        }
        gOptions.dump_tres = FALSE;
#endif

        write_phoenix_files(fname);

#ifdef REMOVED
        if (gOptions.dump_raw) {
            write_image_files(IMAGE_DATA_FILES);
        }
#endif
    }  /* end if PHOENIX file */


    /*******************************************/
    /* Dump All headers, TREs, and image files */
    /*******************************************/

    if (write_flag == NITF_AND_IMAGES ) {    /* Write Separate output files */
        /*write_nitf_headers(); */
#ifdef REMOVED
        if (gOptions.dump_tres) {
            dump_tres(NULL);
        }
        gOptions.dump_tres = FALSE;
#endif

        if (gOptions.dump_jpeg) {
            write_image_files(JPEG_FILES);
        }

        if (gOptions.dump_raw) {
            write_image_files(IMAGE_DATA_FILES);
        }
    } /* if (Dump NITF headers, TREs, images) */


    /********************************************/
    /* Dump All headers, TREs, and output files */
    /********************************************/

    if (write_flag == DUMP_ALL) {    /* Write Separate output files */
        if (gOptions.dump_tres) {
            dump_tres(NULL);
        }
        gOptions.dump_tres = FALSE;

        if (gOptions.dump_text) {
            write_text_files_separately();
        }

        if (gOptions.dump_jpeg) {
            write_image_files(JPEG_FILES);
        }

        if (gOptions.dump_raw) {
            write_image_files(IMAGE_DATA_FILES);
        }
    } /* if (DUMP_ALL) */
}


/*******************************************************************************
 * Name:    Write special files
 *
 * Description:     This function goes through the data in memory using an
 *                  algorithm, and if there are any files that need special
 *                  handling when written, do it and mark the segments as
 *                  were written (in the segment structure arrays)
 *
 * Parameters:
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

void write_special_files(void)
{
    int text_num;
    int rc;
    char sTag[200];
    char *s;
    char *s1;
    float version1;     /* 1st version # */
    //float version2;     /* 2nd version # */
    char sBuffer[1000];

    /***********************************************************/
    /* Search through all text files looking for Veridian tags */
    /***********************************************************/

    for (text_num = 0; text_num < number_of_text_files; text_num++) {
        rc = find_text_tag("Veridian", text_info[text_num].pData, sTag);
        if (rc) {
            /* printf("Tag found in text segment %d: '%s'\n", text_num + 1, sTag); */
            text_info[text_num].bFile_written = TRUE;

            /* parse tag */
            /* <Veridian, JPEG_extra_tags, 2.0, 1.0> */
            s1 = strchr(sTag, ',');
            if (s1 == NULL) {
                sprintf(sBuffer, "Error in Veridian tag: '%s'\n", sTag);
                errmessage(sBuffer);
                continue;
            }
            s = s1 + 1; /* advance past the comma */
            while (s[0] != '\0' && isspace(s[0])) s++;

            if (strncmp(s, "JPEG_extra_tags", 15) == 0) {
                /* Check 1st Version #.  Must be at least 2.0 */
                s = strchr(s, ',');
                if (s == NULL) {
                    sprintf(sBuffer, "Error in Veridian tag: '%s'\n", sTag);
                    errmessage(sBuffer);
                    continue;
                }
                s = s + 1; /* advance past the comma */
                while (s[0] != '\0' && isspace(s[0])) s++;
                version1 = atof(s);

                if (gOptions.dump_jpeg && version1 >= 2.0) {
                    write_JPEG_image(text_num); /* Write JPEG, mark as written */
                }
            }
        }
        else {
            /* Look for files with special strings in them that don't have */
            /* the Veridian tag in the file */

            if (!bExtractAllTextFiles) {
                /* Don't write .Geo file */
                s = strstr(text_info[text_num].pData, "CalcImgCornerLat1");
                /* printf("text_num %d val = %100s\n", text_num, text_info[text_num].pData); */
                if (s) {
                    text_info[text_num].bFile_written = TRUE;
                }
            }
        }
    }
}





/*******************************************************************************
 * Name:    write_graphic_files
 *
 * Description: This function writes any graphic files
 *
 * Parameters:
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

void write_graphic_files(void)
{
    int  graphic_num;
    int  fh;
    
    char *s;
    char sFilename[1024];

    
    if (iNITF_version > V2_0) {
        for (graphic_num = 0; graphic_num < number_of_graphics; graphic_num++) {
            if (graphics_info[graphic_num].bFile_written) continue;

            /* Create filename */
            if (gOptions.bVeridianNitfFile && gOptions.dump_standard_filenames == FALSE) {
                strcpy(sFilename, sOutputPath);
                s = strchr(sFilename, '\0');

                if (iNITF_version == V2_1) {
                    strncpy(s, g21hdr[graphic_num].sid, 10);
                }
                s[10] = '\0';
                remove_trailing_spaces(s);

#ifdef PC
                fh = open(sFilename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, S_IWRITE);
#else
                fh = open(sFilename, O_WRONLY | O_CREAT | O_TRUNC, 0660);
#endif
                if (fh < 0) {
                    printf("Error opening output file '%s'.  ", sFilename);
                    sprintf(sFilename, "%sgraphic_seg%d", sOutputPath, graphic_num);
                    printf("Trying file name: '%s'\n", sFilename);

                    fh = open_write_clear_ufile(sFilename);
                }
            }
            else
            {
                sprintf(sFilename, "%sgraphic_seg%d", sOutputPath, graphic_num);
                fh = open_write_clear_ufile(sFilename);
            }

            write_verify(fh, graphics_info[graphic_num].pData, graphics_info[graphic_num].length_of_data, "write_output_files: Error writing graphic\n");

            close(fh);
        }
    } /* end if version > 2.0 */
}



/*******************************************************************************
 * Name:    write_image_files
 *
 * Description: This function writes image files to separate files of the
 *              types requested.
 *
 *              This function checks the image type, creates and opens the
 *              output file, and calls functions to finish the job.
 *
 * Parameters:
 *      image_types     Types {JPEG_FILES, IMAGE_DATA_FILES}
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

void write_image_files(int image_types)
{
    int fh;
    char *s;
    char sFilename[1024];
    int image_num;
    int jpeg_files;
    int image_data_files;
    int bJpeg;
    char sIC[6];

    jpeg_files = image_types & JPEG_FILES;
    image_data_files = image_types & IMAGE_DATA_FILES;

    for (image_num = 0; image_num < number_of_images; image_num++) {
        if (image_info[image_num].bFile_written) continue;

        bJpeg = FALSE;
        if (iNITF_version == V2_0) {
            get_string(sIC, i20hdr[image_num].ic, 2);
            if (i20hdr[image_num].ic[0] == 'C' &&
                            i20hdr[image_num].ic[1] == '3') bJpeg = TRUE;
        }
        else if (iNITF_version == V2_1) {
            get_string(sIC, i21hdr[image_num].ic, 2);
            if (i21hdr[image_num].ic[0] == 'C' &&
                            i21hdr[image_num].ic[1] == '3') bJpeg = TRUE;
        }

        if (bJpeg && !jpeg_files) continue;
        if (!bJpeg && !image_data_files) continue;

        /* Create filename */
        if (gOptions.bVeridianNitfFile && gOptions.dump_standard_filenames == FALSE) {
            strcpy(sFilename, sOutputPath);
            s = strchr(sFilename, '\0');

            if (iNITF_version == V2_0) {
                   strncpy(s, i20hdr[image_num].ititle, 80);
            }
            if (iNITF_version == V2_1) {
                   strncpy(s, i21hdr[image_num].iid2, 80);
            }
            s[80] = '\0';
            remove_trailing_spaces(s);

#ifdef PC
            fh = open(sFilename, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY, S_IWRITE);
#else
            fh = open(sFilename, O_WRONLY | O_CREAT | O_TRUNC, 0660);
#endif
            if (fh < 0) {
                printf("Error opening output file '%s'.  ", sFilename);
                sprintf(sFilename, "%simage_seg%d", sOutputPath, image_num);
                printf("Trying file name: '%s'\n", sFilename);

                fh = open_write_clear_ufile(sFilename);
            }
        }
        else
        {
            if (bJpeg) {
                sprintf(sFilename, "%simage%d.jpg", sOutputPath, image_num);
            }
            else if (strcmp(sIC, "NC") == 0) {
                sprintf(sFilename, "%simage%d.raw", sOutputPath, image_num);
            }
            else
            {
                sprintf(sFilename, "%simage%d.dat", sOutputPath, image_num);
            }
            fh = open_write_clear_ufile(sFilename);
        }

        write_image_data(fh, image_num, sFilename);
        close(fh);
    } /* end loop write images */
}



/*******************************************************************************
 * Name:    write_image_data
 *
 * Description: This function selects a method to write the image data
 *              (sequential or de-block).
 *
 *              If de-blocking is necessary, blocking information is collected,
 *              and another fn is called to de-block and write the image.
 *
 * Parameters:
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

void write_image_data(int fh, int image_num, char *sFilename)
{
    int nbpc;   /* # blocks per column (# blocks high) */
    int nbpr;   /* # blocks per row (wide) */
    long bands;
    //float  bytesPerPixel;
    //long oneBandSize;
    //long paddedBandSize;
    //long band_num;
    int  nppbv;
    int  nppbh;
    long nrows;
    long ncols;
    int nbpp;
    long rc;
    int bWriteStraight = FALSE;
    int flag = FALSE;

    /*
        sprintf(temp_filename, "temp_img%d", image_num);
        temp_fh = open_read_ufile(temp_filename);
    */

    /* Check if image is a JPEG image */
    if (iNITF_version == V2_0) {
        if (i20hdr[image_num].ic[0] == 'C' && i20hdr[image_num].ic[1] == '3') {
            bWriteStraight = TRUE;
        }
    }
    else if (iNITF_version == V2_1) {
        if (i21hdr[image_num].ic[0] == 'C' && i21hdr[image_num].ic[1] == '3') {
            bWriteStraight = TRUE;
        }
    }

    if (iNITF_version == V2_0) {

        nppbv = get_long(i20hdr[image_num].nppbv, 4);
        nppbh = get_long(i20hdr[image_num].nppbh, 4);
        nrows = get_long(i20hdr[image_num].nrows, 8);
        ncols = get_long(i20hdr[image_num].ncols, 8);

        nbpp  = get_long(i20hdr[image_num].nbpp, 2);
        nbpc  = get_long(i20hdr[image_num].nbpc, 4);
        nbpr  = get_long(i20hdr[image_num].nbpr, 4);
        bands = get_long(i20hdr[image_num].nbands, 1);

        /* Conditions we can unblock */
        rc = (i20hdr[image_num].imode[0] == 'B' && i20hdr[image_num].nbands[0] == '1')
            || (i20hdr[image_num].imode[0] == 'S' && i20hdr[image_num].nbands[0] != '1');

        if (rc == FALSE) {
            if (nbpc != 1 || nbpr != 1) {
                printf("Notice: image %d is blocked, but this software is unable to unblock.  Extracting in raw (unmodified, still blocked) format.\n", image_num);
                bWriteStraight = TRUE;
            }
        }
    }
    else if (iNITF_version == V2_1) {

        nppbv = get_long(i21hdr[image_num].nppbv, 4);
        nppbh = get_long(i21hdr[image_num].nppbh, 4);
        nrows = get_long(i21hdr[image_num].nrows, 8);
        ncols = get_long(i21hdr[image_num].ncols, 8);

        nbpp  = get_long(i21hdr[image_num].nbpp, 2);
        nbpc  = get_long(i21hdr[image_num].nbpc, 4);
        nbpr  = get_long(i21hdr[image_num].nbpr, 4);
        bands = get_long(i21hdr[image_num].nbands, 1);

        if (bands < 1) {
            bands = get_long(i21hdr[image_num].xbands, 5);
        }

        /* Conditions we can unblock */
        rc = (i21hdr[image_num].imode[0] == 'B' && i21hdr[image_num].nbands[0] == '1')
            || (i21hdr[image_num].imode[0] == 'S' && i21hdr[image_num].nbands[0] != '1');

        if (rc == FALSE) {
            if (nbpc != 1 || nbpr != 1) {
                printf("Error - image %d is blocked, but this software is unable to unblock.  Extracting in raw blocked format.\n", image_num);
                bWriteStraight = TRUE;
            }
        }
    }

    /* Open and position to start of image data */
    hNITF = open_read_ufile(sNITFfilename);
    rc = lseek(hNITF, image_info[image_num].nitf_file_offset, SEEK_SET);
/*printf("lseek result: %ld\n", rc);*/

    if ((nbpc == 1 && nbpr == 1) || bWriteStraight) {  /* write straight (unblocked) */
        if (gOptions.dump_qpm) {
            flag = QPM;
        }

        if (gOptions.mag_only && gOptions.bands == 2) {
            printf("Extracting magnitude only\n");
            copy_fh_2_fh2(fh, hNITF, image_info[image_num].length_of_data / 2, flag);
        }
        else
        {
            copy_fh_2_fh2(fh, hNITF, image_info[image_num].length_of_data, flag);
        }
    }
    else { //*if (nbpr > 1) {    /* Blocked, more than 1 block wide */
        if (gOptions.dump_qpm) {
            flag = QPM;
        }

        if (gOptions.mag_only && gOptions.bands == 2) {
            printf("Extracting magnitude only\n");
            bands = 1;
            unblock_image(hNITF, fh, nbpp, ncols, nrows, nbpr, nbpc, nppbh, nppbv, bands, flag);
        }
        else
        {
            unblock_image(hNITF, fh, nbpp, ncols, nrows, nbpr, nbpc, nppbh, nppbv, bands, flag);
        }
    }

    close(hNITF);
}


/*******************************************************************************
 * Name:    write_JPEG_image
 *
 * Description: This function writes a JPEG data file, converting it to
 *              commercial JPEG as it writes it.  It uses information
 *              previously read from a
 *              text segment to do the conversion.
 *
 * Parameters:
 *      text_num    # of the text segment
 *
 * Returns:
 *
 ******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void write_JPEG_image(int text_num)
{
    char *s;
    char var[81];
    char val[81];
    //long image_header_data[5];
    char msg[255];
    char sBuffer1[255];
    long rc;
    int x;
    //bool bFound = FALSE;
    static int image_num = -1;
    int bands;
    char *pTemp;

    image_num++;     /* Start at 1st image, each time called, go to next */

    if (iNITF_version == V2_0) {
        /* Loop until you find JPEG image segment */
        while (i20hdr[image_num].ic[0] != 'C' || i20hdr[image_num].ic[1] != '3') {
            image_num++;
            if (image_num >= number_of_images) return;
        }
        bands = get_long(i20hdr[image_num].nbands, 1);
    }
    else if (iNITF_version == V2_1) {
        /* Loop until you find JPEG image segment */
        while (i21hdr[image_num].ic[0] != 'C' || i21hdr[image_num].ic[1] != '3') {
            image_num++;
            if (image_num >= number_of_images) return;
        }
        bands = get_long(i21hdr[image_num].nbands, 1);
    }

    if (bands == 3) {
        jpeg_info.rgb_mode = TRUE;
    }
    else {
        jpeg_info.rgb_mode = FALSE;
    }
    jpeg_info.ffe6_remove = -1;
    jpeg_info.ffc0_remove = -1;
    jpeg_info.ffda_remove = -1;
    jpeg_info.ffdb_remove = -1;
    strcpy(jpeg_info.ffe0_data, "");
    strcpy(jpeg_info.ffc0_data, "");
    strcpy(jpeg_info.ffda_data, "");
    strcpy(jpeg_info.ffdb_data, "");

    if (image_info[image_num].bFile_written) {
        sprintf(msg, "write_JPEG_image: JPEG image (%d) for tagged text segment was already written!\n", image_num);
        errmessage(msg);
        iQuit(1);
    }

    pTemp = (char *) malloc(strlen(text_info[text_num].pData) + 1);
    if (pTemp == NULL) {
        errmessage("write_JPEG_image: Error allocating temporary memory\n");
        iQuit(1);
    }
    strcpy(pTemp, text_info[text_num].pData);

    /* Parse text data by line to get data for recreating original JPEG  */
    s = pTemp;
    rc = get_stored_line(s, sBuffer1);
    if (rc == FALSE) {
        errmessage("write_SAR_image: error reconstructing SAR header\n");
        iQuit(1);
    }
    for (x = 0; x < 8; x++) {
        parse_phoenix_line(sBuffer1, var, val);

        if (strcmp_caseless(var, "ffc0_remove") == 0) {
            jpeg_info.ffc0_remove = atol(val);
        }
        else if (strcmp_caseless(var, "ffda_remove") == 0) {
            jpeg_info.ffda_remove = atol(val);
        }
        else if (strcmp_caseless(var, "ffdb_remove") == 0) {
            jpeg_info.ffdb_remove = atol(val);
        }
        else if (strcmp_caseless(var, "ffe6_remove") == 0) {
            jpeg_info.ffe6_remove = atol(val);
        }
        else if (strcmp_caseless(var, "ffc0") == 0) {
            strcpy(jpeg_info.ffc0_data, val);
        }
        else if (strcmp_caseless(var, "ffda") == 0) {
            strcpy(jpeg_info.ffda_data, val);
        }
        else if (strcmp_caseless(var, "ffdb") == 0) {
            strcpy(jpeg_info.ffdb_data, val);
        }
        else if (strcmp_caseless(var, "ffe0") == 0) {
            strcpy(jpeg_info.ffe0_data, val);
        }
        else if (strcmp_caseless(var, "NumberOfRows") == 0) {
            /* ignore */
        }
        else if (strcmp_caseless(var, "NumberOfColumns") == 0) {
            /* ignore */
        }
        else {
            sprintf(msg, "write_JPEG_image: Can't match string from JPEG_extra_tags section:'%s'\n", var);
            errmessage(msg);
            iQuit(1);
        }

        rc = get_stored_line(NULL, sBuffer1);
        if (!rc) {
             errmessage("error recreating image header data\n");
             iQuit(1);
        }
    }
    rebuild_jpeg(image_num);

    /* Mark the segments as having been written */
    image_info[image_num].bFile_written = TRUE;
    text_info[text_num].bFile_written = TRUE;
    free(pTemp);
}



/******************************************************************************
 *
 *  qpm_data    This function converts data from 32 bit float magnitude only
 *              to 32-bit float QPM (quarter power magnitude)
 *
 *  Parameters:
 *      f           Pointer to array of floats to convert
 *      read_count  # floats to convert
 *
 *****************************************************************************/

void qpm_data(float *f, int read_count)
{
    int x;

    for (x = 0; x < read_count; x++) {
        *f = sqrt(*f);
        f++;
    }
}


/****************************************************************************
 * unblock_image This function reads data from fh_in, unblocks the data, and
 *               writes it to fh_out.
 *
 *               Read each block in a horizontal row, then write the image
 *               rows, thereby unblocking the image.
 *
 ****** Warning !! Warning !! Warning !! Warning !! Warning !! Warning *******
 *
 *   This function can only handle band-sequential data (i.e. no type of
 *   band interleaving.)
 *
 ****** Warning !! Warning !! Warning !! Warning !! Warning !! Warning *******
 *
 * Parameters:
 *
 *     fh_in        File handle for input file
 *     fh_out       File handle for output to write
 *
 *     nbpp         Number of bits per pixel
 *
 *     pixels_w     Pixels across entire image
 *     pixels_v     Pixel height of entire image
 *
 *     blocks_w     block image # blocks wide
 *     blocks_v     block image # blocks high
 *
 *     ppb_w        Pixels Per block horizontal (width of block)
 *     ppb_v        Height of block
 *
 *     bands        # of bands in image
 *     data_type    Type of data being blocked
 *
 *****************************************************************************/

void unblock_image(int fh_in, int fh_out, int nbpp,
        long pixels_w, long pixels_v, int blocks_w, int blocks_v,
        int ppb_w, int ppb_v, int bands, int flag)
{
    typedef struct {
        char *pdata;
        } block_type;

    block_type *block;

    int band_num;
    int vblock_num;
    int wblock_num;
    int y;
    long rc;
    long band_bytes_written;
    long band_size;         /* bytes in a band */
    int bBand_done;
    char sBuffer1[256];
    long r_pad;             /* right pad width */
    long block_width_bytes;
    long bytes_to_write;
    float bytes_per_pixel;

    bytes_per_pixel = nbpp / 8.0;

    if ((int) (bytes_per_pixel * 2) != bytes_per_pixel * 2) {
        printf("unblock_image: Can't handle nbpp (number bits per pixel) of %d\n", nbpp);
        iQuit(1);
    }

    if (ppb_w * bytes_per_pixel != (int)((float) (ppb_w * bytes_per_pixel))) {
        printf("unblock_image: Can't handle data with a block width that's not an even multiple of a byte (%f bytes wide)\n", ppb_w * bytes_per_pixel);
        iQuit(1);
    }

    r_pad = (blocks_w * ppb_w) - pixels_w;
    if (r_pad < 0) {
        printf("unblock_image: Right padding width is negative %ld pixels wide.\n", r_pad);
        iQuit(1);
    }

    if ((ppb_w - r_pad) * bytes_per_pixel != (int)((float) ((ppb_w - r_pad) * bytes_per_pixel))) {
        printf("unblock_image: Partial block data (right-padded data) has a width that's not an even multiple of a byte (%f bytes wide)\n", (ppb_w - r_pad) * bytes_per_pixel);
        iQuit(1);
    }

    block_width_bytes = (long) (ppb_w * bytes_per_pixel);

    /* Allocate an array of blocks (pointers to horizontal row of blocks) */
    block = (block_type *) malloc(sizeof(block_type) * blocks_w);
    if (block == NULL) {
        errmessage("unblock_image(): Error allocating memory for array of blocks\n");
        iQuit(1);
    }

    /* Allocate memory to store 1 row of blocks across the width of the image */
    for (wblock_num = 0; wblock_num < blocks_w; wblock_num++) {
        block[wblock_num].pdata = (char *)
                    malloc(block_width_bytes * ppb_v);

        if (block[wblock_num].pdata == NULL) {
            errmessage("unblock_image(): Error allocating memory for a block\n");
            iQuit(1);
        }
    }

    band_size = (long) (pixels_w * pixels_v * bytes_per_pixel);

    /***************************************/
    /* Read, unblock the image data, write */
    /***************************************/

    /* Read / unblock / write each band sequentially */
    for (band_num = 0; band_num < bands; band_num++) {

        bBand_done = FALSE;
        band_bytes_written = 0;

        /* loop through the rows of blocks from top to bottom of the band */
        for (vblock_num = 0; vblock_num < blocks_v; vblock_num++) {

            /****************************************************************
            * read blocks from file to blocks in memory (loop through blocks
            * horizontally, reading the entire contents of each before going
            * to the next)
            ****************************************************************/

            for (wblock_num = 0; wblock_num < blocks_w; wblock_num++) {

                rc = read (fh_in, block[wblock_num].pdata,
                        ppb_v * block_width_bytes);
                if (rc < ppb_v * block_width_bytes) {
                    sprintf(sBuffer1, "unblock_image: Error reading input file.  Returned %ld, not %ld\n", rc, ppb_v * block_width_bytes);
                    errmessage(sBuffer1);
                    iQuit(1);
                }
            }

            /*******************************************************************
            * Write an entire row of blocks, an entire line across at a time.
            * Handle special cases:
            *   1) Right padded
            *   2) Bottom padded
            *   3) Right and bottom padded
            ******************************************************************/

            /* Loop through block vertically */
            for (y = 0; y < ppb_v; y++) {
                for (wblock_num = 0; (wblock_num < blocks_w) && (bBand_done == FALSE); wblock_num++) {

                    /* Write image row by writing row from each horizontal block */
                    bytes_to_write = block_width_bytes;

                    if (wblock_num == blocks_w - 1) {
                        bytes_to_write = (long) (block_width_bytes - (r_pad * bytes_per_pixel));
                    }

                    if (flag == QPM) {
                        if (bytes_to_write%4) {
                            printf("unblock_data: bytes_to_write is not a multiple of 4!\n");
                            iQuit(1);
                        }
                        qpm_data((float *) &block[wblock_num].pdata[y * block_width_bytes], bytes_to_write/4);
                    }

                    rc = write (fh_out, &block[wblock_num].pdata[y * block_width_bytes], bytes_to_write);
                    if (rc < bytes_to_write) {
                        sprintf(sBuffer1, "unblock_image: Error reading input file.  Returned %ld, not %ld\n", rc, bytes_to_write);
                        errmessage(sBuffer1);
                        iQuit(1);
                    } else if (rc == 0) {
                        if (vblock_num != blocks_v - 1) {
                            errmessage("unblock_image: Band ended before bottom-most row!\n");
                        }
                    }

                    band_bytes_written += bytes_to_write;

                    if (band_bytes_written == band_size) {
                        bBand_done = TRUE;
                    }
                }
            }
        } /* end for vblock_num = ... */
    } /* end for band_num = ... */
}

