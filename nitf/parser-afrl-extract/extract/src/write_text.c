/* write_text.c - functions pertaining to writing text portions *************/

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
*       Functions to rearrange images from a tiled (blocked) format to a flat
* (unblocked, straight left-to-right, top to bottom data) format.
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
#include <sys\stat.h>
#endif

#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"

/*******************************************************************************
 * Name:    write_text_files_separately
 *
 * Description: This function writes text files separately
 *
 * Parameters:
 * Returns:
 ******************************************************************************/

void write_text_files_separately(void)
{
    char sFilename[1024];
    int text_num;
    int fh;
    char *s;

    /* Write text files */
    for (text_num = 0; text_num < number_of_text_files; text_num++) {
        if (text_info[text_num].bFile_written
                    && bExtractAllTextFiles == FALSE) continue;

        /* Create filename */
        if (gOptions.bVeridianNitfFile && gOptions.dump_standard_filenames == FALSE) {
            strcpy(sFilename, sOutputPath);
            s = strchr(sFilename, '\0');

            if (iNITF_version == V2_0) {
                strncpy(s, t20hdr[text_num].txtitl, 80);
            }
            if (iNITF_version == V2_1) {
                strncpy(s, t21hdr[text_num].txtitl, 80);
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
                sprintf(sFilename, "%stext_seg%d.txt", sOutputPath, text_num);
                printf("Trying file name: '%s'\n", sFilename);

                fh = open_write_clear_ufile(sFilename);
            }
        }
        else
        {
            sprintf(sFilename, "%stext_seg%d.txt", sOutputPath, text_num);
            fh = open_write_clear_ufile(sFilename);
        }

        write_meta_data(fh, text_num);

        close(fh);
    } /* end write text files loop */
}



/**************************************************************************
 *
 * Function:    dump_geo
 * Description: This function creates a .geo file for any image segments
 *              with the IGEOLO filled-in.
 *
 * Parameters:
 **************************************************************************/

void dump_geo(void)
{
    FILE *fp;
    char filename[1024];
    int image_num;
    //int rc;
    char s_in[100];

    //char sTemp[256];                   
    char *s;
    
    for (image_num = 0; image_num < number_of_images; image_num++) {
        if (iNITF_version == V2_0) {

            /* Create .geo filename */

            if (gOptions.bVeridianNitfFile) {
                strcpy(filename, sOutputPath);
                s = strchr(filename, '\0');

                strncpy(s, i20hdr[image_num].ititle, 80);
                s[80] = '\0';
                remove_trailing_spaces(s);

                strcat(filename, ".geo");

                fp = fopen(filename, "wb");
                if (fp == NULL) {
                    printf("Error opening output file '%s'.  ", filename);
                    sprintf(filename, "%stext_seg%d.txt", sOutputPath, image_num);
                    printf("Trying file name: '%s'\n", filename);

                    fp = open_write_file(filename);
                }
            }
            else
            {
                sprintf(filename, "%simage_seg%d.geo", sOutputPath, image_num);
                fp = open_write_file(filename);
            }

            if (i20hdr[image_num].icords[0] == 'G') {
                get_string(s_in, i20hdr[image_num].igeolo, 60);

                fprintf(fp, "CalcImgCornerLat1= %6.6s.00000 %1.1s\r\n", &s_in[0], &s_in[6]);
                fprintf(fp, "CalcImgCornerLon1= %7.7s.00000 %1.1s\r\n", &s_in[7], &s_in[14]);
                fprintf(fp, "CalcImgCornerLat2= %6.6s.00000 %1.1s\r\n", &s_in[15], &s_in[21]);
                fprintf(fp, "CalcImgCornerLon2= %7.7s.00000 %1.1s\r\n", &s_in[22], &s_in[29]);
                fprintf(fp, "CalcImgCornerLat3= %6.6s.00000 %1.1s\r\n", &s_in[30], &s_in[36]);
                fprintf(fp, "CalcImgCornerLon3= %7.7s.00000 %1.1s\r\n", &s_in[37], &s_in[44]);
                fprintf(fp, "CalcImgCornerLat4= %6.6s.00000 %1.1s\r\n", &s_in[45], &s_in[51]);
                fprintf(fp, "CalcImgCornerLon4= %7.7s.00000 %1.1s\r\n", &s_in[52], &s_in[59]);
            }
        }
        else if (iNITF_version == V2_1)
        {
            /* Create .geo filename */

            if (gOptions.bVeridianNitfFile) {
                strcpy(filename, sOutputPath);
                s = strchr(filename, '\0');

                strncpy(s, i21hdr[image_num].iid2, 80);
                s[80] = '\0';
                remove_trailing_spaces(s);

                strcat(filename, ".geo");

                fp = fopen(filename, "wb");
                if (fp == NULL) {
                    printf("Error opening output file '%s'.  ", filename);
                    sprintf(filename, "%stext_seg%d.txt", sOutputPath, image_num);
                    printf("Trying file name: '%s'\n", filename);

                    fp = open_write_file(filename);
                }
            }
            else
            {
                sprintf(filename, "%simage_seg%d.geo", sOutputPath, image_num);
                fp = open_write_file(filename);
            }

            if (i21hdr[image_num].icords[0] == 'G') {
                get_string(s_in, i21hdr[image_num].igeolo, 60);

                fprintf(fp, "CalcImgCornerLat1= %6.6s.00000 %1.1s\r\n", &s_in[0], &s_in[6]);
                fprintf(fp, "CalcImgCornerLon1= %7.7s.00000 %1.1s\r\n", &s_in[7], &s_in[14]);
                fprintf(fp, "CalcImgCornerLat2= %6.6s.00000 %1.1s\r\n", &s_in[15], &s_in[21]);
                fprintf(fp, "CalcImgCornerLon2= %7.7s.00000 %1.1s\r\n", &s_in[22], &s_in[29]);
                fprintf(fp, "CalcImgCornerLat3= %6.6s.00000 %1.1s\r\n", &s_in[30], &s_in[36]);
                fprintf(fp, "CalcImgCornerLon3= %7.7s.00000 %1.1s\r\n", &s_in[37], &s_in[44]);
                fprintf(fp, "CalcImgCornerLat4= %6.6s.00000 %1.1s\r\n", &s_in[45], &s_in[51]);
                fprintf(fp, "CalcImgCornerLon4= %7.7s.00000 %1.1s\r\n", &s_in[52], &s_in[59]);
            }
        }
        fclose(fp);
    }
}



/**************************************************************************
 *
 * dump_ssummary    This function outputs a summary to the file passed.
 *                  If the file doesn't exist, a label is added as the
 *                  first line to make the file somewhat user friendly.
 *
 * Parameters:
 *      filename    Name of the file to store the summary information
 **************************************************************************/

void dump_ssummary(char *filename)
{
    FILE *fp;
    int image_num;
    int rc;
    char sBuffer1[100];
    char sBuffer2[200];

    fp = open_write_append(filename);
    fclose(fp);
    rc = get_file_length(filename);
    fp = open_write_append(filename);

    if (rc <= 10) {
        /* if summary file is new, put label line as first line */
        fprintf(fp, "filename, lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4, IC, PVTYPE, ABPP, IREP, ICAT, #BANDS, IMODE, NROWS, NCOLS, #images, #text files, #symbols/graphics, #labels, NITF header TREs; Image header TREs\n");
    }

    for (image_num = 0; image_num < number_of_images; image_num++) {
        fprintf(fp, "%s, ", sNITFfilename);

        if (iNITF_version == V2_0) {
            if (i20hdr[image_num].icords[0] != 'N') {
                /*fDumpChars(fp, i20hdr[image_num].igeolo, 60); */
                get_string(sBuffer1, i20hdr[image_num].igeolo, 60);
                parse_igeolo20(i20hdr[image_num].icords[0], sBuffer1, sBuffer2);
                fprintf(fp, "%s, ", sBuffer2);
            }
            else {
                fprintf(fp, "lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4, ");
            }
        }
        else if (iNITF_version == V2_1)
        {
            if (i21hdr[image_num].icords[0] != ' ') {
                /* fDumpChars(fp, i21hdr[image_num].igeolo, 60);*/
                get_string(sBuffer1, i21hdr[image_num].igeolo, 60);
                parse_igeolo21(i21hdr[image_num].icords[0], sBuffer1, sBuffer2);
                fprintf(fp, "%s, ", sBuffer2);
            }
            else {
                fprintf(fp, "lat1, lon1, lat2, lon2, lat3, lon3, lat4, lon4, ");
            }
        }


        if (iNITF_version == V2_0) {
            fDumpChars(fp, i20hdr[image_num].ic, 2);
            fprintf(fp, ", ");

            fDumpChars(fp, i20hdr[image_num].pvtype, 3);
            fprintf(fp, ", ");

            fDumpChars(fp, i20hdr[image_num].abpp, 2);
            fprintf(fp, ", ");

            fDumpChars(fp, i20hdr[image_num].irep, 8);
            fprintf(fp, ", ");

            fDumpChars(fp, i20hdr[image_num].icat, 8);
            fprintf(fp, ", ");


            fDumpChars(fp, i20hdr[image_num].nbands, 1);
            fprintf(fp, ", ");

            fprintf(fp, "%c, ", i20hdr[image_num].imode[0]);

            fDumpChars(fp, i20hdr[image_num].nrows, 8);
            fprintf(fp, ", ");

            fDumpChars(fp, i20hdr[image_num].ncols, 8);
            fprintf(fp, ", ");
        }
        else if (iNITF_version == V2_1)
        {
            fDumpChars(fp, i21hdr[image_num].ic, 2);
            fprintf(fp, ", ");


            fDumpChars(fp, i21hdr[image_num].pvtype, 3);
            fprintf(fp, ", ");

            fDumpChars(fp, i21hdr[image_num].abpp, 2);
            fprintf(fp, ", ");

            fDumpChars(fp, i21hdr[image_num].irep, 8);
            fprintf(fp, ", ");

            fDumpChars(fp, i21hdr[image_num].icat, 8);
            fprintf(fp, ", ");

            if (i21hdr[image_num].nbands[0] != '0') {
                fDumpChars(fp, i21hdr[image_num].nbands, 1);
            }
            else {
                fDumpChars(fp, i21hdr[image_num].xbands, 5);
            }
            fprintf(fp, ", ");

            fprintf(fp, "%c, ", i21hdr[image_num].imode[0]);

            fDumpChars(fp, i21hdr[image_num].nrows, 8);
            fprintf(fp, ", ");

            fDumpChars(fp, i21hdr[image_num].ncols, 8);
            fprintf(fp, ", ");
        }

        if (iNITF_version == V2_0) {
            fprintf(fp, "%d, %d, %d, %d, ", number_of_images, number_of_text_files, number_of_symbols, number_of_labels);
        }
        if (iNITF_version == V2_1) {
            fprintf(fp, "%d, %d, %d, 0, ", number_of_images, number_of_text_files, number_of_graphics);
        }

        /* Append TRE list */
        loop_through_TREs(fp, -1, DO_ALL | TRE_NAMES, "");
        fprintf(fp, "; ");

        loop_through_TREs(fp, image_num, DO_ALL | TRE_NAMES, "");

        fprintf(fp, "\n");
    }

    fclose(fp);
}



/*******************************************************************************
 * Name:        write_nitf_headers
 *
 * Description: This function writes separate NITF headers to files
 *
 * Parameters:
 *
 * Returns:
 *
 ******************************************************************************/

void write_nitf_headers(void)
{
    int i;
    char Filename[1024];
    char sBuffer[1000];

    sprintf(Filename, "%smainhdr_out.txt", sOutputPath);
    if (gOptions.debug) printf("using dff file '%s'\n", Filename);
    DFPExportData(Filename, "main.dff", sBuffer, FALSE, 50);

    if (iNITF_version == V2_0) {
        for (i = 0; i < number_of_symbols; i++) {
            sprintf(Filename, "symhdr%d.txt", i);
            if (gOptions.debug) printf("using dff file 'symhdr.dff'\n");
            DFPExportData(Filename, "symhdr.dff", sBuffer, FALSE, 50);
        }
        for (i = 0; i < number_of_labels; i++) {
            sprintf(Filename, "lblhdr%d.txt", i);
            if (gOptions.debug) printf("using dff file 'lblhdr.dff'\n");
            DFPExportData(Filename, "lblhdr.dff", sBuffer, FALSE, 50);
        }
    }
    else if (iNITF_version == V2_1) {
        for (i = 0; i < number_of_graphics; i++) {
            sprintf(Filename, "grphdr%d.txt", i);
            if (gOptions.debug) printf("using dff file 'grphdr.dff'\n");
            DFPExportData(Filename, "grphdr.dff", sBuffer, FALSE, 50);
        }
    }

    for (i = 0; i < number_of_images; i++) {
        sprintf(Filename, "imghdr%d.txt", i);
        if (gOptions.debug) printf("using dff file 'imghdr.dff'\n");
        DFPExportData(Filename, "imghdr.dff", sBuffer, FALSE, 50);
    }

    for (i = 0; i < number_of_text_files; i++) {
        sprintf(Filename, "txthdr%d.txt", i);
        if (gOptions.debug) printf("using dff file 'txthdr.dff'\n");
        DFPExportData(Filename, "txthdr.dff", sBuffer, FALSE, 50);
    }
}



/**************************************************************************
 *
 * Function:    write_phoenix_files
 * Description: This function writes a phoenix file
 *
 * Parameters:
 *      fname       Name of the file to write
 **************************************************************************/

void write_phoenix_files(char *fname)
{
    char sFilename[260];
    char pff_filename[1024];
    char sBuffer1[2048];
    char sBuffer2[1024];
    char *s;

    int fh;
    int image_to_write;
    int image_num;
    int  file_position;     /* hold file position */
    int  len;
    int  i;
    int  nbpp;
    int text_num;
    int rc;

    long totalLength;
    long PhoenixHeaderSize;

    FILE *outFile;

    static char phoenixHead[] = "\n[PhoenixHeaderVer01.05]\nPhoenixHeaderLength= 00000\nPhoenixSigSize= 0000000000\nPhoenixSigNum= 0001\nPhoenixHeaderCallingSequence=\nnative_header_length= 0\n";
    static char phoenixFoot[] = "[EndofPhoenixHeader]\n";

/*
    Things to know:

    \n[PhoenixHeaderVer01.05]
    \nPhoenixHeaderLength= 00000
    \nPhoenixSigSize= 0000000000
    \nPhoenixSigNum= 0001
    \nPhoenixHeaderCallingSequence=

    [EndofPhoenixHeader]

    printf("Head:%d Foot:%d\n", sizeof phoenixHead, sizeof phoenixFoot);

#ifdef REMOVED
"\n[PhoenixH
eaderVer01
.05]\nPhoen
ixHeaderLe
ngth= 0000
0\nPhoenixS
igSize= 00
00000000\nPhoenixSigNum= 0001\nPhoenixHeaderCallingSequence=\nnative_header_length= 0\n";
#endif
    */

    printf("Making phoenix file\n");

    image_to_write = -1;

    /* Find First Non-JPEG image, put image # in image_to_write */
    for (image_num = 0; image_num < number_of_images; image_num++) {
        /* if (image_info[image_num].bFile_written) continue; */

        if (iNITF_version == V2_0) {
            if ('N' == i20hdr[image_num].ic[0] && 'C' == i20hdr[image_num].ic[1]) {
                 image_to_write = image_num;
                 break;
            }
        }
        else if (iNITF_version == V2_1) {
            if ('N' == i21hdr[image_num].ic[0] && 'C' == i21hdr[image_num].ic[1]) {
                 image_to_write = image_num;
                 break;
            }
        }
    }

    if (image_to_write < 0) {
        printf("write_output_files: Error - Write Phoenix option, but no raw data to put in phoenix file\n");
        iQuit(1);
    }

    /* Create Phoenix file's filename */
    strcpy(sFilename, sOutputPath);
    strcat(sFilename, fname);
    remove_trailing_spaces(sFilename);

    fh = open_write_clear_ufile(sFilename);

    /* write phoenix header first */
    s = phoenixHead;
    sBuffer1[0] = '\0';
    rc = get_stored_line(s, sBuffer1);
    do {
        strcat(sBuffer1, "\n");
        write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");
        rc = get_stored_line(NULL, sBuffer1);
    } while (rc);

    if (gOptions.dump_phoenix_tre) {

        /*** Build raw data information, because it's not in the meta-data ***/
        if (iNITF_version == V2_0) {
            get_string(sBuffer2, i20hdr[image_to_write].ititle, 80);
            sprintf(sBuffer1, "Filename= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");

            file_position = lseek(hNITF, 0, SEEK_CUR);
            /* lseek(hNITF, image_info[image_to_write].nitf_file_offset, SEEK_SET); */
            md5_subfile(hNITF, image_info[image_to_write].nitf_file_offset,
                        image_info[image_to_write].length_of_data, sBuffer2);
            lseek(hNITF, file_position, SEEK_SET);

            sprintf(sBuffer1, "DataCheckSum= %s\n", sBuffer2); 
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");

            sprintf(sBuffer1, "DataSize= %ld\n", image_info[image_to_write].length_of_data);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");

            get_string(sBuffer2, i20hdr[image_to_write].nbands, 1);
            i = atol(sBuffer2);
            if (i > 2) {
                strcpy(sBuffer2, "Image Cube - Band Sequential");
            }
            else
            {
                strcpy(sBuffer2, "Image");
            }
            sprintf(sBuffer1, "DataDomain= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i20hdr[image_to_write].nbpp, 2);
            nbpp = atol(sBuffer2);

            get_string(sBuffer2, i20hdr[image_to_write].pvtype, 3);
            trim(sBuffer2);
            if (strcmp(sBuffer2, "INT") == 0) {
                sprintf(sBuffer2, "unsigned %d bit integer", nbpp);
            }
            else if (strcmp(sBuffer2, "SI") == 0) {
                sprintf(sBuffer2, "signed %d bit integer", nbpp);
            }
            else if (strcmp(sBuffer2, "R") == 0) {
                strcpy(sBuffer2, "Float");
            }
            else
            {
                printf("write_output_files: Error - don't know how to convert pvtype '%s' to Phoenix format\n", sBuffer2);
                iQuit(1);
            }
            sprintf(sBuffer1, "DataFormat= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i20hdr[image_to_write].nbands, 1);
            i = atol(sBuffer2);
            if (i != 2) {
                strcpy(sBuffer2, "Magnitude");
            }
            else
            {
                strcpy(sBuffer2, "Mag-Phase");
            }
            sprintf(sBuffer1, "DataType= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i20hdr[image_to_write].nbpp, 2);
            i = atol(sBuffer2);
            sprintf(sBuffer1, "BitsPerSample= %d\n", i);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            sprintf(sBuffer1, "DataByteOrder= Big-Endian\n");
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i20hdr[image_to_write].nrows, 8);
            i = atol(sBuffer2);
            sprintf(sBuffer1, "NumberOfRows= %d\n", i);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i20hdr[image_to_write].ncols, 8);
            i = atol(sBuffer2);
            sprintf(sBuffer1, "NumberOfColumns= %d\n", i);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");
        }
        else if (iNITF_version == V2_1) {
            get_string(sBuffer2, i21hdr[image_to_write].iid2, 80);
            sprintf(sBuffer1, "Filename= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");

            file_position = lseek(hNITF, 0, SEEK_CUR);
            lseek(hNITF, image_info[image_to_write].nitf_file_offset, SEEK_SET);
            md5_subfile(hNITF, image_info[image_to_write].nitf_file_offset,
                    image_info[image_to_write].length_of_data, sBuffer2);
            lseek(hNITF, file_position, SEEK_SET);

            sprintf(sBuffer1, "DataCheckSum= %s\n", sBuffer2); 
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            sprintf(sBuffer1, "DataSize= %ld\n", image_info[image_to_write].length_of_data);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i21hdr[image_to_write].nbands, 1);
            i = atol(sBuffer2);
            if (i > 2 || i == 0) {
                strcpy(sBuffer2, "Image Cube - Band Sequential");
            }
            else
            {
                strcpy(sBuffer2, "Image");
            }
            sprintf(sBuffer1, "DataDomain= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");

            get_string(sBuffer2, i21hdr[image_to_write].nbpp, 2);
            nbpp = atol(sBuffer2);

            get_string(sBuffer2, i21hdr[image_to_write].pvtype, 3);
            trim(sBuffer2);
            if (strcmp(sBuffer2, "INT") == 0) {
                sprintf(sBuffer2, "unsigned %d bit integer", nbpp);
            }
            if (strcmp(sBuffer2, "SI") == 0) {
                sprintf(sBuffer2, "signed %d bit integer", nbpp);
            }
            else if (strcmp(sBuffer2, "R") == 0) {
                strcpy(sBuffer2, "Float");
            }
            else
            {
                printf("write_output_files: Error - don't know how to convert pvtype '%s' to Phoenix format\n", sBuffer2);
                iQuit(1);
            }
            sprintf(sBuffer1, "DataFormat= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i21hdr[image_to_write].nbands, 1);
            i = atol(sBuffer2);
            if (i != 2) {
                strcpy(sBuffer2, "Magnitude");
            }
            else
            {
                strcpy(sBuffer2, "Mag-Phase");
            }
            sprintf(sBuffer1, "DataType= %s\n", sBuffer2);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i21hdr[image_to_write].nbpp, 2);
            i = atol(sBuffer2);
            sprintf(sBuffer1, "BitsPerSample= %d\n", i);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            sprintf(sBuffer1, "DataByteOrder= Big-Endian\n");
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i21hdr[image_to_write].nrows, 8);
            i = atol(sBuffer2);
            sprintf(sBuffer1, "NumberOfRows= %d\n", i);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");


            get_string(sBuffer2, i21hdr[image_to_write].ncols, 8);
            i = atol(sBuffer2);
            sprintf(sBuffer1, "NumberOfColumns= %d\n", i);
            write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing phoenix header\n");
        }

        close(fh);
        outFile = open_write_append(sFilename);

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
            printf("write_output_files: dump main header: error allocating memory\n");
            iQuit(1);
        }

        /* Rewind to start of file */
        rc = lseek(hNITF, 0, SEEK_SET);
        if (rc != 0) {
            printf("write_output_files: Error positioning NITF file\n");
            iQuit(1);
        }

        /* Read NITF File header */
        read_verify(hNITF, s, len, "write_output_files: Error reading main header");

        /* Dump NITF file header */
        if (iNITF_version == V2_0) {
            sprintf(pff_filename, "%smain20.pff", sDFFpath);
            if (gOptions.debug) printf("using pff file '%s'\n", pff_filename);
            DFPExportDataFp(outFile, pff_filename, s, FALSE, 50);
        }
        else if (iNITF_version == V2_1) {
            sprintf(pff_filename, "%smain21.pff", sDFFpath);
            if (gOptions.debug) printf("using pff file '%s'\n", pff_filename);
            DFPExportDataFp(outFile, pff_filename, s, FALSE, 50);
        }

        free(s);
        s = NULL;

        /* Dump image segment subheaders */
        rc = lseek(hNITF, image_info[image_to_write].nitf_file_offset - image_info[image_to_write].length_of_subheader, SEEK_SET);
        if (rc < 0) {
            printf("write_output_files: Error positioning NITF file\n");
            iQuit(1);
        }

        len = image_info[image_to_write].length_of_subheader;
        s = (char *) malloc(len);
        if (s == NULL) {
            printf("write_output_files: error allocating memory\n");
            iQuit(1);
        }

        read_verify(hNITF, s, len, "write_output_files: Error reading image sub-header");

        if (iNITF_version == V2_0) {
            /*DFPExportDataFp(outFile, "imghdr20.pff", s); */
        }
        else if (iNITF_version == V2_1) {
            sprintf(pff_filename, "%simghdr21.pff", sDFFpath);
            if (gOptions.debug) printf("using pff file '%s'\n", pff_filename);
            DFPExportDataFp(outFile, pff_filename, s, FALSE, 50);
        }

        /* Reposition file pointer */
        len = lseek(hNITF, file_position, SEEK_SET);
        if (len != file_position) {
            printf("write_output_files: dump main header: error repositioning file pointer\n");
            iQuit(1);
        }

        dump_tres(outFile);
        lseek(hNITF, file_position, SEEK_SET);
        fclose(outFile);
        open_write_append_ufile(sFilename);
    }
    else
    {
        /* Write text files */
        for (text_num = 0; text_num < number_of_text_files; text_num++) {
            if (text_info[text_num].bFile_written
                                && bExtractAllTextFiles == FALSE) continue;

            write_meta_data(fh, text_num);
        } /* end for */
    }

    /* write the footer to the file */
    write_verify(fh, phoenixFoot, strlen(phoenixFoot), "write_output_files: Error writing metadata file\n");

    PhoenixHeaderSize = lseek(fh, 0, SEEK_END);

    sprintf(sBuffer1, "%s (Image data in phoenix file)\n", sFilename);

    write_image_data(fh, image_to_write, sBuffer1);

    totalLength = lseek(fh, 0, SEEK_END);
    close(fh);

#ifdef PC
    outFile = fopen(sFilename, "r+b");
#else
    outFile = fopen(sFilename, "r+");
#endif
    if (!outFile) {
        printf("write_output_files-Phoenix: Error opening output file %s 11\n", sFilename);
        iQuit(1);
    }

/* now we use lengths to calculate sizes */
    fseek(outFile, 46, SEEK_SET);
    fprintf(outFile, "%05ld", PhoenixHeaderSize);
    fseek(outFile, 68, SEEK_SET);
    fprintf(outFile, "%010ld", totalLength); /* - PhoenixHeaderSize); */

    /* then we have to write those values into the phoenix header part. */
/*printf("Total:%08d File:%05d Difference:%d\n", totalDataLength, fileDataLength, totalDataLength-fileDataLength); */

    fclose(outFile);
}



/*******************************************************************************
 * Name:    write_meta_data
 *
 * Description: This function parses and writes text data to a file handle.
 *              End of lines are newlines (linefeeds).
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

void write_meta_data(int fh, int text_num)
{
    char *s;
    char sBuffer1[1000];
    long rc;
    //static int found_filename = FALSE;
    long val;

    s = text_info[text_num].pData;
    sBuffer1[0] = '\0';
    rc = get_stored_line(s, sBuffer1);
    do {
        /* If magnitude only, fix up the Phoenix file to compensate. */
        if (gOptions.mag_only && gOptions.bands == 2) {
            if (strncmp_caseless(sBuffer1, "DataType=", 9) == 0) {
                sprintf(sBuffer1, "DataType= Magnitude");
            }
            else if (strncmp_caseless(sBuffer1, "DataCheckSum=", 13) == 0) {
                goto next_line;
            }
            else if (strncmp_caseless(sBuffer1, "DataSize=", 9) == 0) {
                s = strchr(sBuffer1, '=');
                if (s) {
                    s++;
                    val = atol(s);
                    sprintf(sBuffer1, "DataSize= %ld", val/2);
                }
                else
                {
                    printf("write_meta_data: Unable to get DataSize\n");
                    iQuit(1);
                }
            }
        } /* end of mag_only */

        strcat(sBuffer1, "\n");
        write_verify(fh, sBuffer1, strlen(sBuffer1), "write_meta_data: Error writing text file\n");
next_line:
        rc = get_stored_line(NULL, sBuffer1);
    } while (rc);
}



/*******************************************************************************
 * Name:    write_text_data
 *
 * Description: This function parses and writes text data to a file handle.
 *              End of lines are newlines (linefeeds).
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
#ifdef REMOVED
void write_text_data(int fh, int text_num)
{
    char *s;
    char sBuffer1[1000];
    long rc;
    static found_filename = FALSE;

    s = text_info[text_num].pData;
    sBuffer1[0] = '\0';
    rc = get_stored_line(s, sBuffer1);
    do {

#ifdef REMOVED
        if (strncmp_caseless(sBuffer1, "Filename=", 9) == 0) {
            if (!found_filename) {
                found_filename = TRUE;
            }
            else
            {
                goto next_line;
            }
        }
#endif

        strcat(sBuffer1, "\n");
        write_verify(fh, sBuffer1, strlen(sBuffer1), "write_output_files: Error writing text file\n");
next_line:
        rc = get_stored_line(NULL, sBuffer1);
    } while (rc);
}
#endif



