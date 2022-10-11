/* V */
/* image.c - read the images from an NITF file */

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
*       Routines to read the images from an NITF file
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

#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"


/* defines */


/*******************************************************************************
 * Name:  read_image_data
 *
 * Description: Fills in the image subheaders & copies image data to a temp file
 *
 * Parameters:
 *      number_of_images
 *      image_info         array of structures of image data (hdr, data length)
 * Returns:
 *    >= 0   success, # of images read
 *    -1     error
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int read_image_data(int number_of_images, image_info_type *image_info)
{
    int image_num;
    int number_of_bands;
    int number_of_LUT_entries;
    int number_of_LUTs;
    int x;
    long header_start;
    long rc;
    long user_defined_data_length;
    long extended_subheader_data_length;
    //int  temp_fh;
    //char temp_filename[100];
    //FILE *fp;
    char dump_filename[1024];
    char dff_filename[1024];
    char *s;
    long len;
    int  bNonCompressed;
    int  image_width;
    int  nbpp;
    char sBuffer[2048];
    
    if (number_of_images <= 0) return number_of_images;

    /******************************************************************
     * Allocate memory for structure array to hold image subheader(s)
     ******************************************************************/

    switch(iNITF_version) {
     case V2_0:
         i20hdr = (nitf_20_imagesub_type *) malloc(
                sizeof(nitf_20_imagesub_type) * number_of_images);
         if (i20hdr == NULL) {
             errmessage("Error allocating memory");
             iQuit(1);
         }
         break;

     case V2_1:
         i21hdr = (nitf_21_imagesub_type *) malloc(
                sizeof(nitf_21_imagesub_type) * number_of_images);
         if (i21hdr == NULL) {
             errmessage("Error allocating memory");
             iQuit(1);
         }
         break;
     default:
         errmessage("Error - unknown NITF format (shouldn't get here!\n");
         iQuit(1);
         break;
   }

   for (image_num = 0; image_num < number_of_images; image_num++) {
        len = image_info[image_num].length_of_subheader;
        s = (char *) malloc(len);
        if (s == NULL) {
            errmessage("read_image_data: error allocating memory\n");
            iQuit(1);
        }

        header_start = lseek(hNITF, 0, SEEK_CUR);
        read_verify(hNITF, s, len, "Error reading image sub-header");

        sprintf(dump_filename, "%simage_hdr%d.txt", sOutputPath, image_num);

        if (iNITF_version == V2_0) {
            /*DFPExportData(dump_filename, "imghdr20.dff", s); */
        }
        else if (iNITF_version == V2_1) {
            sprintf(dff_filename, "%simghdr21.dff", sDFFpath);
            DFPExportData(dump_filename, dff_filename, s, FALSE, 50);
        }

        len = lseek(hNITF, header_start, SEEK_SET);
        if (len != header_start) {
            errmessage("read_image_data: error repositioning file pointer\n");
            iQuit(1);
        }
        free(s);
        s = NULL;

/*printf("header starting pos = %ld\n", header_start); */

       switch(iNITF_version) {
           case V2_0:
               read_verify(hNITF, (char *) &(i20hdr[image_num]),
                    290, /* read up through field isdwng (2.0), isctln (2.1) */
                    "Error reading image sub-header");




               /* If conditional field fsdwng exists, read it, otherwise skip it. */
               if (strncmp(i20hdr[image_num].isdwng, "999998", 6) == 0) {

                   /* Read isdevt */

                   read_verify(hNITF, i20hdr[image_num].isdevt,
                        40, "Error reading image subheader");

                    /*printf("isdevt = '%s'\n", i20hdr[image_num].isdevt); */
               }
               else
               {
                    /*printf("skip isdevt\n"); */

                   /* Skip fsdevt & read rest of header B */

                   memset(i20hdr[image_num].isdevt, ' ', 40);
               }

               /* Read encryp..icords */

               read_verify(hNITF, i20hdr[image_num].encryp,
                    82, "Error reading NITF image subheader");

               if (i20hdr[image_num].icords[0] == 'G' || i20hdr[image_num].icords[0] == 'U' || i20hdr[image_num].icords[0] == 'C') {
                   read_verify(hNITF, i20hdr[image_num].igeolo, 60,
                            "Error reading image subheader");

#ifdef REMOVED
                   if (gOptions.dump_ssummary) {
                       printf("dump ssummary\n");
                   }
#endif

               }
               else
               {
                   memset(i20hdr[image_num].igeolo, ' ', 60);
                   if (gOptions.dump_ssummary) {
                       printf("dump ssummary: No IGEOLO for image %d\n", image_num + 1);
                   }
               }

               /* Read nicom */

               read_verify(hNITF, i20hdr[image_num].nicom,
                        1, "Error reading NITF image subheader");
               strncpy(sBuffer, i20hdr[image_num].nicom, 1);
               sBuffer[1] = '\0';

               x = atoi(sBuffer);

               if (x > 0) {
                   /* Allocate Space for comment lines */
                   i20hdr[image_num].pIcomn = (char *) malloc(x * 80);
                   if (i20hdr[image_num].pIcomn == NULL) {
                       errmessage("Error allocating memory for \
i20hdr[image_num].icomn");
                       iQuit(1);
                   }

                   /* Read Image subheader comment lines */
                   read_verify(hNITF, i20hdr[image_num].pIcomn, x * 80,
                        "Error reading image subheader");
               }
               else
               {
                   i20hdr[image_num].pIcomn = NULL;
               }

               /* Read ic */
               read_verify(hNITF, i20hdr[image_num].ic,
                                2, "Error reading NITF image subheader");

               bNonCompressed = FALSE;

               /* Read comrat if necessary */
               switch(i20hdr[image_num].ic[0]) {
               case 'C': /* C0 C1 C2 C3 C4 */
                   switch(i20hdr[image_num].ic[1]) {
                   case '0': case '1': case '2': case '3': case '4':
                       read_verify(hNITF, i20hdr[image_num].comrat,
                                    4, "Error reading NITF image subheader");
                       break;
                   }
                   break;

               case 'M':  /* M0 M3 M4 */
                   switch(i20hdr[image_num].ic[1]) {
                   case '0': case '3': case '4':
                       read_verify(hNITF, i20hdr[image_num].comrat,
                                    4, "Error reading NITF image subheader");
                       break;
                   }
                   break;
               case 'N':  /* NC */
                   switch(i20hdr[image_num].ic[1]) {
                   case 'C':
                       bNonCompressed = TRUE;
                       break;
                   }
                   break;
               }

               /* Read nbands */
               read_verify(hNITF, i20hdr[image_num].nbands,
                                    1, "Error reading NITF image subheader");

               strncpy(sBuffer, i20hdr[image_num].nbands, 1);
               sBuffer[1] = '\0';

               number_of_bands = atoi(sBuffer);

               if (bNonCompressed) {
                   gOptions.bands = number_of_bands;
               }

               if (number_of_bands > 0) {
                   /* Allocate Space for image band structures */
                   i20hdr[image_num].pBand_data = (image_band_data_type *)
                   malloc(number_of_bands * sizeof(image_band_data_type));
                   if (i20hdr[image_num].pBand_data == NULL) {
                       errmessage("Error allocating memory for \
i20hdr[image_num].band_data");
                       iQuit(1);
                   }

                   /* Read bands */
                   for (x = 0; x < number_of_bands; x++) {

                       /* Read IREPBANDnn..NLUTSnn */
                       read_verify(hNITF,
                                (char *) &i20hdr[image_num].pBand_data[x],
                                (2+6+1+3+1),
                                "Error reading image subheader");

                       /* Get # of look up tables (LUTS) for this band */

                       strncpy(sBuffer,
                                i20hdr[image_num].pBand_data[x].NLUTSnn, 1);
                       sBuffer[1] = '\0';

                       number_of_LUTs = atoi(sBuffer);

                       /* Read LUTs entries per LUT */
                       if (number_of_LUTs > 0) {

                          /* Read # of LUT entries */
                          read_verify(hNITF,
                                i20hdr[image_num].pBand_data[x].NELUTnn, 5,
                                "Error reading image subheader");

                          strncpy(sBuffer,
                                i20hdr[image_num].pBand_data[x].NELUTnn, 5);
                          sBuffer[5] = '\0';

                          number_of_LUT_entries = atoi(sBuffer);


                          /* Allocate space for the LUT entries */

                          i20hdr[image_num].pBand_data[x].LUTDnnm =
(char *) malloc(number_of_LUTs * number_of_LUT_entries);

                          if (i20hdr[image_num].pBand_data[x].LUTDnnm == NULL) {
                              errmessage("Error allocating memory for \
i20hdr[image_num].band_data[x].LUTDnnm");
                              iQuit(1);
                          }

                          /* Read LUT entries */
/*printf("number of LUT entries (NELUTnn)= %d\n", number_of_LUT_entries); */

                          read_verify(hNITF,
                                    i20hdr[image_num].pBand_data[x].LUTDnnm,
                                    number_of_LUTs * number_of_LUT_entries,
                                    "Error reading image subheader");
                       }
                       else
                       {
                           i20hdr[image_num].pBand_data[x].LUTDnnm = NULL;
                       }
                   } /* end for */
               }
               else
               {
                   i20hdr[image_num].pBand_data = NULL;
               }

               /* Read isync..udidl */

               read_verify(hNITF, i20hdr[image_num].isync,
                    (1+1+4*4+2+3+3+10+4+5),
                    "Error reading NITF image subheader");

               strncpy(sBuffer, i20hdr[image_num].udidl, 5);
               sBuffer[5] = '\0';

               user_defined_data_length = atol(sBuffer);

printf("user defined data length = %ld\n",user_defined_data_length);

               if (user_defined_data_length > 0) {
                   user_defined_data_length -= 3;
                   read_verify(hNITF, i20hdr[image_num].udofl, 3,
                                    "Error reading NITF image subheader");

                   i20hdr[image_num].pUdid = (char *)
                            malloc(user_defined_data_length);
                   if (i20hdr[image_num].pUdid == NULL) {
                       errmessage("Error allocating memory for \
i20hdr[image_num].udid\n");
                       iQuit(1);
                   }
                   read_verify(hNITF, i20hdr[image_num].pUdid,
                            user_defined_data_length,
                            "Error reading NITF image subheader");
               }
               /* print_tre_list(image_num); */
               read_verify(hNITF, i20hdr[image_num].ixshdl, 5,
                                    "Error reading NITF image subheader");

               strncpy(sBuffer, i20hdr[image_num].ixshdl, 5);
               sBuffer[5] = '\0';

               extended_subheader_data_length = atol(sBuffer);

/*printf("extended subheader data length = %ld\n", extended_subheader_data_length); */

               if (extended_subheader_data_length >= 3) {
                   extended_subheader_data_length -= 3;
                   read_verify(hNITF, i20hdr[image_num].ixsofl, 3,
                                    "Error reading NITF image subheader");

                   i20hdr[image_num].pIxshd = (char *)
                            malloc(extended_subheader_data_length);
                   if (i20hdr[image_num].pIxshd == NULL) {
                       errmessage("Error allocating memory for \
i20hdr[image_num].pIxshd\n");
                       iQuit(1);
                   }

                   read_verify(hNITF, i20hdr[image_num].pIxshd,
                                    extended_subheader_data_length,
                                    "Error reading NITF image subheader");
               }
               else
               {
                   extended_subheader_data_length = 0;
               }
               strncpy(sBuffer,i20hdr[image_num].ic, 2);
               sBuffer[2] = '\0';
               if (strcmp(sBuffer, "NM") == 0 || strcmp(sBuffer, "M0") == 0
                                || strcmp(sBuffer, "M3") == 0
                                || strcmp(sBuffer, "M4") == 0) {
                   errmessage("Error - Masked images aren't implemented yet.\n");
                   iQuit(1);
               }

               nbpp = get_long(i20hdr[image_num].nbpp, 2);
               image_width = get_long(i20hdr[image_num].ncols, 8);

               if (strcmp(sBuffer, "NC") == 0 && nbpp == 12 && image_width %2 != 0) {
                   set_long(i20hdr[image_num].ncols, image_width + 1, 8);
                   gOptions.added_pad_pixels = 1;
                   printf("\n\nWARNING - increased image width by 1 to make the width even.\n");
               }
               break;

           case V2_1:
               read_verify(hNITF, (char *) &(i21hdr[image_num]),
                    (2+10+14+17+80+1+2+11+2+20+2+8+4+1+8+43+1+40+1+8+15+1+42+8+8+3+8+8+2+1+1), /* read up through field icords */
                    "Error reading image sub-header");

               if (i21hdr[image_num].icords[0] != ' ') {
                   read_verify(hNITF, i21hdr[image_num].igeolo, 60,
                            "Error reading image subheader");

#ifdef REMOVED /* dumping occurs in extract.c */
                   if (gOptions.dump_ssummary) {
                       printf("dump ssummary\n");
                   }
#endif

               }
               else
               {
                   memset(i21hdr[image_num].igeolo, ' ', 60);
                   if (gOptions.dump_ssummary) {
                       printf("Image %d: Empty IGEOLO\n", image_num+1);
                   }
               }

               /* Read nicom */

               read_verify(hNITF, i21hdr[image_num].nicom,
                        1, "Error reading NITF image subheader");
               strncpy(sBuffer, i21hdr[image_num].nicom, 1);
               sBuffer[1] = '\0';

               x = atoi(sBuffer);

               if (x > 0) {
                   /* Allocate Space for image information arrays */
                   i21hdr[image_num].pIcomn = (char *) malloc(x * 80);
                   if (i21hdr[image_num].pIcomn == NULL) {
                       errmessage("Error allocating memory for \
i21hdr[image_num].icomn");
                       iQuit(1);
                   }

                   /*printf("nicom = %d\n", x); */

                   /* Read Image subheader / data lengths */
                   read_verify(hNITF, i21hdr[image_num].pIcomn, x * 80,
                        "Error reading image subheader");
                    /*printf("Assigned comment lines\n"); */
               }
               else
               {
                   i21hdr[image_num].pIcomn = NULL;
               }

               /* Read ic */
               read_verify(hNITF, i21hdr[image_num].ic,
                    2, "Error reading NITF image subheader");

               /* Read comrat if necessary */
               if (strncmp(i21hdr[image_num].ic, "NC", 2) != 0 &&
                            strncmp(i21hdr[image_num].ic, "NM", 2) != 0) {

                   read_verify(hNITF, i21hdr[image_num].comrat,
                                    4, "Error reading NITF image subheader");
               }
               bNonCompressed = FALSE;
               if (strncmp(i21hdr[image_num].ic, "NC", 2) == 0) {
                   bNonCompressed = TRUE;
               }

               /* Read nbands */
               read_verify(hNITF, i21hdr[image_num].nbands,
                    1, "Error reading NITF image subheader");

               strncpy(sBuffer, i21hdr[image_num].nbands, 1);
               sBuffer[1] = '\0';

               number_of_bands = atoi(sBuffer);
               if (number_of_bands == 0) {
                   /* Read xbands */
                   read_verify(hNITF, i21hdr[image_num].xbands,
                                5, "Error reading NITF image subheader");

                   strncpy(sBuffer, i21hdr[image_num].xbands, 5);
                   sBuffer[5] = '\0';

                   number_of_bands = atoi(sBuffer);
               }
               else
               {
                   memset(i21hdr[image_num].xbands, 0, 5);
               }

               if (bNonCompressed) {
                   gOptions.bands = number_of_bands;
               }

               if (number_of_bands > 0) {
                   /* Allocate Space for image band structures */
                   i21hdr[image_num].pBand_data = (image_band_data_type *)
                   malloc(number_of_bands * sizeof(image_band_data_type));
                   if (i21hdr[image_num].pBand_data == NULL) {
                       errmessage("Error allocating memory for \
i21hdr[image_num].band_data");
                       iQuit(1);
                   }

                   /* Read bands */
                   for (x = 0; x < number_of_bands; x++) {

                       /* Read IREPBANDnn..NLUTSnn */
                       read_verify(hNITF,
                                (char *) &i21hdr[image_num].pBand_data[x],
                                (2+6+1+3+1),
                                "Error reading image subheader");

                       /* Get # of look up tables (LUTS) for this band */

                       strncpy(sBuffer,
                                i21hdr[image_num].pBand_data[x].NLUTSnn, 1);
                       sBuffer[1] = '\0';

                       number_of_LUTs = atoi(sBuffer);

                       /* Read LUTs entries per LUT */
                       if (number_of_LUTs > 0) {

                          /* Read # of LUT entries */
                          read_verify(hNITF,
                                i21hdr[image_num].pBand_data[x].NELUTnn, 5,
                                "Error reading image subheader");

                          strncpy(sBuffer,
                                i21hdr[image_num].pBand_data[x].NELUTnn, 5);
                          sBuffer[5] = '\0';

                          number_of_LUT_entries = atoi(sBuffer);


                          /* Allocate space for the LUT entries */

                          i21hdr[image_num].pBand_data[x].LUTDnnm =
(char *) malloc(number_of_LUTs * number_of_LUT_entries);

                          if (i21hdr[image_num].pBand_data[x].LUTDnnm == NULL) {
                              errmessage("Error allocating memory for \
i21hdr[image_num].band_data[x].LUTDnnm");
                              iQuit(1);
                          }

                          /* Read LUT entries */
/*printf("number of LUT entries (NELUTnn)= %d\n", number_of_LUT_entries); */

                          read_verify(hNITF,
                                    i21hdr[image_num].pBand_data[x].LUTDnnm,
                                    number_of_LUTs * number_of_LUT_entries,
                                    "Error reading image subheader");
                       }
                       else
                       {
                           i21hdr[image_num].pBand_data[x].LUTDnnm = NULL;
                       }
                   } /* end for (band_num = 0... */
               }
               else
               {
                   i21hdr[image_num].pBand_data = NULL;
               }

               /* Read isync..udidl */

               read_verify(hNITF, i21hdr[image_num].isync,
                    (1+1+4*4+2+3+3+10+4+5),
                    "Error reading NITF image subheader");

               strncpy(sBuffer, i21hdr[image_num].udidl, 5);
               sBuffer[5] = '\0';

               user_defined_data_length = atol(sBuffer);

/*printf("user defined data length = %ld\n",user_defined_data_length); */

               if (user_defined_data_length > 0) {
                   user_defined_data_length -= 3;
                   read_verify(hNITF, i21hdr[image_num].udofl, 3,
                        "Error reading NITF image subheader");

                   i21hdr[image_num].pUdid = (char *)
                            malloc(user_defined_data_length);
                   if (i21hdr[image_num].pUdid == NULL) {
                       errmessage("Error allocating memory for \
i21hdr[image_num].udid\n");
                       iQuit(1);
                   }
                   read_verify(hNITF, i21hdr[image_num].pUdid,
                        user_defined_data_length,
                        "Error reading NITF image subheader");
               }
               /* print_tre_list(image_num); */
               read_verify(hNITF, i21hdr[image_num].ixshdl, 5,
                    "Error reading NITF image subheader");

               strncpy(sBuffer, i21hdr[image_num].ixshdl, 5);
               sBuffer[5] = '\0';

               extended_subheader_data_length = atol(sBuffer);

/*printf("extended subheader data length = %ld\n", extended_subheader_data_length); */

               if (extended_subheader_data_length >= 3) {
                   extended_subheader_data_length -= 3;
                   read_verify(hNITF, i21hdr[image_num].ixsofl, 3,
                        "Error reading NITF image subheader");

                   i21hdr[image_num].pIxshd = (char *)
                            malloc(extended_subheader_data_length);
                   if (i21hdr[image_num].pIxshd == NULL) {
                       errmessage("Error allocating memory for \
i21hdr[image_num].pIxshd\n");
                       iQuit(1);
                   }

                   read_verify(hNITF, i21hdr[image_num].pIxshd,
                        extended_subheader_data_length,
                        "Error reading NITF image subheader");
               }
               else
               {
                   extended_subheader_data_length = 0;
               }

               strncpy(sBuffer,i21hdr[image_num].ic, 2);
               sBuffer[2] = '\0';
               if (strcmp(sBuffer, "NM") == 0 || strcmp(sBuffer, "M1") == 0
                                || strcmp(sBuffer, "M3") == 0
                                || strcmp(sBuffer, "M4") == 0
                                || strcmp(sBuffer, "M5") == 0) {
                   errmessage("Error: Masked images aren't implemented yet.\n");
                   iQuit(1);
               }

               nbpp = get_long(i21hdr[image_num].nbpp, 2);
               image_width = get_long(i21hdr[image_num].ncols, 8);

               if (strcmp(sBuffer, "NC") == 0 && nbpp == 12 && image_width %2 != 0) {
                   set_long(i21hdr[image_num].ncols, image_width + 1, 8);
                   gOptions.added_pad_pixels = 1;
                   printf("\n\nWARNING - increased image width by 1 to make the width even.\n");
               }

               break;

           default:
               errmessage("Error - unknown NITF format (shouldn't get here!\n");
               iQuit(1);
               break;
        }
/*
printf("image subheader header length = %ld\n\n",
                        image_info[image_num].length_of_subheader);
*/
/*rc = lseek(hNITF, 0, SEEK_CUR);*/
/*printf("Ending pos of image subheader = %ld, Size of image subheader read = \
%ld\n", rc, rc - header_start);
*/

/*printf("Size of image = %ld\n",image_info[image_num].length_of_data); */

        image_info[image_num].nitf_file_offset = lseek(hNITF, 0, SEEK_CUR);

        /* Advance past image data */
        rc = lseek(hNITF, image_info[image_num].length_of_data, SEEK_CUR);
		rc = rc;

        /* Copy image to temporary file */

    /*
        sprintf(temp_filename, "temp_img%d", image_num);
        temp_fh = open_write_clear_ufile(temp_filename);
        copy_fh_2_fh(temp_fh, hNITF, image_info[image_num].length_of_data);
        close(temp_fh);
    */

    }  /* end for (image_num = 0... */
    return 0;
}



/*******************************************************************************
 * Name:  dump_image_information
 *
 * Description: Dumps image information so user can more easily use the image
 *              data.
 *
 * Parameters:
 * Returns:
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void dump_image_information(int number_of_images)
{
    int image_num;
    char ImageInfoFilename[1024];
    FILE *fp;
    char sBuffer[2048];

    for (image_num = 0; image_num < number_of_images; image_num++) {
        if (iNITF_version == V2_0) {
            sprintf(ImageInfoFilename, "%simage_info%d.txt", sOutputPath, image_num);
            fp = fopen(ImageInfoFilename, "w");
            if (fp == NULL) {
                sprintf(sBuffer, "Error opening image information file %s\n", ImageInfoFilename);
                errmessage(sBuffer);
                iQuit(1);
            }

            /* Classification Check & Warning Message */
            if (toupper(i20hdr[image_num].isclass[0]) != 'U' ||
                    toupper(nitf_20_main.fsclas[0]) != 'U') {
                fprintf(fp, "!!WARNING!! This data is restricted or classified.  Please check the\n");
                fprintf(fp, "!!WARNING!! ISCLAS field and take the proper marking and handling precautions.\n\n\n");
            }

            fprintf(fp, "Most Important Image Attribute Fields:\n\n");
            fDumpStr(fp, "ic", i20hdr[image_num].ic, 2);

            fDumpStr(fp, "pvtype (pixel value type)", i20hdr[image_num].pvtype, 3);
            fDumpStr(fp, "nbpp (# bits per pixel)", i20hdr[image_num].nbpp, 2);
            fDumpStr(fp, "abpp (actual # bpp)", i20hdr[image_num].abpp, 2);
            fDumpStr(fp, "pjust (pixel justification)", i20hdr[image_num].pjust, 1);

            fDumpStr(fp, "irep", i20hdr[image_num].irep, 8);
            fDumpStr(fp, "icat", i20hdr[image_num].icat, 8);

            fDumpStr(fp, "nbands", i20hdr[image_num].nbands, 1);
            fprintf(fp, "imode = '%c'", i20hdr[image_num].imode[0]);
            switch(i20hdr[image_num].imode[0]) {
            case 'B': fprintf(fp, " (Band Interleaved by block)\n");
                break;
            case 'P': fprintf(fp, " (Band Interleaved by pixel)\n");
                break;
            case 'R': fprintf(fp, " (Band Interleaved by row)\n");
                break;
            case 'S': fprintf(fp, " (Band Sequential)\n");
                break;
            }

            fDumpStr(fp, "nrows", i20hdr[image_num].nrows, 8);
            fDumpStr(fp, "ncols", i20hdr[image_num].ncols, 8);

            fprintf(fp, "\nNote: extract unblocks data unless you are notified otherwise.\n\n");

            fDumpStr(fp, "nbpr (# blocks per row)", i20hdr[image_num].nbpr, 4);
            fDumpStr(fp, "nbpc (# blocks per column)", i20hdr[image_num].nbpc, 4);
            fDumpStr(fp, "nppbh (#pixels per block horizontal)", i20hdr[image_num].nppbh, 4);
            fDumpStr(fp, "nppbv (#pixels per block vertical)", i20hdr[image_num].nppbv, 4);

            fprintf(fp, "\n\nImportant NITF 2.0 header fields:\n\n");
            fDumpStr(fp, "ostaid", nitf_20_main.ostaid, 10);
            fDumpStr(fp, "FDT", nitf_20_main.fdt, 14);
            fDumpStr(fp, "ftitle", nitf_20_main.ftitle, 80);
            fDumpStr(fp, "fsclas", nitf_20_main.fsclas, 1);
            fDumpStr(fp, "oname", nitf_20_main.oname, 27);
            fDumpStr(fp, "ophone", nitf_20_main.ophone, 18);

            fprintf(fp, "\n\nDump of entire image subheader:\n\n");
            fDumpStr(fp, "im", i20hdr[image_num].im, 2);
            fDumpStr(fp, "iid", i20hdr[image_num].iid, 10);
            fDumpStr(fp, "idatim", i20hdr[image_num].idatim, 14);
            fDumpStr(fp, "tgtid", i20hdr[image_num].tgtid, 17);
            fDumpStr(fp, "ititle", i20hdr[image_num].ititle, 80);
            fDumpStr(fp, "isclass", i20hdr[image_num].isclass, 1);
            fDumpStr(fp, "iscode", i20hdr[image_num].iscode, 40);
            fDumpStr(fp, "isctlh", i20hdr[image_num].isctlh, 40);
            fDumpStr(fp, "isrel", i20hdr[image_num].isrel, 40);
            fDumpStr(fp, "iscaut", i20hdr[image_num].iscaut, 20);
            fDumpStr(fp, "isctln", i20hdr[image_num].isctln, 20);
            fDumpStr(fp, "isdwng", i20hdr[image_num].isdwng, 6);
            fDumpStr(fp, "isdevt", i20hdr[image_num].isdevt, 40);
            fDumpStr(fp, "encryp", i20hdr[image_num].encryp, 1);
            fDumpStr(fp, "isorce", i20hdr[image_num].isorce, 42);
            fDumpStr(fp, "nrows", i20hdr[image_num].nrows, 8);
            fDumpStr(fp, "ncols", i20hdr[image_num].ncols, 8);
            fDumpStr(fp, "pvtype", i20hdr[image_num].pvtype, 3);
            fDumpStr(fp, "irep", i20hdr[image_num].irep, 8);
            fDumpStr(fp, "icat", i20hdr[image_num].icat, 8);
            fDumpStr(fp, "abpp", i20hdr[image_num].abpp, 2);
            fDumpStr(fp, "pjust", i20hdr[image_num].pjust, 1);
            fDumpStr(fp, "icords", i20hdr[image_num].icords, 1);
            fDumpStr(fp, "igeolo", i20hdr[image_num].igeolo, 60);
            fDumpStr(fp, "nicom", i20hdr[image_num].nicom, 1);
            strncpy(sBuffer,i20hdr[image_num].nicom, 1);
            sBuffer[1] = '\0';
            fDumpStr(fp, "icomn", i20hdr[image_num].pIcomn, atoi(sBuffer) * 80);
            fDumpStr(fp, "ic", i20hdr[image_num].ic, 2);
            fDumpStr(fp, "comrat", i20hdr[image_num].comrat, 4);
            fDumpStr(fp, "nbands", i20hdr[image_num].nbands, 1);
            fDumpStr(fp, "isync", i20hdr[image_num].isync, 1);
            fDumpStr(fp, "imode", i20hdr[image_num].imode, 1);
            fDumpStr(fp, "nbpr", i20hdr[image_num].nbpr, 4);
            fDumpStr(fp, "nbpc", i20hdr[image_num].nbpc, 4);
            fDumpStr(fp, "nppbh", i20hdr[image_num].nppbh, 4);
            fDumpStr(fp, "nppbv", i20hdr[image_num].nppbv, 4);
            fDumpStr(fp, "nbpp", i20hdr[image_num].nbpp, 2);
            fDumpStr(fp, "idlvl", i20hdr[image_num].idlvl, 3);
            fDumpStr(fp, "ialvl", i20hdr[image_num].ialvl, 3);
            fDumpStr(fp, "iloc", i20hdr[image_num].iloc, 10);
            fDumpStr(fp, "imag", i20hdr[image_num].imag, 4);
            fDumpStr(fp, "udidl", i20hdr[image_num].udidl, 5);
            fDumpStr(fp, "udofl", i20hdr[image_num].udofl, 3);
            fDumpStr(fp, "ixshdl", i20hdr[image_num].ixshdl, 5);
            fDumpStr(fp, "ixsofl", i20hdr[image_num].ixsofl, 3);

            /* Classification Check & Warning Message */
            if (toupper(i20hdr[image_num].isclass[0]) != 'U') {
                fprintf(fp, "!!WARNING!! This data is restricted or classified.  Please check the\n");
                fprintf(fp, "!!WARNING!! ISCLAS field and take the proper marking and handling precautions.\n\n\n");
            }
            fclose(fp);
        }
        else if (iNITF_version == V2_1) {
            sprintf(ImageInfoFilename, "%simage_info%d.txt", sOutputPath, image_num);
            fp = fopen(ImageInfoFilename, "w");
            if (fp == NULL) {
                sprintf(sBuffer, "Error opening image information file %s\n", ImageInfoFilename);
                errmessage(sBuffer);
                iQuit(1);
            }

            /* Classification Check & Warning Message */
            if (toupper(i21hdr[image_num].isclass[0]) != 'U' ||
                    toupper(nitf_21_main.fsclas[0]) != 'U') {
                fprintf(fp, "!!WARNING!! This data is restricted or classified.  Please check the\n");
                fprintf(fp, "!!WARNING!! ISCLAS field and take the proper marking and handling precautions.\n\n\n");
            }

            fprintf(fp, "Most Important Image Attribute Fields:\n\n");
            fDumpStr(fp, "ic", i21hdr[image_num].ic, 2);

            fDumpStr(fp, "pvtype (pixel value type)", i21hdr[image_num].pvtype, 3);
            fDumpStr(fp, "nbpp (# bits per pixel)", i21hdr[image_num].nbpp, 2);
            fDumpStr(fp, "abpp (actual # bpp)", i21hdr[image_num].abpp, 2);
            fDumpStr(fp, "pjust (pixel justification)", i21hdr[image_num].pjust, 1);

            fDumpStr(fp, "irep", i21hdr[image_num].irep, 8);
            fDumpStr(fp, "icat", i21hdr[image_num].icat, 8);

            fDumpStr(fp, "nbands", i21hdr[image_num].nbands, 1);
            fDumpStr(fp, "xbands", i21hdr[image_num].xbands, 5);
            fprintf(fp, "imode = '%c'", i21hdr[image_num].imode[0]);
            switch(i21hdr[image_num].imode[0]) {
            case 'B': fprintf(fp, " (Band Interleaved by block)\n");
                break;
            case 'P': fprintf(fp, " (Band Interleaved by pixel)\n");
                break;
            case 'R': fprintf(fp, " (Band Interleaved by row)\n");
                break;
            case 'S': fprintf(fp, " (Band Sequential)\n");
                break;
            }

            fDumpStr(fp, "nrows", i21hdr[image_num].nrows, 8);
            fDumpStr(fp, "ncols", i21hdr[image_num].ncols, 8);

            fprintf(fp, "\nNote: extract unblocks data unless you are notified otherwise.\n\n");

            fDumpStr(fp, "nbpr (# blocks per row)", i21hdr[image_num].nbpr, 4);
            fDumpStr(fp, "nbpc (# blocks per column)", i21hdr[image_num].nbpc, 4);
            fDumpStr(fp, "nppbh (#pixels per block horizontal)", i21hdr[image_num].nppbh, 4);
            fDumpStr(fp, "nppbv (#pixels per block vertical)", i21hdr[image_num].nppbv, 4);

            fprintf(fp, "\n\nImportant NITF 2.1 header fields:\n\n");
            fDumpStr(fp, "ostaid", nitf_21_main.ostaid, 10);
            fDumpStr(fp, "FDT", nitf_21_main.fdt, 14);
            fDumpStr(fp, "ftitle", nitf_21_main.ftitle, 80);
            fDumpStr(fp, "fsclas", nitf_21_main.fsclas, 1);
            fDumpStr(fp, "oname", nitf_21_main.oname, 24);
            fDumpStr(fp, "ophone", nitf_21_main.ophone, 18);

            fprintf(fp, "\n\nDump of entire image subheader:\n\n");
            fDumpStr(fp, "im", i21hdr[image_num].im, 2);
            fDumpStr(fp, "iid1", i21hdr[image_num].iid1, 10);
            fDumpStr(fp, "idatim", i21hdr[image_num].idatim, 14);
            fDumpStr(fp, "tgtid", i21hdr[image_num].tgtid, 17);
            fDumpStr(fp, "iid2", i21hdr[image_num].iid2, 80);
            fDumpStr(fp, "isclas", i21hdr[image_num].isclass, 1);
            fDumpStr(fp, "isclsy", i21hdr[image_num].isclsy, 2);
            fDumpStr(fp, "iscode", i21hdr[image_num].iscode, 11);
            fDumpStr(fp, "isctlh", i21hdr[image_num].isctlh, 2);
            fDumpStr(fp, "isrel", i21hdr[image_num].isrel, 20);
            fDumpStr(fp, "isdctp", i21hdr[image_num].isdctp, 2);
            fDumpStr(fp, "isdcdt", i21hdr[image_num].isdcdt, 8);
            fDumpStr(fp, "isdcxm", i21hdr[image_num].isdcxm, 4);
            fDumpStr(fp, "isdg", i21hdr[image_num].isdg, 1);
            fDumpStr(fp, "isdgdt", i21hdr[image_num].isdgdt, 8);
            fDumpStr(fp, "iscltx", i21hdr[image_num].iscltx, 43);
            fDumpStr(fp, "iscatp", i21hdr[image_num].iscatp, 1);

            fDumpStr(fp, "iscaut", i21hdr[image_num].iscaut, 40);

            fDumpStr(fp, "iscrsn", i21hdr[image_num].iscrsn, 1);
            fDumpStr(fp, "issrdt", i21hdr[image_num].issrdt, 8);

            fDumpStr(fp, "isctln", i21hdr[image_num].isctln, 15);
            fDumpStr(fp, "encryp", i21hdr[image_num].encryp, 1);
            fDumpStr(fp, "isorce", i21hdr[image_num].isorce, 42);
            fDumpStr(fp, "nrows", i21hdr[image_num].nrows, 8);
            fDumpStr(fp, "ncols", i21hdr[image_num].ncols, 8);
            fDumpStr(fp, "pvtype", i21hdr[image_num].pvtype, 3);
            fDumpStr(fp, "irep", i21hdr[image_num].irep, 8);
            fDumpStr(fp, "icat", i21hdr[image_num].icat, 8);
            fDumpStr(fp, "abpp", i21hdr[image_num].abpp, 2);
            fDumpStr(fp, "pjust", i21hdr[image_num].pjust, 1);
            fDumpStr(fp, "icords", i21hdr[image_num].icords, 1);
            fDumpStr(fp, "igeolo", i21hdr[image_num].igeolo, 60);
            fDumpStr(fp, "nicom", i21hdr[image_num].nicom, 1);
            strncpy(sBuffer,i21hdr[image_num].nicom, 1);
            sBuffer[1] = '\0';
            fDumpStr(fp, "icomn", i21hdr[image_num].pIcomn, atoi(sBuffer) * 80);
            fDumpStr(fp, "ic", i21hdr[image_num].ic, 2);
            fDumpStr(fp, "comrat", i21hdr[image_num].comrat, 4);
            fDumpStr(fp, "nbands", i21hdr[image_num].nbands, 1);
            fDumpStr(fp, "xbands", i21hdr[image_num].xbands, 5);
            fDumpStr(fp, "isync", i21hdr[image_num].isync, 1);
            fDumpStr(fp, "imode", i21hdr[image_num].imode, 1);
            fDumpStr(fp, "nbpr", i21hdr[image_num].nbpr, 4);
            fDumpStr(fp, "nbpc", i21hdr[image_num].nbpc, 4);
            fDumpStr(fp, "nppbh", i21hdr[image_num].nppbh, 4);
            fDumpStr(fp, "nppbv", i21hdr[image_num].nppbv, 4);
            fDumpStr(fp, "nbpp", i21hdr[image_num].nbpp, 2);
            fDumpStr(fp, "idlvl", i21hdr[image_num].idlvl, 3);
            fDumpStr(fp, "ialvl", i21hdr[image_num].ialvl, 3);
            fDumpStr(fp, "iloc", i21hdr[image_num].iloc, 10);
            fDumpStr(fp, "imag", i21hdr[image_num].imag, 4);
            fDumpStr(fp, "udidl", i21hdr[image_num].udidl, 5);
            fDumpStr(fp, "udofl", i21hdr[image_num].udofl, 3);
            fDumpStr(fp, "ixshdl", i21hdr[image_num].ixshdl, 5);
            fDumpStr(fp, "ixsofl", i21hdr[image_num].ixsofl, 3);

            /* Classification Check & Warning Message */
            if (toupper(i21hdr[image_num].isclass[0]) != 'U') {
                fprintf(fp, "!!WARNING!! This data is restricted or classified.  Please check the\n");
                fprintf(fp, "!!WARNING!! ISCLAS field and take the proper marking and handling precautions.\n\n\n");
            }
            fclose(fp);
        }
    }
}


