/* V */
/* text.c - functions pertaining to the text segments */

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
*       Functions that read text subheaders / text segments from NITF files
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
 * Name:  read_text_data
 *
 * Description: read all text segment headers and text segment data
 *
 * Parameters:
 *      number_of_texts   number of text files
 *      text_info         array of structures of text data (hdr, data length)
 * Returns:
 *    >= 0   success, # of texts read
 *    -1     error
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int read_text_data(int number_of_texts, segment_info_type *text_info)
{
   int text_num;
/*extern nitf_20_textsub_type *t20hdr; */
/*extern nitf_21_textsub_type *t21hdr; */
    //long header_start;
    //long rc;
    long extended_subheader_data_length;
    char *s;
    long text_bytes_remaining;
    //int bytes_to_print;
    char sBuffer[1000];

    if (number_of_texts <= 0) return number_of_texts;

    switch(iNITF_version) {
     case V2_0:
         t20hdr = (nitf_20_textsub_type *) malloc(
                sizeof(nitf_20_textsub_type) * number_of_texts);
         if (t20hdr == NULL) {
             errmessage("Error allocating memory");
             iQuit(1);
         }
         break;

     case V2_1:
         t21hdr = (nitf_21_textsub_type *) malloc(
                sizeof(nitf_21_textsub_type) * number_of_texts);
         if (t21hdr == NULL) {
             errmessage("Error allocating memory");
             iQuit(1);
         }
         break;
     default:
         errmessage("Error - unknown NITF format (shouldn't get here!\n");
         iQuit(1);
         break;
   }

   for (text_num = 0; text_num < number_of_texts; text_num++) {

/*printf("text header length = %ld\n\n",
                        text_info[text_num].length_of_subheader);
header_start = lseek(hNITF, 0, SEEK_CUR);
*/
/*printf("header starting pos = %ld\n", header_start); */

       switch(iNITF_version) {
           case V2_0:
               /* Read te..tsdwng */
               read_verify(hNITF, (char *) &(t20hdr[text_num]),
                    (2+10+14+80+1+(40*3)+20+20+6),
                    "Error reading text sub-header");
/*
               dump_str("te", t20hdr[text_num].te, 2);
               dump_str("textid", t20hdr[text_num].textid, 10);
               dump_str("txtdt", t20hdr[text_num].txtdt, 14);
               dump_str("txtitl", t20hdr[text_num].txtitl, 80);
               dump_str("tsclas", t20hdr[text_num].tsclas, 1);
               dump_str("tscode", t20hdr[text_num].tscode, 40);
               dump_str("tsctlh", t20hdr[text_num].tsctlh, 40);
               dump_str("tsrel", t20hdr[text_num].tsrel, 40);
               dump_str("tscaut", t20hdr[text_num].tscaut, 20);
               dump_str("tsctln", t20hdr[text_num].tsctln, 20);
               dump_str("tsdwng", t20hdr[text_num].tsdwng, 6);
*/
               /* If conditional field fsdwng exists, read it, otherwise skip it. */
               if (strncmp(t20hdr[text_num].tsdwng, "999998", 6) == 0) {

                   /* Read tsdevt */

                   read_verify(hNITF, t20hdr[text_num].tsdevt,
                        40, "Error reading text subheader");

/*printf("isdevt = '%s'\n", t20hdr[text_num].tsdevt); */
               }
               else
               {
/*printf("skip tsdevt\n"); */

                   /* Skip fsdevt & read rest of header B */

                   memset(t20hdr[text_num].tsdevt, ' ', 40);
               }

               /* Read encryp..txshdl */

               read_verify(hNITF, t20hdr[text_num].encryp,
                    (1+3+5), "Error reading NITF text subheader");
/*
               dump_str("encryp", t20hdr[text_num].encryp, 1);
               dump_str("txtfmt", t20hdr[text_num].txtfmt, 3);
               dump_str("txshdl", t20hdr[text_num].txshdl, 5);
*/
               strncpy(sBuffer, t20hdr[text_num].txshdl, 5);
               sBuffer[5] = '\0';

               extended_subheader_data_length = atol(sBuffer);

               if (extended_subheader_data_length >= 3) {

                   read_verify(hNITF, t20hdr[text_num].txsofl, 3,
                        "Error reading NITF text subheader");

                   dump_str("txsofl", t20hdr[text_num].txsofl, 3);

                   extended_subheader_data_length -= 3;

                   t20hdr[text_num].pTxshd = (char *)
                            malloc(extended_subheader_data_length);

                   if (t20hdr[text_num].pTxshd == NULL) {
                       errmessage("Error allocating memory for \
t20hdr[text_num].pTxshd\n");
                       iQuit(1);
                   }
                   read_verify(hNITF, t20hdr[text_num].pTxshd,
                        extended_subheader_data_length,
                        "Error reading NITF text subheader");
               }
               else
               {
                   extended_subheader_data_length = 0;
               }
               break;

           case V2_1:
               /* Read te..txshdl */
               read_verify(hNITF, (char *) &(t21hdr[text_num]),
                    (2+7+3+14+80+1+2+11+2+20+2+8+4+1+8+43+1+40+1+8+15+1+3+5),
                    "Error reading text sub-header");
#ifdef REMOVED
               dump_str("te", t21hdr[text_num].te, 2);
               dump_str("textid", t21hdr[text_num].textid, 7);
               dump_str("txtalvl", t21hdr[text_num].txtalvl, 3);
               dump_str("txtdt", t21hdr[text_num].txtdt, 14);
               dump_str("txtitl", t21hdr[text_num].txtitl, 80);
               dump_str("tsclas", t21hdr[text_num].tsclas, 1);
               dump_str("tscode", t21hdr[text_num].tscode, 11);
               dump_str("tsctlh", t21hdr[text_num].tsctlh, 2);
               dump_str("tsrel", t21hdr[text_num].tsrel, 20);
               dump_str("tscaut", t21hdr[text_num].tscaut, 40);
               dump_str("tsctln", t21hdr[text_num].tsctln, 15);

               dump_str("encryp", t21hdr[text_num].encryp, 1);
               dump_str("txtfmt", t21hdr[text_num].txtfmt, 3);
               dump_str("txshdl", t21hdr[text_num].txshdl, 5);
#endif
               strncpy(sBuffer, t21hdr[text_num].txshdl, 5);
               sBuffer[5] = '\0';

               extended_subheader_data_length = atol(sBuffer);

               if (extended_subheader_data_length >= 3) {

                   read_verify(hNITF, t21hdr[text_num].txsofl, 3,
                        "Error reading NITF text subheader");

                   dump_str("txsofl", t21hdr[text_num].txsofl, 3);

                   extended_subheader_data_length -= 3;

                   t21hdr[text_num].pTxshd = (char *)
                            malloc(extended_subheader_data_length);

                   if (t21hdr[text_num].pTxshd == NULL) {
                       errmessage("Error allocating memory for \
t21hdr[text_num].pTxshd\n");
                       iQuit(1);
                   }
                   read_verify(hNITF, t21hdr[text_num].pTxshd,
                        extended_subheader_data_length,
                        "Error reading NITF text subheader");
               }
               else
               {
                   extended_subheader_data_length = 0;
               }
               break;

           default:
               errmessage("Error - unknown NITF format (shouldn't get here!\n");
               iQuit(1);
               break;
        }

/*rc = lseek(hNITF, 0, SEEK_CUR); */
/*printf("Size of text subheader read = %ld\n", (long) rc - header_start); */

        text_info[text_num].pData = (char *)
                            malloc(text_info[text_num].length_of_data+1);
        if (text_info[text_num].pData == NULL) {
            errmessage("Error allocating memory\n");
            iQuit(1);
        }
/*printf("Size of text = %ld\n",text_info[text_num].length_of_data); */
        read_verify(hNITF, text_info[text_num].pData,
                    text_info[text_num].length_of_data,
                    "Error reading text data\n");

        text_info[text_num].pData[text_info[text_num].length_of_data] = '\0';

        text_bytes_remaining = text_info[text_num].length_of_data;
        s = text_info[text_num].pData;

/* Print text data
        printf("Text = ");
        while (text_bytes_remaining > 0) {

            if (text_bytes_remaining > 999) {
                bytes_to_print = 999;
            }
            else {
                bytes_to_print = text_bytes_remaining;
            }

            strncpy(sBuffer, s, bytes_to_print);
            sBuffer[bytes_to_print] = '\0';
            printf("%s",sBuffer);
            text_bytes_remaining -= bytes_to_print;
            s += bytes_to_print;
        }
*/
    }  /* end for (text_num = 0... */
	s = s;
	text_bytes_remaining = text_bytes_remaining;
    return 0;
}

