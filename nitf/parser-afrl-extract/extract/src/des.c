/* V */
/* des.c - read the data extension segments from an NITF file */

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
*       Functions read the data extension segments from an NITF file
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
 * Name:  read_des_data
 *
 * Description: read all DES headers and DES data
 *
 * Parameters:
 *      number_of_DESs    number of text files
 *      des_info          array of structures of DES data (hdr, data length)
 *
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

int read_DES_data(int number_of_DESs, segment_info_type *DES_info)
{
    int des_num;
/*extern nitf_20_textsub_type *d20hdr; */
/*extern nitf_21_textsub_type *d21hdr; */
    //long header_start;
    //long rc;
    long size_of_des_defined_data;
    char sBuffer[1000];
    
    if (number_of_DESs <= 0) return number_of_DESs;

    switch(iNITF_version) {
     case V2_0:
         d20hdr = (nitf_20_dessub_type *) malloc(
                sizeof(nitf_20_dessub_type) * number_of_DESs);
         if (d20hdr == NULL) {
             errmessage("Error allocating memory for DES header structure array");
             iQuit(1);
         }
         break;

     case V2_1:
         d21hdr = (nitf_21_dessub_type *) malloc(
                sizeof(nitf_21_dessub_type) * number_of_DESs);
         if (d21hdr == NULL) {
             errmessage("Error allocating memory for DES header structure array");
             iQuit(1);
         }
         break;
     default:
         errmessage("Error - unknown NITF format (shouldn't get here!\n");
         iQuit(1);
         break;
   }

   for (des_num = 0; des_num < number_of_DESs; des_num++) {

/*printf("DES header length = %ld\n\n", DES_info[des_num].length_of_subheader); */
/*header_start = lseek(hNITF, 0, SEEK_CUR); */
/*printf("header starting pos = %ld\n", header_start); */

       switch(iNITF_version) {
           case V2_0:
               /* Read de..desdwng */
               read_verify(hNITF, (char *) &(d20hdr[des_num]),
                    (2+25+2+1+(40*3)+20+20+6),
                    "Error reading DES sub-header");

#ifdef REMOVED
               dump_str("de", d20hdr[des_num].de, 2);
               dump_str("destag", d20hdr[des_num].destag, 25);
               dump_str("desver", d20hdr[des_num].desver, 2);

               dump_str("desclas", d20hdr[des_num].desclas, 1);
               dump_str("descode", d20hdr[des_num].descode, 40);
               dump_str("desctlh", d20hdr[des_num].desctlh, 40);
               dump_str("desrel", d20hdr[des_num].desrel, 40);
               dump_str("descaut", d20hdr[des_num].descaut, 20);
               dump_str("desctln", d20hdr[des_num].desctln, 20);
               dump_str("desdwng", d20hdr[des_num].desdwng, 6);
#endif

               if (strncmp(d20hdr[des_num].desdwng, "999998", 6) == 0) {
                   read_verify(hNITF, (char *) &(d20hdr[des_num].desdevt),
                            40, "Error reading DES sub-header");
/*                   dump_str("desdevt", d20hdr[des_num].desdevt, 40); */
               }
               else {
                   memset(d20hdr[des_num].desdevt, ' ', 40);
                }

               if (strncmp(d20hdr[des_num].destag,
                            "Registered Extensions", 21) == 0 ||
                   strncmp(d20hdr[des_num].destag,
                            "Controlled Extensions", 21) == 0) {
                   read_verify(hNITF, (char *) &(d20hdr[des_num].desoflw),
                            6+3, "Error reading DES sub-header");
/*
                   dump_str("desoflw", d20hdr[des_num].desoflw, 6);
                   dump_str("desitem", d20hdr[des_num].desitem, 3);
*/
               }

               read_verify(hNITF, (char *) &(d20hdr[des_num].desshl),
                            4, "Error reading DES sub-header");
/*               dump_str("desshl", d20hdr[des_num].desshl, 4); */

               strncpy(sBuffer, d20hdr[des_num].desshl, 4);
               sBuffer[4] = '\0';
               size_of_des_defined_data = atol(sBuffer);

               if (size_of_des_defined_data > 0) {

                   d20hdr[des_num].pDesshf = (char *)
                                malloc(size_of_des_defined_data);
                   if (d20hdr[des_num].pDesshf == NULL) {
                       errmessage("Error allocating memory for \
DES user-defined fields\n");
                       iQuit(1);
                   }

                   read_verify(hNITF, (char *) &(d20hdr[des_num].pDesshf),
                            size_of_des_defined_data,
                            "Error reading DES user-defined fields");
               }

               break;

           case V2_1:
               /* Read de..desctln */
               read_verify(hNITF, (char *) &(d21hdr[des_num]),
                    (2+25+2+1+2+11+2+20+2+8+4+1+8+43+1+40+1+8+15),
                    "Error reading DES sub-header");
#ifdef REMOVED
               dump_str("de", d21hdr[des_num].de, 2);
               dump_str("desid", d21hdr[des_num].desid, 25);
               dump_str("desver", d21hdr[des_num].desver, 2);

               dump_str("declas", d21hdr[des_num].declas, 1);
               dump_str("descode", d21hdr[des_num].descode, 11);
               dump_str("desctlh", d21hdr[des_num].desctlh, 2);
               dump_str("desrel", d21hdr[des_num].desrel, 20);
               dump_str("descaut", d21hdr[des_num].descaut, 40);
               dump_str("desctln", d21hdr[des_num].desctln, 15);
#endif

               if (strncmp(d21hdr[des_num].desid, "TRE_OVERFLOW", 12) == 0) {
                   read_verify(hNITF, (char *) (d21hdr[des_num].desoflw),
                            6+3, "Error reading DES sub-header");
/*
                   dump_str("desoflw", d21hdr[des_num].desoflw, 6);
                   dump_str("desitem", d21hdr[des_num].desitem, 3);
*/
               }

               read_verify(hNITF, (char *) &(d21hdr[des_num].desshl),
                            4, "Error reading DES sub-header");
/*               dump_str("desshl", d21hdr[des_num].desshl, 4); */

               strncpy(sBuffer, d21hdr[des_num].desshl, 4);
               sBuffer[4] = '\0';
               size_of_des_defined_data = atol(sBuffer);

               if (size_of_des_defined_data > 0) {

                   d21hdr[des_num].pDesdata = (char *)
                                malloc(size_of_des_defined_data);
                   if (d21hdr[des_num].pDesdata == NULL) {
                       errmessage("Error allocating memory for \
DES-defined data fields\n");
                       iQuit(1);
                   }

                   read_verify(hNITF, (char *) &(d21hdr[des_num].pDesdata),
                            size_of_des_defined_data,
                            "Error reading DES user-defined fields");
               }
               break;

           default:
               errmessage("Error - unknown NITF format (shouldn't get here!\n");
               iQuit(1);
               break;
        }

/*rc = lseek(hNITF, 0, SEEK_CUR); */
/*printf("Size of DES subheader read = %ld\n", (long) rc - header_start); */

/*printf("Size of DES = %ld\n",DES_info[des_num].length_of_data); */

    }
    return 0;
}
