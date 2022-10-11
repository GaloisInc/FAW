/* Version  */
/* label.c - read the labels from an NITF file */

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
*       Routines to read label subheaders/data from an NITF file
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
 * Name:  read_label_data
 *
 * Description: read all labels and label data
 *
 * NITF 2.0 ONLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *
 * Parameters:
 *      number_of_labels
 *      label_info         array of structures of label data (hdr, data length)
 * Returns:
 *    >= 0   success, # of labels read
 *    -1     error
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int read_label_data(int number_of_labels, segment_info_type *label_info)
{
    int label_num;
    //long header_start;
    //long rc;
    long extended_subheader_data_length;
    char sBuffer[1000];
    
    if (number_of_labels <= 0) return number_of_labels;

    if (iNITF_version >= V2_1) {
        errmessage("read_label_data: label data is not part of the \
NITF 2.1+ Spec!");
        iQuit(1);
    }
    l20hdr = (nitf_20_labelsub_type *) malloc(
            sizeof(nitf_20_labelsub_type) * number_of_labels);
    if (l20hdr == NULL) {
        errmessage("Error allocating memory");
        iQuit(1);
    }

    for (label_num = 0; label_num < number_of_labels; label_num++) {
        /* printf("label %d header length = %ld\n",
                    label_num, label_info[label_num].length_of_subheader); */
        /*header_start = lseek(hNITF, 0, SEEK_CUR); */
        /*printf("label header starting pos = %ld\n", header_start); */

        /* Read la..lsdwng */
        read_verify(hNITF, (char *) &(l20hdr[label_num]),
                (2+10+1+(40*3)+20+20+6),
                "Error reading labels sub-header");
/*
        dump_str("la", l20hdr[label_num].la, 2);
        dump_str("lid", l20hdr[label_num].lid, 10);
        dump_str("lsclas", l20hdr[label_num].lsclas, 1);
        dump_str("lscode", l20hdr[label_num].lscode, 40);
        dump_str("lsctlh", l20hdr[label_num].lsctlh, 40);
        dump_str("lsrel", l20hdr[label_num].lsrel, 40);
        dump_str("lscaut", l20hdr[label_num].lscaut, 20);
        dump_str("lsctln", l20hdr[label_num].lsctln, 20);
        dump_str("lsdwng", l20hdr[label_num].lsdwng, 6);
*/


        /* If conditional field ssdwng exists, read it, otherwise skip it. */
        if (strncmp(l20hdr[label_num].lsdwng, "999998", 6) == 0) {

            /* Read lsdevt */
            read_verify(hNITF, l20hdr[label_num].lsdevt,
                               40, "Error reading labels subheader");

            /*printf("lsdevt = '%s'\n", l20hdr[label_num].lsdevt); */
        }
        else
        {
            /*printf("skip isdevt\n"); */

            /* Skip fsdevt & read rest of header B */
            memset(l20hdr[label_num].lsdevt, ' ', 40);
        }

        /* Read encryp..lxshdl */

        read_verify(hNITF, l20hdr[label_num].encryp,
                (1+1+2+2+3+3+10+3+3+5),
                "Error reading NITF labels subheader");
/*
        dump_str("lsdevt", l20hdr[label_num].lsdevt, 40);
        dump_str("encryp", l20hdr[label_num].encryp, 1);
        dump_str("lfs", l20hdr[label_num].lfs, 1);
        dump_str("lcw", l20hdr[label_num].lcw, 2);
        dump_str("lch", l20hdr[label_num].lch, 2);
        dump_str("ldlvl", l20hdr[label_num].ldlvl, 3);
        dump_str("lalvl", l20hdr[label_num].lalvl, 3);
        dump_str("lloc", l20hdr[label_num].lloc, 10);
        dump_bytes_decimal("ltc", l20hdr[label_num].ltc, 3);
        dump_bytes_decimal("lbc", l20hdr[label_num].lbc, 3);
        dump_str("lxshdl", l20hdr[label_num].lxshdl, 5);
*/

        strncpy(sBuffer, l20hdr[label_num].lxshdl, 5);
        sBuffer[5] = '\0';

        extended_subheader_data_length = atol(sBuffer);

        if (extended_subheader_data_length >= 3) {
            extended_subheader_data_length -= 3;
            read_verify(hNITF, l20hdr[label_num].lxsofl, 3,
                            "Error reading NITF labels subheader");

            dump_str("lxsofl", l20hdr[label_num].lxsofl, 3);

            l20hdr[label_num].pLxshd = (char *)
                            malloc(extended_subheader_data_length);
            if (l20hdr[label_num].pLxshd == NULL) {
                errmessage("Error allocating memory for \
l20hdr[label_num].lxshd\n");
                iQuit(1);
            }
            read_verify(hNITF, l20hdr[label_num].pLxshd,
                     extended_subheader_data_length,
                     "Error reading NITF labels subheader");
        }
        else
        {
            extended_subheader_data_length = 0;
        }

        /*rc = lseek(hNITF, 0, SEEK_CUR); */
        /*printf("Size of label subheader read = %ld\n", (long) rc - header_start); */

         label_info[label_num].pData = (char *)
                            malloc(label_info[label_num].length_of_data);
         if (label_info[label_num].pData == NULL) {
             errmessage("Error allocating memory for label\n");
             iQuit(1);
         }


        /*printf("Size of label = %ld\n",label_info[label_num].length_of_data); */

        read_verify(hNITF, label_info[label_num].pData,
                    label_info[label_num].length_of_data,
                    "Error reading label data\n");

    } /* end for (label_num = ... */
    return 0;
}
