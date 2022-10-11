/* V */
/* graphic.c - read the graphic segments from an NITF file */

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
*       Routines to read the graphic segments from an NITF file
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
 * Name:  read_graphic_data
 *
 * Description: read all graphic headers and graphic data
 *
 * NITF 2.1+ ONLY !!!!!!!!!!!!!!!!!
 *
 * Parameters:
 *      number_of_graphics    number of graphic files
 *      graphic_info          array of structures of graphic data (hdr, data length)
 *
 * Returns:
 *    >= 0   success, # of graphics read
 *    -1     error
 ******************************************************************************/

/*******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int read_graphic_data(int number_of_graphics, segment_info_type *graphic_info)
{
    int graphic_num;
/*extern nitf_21_graphicsub_type *g21hdr; */
    //long header_start;
    //long rc;
    long size_of_user_defined_fields;
    char sBuffer[1000];
    
    if (number_of_graphics <= 0) return number_of_graphics;

    if (iNITF_version == V2_0) return -1;
    g21hdr = (nitf_21_graphicsub_type *) malloc(
           sizeof(nitf_21_graphicsub_type) * number_of_graphics);
    if (g21hdr == NULL) {
        errmessage("Error allocating memory for graphic header structure array");
        iQuit(1);
    }


    for (graphic_num = 0; graphic_num < number_of_graphics; graphic_num++) {
/*
printf("graphic header length = %ld\n\n",
                        graphic_info[graphic_num].length_of_subheader);
*/
/*header_start = lseek(hNITF, 0, SEEK_CUR); */
/*printf("header starting pos = %ld\n", header_start); */

        /* Read sy..sxshdl */
        read_verify(hNITF, (char *) &(g21hdr[graphic_num]),
             (2+10+20+1+2+11+2+20+2+8+4+1+8+43+1+40+1+8+15+1+1+13+3+3+10+10+1+10+2+5),
             "Error reading graphic sub-header");

/*
        dump_str("sy", g21hdr[graphic_num].sy, 2);
        dump_str("sid", g21hdr[graphic_num].sid, 10);
        dump_str("sname", g21hdr[graphic_num].sname, 20);

        dump_str("ssclas", g21hdr[graphic_num].ssclas, 1);
        dump_str("ssclsy", g21hdr[graphic_num].ssclsy, 2);
*/

        strncpy(sBuffer, g21hdr[graphic_num].sxshdl, 5);
        sBuffer[5] = '\0';
        size_of_user_defined_fields = atol(sBuffer);

        if (size_of_user_defined_fields > 0) {
            size_of_user_defined_fields -= 3;

            g21hdr[graphic_num].pSxshd = (char *)
                         malloc(size_of_user_defined_fields);

            if (g21hdr[graphic_num].sxsofl == NULL) {
                errmessage("Error allocating memory for graphic user-defined fields\n");
                iQuit(1);
            }

            read_verify(hNITF, (char *) &(g21hdr[graphic_num].sxsofl), 3,
                            "Error reading graphic user-defined fields");

            read_verify(hNITF, (char *) &(g21hdr[graphic_num].pSxshd),
                            size_of_user_defined_fields,
                            "Error reading graphic user-defined fields");
        }


/*rc = lseek(hNITF, 0, SEEK_CUR); */
/*printf("Size of graphic subheader read = %ld\n", (long) rc - header_start); */

/*printf("Size of graphic = %ld\n",graphic_info[graphic_num].length_of_data); */

    } /* end for (graphic_num = 0... */
    return 0;
}


