/* Version  */
/* symbol.c - read the labels from an NITF file */

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
*       Functions to read symbol subheaders / data from NITF file
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
 * Name:  read_symbol_data
 *
 * Description: read all symbols and symbol data
 *
 * NITF 2.0 ONLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 *
 * Parameters:
 *      number_of_symbols
 *      symbol_info         array of structures of symbol data (hdr, data length)
 * Returns:
 *    >= 0   success, # of symbols read
 *    -1     error
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int read_symbol_data(int number_of_symbols, segment_info_type *symbol_info)
{
    int symbol_num;
    //long header_start;
    //long rc;
    long extended_subheader_data_length;
    int nelut;
    int dlut_size;
    char sBuffer[1000];

    if (number_of_symbols <= 0) return number_of_symbols;

    if (iNITF_version >= V2_1) {
        errmessage("read_symbol_data: Symbol data is not part of the \
NITF 2.1+ Spec!");
        iQuit(1);
    }
    s20hdr = (nitf_20_symbolsub_type *) malloc(
            sizeof(nitf_20_symbolsub_type) * number_of_symbols);
    if (s20hdr == NULL) {
        errmessage("Error allocating memory");
        iQuit(1);
    }

    for (symbol_num = 0; symbol_num < number_of_symbols; symbol_num++) {
        /* printf("symbol %d header length = %ld\n",
                    symbol_num, symbol_info[symbol_num].length_of_subheader); */
        /* header_start = lseek(hNITF, 0, SEEK_CUR); */
        /*printf("symbol header starting pos = %ld\n", header_start); */

        /* Read sy..ssdwng */
        read_verify(hNITF, (char *) &(s20hdr[symbol_num]),
                (2+10+20+1+40+40+40+20+20+6),
                "Error reading symbols sub-header");


        /* If conditional field ssdwng exists, read it, otherwise skip it. */
        if (strncmp(s20hdr[symbol_num].ssdwng, "999998", 6) == 0) {

            /* Read ssdevt */
            read_verify(hNITF, s20hdr[symbol_num].ssdevt,
                               40, "Error reading symbols subheader");

            /*printf("ssdevt = '%s'\n", s20hdr[symbol_num].ssdevt); */
        }
        else
        {
            /*printf("skip isdevt\n"); */

            /* Skip fsdevt & read rest of header B */
            memset(s20hdr[symbol_num].ssdevt, ' ', 40);
        }

        /* Read encryp..nelut */

        read_verify(hNITF, s20hdr[symbol_num].encryp,
                (1+1+4+4+4+1+3+3+10+10+1+6+3+3),
                "Error reading NITF symbols subheader");

        strncpy(sBuffer, s20hdr[symbol_num].nelut, 3);
        sBuffer[3] = '\0';
        nelut = atoi(sBuffer);

        if (nelut > 0) {
            /* Allocate Space for look up table */
            if (s20hdr[symbol_num].scolor[0] == 'C') { /* if color look up table */
                dlut_size = nelut * 3;
            }
            else
            {
                dlut_size = nelut;
            }

            s20hdr[symbol_num].pDlut = (char *) malloc(dlut_size);
            if (s20hdr[symbol_num].pDlut == NULL) {
                errmessage("Error allocating memory for \
s20hdr[symbol_num].dlut");
                iQuit(1);
            }

            /* Read dlut data */
            read_verify(hNITF, s20hdr[symbol_num].pDlut, dlut_size,
                        "Error reading NITF symbols subheader");
        }
        else
        {
            s20hdr[symbol_num].pDlut = NULL;
        }


        read_verify(hNITF, s20hdr[symbol_num].sxshdl, 5,
                        "Error reading NITF symbols subheader");

        strncpy(sBuffer, s20hdr[symbol_num].sxshdl, 5);
        sBuffer[5] = '\0';

        extended_subheader_data_length = atol(sBuffer);

        if (extended_subheader_data_length >= 3) {
            extended_subheader_data_length -= 3;
            read_verify(hNITF, s20hdr[symbol_num].sxsofl, 3,
                            "Error reading NITF symbols subheader");

            s20hdr[symbol_num].pSxshd = (char *)
                            malloc(extended_subheader_data_length);
            if (s20hdr[symbol_num].pSxshd == NULL) {
                sprintf(sBuffer, "Error allocating memory for symbol TREs \
s20hdr[%d].pSxshd, length = %ld\n", symbol_num, extended_subheader_data_length);
                errmessage(sBuffer);
                iQuit(1);
            }
            read_verify(hNITF, s20hdr[symbol_num].pSxshd,
                     extended_subheader_data_length,
                     "Error reading NITF symbols subheader");
        }
        else
        {
            extended_subheader_data_length = 0;
        }

        /*rc = lseek(hNITF, 0, SEEK_CUR); */
        /*printf("Size of symbol subheader read = %ld\n", (long) rc - header_start); */

        symbol_info[symbol_num].pData = (char *)
                            malloc(symbol_info[symbol_num].length_of_data);
         if (symbol_info[symbol_num].pData == NULL) {
             errmessage("Error allocating memory for symbol\n");
             iQuit(1);
         }


        /*printf("Size of symbol = %ld\n",symbol_info[symbol_num].length_of_data); */

        read_verify(hNITF, symbol_info[symbol_num].pData,
                    symbol_info[symbol_num].length_of_data,
                    "Error reading symbol data\n");
/*
        dump_str("sy", s20hdr[symbol_num].sy, 2);
        dump_str("sid", s20hdr[symbol_num].sid, 10);
        dump_str("sname", s20hdr[symbol_num].sname, 20);
        dump_str("ssclas", s20hdr[symbol_num].ssclas, 1);
        dump_str("sscode", s20hdr[symbol_num].sscode, 40);
        dump_str("ssctlh", s20hdr[symbol_num].ssctlh, 40);
        dump_str("ssrel", s20hdr[symbol_num].ssrel, 40);
        dump_str("sscaut", s20hdr[symbol_num].sscaut, 20);
        dump_str("ssctln", s20hdr[symbol_num].ssctln, 20);
        dump_str("ssdwng", s20hdr[symbol_num].ssdwng, 6);
        dump_str("ssdevt", s20hdr[symbol_num].ssdevt, 40);
        dump_str("encryp", s20hdr[symbol_num].encryp, 1);
        dump_str("stype", s20hdr[symbol_num].stype, 1);
        dump_str("nlips", s20hdr[symbol_num].nlips, 4);
        dump_str("npixpl", s20hdr[symbol_num].npixpl, 4);
        dump_str("nwdth", s20hdr[symbol_num].nwdth, 4);
        dump_str("nbpp", s20hdr[symbol_num].nbpp, 1);
        dump_str("sdlvl", s20hdr[symbol_num].sdlvl, 3);
        dump_str("salvl", s20hdr[symbol_num].salvl, 3);
        dump_str("sloc", s20hdr[symbol_num].sloc, 10);
        dump_str("sloc2", s20hdr[symbol_num].sloc2, 10);
        dump_str("scolor", s20hdr[symbol_num].scolor, 1);
        dump_str("snum", s20hdr[symbol_num].snum, 6);
        dump_str("srot", s20hdr[symbol_num].srot, 3);
        dump_str("nelut", s20hdr[symbol_num].nelut, 3);
        dump_str("sxshdl", s20hdr[symbol_num].sxshdl, 5);
        dump_str("sxsofl", s20hdr[symbol_num].sxsofl, 3);
*/
    } /* end for (symbol_num = ... */
    return 0;
}
