/* eng_read.c - Read an Engineering Data Extension TRE **********************/


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
/****************************************************************************/

/*****************************************************************************
*
*       Software developed at AFRL/SNAS for the DDB & MSET Projects
*
* File Author:       Jim Stadler, Veridian Engineering
* Creation Date:
* Version:
*
* File Description:
*       Functions to read and extract Engineering Data Extension TREs
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/

#include <stdio.h>

#ifndef PC
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#else
#include <string.h>
#include <io.h>
#include <ctype.h>
#endif

#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"

/* defines */
void export_engrd_fp(FILE *dest_fp, char *CurData, char *out_filename);


/*******************************************************************************
 * Name:    export_engrd_filename
 *
 * Description: This function extracts an Engineering Data Extension to a file
 *
 * Parameters:
 *      OutFilename     File to create and use.  A file with the same name will
 *                      be overwritten
 *      CurData         Points to the TRE
 *
 * Returns:
 *      Pointer to array
 ******************************************************************************/

void export_engrd_filename(char *OutFilename, char *CurData)
{
    FILE *fp;

    fp = open_write_file(OutFilename);
    export_engrd_fp(fp, CurData, OutFilename);
    fclose(fp);
}



/*******************************************************************************
 * Name:    export_engrd_fp
 *
 * Description: This function extracts an Engineering Data Extension to an open
 *              file pointer
 *
 * Parameters:
 *      dest_fp         File pointer to write output to, must be open for write
 *      CurData         Points to the TRE
 *      out_filename    Name of the output file for error messages
 *
 * Returns:
 *      Pointer to array
 ******************************************************************************/

typedef struct {
    int  engln;         /* label size in bytes */
    char englbl[100];   /* Engineering data label */
    int  engmtx;        /* Array / Matrix flag. 0 = not array or matrix,
                           > 0 is number lines in array or matrix */
    int  engmtxr;       /* Matrix row # */
    char engtyp[2];     /* data type {I = unsigned int, S = signed int, R =
                           real, C = complex, A = ASCII */
    int  engdts;        /* data element size in bytes */
    char engdatu[10];   /* units */
    int  engdatc;       /* Data count = # elements in this matrix record. */

} engineering_record_type;


void export_engrd_fp(FILE *dest_fp, char *CurData, char *out_filename)
{
    char resrc[21];
    int record_entry_count;
    char *s;
    engineering_record_type rec;
    int loop;
    char *data;
    int  data_size;

    s = CurData;

    get_string(resrc, s, 20);
    s += 20;
    record_entry_count = get_long(s, 3);
    s += 3;

    for (loop = 0; loop < record_entry_count; record_entry_count++) {
        rec.engln = get_long(s, 2);
        s += 2;

        get_string(rec.englbl, s, rec.engln);
        s += rec.engln;

        rec.engmtx = get_long(s,1);
        s += 1;

        rec.engmtxr = get_long(s,1);
        s += 1;

        get_string(rec.engtyp, s, 1);
        s += 1;

        rec.engdts = get_long(s, 1);
        s += 1;

        get_string(rec.engdatu, s, 2);
        s += 2;

        rec.engdatc = get_long(s, 3);
        s += 3;

        /* Read data for row */
        data_size = rec.engdts * rec.engdatc;
        data = malloc(data_size);
        if (data == NULL) {
            printf("export_engrd_fp: Couldn't allocate %d bytes of memory\n", data_size);
            exit(0);
        }
        memcpy(data, s, rec.engdts * rec.engdatc);
        s += data_size;

        /* do something with the data */

        /* clean-up for loop */
        free(data);
    }
}
