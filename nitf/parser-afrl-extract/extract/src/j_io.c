/* j_io.c -- jim's I/O functions ****************************************/


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
* Software developed at AFRL/SNAS for the DDB Project
*
* Program Author:       Jim Stadler
* Creation Date:        CREATION DATE
* Version:          VERSION NUMBER
*
* File Description:
*       Functions in this file are input/output convenience functions.
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/



#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#ifndef PC
#include <unistd.h>  /* for read/write on Sun C++ */
#else
#include <io.h>
#include <sys\stat.h>
#endif

#include "defines.h"

#ifdef USE_MD5
#include "global.h"
#include "md5.h"
#endif

/* externally defined variables */
//extern int errno;
#include <errno.h>

int iDummy;

/* function prototypes */

int get_line(FILE *f, char *s, int len);
int uget_line(int hf, char *s, int len);

/****************************************************************************/
/*                                                                          */
/*     read_nonblank_line() - read until a non-blank line is read or EOF.   */
/*  FALSE is returned when EOF is found.                                    */
/*                                                                          */
/****************************************************************************/

int read_nonblank_line(FILE *pInputFile, char *pLine, int iMaxLen)
{
    while (get_line(pInputFile, pLine, iMaxLen))
    {
        remove_trailing_spaces(pLine);
        if (strlen(pLine) > 0)
        {
            return (TRUE);
        }
    }
    return(FALSE);
}

/****************************************************************************/
/*                                                                          */
/*  uread_nonblank_line() - read until a non-blank line is read or EOF.     */
/*  FALSE is returned when EOF is found.  Uses unbuffered I/O.              */
/*                                                                          */
/****************************************************************************/

int uread_nonblank_line(int hInputFile, char *pLine, int iMaxLen)
{
    while (uget_line(hInputFile, pLine, iMaxLen))
    {
        remove_trailing_spaces(pLine);
        if (strlen(pLine) > 0)
        {
            return (TRUE);
        }
    }
    return(FALSE);
}

/************************************************************************/
/*                                                                      */
/*     read_line() - discard characters up to a newline.  This function */
/*  is like the PASCAL readln; command.                                 */
/*                                                                      */
/************************************************************************/

int read_line(FILE *f)
{
    char c;

    while (1) {
        c=(char) fgetc(f);
        switch (c) {
          case '\n':
          case '\r':
              return(TRUE);

          case EOF :
              return(FALSE);
        }
    }
    return (0);         /* dummy return */
}

/************************************************************************/
/*                                                                      */
/*    uread_line() - discard characters up to a newline.  This function */
/*  is like the PASCAL readln; command.  This function does unbuffered  */
/*  I/O.                                                                */
/*                                                                      */
/************************************************************************/

int uread_line(int hf)
{
    char c;
    int rc;

    while (1) {
        rc = read(hf, &c, 1);
        if (rc == 0)
            return (FALSE);

        if (rc == -1)
        {
            printf("uread_line: unknown error reading file\n");
            return (FALSE);
        }

        switch (c) {
          case '\n':
          case '\r':
              return(TRUE);
        }
    }
    return (0);         /* dummy return */
}


/************************************************************************/
/*                                                                      */
/*     get_line() - get a line from a stream, stripping off EOL         */
/*                  characters.                                         */
/*                                                                      */
/*  Parameters:                                                         */
/*      f               file pointer -- input stream                    */
/*      s               string that holds line read                     */
/*      len             size of string in characters.  String will      */
/*                      always be null terminated on return of TRUE     */
/*                                                                      */
/*  Return codes:                                                       */
/*      TRUE            successful -- string read                       */
/*      FALSE           either error reading file, or EOF               */
/*                                                                      */
/************************************************************************/

int get_line(FILE *f, char *s, int len)
{
    char *newline_position1;
    char *newline_position2;
    char *pcRC;                    /* hold return code */

    pcRC = fgets(s, len, f);
    if (pcRC == NULL)
    {
       return(FALSE);  /* no characters read and EOF */
    }

    /* if s contains a newline, remove it. */
    newline_position1 = strchr(s, '\n');
    newline_position2 = strchr(s, '\r');
    if (newline_position1 != NULL) {
        newline_position1[0] = '\0';    /* strip it */
    }
    if (newline_position2 != NULL) {
        newline_position2[0] = '\0';    /* strip it */
    }

    if (newline_position1 == NULL && newline_position2 == NULL) {
        /* newline not found in len characters read, so read and */
        /* discard to end of line or EOF, whichever comes first */
        iDummy = read_line(f);
    }
    return(TRUE);
}

/************************************************************************/
/*                                                                      */
/*    uget_line() - get a line from a file.                             */
/*                                                                      */
/*  Parameters:                                                         */
/*      hf              file handle -- input file                       */
/*      s               string that holds line read                     */
/*      len             size of string in characters.  String will      */
/*                      always be null terminated on return of TRUE     */
/*                                                                      */
/*  Return codes:                                                       */
/*      TRUE            successful -- string read                       */
/*      FALSE           either error reading file, or EOF               */
/*                                                                      */
/************************************************************************/

int uget_line(int hf, char *s, int len)
{
/*    char *newline_position; */
    char *current;
    int rc;

    current = s;
    while (1) {
        rc = read(hf, current, 1);
        if (rc == 0) /* EOF */
        {
            if (s == current) /* EOF */
            return (FALSE);
            else
            {
            current[0] = '\0';      /* NULL-terminate string */
            return (TRUE);
            }
        }
        if (rc == -1)
        {
            printf("uget_line -- unknown error reading file\n");
            return (FALSE);
        }
        switch(current[0]) {
          case '\n':
          case '\r':
              current[0] = '\0';      /* NULL-terminate string */
              /* newline not found in len characters read, so read and */
              /* discard to end of line or EOF, whichever comes first */
              /*iDummy = uread_line(hf); */
              return(TRUE);
        }
        current++;
    }
    return (TRUE);      /* dummy return to fool the compiler */
}


/*******************************************************************************
 * Name:  get_file_length
 *
 * Description:  Gets and returns the length of the file passed
 *
 * Parameters:
 *      filename
 * Returns:
 *      length of filename
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

long get_file_length(char *filename)
{
    long rc;
    int  fh;    /* file handle */
    char msg[100];

#ifdef PC
    rc = open(filename, O_RDONLY | O_BINARY);
#else
    rc = open(filename, O_RDONLY);
#endif
    if (rc < 0) {
        sprintf(msg, "get_file_length: Error opening input file %s\n", filename);
        errmessage(msg);
        iQuit(1);
    }

    fh = rc;

    rc = lseek(fh, 0, SEEK_END);
    if (rc < 0) {
          errmessage("Error getting file length\n");
          iQuit(1);
    }
    close(fh);
    return rc;
}


/*******************************************************************************
 * Name:    read_text_file_to_cr_lf
 *
 * Description:     This function reads a text file terminated with CRs or LFs
 *                  or both.  It re-terminates EOLs with <CR><LF>, and
 *                  terminates the file with a NULL (not included in length).
 *
 *                  If the file is empty, it puts a space in it.
 *
 * Parameters:
 *      fp      file pointer
 *      p       pointer to memory where the text file will be stored.  This space
 *              must be large enough to store extra EOL chars if the
 *              source file only has one per line.
 *
 * Returns:
 *      length of data read, not counting the NULL
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 * jls     98-11-19   If file is empty, put a space in it.
 ******************************************************************************/

long read_text_file_to_cr_lf(FILE *fp, char *p)
{
    char sBuffer1[1000];
    char *s;

    s = p;

    while (get_line(fp, sBuffer1, 999)) {
        strcpy(s, sBuffer1);
        s += strlen(sBuffer1);
        s[0] = '\x0D'; /* CR */
        s[1] = '\x0A'; /* LF */
        s += 2;
    }
    if (s == p) {
        s[0] = ' ';
        s++;
    }
    s[0] = '\0';
//    s++;  /* don't want NULL to be put in output when it comes time to write. */
    return s - p;
}



/*******************************************************************************
 * Name:  copy_fh_2_fh
 *
 * Description:   This function writes the contents of a file to the filehandle
 *                passed.  A file handle is used as the input file parameter
 *                so it can be positioned at the beginning of data if necessary.
 *                A maximum length parameter is also included in case the end
 *                of data comes before the end of the file.
 *
 * Parameters:
 *      fh_out      file handle
 *      fh_in       file handle
 *      len         max # bytes to read.  Send a -1 for entire file.
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

void copy_fh_2_fh(int fh_out, int fh_in, long len /*, bool bMD5*/)
{
#define COPY_BUFFER_SIZE (4096*16)
    long rc;
    char sBuffer1[COPY_BUFFER_SIZE];
    char msg[256];
    long to_read = len;
    long read_count;
    long write_count;
                                /*2,147,483,647 */
    if (to_read < 0) to_read = 2147483647;

#ifdef REMOVED
    MD5_CTX context;
    unsigned char digest[16]; /* MD5 */

    if (bMD5) {
         errmessage("Begin reading file / MD5'ing it, writing file\n");
         MD5Init (&context);
    }
    else errmessage("Begin reading/writing file\n");
#endif

    while(to_read > 0) {
/*printf("[");fflush(stdout); */
        read_count = COPY_BUFFER_SIZE;
        if (to_read < read_count) read_count = to_read;

        rc = read(fh_in, sBuffer1, read_count);
        if (rc < 0) {
            errmessage("copy_fh_2_fh: error reading from input file");
            iQuit(1);
        }

/*        if (bMD5) MD5Update (&context, sBuffer1, rc); */

/*printf("r]");fflush(stdout); */
        write_count = COPY_BUFFER_SIZE;
        if (rc < COPY_BUFFER_SIZE) write_count = rc;
        if (rc == 0) break;  /* Done!, EOF */

/*printf("[");fflush(stdout); */
        rc = write(fh_out, sBuffer1, write_count);
        if (rc < 0) {
            errmessage("copy_fh_2_fh: Error writing to output file\n");
            iQuit(1);
        }
/*printf("w]");fflush(stdout); */
        if (rc != write_count) {
            sprintf(msg, "copy_fh_2_fh: Error writing to output file: write \
didn't write the full %ld bytes (wrote %ld).\n", write_count, rc);
            errmessage(msg);
            iQuit(1);
        }
        to_read -= read_count;
    }


/*
    if (bMD5) {
        MD5Final (digest, &context);
        MD5Print (digest);
        printf ("\n");
        errmessage("Done reading/MD5'ing/writing file\n");
    }
    else errmessage("Done reading/writing file\n");
*/

}



/*******************************************************************************
 * Name:  copy_fh_2_fh2
 *
 * Description:   This function writes the contents of a file to the filehandle
 *                passed.  A file handle is used as the input file parameter
 *                so it can be positioned at the beginning of data if necessary.
 *                A maximum length parameter is also included in case the end
 *                of data comes before the end of the file.
 *
 * Parameters:
 *      fh_out      file handle
 *      fh_in       file handle
 *      len         max # bytes to read.  Send a -1 for entire file.
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
#ifdef QPM
void copy_fh_2_fh2(int fh_out, int fh_in, long len, int flag)
{
#define COPY_BUFFER_SIZE (4096*16)
    long rc;
    char sBuffer1[COPY_BUFFER_SIZE];
    char msg[256];
    long to_read = len;
    long read_count;
    long write_count;
    //float *f;

                                /*2,147,483,647 */
    if (to_read < 0) to_read = 2147483647;

#ifdef REMOVED
    MD5_CTX context;
    unsigned char digest[16]; /* MD5 */

    if (bMD5) {
         errmessage("Begin reading file / MD5'ing it, writing file\n");
         MD5Init (&context);
    }
    else errmessage("Begin reading/writing file\n");
#endif

    while(to_read > 0) {
/*printf("[");fflush(stdout); */
        read_count = COPY_BUFFER_SIZE;
        if (to_read < read_count) read_count = to_read;

        if (flag == QPM && to_read % 4) {
            printf("copy_fh_2_fh2: Error - buffer size is not a multiple of 4 (and QPM mode is on)\n");
            iQuit(1);
        }

        rc = read(fh_in, sBuffer1, read_count);
        if (rc < 0) {
            errmessage("copy_fh_2_fh2: error reading from input file");
            iQuit(1);
        }

        if (flag == QPM) {
            qpm_data((float*) sBuffer1, read_count/4);
        }

/*        if (bMD5) MD5Update (&context, sBuffer1, rc); */

/*printf("r]");fflush(stdout); */
        write_count = COPY_BUFFER_SIZE;
        if (rc < COPY_BUFFER_SIZE) write_count = rc;
        if (rc == 0) break;  /* Done!, EOF */

/*printf("[");fflush(stdout); */
        rc = write(fh_out, sBuffer1, write_count);
        if (rc < 0) {
            errmessage("copy_fh_2_fh2: Error writing to output file\n");
            iQuit(1);
        }
/*printf("w]");fflush(stdout); */
        if (rc != write_count) {
            sprintf(msg, "copy_fh_2_fh2: Error writing to output file: write \
didn't write the full %ld bytes (wrote %ld).\n", write_count, rc);
            errmessage(msg);
            iQuit(1);
        }
        to_read -= read_count;
    }


/*
    if (bMD5) {
        MD5Final (digest, &context);
        MD5Print (digest);
        printf ("\n");
        errmessage("Done reading/MD5'ing/writing file\n");
    }
    else errmessage("Done reading/writing file\n");
*/

}
#endif


/*******************************************************************************
 * Name:  MD5
 *
 * Description:   This function calculates the MD5 checksum for the file passed.
 *
 * Parameters:
 *      filename    file to MD5
 *      output      MD5 checksum characters
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
#ifdef USE_MD5

void MD5(char *filename, char *output)
{
#define COPY_BUFFER_SIZE (4096*16)
    long rc;
    char sBuffer1[COPY_BUFFER_SIZE];
    //char msg[256];
    long to_read = 2147483647;   /*2,147,483,647 */
    long read_count;
    //long write_count;
    MD5_CTX context;
    unsigned char digest[16]; /* MD5 */
    int fh;
    char *s;
    int i;

    fh = open_read_ufile(filename);

    MD5Init (&context);

    while(to_read > 0) {
        read_count = COPY_BUFFER_SIZE;
        if (to_read < read_count) read_count = to_read;

        rc = read(fh, sBuffer1, read_count);
        if (rc < 0) {
            errmessage("MD5: error reading file\n");
            iQuit(1);
        }

        MD5Update (&context, (unsigned char *) sBuffer1, rc);

        to_read -= read_count;
    }

    MD5Final (digest, &context);

    s = output;
    for (i = 0; i < 16; i++) {
        sprintf (s, "%02x", digest[i]);
        s += 2;
    }
    s[0] = '\0';
}



/*******************************************************************************
 * Name:  md5_subfile
 *
 * Description:   This function reads the contents of the subfile passed in, and
 *                MD5's it.
 *
 * Parameters:
 *      fh_in       file handle
 *      len         max # bytes to read.  Send a -1 for entire file.
 *      sResult     The MD5 result, at least 33 characters long
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

void md5_subfile(int fh_in, long file_start_offset, long subfile_len, char *sResult)
{
#define COPY_BUFFER_SIZE (4096*16)
    long rc;
    char sBuffer1[COPY_BUFFER_SIZE];
    //char msg[256];
    long to_read = subfile_len;
    long read_count;
    char *s;
    long orig_pos;
    int i;
    
    MD5_CTX context;
    unsigned char digest[16]; /* MD5 */


    orig_pos = lseek(fh_in, 0, 0);
    rc = lseek(fh_in, file_start_offset, 0);    /* position input file */
    if (rc < 0) {
        printf("md5_subfile: error seeking to file offset %ld\n", file_start_offset);
        iQuit(1);
    }

    printf("Begin reading / MD5'ing file\n");
    MD5Init (&context);


    while(to_read > 0) {
/*printf("[");fflush(stdout); */
        read_count = COPY_BUFFER_SIZE;
        if (to_read < read_count) read_count = to_read;

        rc = read(fh_in, sBuffer1, read_count);
        if (rc == 0) break;  /* Done!, EOF */
        if (rc < 0) {
            errmessage("copy_fh_2_fh: error reading from input file");
            iQuit(1);
        }

        MD5Update (&context, (unsigned char *) sBuffer1, rc);

/*printf("r]");fflush(stdout); */

        to_read -= read_count;
    }

    lseek(fh_in, orig_pos, 0);   /* position input file */

    MD5Final (digest, &context);

/*
    MD5Print (digest);
    printf ("\n\n");
    printf("Done reading/MD5'ing file\n");
*/

    /* Copy results */
    s = sResult;
    for (i = 0; i < 16; i++) {
        sprintf (s, "%02x", digest[i]);
        s += 2;
    }
    s[0] = '\0';

}
#endif /* USE_MD5 */


/*******************************************************************************
 * Name:  copy_fh_2_fh_byteswap
 *
 * Description:   This function writes the contents of a file to the filehandle
 *                passed.  The contents are byte-swapped in the process.
 *                      A file handle is used as the input file parameter
 *                so it can be positioned at the beginning of data if necessary.
 *                A maximum length parameter is also included in case the end
 *                of data comes before the end of the file.
 *
 * Parameters:
 *      fh_out      file handle
 *      fh_in       file handle
 *      len         max # bytes to read.  Send a 0 for EOF.  (NOT IMPLEMENTED)
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

void copy_fh_2_fh_byteswap(int fh_out, int fh_in, long len)
{
    long rc;
    int  x;
    char sBuffer1[4096];
    char msg[256];
    long write_count;
    char cTemp;
    struct swap {
        char a;
        char b;
        } *pSwap;

    while(1) {
        rc = read(fh_in, sBuffer1, 4096);
        if (rc < 0) {
            errmessage("copy_fh_2_fh_byteswap: error reading from input file");
            iQuit(1);
        }
        write_count = 4096;
        if (rc < 4096) write_count = rc;
        if (rc == 0) break;  /* Done! */

        /* Byte-swap */
        pSwap = (struct swap *) sBuffer1;
        for (x = 0; x < write_count/2; x++) {
            cTemp = pSwap->a;
            pSwap->a = pSwap->b;
            pSwap->b = cTemp;
            pSwap++;
        }

        rc = write(fh_out, sBuffer1, write_count);
        if (rc < 0) {
            errmessage("copy_fh_2_fh_byteswap: Error writing to output file\n");
            iQuit(1);
        }
        if (rc != write_count) {
            sprintf(msg, "copy_fh_2_fh_byteswap: Error writing to output file: write \
didn't write the full %ld bytes (wrote %ld).\n", write_count, rc);
            errmessage(msg);
            iQuit(1);
        }
    }
}



/*******************************************************************************
 * Name:  copy_fh_2_fh_sint16_to_int8
 *
 * Description:   This function writes the contents of a file to the filehandle
 *                passed.  The contents are byte-swapped in the process if
 *                necessary, and converted to 8 bit int numbers before being
 *                written out.
 *                      A file handle is used as the input file parameter
 *                so it can be positioned at the beginning of data if necessary.
 *                A maximum length parameter is also included in case the end
 *                of data comes before the end of the file.
 *
 * Parameters:
 *      fh_out      file handle
 *      fh_in       file handle
 *      len         max # bytes to read.  Send a 0 for EOF.  (NOT IMPLEMENTED)
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

void copy_fh_2_fh_sint16_to_int8(int fh_out, int fh_in, int byteswap, long len, float multiplier, int statistics)
{
    long rc;
    int  x;
    char sBuffer1[4096];
    unsigned char sBuffer2[2048];
    char msg[256];
    long write_count;
    char cTemp;
    int val;
    int min_val = 0;
    int max_val = 0;
    long count = 0;
    long neg_count = 0;
    long max_count = 0;

    short int *sip;

    struct swap {
        char a;
        char b;
        } *pSwap;

    while(1) {
        rc = read(fh_in, sBuffer1, 4096);
        if (rc < 0) {
            errmessage("copy_fh_2_fh_sint16_to_int8: error reading from input file");
            iQuit(1);
        }
        write_count = 2048;
        if (rc < 4096) write_count = rc/2;
        if (write_count % 2 != 0) {
            printf("copy_fh_2_fh_sint16_to_int8: WARNING: Input file size is odd.\n");
        }

        if (rc == 0) break;  /* Done! */

        pSwap = (struct swap *) sBuffer1;
        for (x = 0; x < write_count; x++) {
            if (byteswap) {

                /* Byte-swap */
                cTemp = pSwap->a;
                pSwap->a = pSwap->b;
                pSwap->b = cTemp;
                pSwap++;
            }

            sip = (short int *) &sBuffer1[x*2];
            val = *sip;
            count++;

            /* map negative value */
            if (val < 0) {
                if (val < -200) {
/*                    printf("copy_fh_2_fh_sint16_to_int8: val is %d\n",val); */
                }
                neg_count++;
                if (val < min_val) min_val = val;
                val = 0;
            }
            else  /* map positive value */
            {
                val = (int) (val * multiplier); /* map 12 bits to 8 */
                if (val >= 256) {
                    max_count++;
                    if (val > max_val) max_val = val;
/*                    printf("copy_fh_2_fh_sint16_to_int8: val is %d (should be 0..255)\n",val); */
                    val = 255;
                }
            }

            /* scale to 8 bits */
            sBuffer2[x] = (unsigned char) val;
        }

        rc = write(fh_out, sBuffer2, write_count);
        if (rc < 0) {
            errmessage("copy_fh_2_fh_sint16_to_int8: Error writing to output file\n");
            iQuit(1);
        }
        if (rc != write_count) {
            sprintf(msg, "copy_fh_2_fh_sint16_to_int8: Error writing to output file: write \
didn't write the full %ld bytes (wrote %ld).\n", write_count, rc);
            errmessage(msg);
            iQuit(1);
        }
    }
    if (statistics) {
        printf("copy_fh_2_fh_sint16_to_int8: minimum value found (before conversion) is %d, negative values are %f%% of the values\n",min_val, 100.0 * ((float)neg_count)/count);
        printf("copy_fh_2_fh_sint16_to_int8: max value found (after conversion) is %d, clipped values are %f%% of the values\n",max_val, 100.0 * ((float)max_count)/count);
    }
}




/*******************************************************************************
 * Name:  read_verify
 *
 * Description: Reads data from the file handle, writes to destination pointer.
 *
 * Parameters:
 *  fh              file handle
 *  destination     location to store data read
 *  sErrorMessage   Error message to print if an error occurs
 *
 * Returns:
 *   # of bytes read from the file
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

long read_verify(int fh, char *destination, long length, const char *sErrorMessage)
{
    long rc;
    long start;
    long file_len;
    static char sTemp[150];

    rc = read(fh, destination, length);
    if (rc == -1) {
        start = lseek(fh, 0, SEEK_CUR);
        file_len = lseek(fh, 0, SEEK_END);
        sprintf(sTemp, "Error reading, read returned %ld. (start = %ld, \
read length = %ld, file_length = %ld\n%s\n",
                    rc, start, length, file_len, sErrorMessage);
        errmessage(sTemp);
        iQuit(1);
    }
    else if (rc != length) {
        start = lseek(fh, 0, SEEK_CUR) - rc;
        file_len = lseek(fh, 0, SEEK_END);
        sprintf(sTemp, "Error reading, read returned %ld. (start = %ld, \
read length = %ld, file_length = %ld\n%s\n",
                    rc, start, length, file_len, sErrorMessage);
        errmessage(sTemp);
        printf("errno=%d\n", errno);
        iQuit(1);
    }
    return rc;
}


/*******************************************************************************
 * Name:  write_verify
 *
 * Description: Writes data to the file handle.
 *
 * Parameters:
 *  fh              file handle
 *  source          data to write
 *  length          # bytes to write
 *  sErrorMessage   Error message to print if an error occurs
 *
 * Returns:
 *   # of bytes written to the file
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

long write_verify(int fh, char *source, long length, const char *sErrorMessage)
{
    long rc;
    static char sTemp[150];

    rc = write(fh, (void *) source, length);
    if (rc != length) {
        sprintf(sTemp, "Error writing, write returned %ld.\n%s\n",
rc, sErrorMessage);
        errmessage(sTemp);
        iQuit(1);
    }

    return length;
}



/*******************************************************************************
 * Name:    open_read_ufile
 *
 * Description:     This function opens an unbuffered file for reading, and does
 *                  it on Suns and PCs.
 *
 * Parameters:
 *      filename    Filename to open
 *
 * Returns:
 *      file handle
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int open_read_ufile(char *filename)
{
    char msg[255];
    int rc;

#ifdef PC
    rc = open(filename, O_RDONLY | O_BINARY);
#else
    rc = open(filename, O_RDONLY);
#endif
    if (rc < 0) {
        sprintf(msg, "open_read_ufile: Error opening input file %s\n", filename);
        errmessage(msg);
        iQuit(1);
    }
    return rc;
}


/*******************************************************************************
 * Name:    open_read_file
 *
 * Description:     This function opens a buffered file for reading, does
 *                  it on Suns and PCs, and returns a file pointer.
 *
 * Parameters:
 *      filename    Filename to open
 *
 * Returns:
 *      file handle
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

FILE *open_read_file(char *filename)
{
    char msg[255];
    //int rc;
    FILE *fp;

    fp = fopen(filename, "rb");
    if (fp == NULL) {
        sprintf(msg, "open_read_file: Error opening input file %s\n", filename);
        errmessage(msg);
        iQuit(1);
    }
    return fp;
}


/*******************************************************************************
 * Name:    open_write_file
 *
 * Description:     This function opens a buffered file for writing, does
 *                  it on Suns and PCs, and returns a file pointer.
 *
 * Parameters:
 *      filename    Filename to open
 *
 * Returns:
 *      file handle
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

FILE *open_write_file(char *filename)
{
    char msg[255];
    //int rc;
    FILE *fp;

    fp = fopen(filename, "wb");
    if (fp == NULL) {
        sprintf(msg, "open_write_file: Error opening output file '%s'\n", filename);
        errmessage(msg);
        iQuit(1);
    }
    return fp;
}



/*******************************************************************************
 * Name:    open_write_append
 *
 * Description:     This function opens a buffered file for writing (append
 *                  mode), does it on Suns and PCs, and returns a file pointer.
 *
 * Parameters:
 *      filename    Filename to open
 *
 * Returns:
 *      file handle
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

FILE *open_write_append(char *filename)
{
    char msg[255];
    //int rc;
    FILE *fp;

    fp = fopen(filename, "ab");
    if (fp == NULL) {
        sprintf(msg, "open_write_append: Error opening output file %s\n",
                        filename);
        errmessage(msg);
        iQuit(1);
    }
    return fp;
}



/*******************************************************************************
 * Name:    open_write_clear_ufile
 *
 * Description:     This function opens an unbuffered file for writing, and does
 *                  it on Suns and PCs.  If the file exists, it is cleared.
 *
 * Parameters:
 *      filename    Filename to open
 *
 * Returns:
 *      file handle
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int open_write_clear_ufile(char *filename)
{
    char msg[255];
    int rc;

#ifdef PC
    rc = open(filename, O_RDWR | O_CREAT | O_TRUNC | O_BINARY, S_IWRITE);
#else
    rc = open(filename, O_WRONLY | O_CREAT | O_TRUNC, 0660);
#endif
    if (rc < 0) {
        sprintf(msg, "open_write_clear_ufile: Error opening output file %s\n", filename);
        errmessage(msg);
        iQuit(1);
    }
    return rc;
}



/*******************************************************************************
 * Name:    open_write_append_ufile
 *
 * Description:     This function opens an unbuffered file for appending, and does
 *                  it on Suns and PCs.
 *
 * Parameters:
 *      filename    Filename to open
 *
 * Returns:
 *      file handle
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int open_write_append_ufile(char *filename)
{
    char msg[255];
    int rc;

#ifdef PC
    rc = open(filename, O_WRONLY | O_CREAT | O_APPEND | O_BINARY, S_IWRITE);
#else
    rc = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0660);
#endif
    if (rc < 0) {
        sprintf(msg, "open_write_append_ufile: Error opening output file %s\n", filename);
        errmessage(msg);
        iQuit(1);
    }
    return rc;
}



/*******************************************************************************
 * Name:  write_pad_bytes
 *
 * Description:   This function writes image pad bytes to the
 *                filehandle passed.  This function is used for filling out
 *                image blocks where the image doesn't completely fill an image
 *                block.
 *
 * Parameters:
 *      fh        file handle.  Must be opened for writing & positioned in file
 *      len       # pad bytes to write.
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

void write_pad_bytes(int fh, long len)
{
    long rc;
    char sBuffer1[4096*16];
    char msg[256];
    long write_count;
    long len_remaining;

    len_remaining = len;

    memset(sBuffer1, 0, 4096*16);   /* initialize memory */
    while(len_remaining > 0) {
        if (len_remaining >= 4096*16)
            write_count = 4096*16;
        else
            write_count = len_remaining;

        rc = write(fh, sBuffer1, write_count);
        if (rc < 0) {
            errmessage("write_pad_bytes: Error writing to output file\n");
            iQuit(1);
        }
        if (rc != write_count) {
            sprintf(msg, "write_pad_bytes: Error writing to output file: write \
didn't write the full %ld bytes (wrote %ld).\n", write_count, rc);
            errmessage(msg);
            iQuit(1);
        }
        len_remaining -= write_count;
    }
}


/*******************************************************************************
 * Name:        write_screen_lines
 *
 * Description: This function prints lines to the screen, and returns the #
 *              of lines printed so far.  Once the maximum number of lines is
 *              reached, it prompts the user for
 *
 *              "-<More, press Enter to continue>-",
 *
 *              then continues.
 *              It is used for "more" like functionality, so text doesn't scroll
 *              off the screen.
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

int write_screen_lines(int bReset, int max_lines, const char *sLine)
{
    char sBuffer[20];
    char *s;
    static int line_counter = 0;

    s = (char*)sLine;
    if (bReset) line_counter = 0;
    while ((s = strchr(s, '\n'))) {
        s++;
        line_counter++;
    }
    printf("%s", sLine);
    if (line_counter >= max_lines-1) {
        line_counter = 0;
        printf("-<More, press Enter to continue>-");
        get_line(stdin, sBuffer, 10);
    }

    return line_counter;
}


/*******************************************************************************
 * Name:    write_byte
 *
 * Description: This function writes an unsigned character to the file ptr
 *              passed in, and does the error checking.
 *
 * Parameters:
 *      fp      Output file pointer
 *      c       Unsigned character to write
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void write_byte(FILE *fp, unsigned char c)
{
    long rc;

    rc = fwrite(&c, 1, 1, fp);
    if (rc < 1) {
        fprintf(stderr, "Error writing to file\n");
        iQuit(0);
    }
}



/*******************************************************************************
 * Name:    read_byte
 *
 * Description: This function reads an unsigned character from the file ptr
 *              passed in, does the error checking, and returns the unsigned
 *              char.
 *
 * Parameters:
 *      fp      Input file pointer
 *
 * Returns:
 *      The unsigned character read from the file
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

unsigned char read_byte(FILE *fp)
{
    long rc;
    char c;

    rc = fread(&c, 1, 1, fp);
    if (rc < 1) {
        fprintf(stderr, "Error: unable to read input file\n");
        iQuit(1);
    }
    return (unsigned char) c;
}



/*******************************************************************************
 * Name:        copy_file_PFM
 *
 * Description: (PFM = Preprocess For (using) Markers)
 *              This function copies the file to the destination, adding a
 *              space and the requested number of marker characters (initially
 *              spaces) to the end of each line in the file.
 *
 * Parameters:
 *
 * Returns:
 *      0   success
 *      -1  failure
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int copy_file_PFM(char *in_filename, char *out_filename, int number_marker_chars, int bAppend)
{
    FILE *fIn;
    FILE *fOut;
    long rc;
    char sBuffer[201];
    char sBuffer1[201];
    char sMarkers[101];
    fIn = fopen(in_filename, "rb");
    if (fIn == NULL) {
        sprintf(sBuffer, "copy_file_PFM: Error opening input file '%s'\n", in_filename);
        errmessage(sBuffer);
        iQuit(-1);
    }

    if (bAppend) {
        fOut = fopen(out_filename, "ab");
    }
    else
    {
        fOut = fopen(out_filename, "wb");
    }
    if (fOut == NULL) {
        sprintf(sBuffer, "copy_file_PFM: Error opening output file '%s'\n", out_filename);
        errmessage(sBuffer);
        iQuit(-1);
    }

    memset(sMarkers, ' ', number_marker_chars);
    sMarkers[number_marker_chars] = '\0';

    while ((rc = get_line(fIn, sBuffer1, 200))) {
        trim(sBuffer1);
        if (strlen(sBuffer1) > 0) {
            fprintf(fOut, "%s %s\n", sBuffer1, sMarkers);
        }
    }

    fclose(fIn);
    fclose(fOut);
    return 0;
}



/*******************************************************************************
 * Name:        mark_line
 *
 * Description: This function places a marker at the end of the line just read
 *              from the open file pointer passed.  The marker is placed at
 *              the position passed.
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

int mark_line(FILE *fp, char marker, int marker_num, int number_of_markers)
{
    int position_to_mark;
    int rc;

    position_to_mark = marker_num - number_of_markers - 2;

    rc = fseek(fp,position_to_mark,1);
    rc = fprintf(fp, "%c", marker);

    position_to_mark = -position_to_mark - 1;

    rc = fseek(fp,position_to_mark,1);
	rc = rc;
    return 0;
}



/*******************************************************************************
 * Name:        print_unmarked_lines
 *
 * Description: This function prints all unmarked lines
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

void report_unmarked_lines(char *in_filename, char *report_filename, char marker, int marker_position, int number_of_markers)
{
    FILE *fp;
    FILE *fp2;
    int marker_offset;
    char *s;
    char sBuffer1[1001];
    char sBuffer[1001];
    long rc;

    marker_offset = marker_position - number_of_markers - 1;
    fp = fopen(in_filename, "rb");
    if (fp == NULL) {
        sprintf(sBuffer, "report_unmarked_lines: Error opening file %s\n", in_filename);
        errmessage(sBuffer);
        iQuit(-1);
    }

    fp2 = fopen(report_filename, "wb");
    if (fp2 == NULL) {
        sprintf(sBuffer, "report_unmarked_lines: Error opening file %s\n", report_filename);
        errmessage(sBuffer);
        iQuit(-1);
    }

    while ((rc = get_line(fp, sBuffer1, 1000))) {
        s = strchr(sBuffer1, 0);
        if (s[marker_offset] != marker) {
            fprintf(fp2, "%s\n", sBuffer1);
        }
    }
    fclose(fp);
    fclose(fp2);
}



/*******************************************************************************
 * Name:        FloatNative2BigEndian
 *
 * Description: This function converts a native float to a big-endian float.
 *
 !!!  WARNING  !!!  WARNING  !!!  WARNING  !!!  WARNING  !!! WARNING  !!!
 !
 !     This function is not very smart - it converts it if PC is
 !     defined, and leaves it alone if not.
 !
 !!!  WARNING  !!!  WARNING  !!!  WARNING  !!!  WARNING  !!! WARNING  !!!
 *
 * Parameters:
 *      f1      input float #
 *      f2      output float #
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void FloatNative2BigEndian(float *f1, float *f2)
{
#ifdef PC
    char *s1, *s2;

    /* set up pointers */
    s1 = (char *) f1;
    s2 = (char *) f2;

    /* Re-arrange bytes */
    s2[0] = s1[3];
    s2[1] = s1[2];
    s2[2] = s1[1];
    s2[3] = s1[0];

#else
    *f2 = *f1;
#endif
}



/*******************************************************************************
 * Name:        file_exists
 *
 * Description: This function determines whether a file exists
 *
 * Parameters:
 *
 * Returns:
 *    FALSE - no
 *    TRUE  - yes
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int file_exists(char *filename)
{
    int fh;

    fh = open(filename, O_RDONLY);
    if (fh > -1) {
        close(fh);
        return TRUE;
    }
    return FALSE;
}


#define DETAB_FILE_MAX_LEN 1000
void detab_file(char *in_file, char *out_file)
{
    FILE *fp_in, *fp_out;
    char sBuffer[DETAB_FILE_MAX_LEN + 1];
    int i;
    int j;
    int spaces;
    int index;
    int length;

    fp_in = open_read_file(in_file);
    fp_out = open_write_file(out_file);

    while (get_line(fp_in, sBuffer, DETAB_FILE_MAX_LEN)) {
        length = strlen(sBuffer);
        index = 0;
        for (i=0; i<length; i++) {
            if (sBuffer[i] != '\t') {
                fputc(sBuffer[i], fp_out);
                index++;
            } else {
                spaces = 8 - (index % 8);
                for (j=0; j<spaces; j++) {
                   fputc(' ', fp_out);
                }
                index += spaces;
            }
        }
        fputc('\n', fp_out);
    }
    fclose(fp_in);
    fclose(fp_out);
}



/*******************************************************************************
 * Name:        local_filename
 *
 * Description: This function fixes a file pathname for the local computer
 *
 * Parameters:
 *
 * Returns:
 *    FALSE - no
 *    TRUE  - yes
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void local_filename(char *filename)
{
    char *s;

    if (filename == NULL) {
        printf("local_filename: Error - NULL pointer passed in.\n");
        iQuit(1);
    }

#ifdef PC
    s = filename;
    while (s[0] != '\0') {
        if (s[0] == '/') s[0] = '\\';
        s++;
    }

#else
    s = filename;
    while (s[0] != '\0') {
        if (s[0] == '\\') s[0] = '/';
        s++;
    }
#endif
}

