/* j_string.c -- jim's string functions **************************************/

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
*       String manipulation functions
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
#endif

#include "defines.h"

/* externally defined variables */
extern int errno;

/************************************************************************/
/*                                                                      */
/*  remove_trailing_spaces -- this function removes any trailing whitespace */
/*  between the last non-whitespace character and the NULL character.       */
/*                                                                      */
/************************************************************************/

void remove_trailing_spaces(char *s)
{
    char *end;

    end = strchr(s, '\0');
    if (end != NULL)
    {
    while (end >= s)
    {
        end--;
        if (!isspace(end[0]))
        {
            break;
        }
    }
    end[1] = '\0';
    }
}

/****************************************************************************

    FUNCTION: index_caseless

    PURPOSE:  return the index of <pattern> in s, -1 if none, in a non-case
          manner.

****************************************************************************/

int index_caseless(char *string_to_search, char *string_to_find)
{
    int i, j, k;
    int last;     /* hold the last starting pattern index to check.         */
          /* This is so we won't bother comparing if pattern length */
          /* is shorter than the string                             */

    /* compute the last index to check */
    last = strlen(string_to_search) - strlen(string_to_find);

    for (i = 0; i <= last; i++) {
        j = i;
        k = 0;
        while ( (string_to_find[k] != '\0') &&
          (toupper(string_to_search[j]) == toupper(string_to_find[k])) )
        {
            j++;
            k++;
        }

        if (string_to_find[k] == '\0') {
            return(i);
        }
    }
    return(-1);
}


/****************************************************************************

    FUNCTION: strncmp_caseless

    PURPOSE:  Case-insensitive version of strncmp()

*****************************************************************************/

int strncmp_caseless (const char *str1, const char *str2, int n)
{
    while (n--)
    if ((int) (toupper(*str1++)) != (int) (toupper(*str2++)))
        return((int) (toupper(*(str1-1))) - (int) (toupper(*(str2-1))));
    return(0);
}



/****************************************************************************

    FUNCTION: strcmp_caseless

    PURPOSE:  Case-insensitive version of strcmp()

*****************************************************************************/

int strcmp_caseless (const char *str1, const char *str2)
{
    while (TRUE) {
        if ((int) (toupper(*str1)) != (int) (toupper(*str2)))
            return((int) (toupper(*str1)) - (int) (toupper(*str2)));
        if (str1[0] == '\0') return 0;
        str1++; str2++;
    }
}



/****************************************************************************

    FUNCTION: strlinecount

    PURPOSE:  count the # of lines in a string.  This function checks for
              newlines, so if you use linefeeds or carriage returns, this
              function may not count properly.

*****************************************************************************/

int strlinecount(char *s)
{
    int lines;
    char *s1;


    lines = 0;

    while (s) {
        s1 = strchr(s, '\n');
        lines++;
        s = s1;
        if (s) s++; /* advance past the newline */
    }
    return lines;
}



/****************************************************************************

    FUNCTION: strsetlower

    PURPOSE:  This function turns all characters to lower case.

*****************************************************************************/

int strsetlower(char *s)
{
    while (s[0] != '\0') {
        s[0] = (char) tolower(s[0]);
        s++;
    }
    return 0;
}



/****************************************************************************

    FUNCTION: strsetupper

    PURPOSE:  This function turns all characters to upper case.

*****************************************************************************/

int strsetupper(char *s)
{
    while (s[0] != '\0') {
        s[0] = (char) toupper(s[0]);
        s++;
    }
    return 0;
}



/*******************************************************************************
 * Name:  find_first_char
 *
 * Description: This function searches through a string until it finds a
 *              non whitespace character, or until the EOL is reached.
 *
 * Parameters:
 *      s  string to search
 * Returns:
 *    ptr to char    success.
 *    NULL           EOL Found.
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

char *find_first_char(char *s)
{
    while (s[0] != '\0' && isspace(s[0])) s++;
    if (s[0] == '\0') return NULL;
    return s;
}



/*******************************************************************************
 * Name:  check_numeric_string
 *
 * Description: This function searches through a string, making sure:
 *              1) There's at least one numeric character
 *              2) There are no non-numeric characters
 *
 * Parameters:
 *      s       string to examine
 *
 * Returns:
 *     -1       Failure
 *      0       Success
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int check_numeric_string(char *s)
{
    while (*s != '\0') {
        if (!isdigit(*s++)) return -1;
    }

    return 0;
}



/*******************************************************************************
 * Name:  trim
 *
 * Description: This function searches through a string and removes leading and
 *              trailing space characters.
 *
 * Parameters:
 *      s       string to examine
 *
 * Returns:
 *     -1       Failure
 *      0       Success
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void trim(char *s)
{
    char *s1;
    s1 = find_first_char(s);
    if (s1) {
        remove_trailing_spaces(s1);
        if (s1 != s) {
            /* Copy string to remove starting spaces */
            while (s1[0] != '\0') {
                *s++ = *s1++;
            }
            *s = *s1;
        }
    }
    else
    {
        s[0] = '\0';
    }
}



/*******************************************************************************
 * Name: parse_phoenix_line
 *
 * Description: This function parses a phoenix type line (variable = value),
 *              and sets variable and value.
 *
 * Parameters:
 *      line        Input line
 *      variable    text left of =.  Pass ptr to a string of sufficient length.
 *      value       text right of =.  Pass ptr to a string of sufficient length.
 *
 * Returns:
 *      0       Success
 *      -1      Error: no text right of =
 *      -2      Error: no text left of =
 *      -3      Error: no Equal sign found
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int parse_phoenix_line(char *line, char *variable, char *value)
{
    char *p_equal;
    char *p;
    int variable_len;

    p_equal = strchr(line, '=');
    if (p_equal == NULL) return -3;

    variable_len = p_equal - line;
    strncpy(variable, line, variable_len);
    variable[variable_len] = '\0';  /* Null-terminate string */

    p = p_equal+1;

    while (p[0] != '\0' && isspace(p[0])) p++;

    strcpy(value, p);
    remove_trailing_spaces(variable);
    remove_trailing_spaces(value);
    if (strlen(variable) <= 0) return -2;
    if (strlen(value) <= 0) return -1;
    return 0;
}



/*******************************************************************************
 * Name: parse_phoenix_line3
 *
 * Description: This function parses a phoenix type line
 *              (variable = value [units]),
 *              and sets variable, value, units.
 *
 * Parameters:
 *      line        Input line
 *      variable    text left of =.  Pass ptr to a string of sufficient length.
 *      value       text right of =.  Pass ptr to a string of sufficient length.
 *      units       Units.  Defined as <string> <w> = <w> <string> <w> <units>
 *                  where <w> = whitespace (1 or more blanks)
 *
 * Returns:
 *      0       Success
 *      -1      Error: no units
 *      -2      Error: no text right of =
 *      -3      Error: no text left of =
 *      -4      Error: no Equal sign found
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int parse_phoenix_line3(char *line, char *variable, char *value, char *units)
{
    char *p_equal;
    char *p;
    int variable_len;
    char *u;
    char *s;

    variable[0] = '\0';
    value[0] = '\0';
    units[0] = '\0';

    p_equal = strchr(line, '=');
    if (p_equal == NULL) return -4;

    variable_len = p_equal - line;
    strncpy(variable, line, variable_len);
    variable[variable_len] = '\0';  /* Null-terminate string */

    p = p_equal+1;

    while (p[0] != '\0' && isspace(p[0])) p++;
    u = strchr(p, ' ');
    if (u) {
        s = u;
        while (u[0] != '\0' && isspace(u[0])) u++;
        strcpy(units, u);

        s[0] = '\0';    /* Null-terminate for end of value */
    }

    strcpy(value, p);
    remove_trailing_spaces(variable);
    remove_trailing_spaces(value);
    remove_trailing_spaces(units);
    if (strlen(variable) <= 0) return -3;
    if (strlen(value) <= 0) return -2;
    if (strlen(units) <= 0) return -1;
    return 0;
}



/******************************************************************************
 *
 * Name  getContents
 *
 * Description   This function gets the contents of the location /
 *       offset/length
 *
 * Parameters:
 *  location - location of the data to get
 *  offset     offset from the beginning of the location
 *  length     length, in characters, of the data to get
 *  result     The characters retrieved.
 *
 * Returns:
 *   -1 failure
 *   0  success
 *
 *****************************************************************************/
#ifdef REMOVED
int getContents(char *location, int offset, int length, char *result)
{
   char *s;
   int iCharsRetrieved;

   /* Decode location */
   if (strncmp_caseless("nitf_header", location, strlen(location)) == 0) {
       s = (char *) &nitf_main;
       strncpy(result, &(s[offset]), length);
       result[length] = '\0';
   }
   else
   {
   }
}
#endif


/*******************************************************************************
 * Name: dump_str
 *
 * Description:   This function dumps a non-null terminated string from
 *                source, length characters long
 *
 * Parameters:
 *      name       Name to label the string to print
 *      length     # characters to print
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

void dump_str(const char *name, char *source, int length)
{
    char msg[501];
    if (source == NULL) return;
    if (length > 500) {
        errmessage("dump_str: Error - length exceeds 500.  Please change length appropriately.\n");
        length = 500;
    }
    strncpy(msg, source, length);
    msg[length] = '\0';

    printf("%s = %s\n", name, msg);
}



/*******************************************************************************
 * Name: fDumpChars
 *
 * Description:   This function dumps a non-null terminated string from
 *                source, length characters long
 *
 * Parameters:
 *      source     pointer to source of characters to dump
 *      length     # characters to print
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

void fDumpChars(FILE *fp, char *source, int length)
{
    char msg[2401];
    if (source == NULL) return;
    if (length > 2400) {
        errmessage("fDumpStr: Error - length exceeds 2400.  Please change length appropriately.\n");
        length = 2400;
    }
    strncpy(msg, source, length);
    msg[length] = '\0';

    fprintf(fp, "%s", msg);
}



/*******************************************************************************
 * Name: fDumpStr
 *
 * Description:   This function dumps a non-null terminated string from
 *                source, length characters long
 *
 * Parameters:
 *      name       Name to label the string to print
 *      length     # characters to print
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

void fDumpStr(FILE *fp, const char *name, char *source, int length)
{
    char msg[2401];
    if (source == NULL) return;
    if (length > 2400) {
        errmessage("fDumpStr: Error - length exceeds 2400.  Please change length appropriately.\n");
        length = 2400;
    }
    strncpy(msg, source, length);
    msg[length] = '\0';

    fprintf(fp, "%s = '%s'\n", name, msg);
}



/*******************************************************************************
 * Name: dump_bytes_decimal
 *
 * Description:   This function gets sequential bytes and outputs them as
 *                decimals, separated by commas.
 *
 * Parameters:
 *      name       Name to label the string to print
 *      length     # characters to print
 *
 * Returns:
 *
 *****************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 *****************************************************************************/

void dump_bytes_decimal(char *name, char *source, int length)
{
    int x;

    printf("%s = ", name);

    for (x = 0; x < length; x++) {
        if (x > 0) printf(", ");
        printf("%d", (int) ((unsigned char) *(source + x)));
    }
    printf("\n");
}



/*******************************************************************************
 * Name:    set_long
 *
 * Description: This function creates a string from a long, pads 0s on the
 *              left, and inserts it at the location passed without null
 *              termination.
 *
 * Parameters:
 *      dest    destination
 *      v       value to set
 *      len     length of character field
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

void set_long(char *dest, long v, int len)
{
    char sBuffer1[100];
    char sFormat[20];

    sprintf(sFormat, "%%0%dld", len);
    sprintf(sBuffer1, sFormat, v);
    strncpy(dest, sBuffer1, len);
}

/*******************************************************************************
 * Name:    set_float
 *
 * Description: This function creates a string from a long, pads 0s on the
 *              left, and inserts it at the location passed without null
 *              termination.
 *
 * Parameters:
 *      dest    destination
 *      v       value to set
 *      format  format used to display the float
 *      max_len max length of output string
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

void set_float(char *dest, float v, char *sFormat, int len)
{
    char sBuffer1[100];
    char sBuffer[100];

    sprintf(sBuffer1, sFormat, v);
    if ((int) strlen(sBuffer1) < len) {    /* Left-zero pad */
        memset(sBuffer, '0', len - strlen(sBuffer1));
        strcpy(&sBuffer[len - strlen(sBuffer1)], sBuffer1);
    }
    else
    {
        strcpy(sBuffer, sBuffer1);
    }
    strncpy(dest, sBuffer, len);
}



/*******************************************************************************
 * Name:    set_string
 *
 * Description: This function copies from the NULL-terminated string to the
 *              destination, without null-termination.  The right side of the
 *              string is space-padded if necessary.  THE INPUT STRING MUST BE
 *              NULL TERMINATED!
 *
 * Parameters:
 *      dest    destination
 *      v       NULL-terminated input string
 *      len     Length of the output field
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

void set_string(char *s, char *v, int len)
{
    int l;

    l = strlen(v);

    if (l >= len) {
        strncpy(s, v, len);
    }
    else {
        strcpy(s, v);
        memset(s+l, ' ', len - l);
    }
}


/*******************************************************************************
 * Name: path2filename
 *
 * Description: This function skips over any path information, and returns a
 *              pointer to the filename.  It returns a pointer to beginning of
 *              filename, or first character after last '/'.
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

char *path2filename(char *filename)
{
    char *s1;   /* Position of "/" */
    char *s2;   /* Position of "\" */
    char *s3;   /* Position of ":" */
    char *s;

    s1 = strrchr(filename, '/');
    s2 = strrchr(filename, '\\');
    s3 = strrchr(filename, ':');

    if (s1 && s2) {     /* both forward and backward slashes.  Figure out */
                        /* which is nearer to the end of the line */
        if (s1 > s2) {
            s = s1;
        }
        else {
            s = s2;
        }
    }
    else if (s1) {      /* Forward slash found */
        s = s1;
    }
    else if (s2) {      /* Back-slash found */
        s = s2;
    }
    else if (s3) {      /* Neither slash found.  Look for double-colon */
        s = s3;
    }
    else
    {
        s = filename - 1;
    }
    s++;

    return s;
}



/*******************************************************************************
 * Name:    NITF_l_to_s
 *
 * Description:     This function takes a long from an NITF file, and fills a
 *                  string holding the long value.  This function will do any
 *                  required byte and word swapping.
 *
 * Parameters:
 *      str     Pointer to string where the long shall be stored
 *      l       long variable to convert to a string
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void NITF_l_to_s(char *str, long l)
{
#ifdef PC
    char *s1, *s2;
    long newlong;

    /* set up pointers */
    s1 = (char *) &l;
    s2 = (char *) &newlong;

    /* Re-arrange bytes */
    s2[0] = s1[3];
    s2[1] = s1[2];
    s2[2] = s1[1];
    s2[3] = s1[0];

    l = newlong;
#endif
    sprintf(str, "%ld", l);
}


/*******************************************************************************
 * Name:    s_to_NITF_l
 *
 * Description:     This function takes a string, and returns a long to insert
 *                  into a NITF file.  This function will do any required byte
 *                  and word swapping.
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

long s_to_NITF_l(char *str)
{

#ifdef PC
    long l;
    char *s1, *s2;
    long newlong;

    l = atol(str);

    /* set up pointers */
    s1 = (char *) &l;
    s2 = (char *) &newlong;

    /* Re-arrange bytes */
    s2[0] = s1[3];
    s2[1] = s1[2];
    s2[2] = s1[1];
    s2[3] = s1[0];

    return newlong;

#else
    return atol(str);
#endif
}



/*******************************************************************************
 * Name:    l_to_NITF_s
 *
 * Description:     This function takes a long, and puts it in a null-terminated
 *                  string for insertion into a NITF file.  This function will
 *                  do any required byte and word swapping.
 *
 * Parameters:
 *      l       long integer to convert
 *      dest    pointer to destination string
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void l_to_NITF_s(long l, char *dest)
{

#ifdef PC
    char *s1;

    /* set up pointers */
    s1 = (char *) &l;

    /* Re-arrange bytes */
    dest[0] = s1[3];
    dest[1] = s1[2];
    dest[2] = s1[1];
    dest[3] = s1[0];
    dest[4] = '\0';
    return;
#endif
    strncpy(dest, (char *) &l, 4);
    dest[4] = '\0';
}



/*******************************************************************************
 * Name:    short_to_NITF_s
 *
 * Description:     This function takes a short int, and puts it in a null-
 *                  terminated string for insertion into a NITF file.  This
 *                  function will do any required byte and word swapping.
 *
 * Parameters:
 *      l       long integer to convert
 *      dest    pointer to destination string
 * Returns:
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

void short_to_NITF_s(short int i, char *dest)
{

#ifdef PC
    char *s1;

    /* set up pointers */
    s1 = (char *) &i;

    /* Re-arrange bytes */
    dest[0] = s1[1];
    dest[1] = s1[0];
    dest[2] = '\0';
    return;
#endif
    strncpy(dest, (char *) &i, 2);
    dest[2] = '\0';
}



/*******************************************************************************
 * Name:    get_long
 *
 * Description:     This function takes a string (non-NULL-terminated), and
 *                  returns a long.
 *
 * Parameters:
 *      s       pointer to characters to convert to long
 *      len     length of the string to convert
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

long get_long(char *s, int len)
{
    char sBuffer1[20];
    long l;

    strncpy(sBuffer1, s, len);
    sBuffer1[len] = '\0';
    l = atol(sBuffer1);

    return l;
}



/*******************************************************************************
 * Name:    get_string
 *
 * Description:     This function takes a string (non-NULL-terminated), and
 *                  copies and null-terminates it to the destination string.
 *
 * Parameters:
 *      dest    pointer to destination string
 *      s       pointer to source string
 *      len     length of the string to convert
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

void get_string(char *dest, char *s, int len)
{
    strncpy(dest, s, len);
    dest[len] = '\0';
    return;
}



/**************************************************************************
 *
 * errmessage   This function prints an error message to the screen
 *
 **************************************************************************/

void errmessage(const char *s)
{
/*    if (args.screen_position > -1) */
/*        gotoRC(args.screen_position+3, 1); */
/*    fprintf(stderr, s); */
    fprintf(stdout, s);
}



/*******************************************************************************
 * Name:    get_stored_line
 *
 * Description: This function parses through a block of text, and uses <CR>s
 *              and <LF>s to find line separations.  One line is returned
 *              after each call, (without any EOL chars) until the end is
 *              reached.
 *
 *
 *
 *  WARNING!! The text is modified by adding NULLS at the end of each line.
 *
 *
 *
 * Parameters:
 *      sText           Text to parse (Pass NULL after 1st call)
 *      sLine           Line from text
 *
 * Returns:
 *      0       Success
 *      -1      Reached Null Termination (no more lines to return)
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int get_stored_line(char *sText, char *sLine)
{
    static char *cur_pos;
    char *s;

    if (sText != NULL) {
       cur_pos = sText;
    }

    s = cur_pos;
    if (s[0] == '\0') return FALSE; /* end of text */

    while (s[0] != '\0') {
        if (s[0] == '\x0A') {
              s[0] = '\0';
              strcpy(sLine, cur_pos);
              if (s[1] == '\x0D') {
                 s++;
              }
              s++;
              cur_pos = s;
              return TRUE;
        }
        else if (s[0] == '\x0D') {
              s[0] = '\0';
              strcpy(sLine, cur_pos);
              if (s[1] == '\x0A') {
                 s++;
              }
              s++;
              cur_pos = s;
              return TRUE;
        }
        s++;
    }
    strcpy(sLine, cur_pos);
    cur_pos = s;
    return TRUE; /* Last line in text, but it has characters, so TRUE */
}


/*******************************************************************************
 * Name:    verify_filename
 *
 * Description: This function makes sure all characters in a filename are
 *              alphanumeric.
 *
 * Parameters:
 *      filename        Filename to check
 *
 * Returns:
 *      0       Success
 *      -1      Filename is NULL or contains invalid characters
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int verify_filename(char *filename)
{
    char *s;
    s = filename;

    if (!s || s[0] == '\0') return -1;

    while (s[0] != '\0') {
        if (!isprint(s[0])) return -1;
        s++;
    }
    return 0;
}



/*******************************************************************************
 * Name:        strip_double_quotes
 *
 * Description: This function removes the double quotes around a string.
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

void strip_double_quotes(char *s)
{
    char *s1;

    if (s[0] == '"') {
        s1 = strchr(s, 0);
        s1--;
        if (s1[0] == '"') {
            s1[0] = '\0';
            s1 = s+1;
            while (s1[0] != '\0') {
                *s++ = *s1++;
            }
            s[0] = '\0';
        }
    }
}



/*******************************************************************************
 * Name:        mm_dd_yy_to_yyyymmdd
 *
 * Description: This function converts a date in MM-DD-YY format to YYYYMMDD
 *              format.  Source years from 00-20 will be 20xx, and source years
 *              from 21-99 will be 19xx.
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

void mm_dd_yy_to_yyyymmdd(char *value)
{
    int year;
    int month;
    int day;
    char *s1, *s2;
    char sTemp[10];

    if (strlen(value) > 8) {
        if (value[8] == ' ' && value[9] == '*') {
        }
        else
        {
            printf("mm_dd_yy_to_yyyymmdd: value '%s' is too long (expecting MM-DD-YY)\n", value);
            return ;
        }
    }

    strcpy(sTemp, value);

    s1 = strchr(sTemp, '-');
    s2 = strchr(s1+1, '-');

    if (s1 == NULL || s2 == NULL) {
        printf("mm_dd_yy_to_yyyymmdd: value '%s' is too long (expecting MM-DD-YY)\n", value);
        return ;
    }

    s1[0] = '\0';
    s1++;
    s2[0] = '\0';
    s2++;
    month = atol(sTemp);
    day = atol(s1);
    year = atol(s2);
    if (year > 20) {
        year += 1900;
    }
    else
    {
        year += 2000;
    }
    sprintf(value,"%04d%02d%02d", year, month, day);
}



/*******************************************************************************
 * Name:        nitf21date_2_nitf20date
 *
 * Description: This function converts a date in YYYYMMDD format to
 *              DDHHMMSSZMONYY format.
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

void nitf21dt_2_nitf20dt(char *value)
{
    char year[3];
    char month_mm[3];
    char month_asc[4];
    char day[3];
    char hour[3];
    char minute[3];
    char second[3];
    int month_num;

    //char *s1, *s2;
    //char sTemp[10];

 /* CCYYMMDDhhmmss DDHHMMSSZMONYY */

    strncpy(year, &value[2], 2);
    year[2] = '\0';
    strncpy(month_mm, &value[4], 2);
    month_mm[2] = '\0';
    strncpy(day, &value[6], 2);
    day[2] = '\0';
    strncpy(hour, &value[8], 2);
    hour[2] = '\0';
    strncpy(minute, &value[10], 2);
    minute[2] = '\0';
    strncpy(second, &value[12], 2);
    second[2] = '\0';

    month_num = atol(month_mm);
    switch(month_num) {
      case 1:
          strcpy(month_asc, "JAN");
          break;
      case 2:
          strcpy(month_asc, "FEB");
          break;
      case 3:
          strcpy(month_asc, "MAR");
          break;
      case 4:
          strcpy(month_asc, "APR");
          break;
      case 5:
          strcpy(month_asc, "MAY");
          break;
      case 6:
          strcpy(month_asc, "JUN");
          break;
      case 7:
          strcpy(month_asc, "JUL");
          break;
      case 8:
          strcpy(month_asc, "AUG");
          break;
      case 9:
          strcpy(month_asc, "SEP");
          break;
      case 10:
          strcpy(month_asc, "OCT");
          break;
      case 11:
          strcpy(month_asc, "NOV");
          break;
      case 12:
          strcpy(month_asc, "DEC");
          break;
      default:
          strcpy(month_asc, "   ");
          break;
    }

    sprintf(value,"%s%s%s%sZ%s%s", day, hour, minute, second,
                month_asc, year);
}



/*******************************************************************************
 * Name:        format_nitf21datetime
 *
 * Description: This function reads a date in YYYYMMDDhhmmss format, and formats
 *              it in MM/DD/YYYY hh:mm:ss format.
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

void format_nitf21datetime(char *value, char *out)
{
    char year[5];
    char month_digits[3];
    //char month_asc[4];
    char day[3];
    char hour[3];
    char minute[3];
    char second[3];

    //char *s1, *s2;
    //char sTemp[10];

 /* CCYYMMDDhhmmss DDHHMMSSZMONYY */

    strncpy(year, value, 4);
    year[4] = '\0';
    strncpy(month_digits, &value[4], 2);
    month_digits[2] = '\0';
    strncpy(day, &value[6], 2);
    day[2] = '\0';
    strncpy(hour, &value[8], 2);
    hour[2] = '\0';
    strncpy(minute, &value[10], 2);
    minute[2] = '\0';
    strncpy(second, &value[12], 2);
    second[2] = '\0';

    sprintf(out,"%s/%s/%s %s:%s:%s", year, month_digits, day, hour, minute, second);
}



/*******************************************************************************
 * Name:        nitf20date_2_nitf21date
 *
 * Description: This function converts a date in DDHHMMSSZMONYY to YYYYMMDD
 *              format.
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

void nitf20dt_2_nitf21dt(char *value)
{
    char year[5];
    char month_mm[3];
    char month_asc[4];
    char day[3];
    char hour[3];
    char minute[3];
    char second[3];
    //int month_num;
    int year_num;

    //char *s1, *s2;
    //char sTemp[10];

 /* DDHHMMSSZMONYY  CCYYMMDDhhmmss */

    strncpy(year, &value[12], 2);
    year[2] = '\0';
    year_num = atol(year);

    /* 0..59 ==> 2000..2059 */

    if (year_num < 60) {
        year_num += 2000;
    }
    else
    {
        year_num += 1900;
    }
    sprintf(year, "%04d", year_num);

    strncpy(month_asc, &value[9], 3);
    month_asc[3] = '\0';

    strncpy(day, &value[0], 2);
    day[2] = '\0';

    strncpy(hour, &value[2], 2);
    hour[2] = '\0';

    strncpy(minute, &value[4], 2);
    minute[2] = '\0';

    strncpy(second, &value[6], 2);
    second[2] = '\0';

    strcpy(month_mm, "  ");

    if (strcmp(month_asc, "JAN") == 0) {
        strcpy(month_mm, "01");
    }
    else if (strcmp(month_asc, "FEB") == 0) {
        strcpy(month_mm, "02");
    }
    else if (strcmp(month_asc, "MAR") == 0) {
        strcpy(month_mm, "03");
    }
    else if (strcmp(month_asc, "APR") == 0) {
        strcpy(month_mm, "04");
    }
    else if (strcmp(month_asc, "MAY") == 0) {
        strcpy(month_mm, "05");
    }
    else if (strcmp(month_asc, "JUN") == 0) {
        strcpy(month_mm, "06");
    }
    else if (strcmp(month_asc, "JUL") == 0) {
        strcpy(month_mm, "07");
    }
    else if (strcmp(month_asc, "AUG") == 0) {
        strcpy(month_mm, "08");
    }
    else if (strcmp(month_asc, "SEP") == 0) {
        strcpy(month_mm, "09");
    }
    else if (strcmp(month_asc, "OCT") == 0) {
        strcpy(month_mm, "10");
    }
    else if (strcmp(month_asc, "NOV") == 0) {
        strcpy(month_mm, "11");
    }
    else if (strcmp(month_asc, "DEC") == 0) {
        strcpy(month_mm, "12");
    }

    sprintf(value,"%04d%s%s%s%s%s", year_num, month_mm, day, hour, minute, second);
}



/*******************************************************************************
 * Name:        strstr_caseless
 *
 * Description: This function finds a substring in a caseless fashion
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

char *strstr_caseless(char *s1, char *s2)
{
    char *s3, *s4;
    char *s;

    s3 = (char*)malloc(strlen(s1)+1);
    s4 = (char*)malloc(strlen(s2)+1);

    strcpy(s3, s1);
    strcpy(s4, s2);

    if (s3 == NULL || s4 == NULL) {
        printf("strstr_caseless: Error allocating memory\n");
        iQuit(1);
    }

    strsetupper(s3);
    strsetupper(s4);

    s = strstr(s3, s4);

    free(s3);
    free(s4);
    return s;
}



/*******************************************************************************
 * Name:        parse_igeolo20
 *
 * Description: This function parses an NITF 2.0 IGEOLO field and returns the
 *              information in a comma-separated string of coordinates in the
 *              format dddmmssV where V = N,S,E, or W.  8 values are returned.
 *
 * Parameters:
 *      icords  NITF icords parameter
 *      s_in    original IGEOLO string from NITF file
 *      s_out   parsed output
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

void parse_igeolo20(char icords, char *s_in, char *s_out)
{
    char sBuffer[100];

    switch(icords) {
      case 'N':
          sprintf(s_out, " ");
          break;
      case 'G':
          sprintf(s_out, "%7.7s, %8.8s, %7.7s, %8.8s, %7.7s, %8.8s, %7.7s, %8.8s", &s_in[0], &s_in[7], &s_in[15], &s_in[22], &s_in[30], &s_in[37], &s_in[45], &s_in[52]);
          strncpy(sBuffer, &s_in[0], 7);
          sBuffer[7] = '\0';
          break;
      case 'U':
          sprintf(s_out, "UTM IGEOLO %s", s_in);
          break;
      case 'C':
/*          sprintf(s_out, "Geocentric IGEOLO %s", s_in); */
          sprintf(s_out, "%7.7s, %8.8s, %7.7s, %8.8s, %7.7s, %8.8s, %7.7s, %8.8s", &s_in[0], &s_in[7], &s_in[15], &s_in[22], &s_in[30], &s_in[37], &s_in[45], &s_in[52]);
          strncpy(sBuffer, &s_in[0], 7);
          sBuffer[7] = '\0';
          break;
      default:
          printf("parse_igeolo: don't know how to parse icord '%c'\n", icords);
          break;
    }
}



/*******************************************************************************
 * Name:        parse_igeolo21
 *
 * Description: This function parses an NITF 2.1 IGEOLO field and returns the
 *              information in a comma-separated string of coordinates in the
 *              format dddmmssV where V = N,S,E, or W.  8 values are returned.
 *
 * Parameters:
 *      icords  NITF icords parameter
 *      s_in    original IGEOLO string from NITF file
 *      s_out   parsed output
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

void parse_igeolo21(char icords, char *s_in, char *s_out)
{
    char sBuffer[100];

    switch(icords) {
      case ' ':
          sprintf(s_out, " ");
          break;
      case 'U':
          sprintf(s_out, "IGEOLO UTM expressed in MGRS %s", s_in);
          break;
      case 'N':
          sprintf(s_out, "IGEOLO Northern hemisphere UTM %s", s_in);
          break;
      case 'S':
          sprintf(s_out, "IGEOLO Southern hemisphere UTM %s", s_in);
          break;
      case 'G':
          sprintf(s_out, "%7.7s, %8.8s, %7.7s, %8.8s, %7.7s, %8.8s, %7.7s, %8.8s", &s_in[0], &s_in[7], &s_in[15], &s_in[22], &s_in[30], &s_in[37], &s_in[45], &s_in[52]);
          strncpy(sBuffer, &s_in[0], 7);
          sBuffer[7] = '\0';
          break;
      case 'D':
          sprintf(s_out, "IGEOLO Decimal degrees %s", s_in);
          break;
      default:
          printf("parse_igeolo: don't know how to parse icord '%c'\n", icords);
          break;
    }
}



/*******************************************************************************
 * Name:        SplitWordAndNum
 *
 * Description: This function takes a line like words<#>, splits apart the words
 *              passed and number, returns the number, and copys the words to
 *              word.  If words contains one or more digits, this function will
 *              ignore them and keep them intact as a part of words.
 *
 * Parameters:
 *      in          String to process
 *      word        Word
 *
 * Returns:
 *      #   Number found in the string
 *      -1  Error - # not found
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int SplitWordAndNum(char *in, char *word)
{
    char *s, *s1;
    long l;
    char sBuffer1[101];

    if (word == NULL) {
        errmessage("SplitWordAndNum: Error: word is NULL pointer!\n");
        iQuit(1);
    }
    if (in == NULL) {
        errmessage("SplitWordAndNum: Error: 'in' is a NULL pointer!\n");
        iQuit(1);
    }

    strcpy(sBuffer1, in);
    trim(sBuffer1);

    s = sBuffer1;

skip_embedded_number:

    while (s[0] != '\0' && !isdigit(s[0])) s++;

    if (s[0] == '\0') {
        strcpy(word, sBuffer1);
        return -1;
    }

    /* If text follows number, this isn't the end. */
    s1 = s;

    while (s1[0] != '\0' && isdigit(s1[0])) s1++;
    if (s1[0] != ' ' && s1[0] != '\0') {
        s = s1;
        goto skip_embedded_number;
    }

    l = atol(s);
    s[0] = '\0';
    trim(sBuffer1);
    strcpy(word, sBuffer1);
    return l;
}


/*******************************************************************************
 * Name:    find_text_tag
 *
 * Description:     This function scans through text, and copies a text tag,
 *                  if one was found, into the caller's string.
 *
 * Assumptions:  This program assumes no NULLs are contained in the text until
 *               the end.
 *
 * Parameters:
 *      sFindTag    Name of tag to find
 *      sText       Pointer to a block of contiguous text
 *      sTag        The text tag, if found (pass ptr to char[80])
 *
 * Returns:
 *      TRUE    Found
 *      FALSE   Not found
 *
 *******************************************************************************
 * Modifications:
 *
 * Initials  Date     Mod
 * jls                Created
 *
 ******************************************************************************/

int find_text_tag(const char *sFindTag, char *sText, char *sTagFound)
{
    char *p, *pEnd;
    char buf[100];

    strcpy(buf, "<");
    strcat(buf, sFindTag);
    p = strstr(sText, buf);
    if (p == NULL) {
        sTagFound[0] = '\0';
        return FALSE;
    }
    pEnd = strchr(p, '>');
    if (pEnd == NULL) {
        errmessage("find_text_tag: Error finding end of tag\n");
        printf("tag to find '%s', found start of tag: '%20.20s' text to search '%s'\n", buf, p, sText);
        return FALSE;
    }
    strncpy(sTagFound, p, pEnd - p + 1);
    sTagFound[pEnd - p + 1] = '\0';
    return TRUE;
}




