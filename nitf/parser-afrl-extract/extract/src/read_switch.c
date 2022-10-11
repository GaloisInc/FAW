/*************************************************************************
 *                                                                       *
 *                       Routine: read_switch()                          *
 *                        Author: Otto Milvang, U of Oslo, Norway        *
 *                          Date: 1990...                                *
 *                                                                       *
 *           This routine is an exact copy of the routine called         *
 *           "read_switch()" found in the B-LAB XITE source routine      *
 *           "readswitch.c", which was written by Otto Milvang, U.       *
 *           of Oslo, Image Processing lab, Dept of Informatics,         *
 *           Copyright 1990. Hence, I am including the following         *
 *           copyright notice...                                         *
 * ________________________________________________________________      *
 *                                                                       *
 *       Copyright 1990, Blab, UiO                                       *
 *       Image processing lab, Department of Informatics                 *
 *       University of Oslo                                              *
 *       E-mail: blab@ifi.uio.no                                         *
 * ________________________________________________________________      *
 *                                                                       *
 *                                                                       *
 * Permission to use, copy, modify and distribute this software and its  *
 * documentation for any purpose and without fee is hereby granted,      *
 * provided that this copyright notice appear in all copies and that     *
 * both that copyright notice and this permission notice appear in       *
 * supporting documentation and that the name of B-lab, Department of    *
 * Informatics or University of Oslo not be used in advertising or       *
 * publicity pertaining to distribution of the software without specific,*
 * written prior permission.                                             *
 *                                                                       *
 * B-LAB DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING*
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT    *
 * SHALL B-LAB BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL      *
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR *
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER        *
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR      *
 * PERFORMANCE OF THIS SOFTWARE.                                         *
 *************************************************************************
 *                                                                       *
 *  This portion of the documentation is essentially my own.  I include  *
 *  it to better help you to understand how to use the subroutine.       *
 *                                                                       *
 *   John Querns, Veda Inc, (937) 255-1116, Ext 2818                     *
 *   email: jquerns@mbvlab.wpafb.af.mil                                  *
 *                                                                       *
 *  read_switch()                                                        *
 *                                                                       *
 *  DESCRIPTION:  This is a nifty subroutine for extracting arguments    *
 *                using switches. I just extracted it from the main C    *
 *                routine to make it more "standalone" for my purposes.  *
 *                                                                       *
 *      FORMAT:   char *read_switch( int* argc,                          * 
 *                                 char** argv,                          *
 *                                 char* name,                           *
 *                                 int args,                             *
 *                                 char* defreturn );                    *
 *                                                                       *
 *                'read_switch' reads a switch from the command line.    *
 *                routine searches for the switch 'name'.  If 'name' is  *
 *                found, and 'args' is zero (i.e. args following switch),*
 *                a pointer to the switch is returned.  If 'name' is     *
 *                and 'args' is non-zero, a string containing the switch *
 *                argument(s) is returned.  If 'name' does not occur in  *
 *                the command line, 'defreturn' is returned.             *
 *                                                                       *
 *                The "nifty" thing about this subroutine is that it only*
 *                looks at the "variable" arguments.  As it finds them,  *
 *                it decrements "argc" till only the REQUIRED arguments  *
 *                are left.  Also, it keeps you from having to fool      *
 *                around trying to parse out the command line.           *
 *                                                                       *
 *    EXAMPLES:                                                          *
 *                                                                       *
 *       #include <math.h>                                               *
 *                                                                       *
 *        char *res, *filnam;                                            *
 *        double d;                                                      *
 *        int i, j, l;                                                   *
 *                                                                       *
 *        d = atof(read_switch(&argc, argv, "-scale", 1, "0.5"));        *
 *        sscanf(read_switch(&argc, argv, "-offset", 2, "128 128"),      *
 *                          "%d%d", &i, &j);                             *
 *        l = read_switch(&argc, argv, "-log", 0, NULL) != NULL;         *
 *        res = read_switch(&argc, argv, "-yes", 0, "-no");              *
 *        filnam = argv[1];                                              *
 *                                                                       *
 *        As you can see, since "read_switch" returns a string, you have *
 *        to use other routines (i.e. atof, atoi, sscanf, etc.) to get   *
 *        the arg values in the proper form....                          *
 *                                                                       *
 *************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *read_switch(int *argc, char **argv, char *name, int args, char *defreturn)
{
 int i, argn;
  char *ret;
  for(i=1; i< *argc; i++)
    {
      if (strcmp(name, argv[i]) == 0)
      {
        argn = i+1;
        if (i + args >= *argc)
          {
            fprintf(stderr,"read_switch: Missing argument after \"%s\"\n",
                    argv[i]);
            exit(2);
          }
        ret = argv[args ? i+1 : i];
        argn = i + 1 + args;
        while(argn < *argc)
            argv[i++] = argv[argn++];
        *argc = *argc - argn + i;
        if (args-- <=1) return(ret);
        while(args--) ret[strlen(ret)] = ' ';
        return(ret);
      }
    }
  return(defreturn);
}
/* Last line of read_switch() */



