/* df_read.c - Routines to read, navigate DFP structures **************/

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
*       Convenience functions to help with navigation of DFP structures 
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/

#include <stdio.h>
#include <ctype.h>

#ifndef PC
#include <strings.h>
#include <string.h>
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


/*******************************************************************************
 * Name:    DFPCreate
 *
 * Description: This function allocates, creates, initializes, and returns
 *              a pointer to a DFParseType structure.
 *
 * Parameters:
 *
 * Returns:
 *      Pointer to array
 ******************************************************************************/

DFParseType *DFPCreate(void)
{
    DFParseType *p;

    p = (DFParseType *) malloc(sizeof(DFParseType));
    if (!p) {
        errmessage("DFPCreate: error allocating\n");
        iQuit(1);
    }

    p->Next = NULL;
    p->Prev = NULL;
    p->Parent = NULL;
    p->bNonPrinting = FALSE;
    p->NodeType = -1;
    strcpy(p->FieldName, "");
    p->FieldWidth = -1;
    p->DataType = -1;
    strcpy(p->units, "");
    strcpy(p->IoConversionFn, "");
    p->pFieldValue = NULL;
    p->Repetitions = -1;
    p->NumItems = -1;
    p->pCondition = NULL;
    p->LoopCondFields = NULL;

    return p;
}

/*******************************************************************************
 * Name:    DFPDestroy
 *
 * Description: This function recursively destroys the structure passed by
 *              pointer, its contents, and all of it's loop nodes.
 *
 * Parameters:
 *      pDFP    pointer to structure
 *
 * Returns:
 *
 ******************************************************************************/

void DFPDestroy(DFParseType *pDFP)
{
    DFParseType *p, *p1, *p2;

    if (!pDFP) {
        errmessage("DFPDestroy: NULL pointer passed\n");
        iQuit(1);
    }

    p = pDFP;

    /* If this is a loop node, destroy looped tags */
    p1 = p->LoopCondFields;
    while (p1) {
        p2 = p1->Next;
        DFPDestroy(p1);
        p1 = p2;
    }
    if (p->pFieldValue) free (p->pFieldValue);
    if (p->pCondition) free(p->pCondition);

    /* Unlink from chain of nodes */
    if (p->Prev) {
        p->Prev->Next = p->Next;
    }
    if (p->Next) {
        p->Next->Prev = p->Prev;
    }
    free(p);
}



/*******************************************************************************
 * Name:    DFPNext
 *
 * Description: This function returns a pointer to the next parse node.
 *
 * Parameters:
 *      pDFP    Ptr to structure
 *
 * Returns:
 *      Ptr to next structure or NULL
 ******************************************************************************/

DFParseType *DFPNext(DFParseType *pDFP)
{
    if (!pDFP) {
        errmessage("DFPNext: NULL pointer passed\n");
        iQuit(1);
    }
    return pDFP->Next;
}



/*******************************************************************************
 * Name:    DFPPrev
 *
 * Description: This function returns a pointer to the previous parse node.
 *
 * Parameters:
 *      pDFP    Ptr to structure
 *
 * Returns:
 *      Ptr to previous structure or NULL
 ******************************************************************************/

DFParseType *DFPPrev(DFParseType *pDFP)
{
    if (!pDFP) {
        errmessage("DFPPrev: NULL pointer passed\n");
        iQuit(1);
    }
    return pDFP->Prev;
}



/*******************************************************************************
 * Name:    DFPFirst
 *
 * Description: This function returns a pointer to the first parse node in the
 *              list.
 *
 * Parameters:
 *      pDFP    Ptr to structure
 *
 * Returns:
 *      Ptr to first structure or NULL
 ******************************************************************************/

DFParseType *DFPFirst(DFParseType *pDFP)
{
    DFParseType *p, *p1;

    if (!pDFP) {
        errmessage("DFPFirst: NULL pointer passed\n");
        iQuit(1);
    }
    p = pDFP;
    while (p) {
        p1 = p;     /* Keep copy of good ptr */
        p = DFPPrev(p1);
    }
    return p1;
}



/*******************************************************************************
 * Name:    DFPLast
 *
 * Description: This function returns a pointer to the last parse node in the
 *              list.
 *
 * Parameters:
 *      pDFP    Ptr to structure
 *
 * Returns:
 *      Ptr to last structure or NULL
 ******************************************************************************/

DFParseType *DFPLast(DFParseType *pDFP)
{
    DFParseType *p, *p1;

    if (!pDFP) {
        errmessage("DFPLast: NULL pointer passed\n");
        iQuit(1);
    }
    p = pDFP;
    while (p) {
        p1 = p;     /* Keep copy of good ptr */
        p = DFPNext(p1);
    }
    return p1;
}



/*******************************************************************************
 * Name:    DFPParent
 *
 * Description: This function returns a pointer to the parent of a node
 *
 * Parameters:
 *      pDFP        pointer to a node
 *
 * Returns:
 *      pointer to parent or NULL if top level
 ******************************************************************************/

DFParseType *DFPParent(DFParseType *pDFP)
{
    if (!pDFP) {
        errmessage("DFPParent: NULL pointer passed\n");
        iQuit(1);
    }
    return pDFP->Parent;
}



/*******************************************************************************
 * Name:    DFPFindFieldName
 *
 * Description: This function searches for a field of the given name (probably
 *              to find a loop count), and
 *              returns a pointer to it.  It looks for the field, first at the
 *              same level as the current node, then the parent level(s)
 *              (between the first node and the current or loop / conditional
 *              node.  The search is not case sensitive.
 *
 * Parameters:
 *      DFPcur        pointer to current node
 *
 * Returns:
 *      pointer to node or NULL if not found.
 ******************************************************************************/

DFParseType *DFPFindFieldName(DFParseType *DFPcur, char *FieldName)
{
    DFParseType *c;
    DFParseType *First;

    if (!DFPcur) {
        errmessage("DFPFindFieldName: NULL DFPcur pointer passed\n");
        iQuit(1);
    }

    if (!FieldName) {
        errmessage("DFPFindFieldName: NULL FieldName pointer passed\n");
        iQuit(1);
    }

    First = DFPFirst(DFPcur);
    while (First) {
        c = First;
        while (c && c != DFPcur) {
            if (strcmp_caseless(c->FieldName, FieldName) == 0) return c;
            c = DFPNext(c);
        }
        First = DFPParent(First);
    }
    return NULL;
}


/*******************************************************************************
 * Name:    DFPReadLine
 *
 * Description: This function reads a line, creates a node, and returns a
 *              pointer to the node.
 *
 * Parameters:
 *      fp      file pointer for file to read
 *
 *      rc      pointer to return code
 *              0 = success
 *              -1 = fail/error
 *
 * Returns:
 *      pointer to node, or NULL (EOF)
 ******************************************************************************/

DFParseType *DFPReadLine(FILE *fp, int *rc)
{
    DFParseType *p;
    char sBuffer[120];      /* Original line read */
    char sBuffer1[120];     /* Buffer used for line manipulation */
    char sBuffer2[200];     /* Buffer used for error string output */
    long rc1;

    p = DFPCreate();

    sBuffer[0] = '\0';
    *rc = 0;
    rc1 = TRUE;
    while (rc1 && strlen(sBuffer) <= 5) {
        rc1 = get_line(fp, sBuffer, 100);
        if (strncmp_caseless(sBuffer, "ENDOFTAGDEF", 11) == 0) return NULL;
    }

    if (rc1 == FALSE) return NULL;

    strcpy(sBuffer1, sBuffer);
    trim(sBuffer1);


    if (sBuffer1[0] == '[') {   /* Loop / conditional node */
        p->NodeType = LOOP;

        if (strncmp_caseless(sBuffer1, "[LOOP]", 6) == 0) {
            *rc = DFPParseLoop(p, sBuffer);
        }
        else if (strncmp_caseless(sBuffer1, "[if contains", 12) == 0) {
            *rc = DFPParseCondition(p, sBuffer);
        }
        else if (strncmp_caseless(sBuffer1, "[if not", 7) == 0) {
            *rc = DFPParseCondition(p, sBuffer);
        }
        else
        {
            sprintf(sBuffer2, "DFPReadLine: Error in definition file line: '%s'\n", sBuffer);
            errmessage(sBuffer2);
            DFPLineHelp();
            return p;
        }
    }
    else    /* Tag node */
    {
        *rc = DFPParseTag(p, sBuffer);
    }
    return p;
}



/*******************************************************************************
 * Name:    DFPParseTag
 *
 * Description: This function parses tag fields, filling in a DFParseType
 *              structure node for use.
 *
 * Parameters:
 *      p           DFParseType structure node
 *      sBuffer     Line read in to be parsed
 * Returns:
 *      0   Success
 *      -1  Failure
 ******************************************************************************/

int DFPParseTag(DFParseType *p, char *sBuffer)
{
    //char *s0;
    char *s;
    char *s1;
    char sBuffer1[120];     /* Buffer used for line manipulation */
//    char sBuffer2[200];     /* Buffer used for error string output */

    p->NodeType = FIELD;
    strcpy(sBuffer1, sBuffer);
    s = strchr(sBuffer1, ' ');
    if (!s) {
        printf("DFPParseTag: Error in definition file line: '%s'\n", sBuffer);
        DFPLineHelp();
        return -1;
    }
    s[0] = '\0';  /* NULL-terminate name */
    s++;

    strcpy(p->FieldName, sBuffer1);

    while (s[0] == ' ' || s[0] == '\t') s++;

    switch (toupper(s[0])) {
    case 'X':
        p->DataType = DATA_ASCII;
        break;
    case 'B':
        p->DataType = DATA_BINARY;
        break;
    case 'S':
        p->DataType = DATA_SIGNED_INT;
        break;
    case 'U':
        p->DataType = DATA_UNSIGNED_INT;
        break;
    case 'F':
        p->DataType = DATA_FLOAT;
        break;
    default:
        printf("DFPParseTag: Error in definition file line: '%s'\n", sBuffer);
        DFPLineHelp();
        return -1;
    }
    if (toupper(s[1]) == 'N') p->bNonPrinting = TRUE;

    s = strchr(s, ' ');
    if (!s) {
        printf("DFPParseTag: Error in definition file line: '%s'\n", sBuffer);
        DFPLineHelp();
        return -1;
    }
    while (s[0] == ' ' || s[0] == '\t') s++;
    p->FieldWidth = atoi(s);

    if (p->FieldWidth < 1) {
        printf("DFPParseTag: Error in definition file line: '%s': field width (%d) must be at least 1\n", sBuffer, p->FieldWidth);
        return -1;
    }

    switch(p->DataType) {
    case DATA_FLOAT:
        if (p->FieldWidth != 4 && p->FieldWidth != 8) {
            printf("DFFParseTag: (F)loat data type must have a width of 4 or 8 bytes\n");
            iQuit(1);
        }
        break;
    case DATA_SIGNED_INT:
        if (p->FieldWidth != 2 && p->FieldWidth != 4) {
            printf("DFFParseTag: (S)igned integer data type must have a width of 2 or 4 bytes\n");
            iQuit(1);
        }
        break;
    case DATA_UNSIGNED_INT:
        if (p->FieldWidth != 2 && p->FieldWidth != 4) {
            printf("DFFParseTag: (U)nsigned integer data type must have a width of 2 or 4 bytes\n");
            iQuit(1);
        }
        break;
    }

    p->pFieldValue = (char *) malloc(p->FieldWidth + 1);
    if (p->pFieldValue == NULL) {
        errmessage("Error allocating memory\n");
        iQuit(1);
    }

    /* Advance to the units field */
    s = strchr(s, '"');
    if (s) {
        s++;
        /* strip off ending " or error message */
        s1 = strchr(s, '"');
        if (s1) {
            s1[0] = '\0';
            strncpy(p->units, s, sizeof(p->units));
            p->units[sizeof(p->units) - 1] = '\0';

            s = s1 + 1;
        }
        else
        {
            printf("DFPParseTag: Error in definition file line: '%s'\n", sBuffer);
            DFPLineHelp();
        }
    }

    /* Advance to the IoConversionFn field */
    if (s)
        s = strchr(s, '"');
    if (s) {
        s++;
        /* strip off ending " or error message */
        s1 = strchr(s, '"');
        if (s1) {
            s1[0] = '\0';
            strncpy(p->IoConversionFn, s, sizeof(p->IoConversionFn));
            p->IoConversionFn[sizeof(p->IoConversionFn) - 1] = '\0';

            s = s1 + 1;
        }
        else
        {
            printf("DFPParseTag: Error in definition file line: '%s'\n", sBuffer);
            DFPLineHelp();
        }
    }


    return 0;
}



/*******************************************************************************
 * Name:    DFPParseLoop
 *
 * Description: This function parses loop fields, filling in a DFParseType
 *              structure node for use.
 *
 * Parameters:
 *      p           DFParseType structure node
 *      sBuffer     Line read in to be parsed
 *
 * Returns:
 *      0   Success
 *      -1  Failure
 ******************************************************************************/

int DFPParseLoop(DFParseType *p, char *sBuffer)
{
    char *s;
    char *s1;
    char sBuffer1[120];     /* Buffer used for line manipulation */
    char sBuffer2[200];     /* Buffer used for error string output */
    long len;

    strcpy(sBuffer1, sBuffer);
    s = strchr(sBuffer1, ']');
    if (!s) {
        sprintf(sBuffer2, "DFPParseLoop: Error in definition file line: '%s'\n", sBuffer);
        errmessage(sBuffer2);
        DFPLineHelp();
        return -1;
    }
    s++;
    while (s[0] == ' ' || s[0] == '\t') s++;

    /* Parse "[x]" */
    if (s[0] != '[') {
        sprintf(sBuffer2, "DFPParseLoop: Error in definition file line: '%s'\n", sBuffer);
        errmessage(sBuffer2);
        DFPLineHelp();
        return -1;
    }
    s++;
    while (s[0] == ' ' || s[0] == '\t') s++;

    if (isalpha(s[0])) {    /* use field name to lookup # loops */
        s1 = strchr(s, ']');
        len = s1 - s;
        strncpy(p->FieldName, s, len);
        p->FieldName[len] = '\0';
    }
    else    /* Static # of loops */
    {
        p->Repetitions = atoi(s);
    }

    /* Parse # fields to loop */
    s = strchr(s, '[');
    if (!s) {
        sprintf(sBuffer2, "DFPReadLine: Error in definition file line: '%s'\n", sBuffer);
        errmessage(sBuffer2);
        DFPLineHelp();
        return -1;
    }
    s++;
    p->NumItems = atoi(s);
    return 0;
}



/*******************************************************************************
 * Name:    DFPParseCondition
 *
 * Description: This function parses condition fields, filling in a
 *              DFParseType structure node for use.
 *
 * Parameters:
 *      p           DFParseType structure node
 *      sBuffer     Line read in to be parsed
 * Returns:
 *      0   Success
 *      -1  Failure
 ******************************************************************************/

int DFPParseCondition(DFParseType *p, char *sBuffer)
{
    //char sBuffer1[1000];
    char sBuffer2[1000];
    int len;
    char *s1;
    char *s2;
    char *s;

    p->NodeType = CONDITION;
    s1 = strchr(sBuffer, '[');
    s2 = strchr(sBuffer, ']');

    if (!s1 || !s2) {
        DFPLineHelp();
        return -1;
    }
    s1++;
    s2[0] = '\0';
    s2++;

    len = strlen(s1);
    p->pCondition = (char *) malloc(len+1);
    strcpy(p->pCondition, s1);

    s = s2;

    while (s[0] == ' ' || s[0] == '\t') s++;


    /* Parse "[fieldname]" */
    if (s[0] != '[') {
        sprintf(sBuffer2, "DFPParseIfContains: Error in definition file line: '%s'\n", sBuffer);
        errmessage(sBuffer2);
        DFPLineHelp();
        return -1;
    }
    s++;
    while (s[0] == ' ' || s[0] == '\t') s++;

    /* store field name for future use */
    s1 = strchr(s, ']');
    len = s1 - s;
    strncpy(p->FieldName, s, len);
    p->FieldName[len] = '\0';

    /* Parse # fields in condition */
    s = strchr(s, '[');
    if (!s) {
        sprintf(sBuffer2, "DFPParseIfContains: Error in definition file line: '%s'\n", sBuffer);
        errmessage(sBuffer2);
        DFPLineHelp();
        return -1;
    }
    s++;
    p->NumItems = atoi(s);
    return 0;
}



/*******************************************************************************
 * Name:    DFPLineHelp
 *
 * Description: This function prints help for the definition file Field format
 *
 * Parameters:
 *
 * Returns:
 *
 ******************************************************************************/

void DFPLineHelp(void)
{
    printf("\
Field format is:\n\
\n\
FieldName    XN  [FieldWidth] \n\
\n\
X is {X = ASCII, B = Binary}\n\
N means nonprinting field (don't put anything and it will print out\n\
FieldWidth is the width (size) of the field in characters\n\
\n\
[LOOP][Repetitions][#FieldsInLoop]\n\
[LOOP][FieldToGetRepetitionsFrom][#FieldsInLoop]\n\
\n\
[IF CONTAINS \"string1\" \"string2\" \"as needed\"][FieldToTest][#FieldsInCondition]\n\
[IF NOT \"as many strings as needed\"][FieldToTest][#FieldsInCondition]\n\
\n\
Repetitions can be a fixed number or can be the name of the field holding\n\
# repetitions.  FieldToTest is the name of the field to test.\n\
Square brackets ([]) must be used as shown.\n");
}



/*******************************************************************************
 * Name:    ReadDefinitionFile
 *
 * Description: This function reads the given definition file, and returns a
 *              pointer to the DFParseType structure built in the process
 *
 * Parameters:
 *      FileName    name of file to read
 *
 * Returns:
 *      pointer to the DFParseType structure built in the process
 *      NULL on failure
 ******************************************************************************/

DFParseType *ReadDefinitionFile(const char *FileName)
{
    DFParseType *root = NULL;
    FILE *fp = NULL;
    int rc;
    char sBuffer1[120];

    fp = fopen(FileName, "rb");
    if (fp == NULL) {
        /*printf("ReadDefinitionFile: Error opening file %s, skipping TRE.\n", FileName);*/
        printf("ReadDefinitionFile: Error opening file %s.\n", FileName);
        return NULL;
    }

    /* Throw out Tag Information Line */
    rc = get_line(fp, sBuffer1, 100);
    if (rc == FALSE) {
        printf("ReadDefinitionFile: File %s is empty, skipping TRE.\n", FileName);
        return NULL;
    }

    root = DFPReadDefinitions(fp, NULL, 1000, &rc);
    if (root == NULL || rc < 0) {
        sprintf(sBuffer1, "ReadDefinitionFile: Error reading file %s, skipping TRE.\n", FileName);
        errmessage(sBuffer1);
        return NULL;
    }
    fclose(fp);
    return root;
}


/*******************************************************************************
 * Name:    DFPReadDefinitions
 *
 * Description: This function recursively reads a definition file, populates
 *              the DFP structure, and returns a pointer to the DFParseType
 *              structure built in the process.
 *
 * Initial Call:Call initially with a NULL Parent pointer, and 1000 RequiredItems
 *              or similar to indicate no limit.
 *
 * Parameters:
 *      fp              FILE pointer to open definition file
 *      Parent          Pointer to parent node
 *      RequiredItems   # of items to read before returning.  This includes
 *                      loops, if conditions, but not their sub-items.  Used
 *                      for reading loops and if conditions.
 *
 *      rc              pointer to return code.
 *                      0 = success
 *                      -1 = fail
 *
 * Returns:
 *      pointer to the DFParseType structure built in the process
 ******************************************************************************/

DFParseType *DFPReadDefinitions(FILE *fp, DFParseType *parent, int RequiredItems, int *rc)
{
    DFParseType *p = NULL;
    DFParseType *curr = NULL;
    DFParseType *root = NULL;
    int rc1;
    char sBuffer1[120];
    //long l;

    *rc = 0;
    while (TRUE) {
        p = DFPReadLine(fp, &rc1);
        if (rc1 < 0) {
            *rc = -1;
            root = NULL;
            goto cleanup_and_return;
        }
        if (p == NULL) {
/*printf("end of file\n"); */
            goto cleanup_and_return;
        }
        p->Parent = parent;
        /*link it in */
        if (root == NULL) {
            root = p;
            curr = p;
        } else {
            curr->Next = p;
            p->Prev = curr;
            curr = p;
        }

        if (p->NodeType == LOOP || p->NodeType == CONDITION) {
            /*recurse */
            p = DFPReadDefinitions(fp, curr, curr->NumItems, &rc1);
            if (rc1 < 0) {
                *rc = -1;
                root = NULL;
                goto cleanup_and_return;
            }
            curr->LoopCondFields = p;
            RequiredItems--;
        }
        else if(p->NodeType == FIELD) {    /* FIELD */
            RequiredItems--;
        }
        else {
            sprintf(sBuffer1, ": unknown NodeType %d\n", p->NodeType);
            errmessage(sBuffer1);
        }
        if (RequiredItems <= 0) return root;
    }
cleanup_and_return:
    return root;
}



