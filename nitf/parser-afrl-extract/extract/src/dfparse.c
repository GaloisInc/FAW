/* dfparse.c - Routines to parse definition files ***************************/

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
*       Routines to parse definition (.pff, .dff) files
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

#include <ctype.h>
#include <fcntl.h>
#include <stdlib.h>

#include "defines.h"


/* defines */

/*******************************************************************************
 * Name:    DFPExportData
 *
 * Description: This function loads the DFP structure with the .dff file passed,
 *              opens the output file, and traverses the DFP structure, filling
 *              in the value fields for later use.  It outputs the contents to
 *              the output file as it goes.
 *
 * Parameters:
 *      out_filename    Filename to export the data to
 *      dff_filename    Name of the .DFF file to load into the DFP structure
 *      data            Pointer to the data to interpret / export
 *
 *      bAutoIncrement  TRUE allows this function to determine the filename,
 *                      file#, and create a unique output filename for this TRE.
 *                      This function looks for the number before the last "."
 *                      in the filename, and increments it until a new file can
 *                      be created.  New filenames have a .txt extension.
 *
 * Returns:
 *      0       Success
 *      -1      Failure
 ******************************************************************************/

int DFPExportData(char *out_filename, const char *dff_filename, char *data, int bAutoIncrement, int data_length)
{
    DFParseType *pt;
    DFParseType *DFPcur;
    FILE *fp;
    char *CurData;
    char *s;
    int x;
    int count_num = 0;    /* individual number for an image with 2 or more of the same TRE */
    char sBuffer[1000];
    char sFormat[20];

    CurData = data;

    s = strrchr(out_filename, '.');
    if (s && bAutoIncrement) {
        s--;
        if (isdigit(s[0])) { /* make sure it's a TRE, not a header */
            while (isdigit(s[-1])) s--;
            count_num = atol(s);
            while (file_exists(out_filename)) {
                printf("Attempting to create file '%s'. File exists. ", out_filename);
                sprintf(s, "%d.txt", ++count_num);
                printf("Trying file '%s'.\n", out_filename);
            }
        }
    }

    pt = ReadDefinitionFile(dff_filename); /* Check return code later */

    fp = fopen(out_filename, "w");
    if (fp == NULL) {
        sprintf(sBuffer, "DFPExportData: Error opening output filename: %s\n", out_filename);
        errmessage(sBuffer);
        iQuit(1);
    }

    DFPcur = pt;

    /* Check to see if we loaded the definition file */
    if (pt == NULL) {
        /* if (gOptions.debug) { */
            printf("Unknown TRE '%s', length %d\n", dff_filename, data_length);

            sprintf(sFormat, "%%%d.%ds", data_length, data_length);
printf("format:'%s'\n", sFormat);
            fprintf(fp, sFormat, data);
        /*****
        }
        else
        {
            return -1;
        }
        *****/
    }

    while (DFPcur) {
        if (DFPcur->NodeType == LOOP || DFPcur->NodeType == CONDITION) {
            CurData = DFPExportLoopCond(fp, CurData, DFPcur, "", out_filename);
        }
        else
        {

/*
if (strcmp_caseless(DFPcur->FieldName, "NUMT") == 0) {
    printf("fieldname NUMT\n");
}
*/

            CurData = DFPFillinField(DFPcur, CurData, out_filename);

            /* Dump Current field */
            if (DFPcur->bNonPrinting == FALSE) {
                if (DFPcur->DataType != DATA_BINARY) {

                    DFPPrintLine(fp, DFPcur, "");
/*
printf("%s = %s\n", DFPcur->FieldName, DFPcur->pFieldValue);
*/
                }
                else    /* Binary */
                {
                    fprintf(fp, "%s = ", DFPcur->FieldName);
                    for (x = 0; x < DFPcur->FieldWidth; x++) {
                        fprintf(fp, "%02Xh ", DFPcur->pFieldValue[x]);
                    }
                    fprintf(fp, " %s\n", DFPcur->units);
/*
                    printf("%s = ", DFPcur->FieldName);
                    for (x = 0; x < DFPcur->FieldWidth; x++) {
                        printf("%2X ", DFPcur->pFieldValue[x]);
                    }
                    printf("\n");
*/
                }
            }
        }
        DFPcur = DFPNext(DFPcur);
    }
    fclose(fp);
    return 0;
}



/*******************************************************************************
 * Name:    DFPExportDataFp
 *
 * Description: This function loads the DFP structure with the .dff file passed,
 *              and traverses the DFP structure, filling
 *              in the value fields for later use.  It outputs the contents to
 *              the output file as it goes.
 *
 * Parameters:
 *      dest_fp         File pointer for where to output to
 *      dff_filename    Name of the .DFF file to load into the DFP structure
 *      data            Pointer to the data to interpret / export
 *      bAutoIncrement  N/A
 *
 * Returns:
 *      0       Success
 *      -1      Failure
 ******************************************************************************/

int DFPExportDataFp(FILE *dest_fp, const char *dff_filename, char *data, int bAutoIncrement, int data_length)
{
    DFParseType *pt;
    DFParseType *DFPcur;
    char *CurData;
    //char *s;
    int x;
    //int count_num = 0;    /* individual number for an image with 2 or more of the same TRE */
    CurData = data;

    pt = ReadDefinitionFile(dff_filename);
    if (pt == NULL) {
        if (gOptions.debug) {
fprintf(dest_fp, "unknown TRE '%s':\n%s\n'\n", dff_filename, data);
        }
        else
        {
            /* no */
            return -1;
        }
    }

    DFPcur = pt;

    while (DFPcur) {
        if (DFPcur->NodeType == LOOP || DFPcur->NodeType == CONDITION) {
            CurData = DFPExportLoopCond(dest_fp, CurData, DFPcur, "", dff_filename);
        }
        else
        {
            CurData = DFPFillinField(DFPcur, CurData, dff_filename);

            /* Dump Current field */
            if (DFPcur->bNonPrinting == FALSE) {
                if (DFPcur->DataType == DATA_ASCII) {
                    DFPPrintLine(dest_fp, DFPcur, "");
/*                    printf("%s = %s\n", DFPcur->FieldName, DFPcur->pFieldValue); */
                }
                else    /* Binary */
                {
                    fprintf(dest_fp, "%s = ", DFPcur->FieldName);
                    for (x = 0; x < DFPcur->FieldWidth; x++) {
                        fprintf(dest_fp, "%02Xh ", DFPcur->pFieldValue[x]);
                    }
                    fprintf(dest_fp, " %s\n", DFPcur->units);
/*
                    printf("%s = ", DFPcur->FieldName);
                    for (x = 0; x < DFPcur->FieldWidth; x++) {
                        printf("%2X ", DFPcur->pFieldValue[x]);
                    }
                    printf("\n");
*/
                }
            }
        }
        DFPcur = DFPNext(DFPcur);
    }
    return 0;
}



/*******************************************************************************
 * Name:    DFPExportLoopCond
 *
 * Description: This function takes a pointer to a DFP loop or condition node,
 *              and exports
 *              data to the file according to the parameters of the loop or
 *              condition.
 *
 * Parameters:
 *      OutFp       Output file pointer
 *      Data        Pointer to current position in data to dump
 *      DFPStruct   Pointer to populated DFP structure
 *
 * Returns:
 *      Pointer to current position in the data
 ******************************************************************************/

char *DFPExportLoopCond(FILE *OutFp, char *Data, DFParseType *p, const char *parent_level_index, const char *output_filename)
{
    DFParseType *DFPcur;
    DFParseType *DFPtemp;
    char *s;
    char *s1;
    int FieldsToLoop;
    int LoopCount;
    int LoopNum;
    int x;
    char sLevelIndex[40];
    Data = DFPFillinField(p, Data, output_filename);

    if (p->NodeType == CONDITION) {
        if (DFPTestCondition(p) == FALSE) return Data;
    }

    s = Data;
    /* Look up # fields to loop */

/*
if (LoopCount == 100) {
    printf("100 loops\n");
}
*/

    DFPtemp = DFPGetLoopCondInfo(p, &FieldsToLoop, &LoopCount);
    for (LoopNum = 1; LoopNum <= LoopCount; LoopNum++) {
        strcpy(sLevelIndex, parent_level_index);
        s1 = strchr(sLevelIndex, 0);
        if (strlen(parent_level_index) > 0 && p->NodeType == LOOP) {
            sprintf(s1, "_%d", LoopNum);
        }
        else if (p->NodeType == LOOP) {
            sprintf(s1, "%d", LoopNum);
        }
        DFPcur = DFPtemp;
        while (DFPcur) {
            if (DFPcur->NodeType == LOOP || DFPcur->NodeType == CONDITION) {
                s = DFPExportLoopCond(OutFp, s, DFPcur, sLevelIndex, output_filename);
            }
            else
            {
                s = DFPFillinField(DFPcur, s, output_filename);

                /* Dump Current field */
                if (DFPcur->bNonPrinting == FALSE) {
                    if (DFPcur->DataType == DATA_ASCII) {
                        DFPPrintLine(OutFp, DFPcur, sLevelIndex);
/*                        printf("%s%s = %s\n", DFPcur->FieldName, sLevelIndex, DFPcur->pFieldValue); */
                    }
                    else    /* Binary */
                    {
                        /* Write to file */
                        fprintf(OutFp, "%s%s = ", DFPcur->FieldName, sLevelIndex);
                        for (x = 0; x < DFPcur->FieldWidth; x++) {
                            fprintf(OutFp, "%02Xh ", DFPcur->pFieldValue[x]);
                        }
                        fprintf(OutFp, "\n");

                        /* Print to screen for debugging */
/*
                        printf("%s = ", DFPcur->FieldName);
                        for (x = 0; x < DFPcur->FieldWidth; x++) {
                            printf("%2X ", DFPcur->pFieldValue[x]);
                        }
                        printf("\n");
*/
                    }
                }
            }
            DFPcur = DFPNext(DFPcur);
        }
    }
    return s;
}



/*******************************************************************************
 * Name:    DFPFillinField
 *
 * Description: This function fills in all applicable fields for the DFP node
 *              passed.
 *
 *              If the node is a regular field, it fills it from the current
 *              data pointer, and returns an updated data pointer.
 *
 *              If the node is a loop or condition field, and the node refers
 *              to another field, it searches for that field and uses it to
 *              fill in the node.
 *
 * Parameters:
 *      DFPStruct   Pointer to populated DFP structure
 *      data        Pointer to the current position in the input data
 *      output_filename  The output filename to provide a good error message
 *
 * Returns:
 *      pointer to current data position
 ******************************************************************************/

char *DFPFillinField(DFParseType *p, char *data, const char *output_filename)
{
    DFParseType *DFPTemp;
    int len;
    char sBuffer[1000];

    if (p == NULL) {
        errmessage("DFPFillinField: Error - p is a NULL pointer\n");
        iQuit(1);
    }

    if (data == NULL) {
        errmessage("DFPFillinField: Error - data is a NULL pointer\n");
        iQuit(1);
    }

    if (p->NodeType == FIELD) {
        if (p->DataType == DATA_ASCII) {
            /* Don't convert units here, convert on output */

            strncpy(p->pFieldValue, data, p->FieldWidth);
            p->pFieldValue[p->FieldWidth] = '\0';
            return (data + p->FieldWidth);
        }
        else if (p->DataType == DATA_BINARY 
                    || p->DataType == DATA_SIGNED_INT
                    || p->DataType == DATA_UNSIGNED_INT 
                    || p->DataType == DATA_FLOAT         ) {
            memcpy(p->pFieldValue, data, p->FieldWidth);
            return (data + p->FieldWidth);
        }
        else
        {
            sprintf(sBuffer, "Error - Unknown DataType %d\n", p->DataType);
            errmessage(sBuffer);
            iQuit(1);
        }
    }
    else if ((p->NodeType == CONDITION || p->NodeType == LOOP) &&
                                   p->FieldName[0] != '\0') {
        /* lookup & copy value */
        trim(p->FieldName);

        DFPTemp = DFPFindFieldName(p, p->FieldName);
        if (DFPTemp == NULL) {
            printf("DFPFillinField for %s: Error - Loop/Condition Node '%s' Number Of Items = %d Couldn't find reference %s\n", output_filename, p->FieldName, p->NumItems, p->FieldName);
            iQuit(1);
        }
        if (DFPTemp->pFieldValue == NULL) {
            printf("DFPFillinField for %s: Error - field found (%s) has no pFieldValue assigned.\n", output_filename, DFPTemp->FieldName);
            iQuit(1);
        }
        strcpy(sBuffer, DFPTemp->pFieldValue);

        if (p->pFieldValue) free(p->pFieldValue);
        /* copy that node's FieldValue to p->pFieldValue (allocate memory for it first!) */
        len = strlen(sBuffer) + 1;
        p->pFieldValue = (char *) malloc(len);
        if (p->pFieldValue == NULL) {
            errmessage("DFPFillinField: Error allocating memory\n");
            iQuit(1);
        }

        strcpy(p->pFieldValue, sBuffer);
    }
    return data;
}



/*******************************************************************************
 * Name:    DFPGetLoopCondInfo
 *
 * Description: This function returns a pointer to the loop or condition fields.
 *              It also
 *              passes back the # fields involved in the loop or condition and
 *              the # repetitions.
 *
 * ----------------------------------------------------------------------------
 * PRECONDITIONS:
 *     Loop Node: p->repetitions or p->FieldValue must be filled-in
 * ----------------------------------------------------------------------------
 *
 * Parameters:
 *      NumItemFields   #fields in loop (filled on return)
 *      LoopCount       # times to loop (1 in the case of a condition node)
 *                      (filled on return)
 *
 * Returns:
 *      pointer to loop fields
 ******************************************************************************/

DFParseType *DFPGetLoopCondInfo(DFParseType *p, int *NumItemFields, int *LoopCount)
{
    if (!p) {
        errmessage("DFPGetLoopCondInfo: NULL pointer passed\n");
        iQuit(1);
    }

    if (p->NumItems < 0) {
        errmessage("DFPGetLoopCondInfo: number of items attached to the loop / condition was not assigned.\n");
    }

    *NumItemFields = p->NumItems;

    /* Cases: */

    /* Static Loop Count */

    /* Conditional Loop Count (get from another field) */
    if (p->NodeType == LOOP) {
        if (p->Repetitions >= 0) {
            *LoopCount = p->Repetitions;
        }
        else
        {
            /* Check for bad data */
            if (p->pFieldValue == NULL) {
                errmessage("DFPGetLoopCondInfo: p->pFieldValue is NULL\n");
                iQuit(1);
            }
            trim(p->pFieldValue);

            /* If not initialized, 1st char will be a space, even though */
            /* the string was trimmed! */
            if (isdigit(p->pFieldValue[0]) == FALSE && p->pFieldValue[0] != ' ') {

                
                if (p->pFieldValue[0] == 0) {   /* Print a warning here! */
                    printf("Warning: DFPGetLoopCondInfo: Null found for # times to loop, using 0.\n");
                    printf("(FieldName '%s' NumItems %d (# fields/loops/conditions to execute in loop or condition))\n", p->FieldName, p->NumItems);
                }
                else
                {
                    printf("DFPGetLoopCondInfo: Trying to get the number of times to loop, but p->pFieldValue is not a number. (p->FieldName = %s, p->NumItems = %d, p->pFieldValue = '%s', p->pFieldValue[0] = %d)\n",
                            p->FieldName, p->NumItems, p->pFieldValue, p->pFieldValue[0]);
                    iQuit(1);
                }
            }

            *LoopCount = atoi(p->pFieldValue);
        }
    }
    else if (p->NodeType == CONDITION) {
        *LoopCount = 1;
    }
    else {
        errmessage("DFPGetLoopCondInfo: Error - called with a node that's not a loop or condition!\n");
        iQuit(1);
    }
    return p->LoopCondFields;
}



/*******************************************************************************
 * Name:    DFPTestCondition
 *
 * Description: This function tests the condition from the conditional node,
 *              and returns TRUE or FALSE.
 *
 * Parameters:
 *      DFPStruct   Pointer to populated DFP structure
 *
 * Returns:
 *      TRUE   Condition was met
 *      FALSE  Condition was not met
 ******************************************************************************/

int DFPTestCondition(DFParseType *p)
{
    char *s;
    char *s2;
    char sBuffer1[1000];
    int  correct_value;
    int  found;
    char sBuffer[1000];
    
    found = -1;
    strcpy(sBuffer, p->pCondition);
    if (strncmp_caseless(sBuffer, "if contains", 11) == 0) {
        s = &sBuffer[11];
        correct_value = TRUE;
    }
    else if (strncmp_caseless(sBuffer, "if not", 6) == 0) {
        s = &sBuffer[6];
        correct_value = FALSE;
    }
    else
    {
        sprintf(sBuffer1, "DFPTestCondition: Error - unknown condition (p->FieldName = %s, p->NumItems = %d, p->pCondition = '%s')\n", p->FieldName, p->NumItems, p->pCondition);
        errmessage(sBuffer1);
        iQuit(1);
    }

    /* Find out whether the value is in the list */
    while (s[0] != '\0') {
        trim(s);
        if (s[0] == '\0') break;
        if (s[0] != '"') {
            sprintf(sBuffer1, "Error in condition (p->FieldName = '%s', p->pCondition = '%s')\n", p->FieldName, p->pCondition);
            errmessage(sBuffer1);
            iQuit(1);
        }
        s++; /* Skip over (") */

        s2 = strchr(s, '"');
        if (s2 == NULL) {
            sprintf(sBuffer1, "Error in condition '%s': unmatched (\")s\n", p->pCondition);
            errmessage(sBuffer1);
            iQuit(1);
        }
        s2[0] = '\0';
        if (strcmp_caseless(s, p->pFieldValue) == 0) {
            found = TRUE;
            break;
        }
        found = FALSE;
        s = s2+1;
    }

    if (found == -1) {
        sprintf(sBuffer1, "DFPTestCondition: no strings to compare were found in condition (p->FieldName = %s, p->NumItems = %d, p->pCondition = '%s')\n", p->FieldName, p->NumItems, p->pCondition);
        errmessage(sBuffer1);
        iQuit(1);
    }

    /* Compare found vs condition */
    if (correct_value == found) return TRUE;
    return FALSE;
}



/*******************************************************************************
 * Name:    DFPPrintLine
 *
 * Description: This function "prints" a TRE line to the output file
 *
 * Parameters:
 *      fp      output file
 *      DFPcur  Pointer to populated DFP structure to print
 *
 * Returns:
 ******************************************************************************/

void DFPPrintLine(FILE *fp, DFParseType *DFPcur, const char *sLevelIndex)
{
    char sTemp[4096];
    //char sTemp1[1024];
    //char *s;

    fprintf(fp, "%s%s = ", DFPcur->FieldName, sLevelIndex);
    if (DFPcur->IoConversionFn[0] != '\0') {
        HandleConversions(DFPcur, sTemp); //->IoConversionFn, DFPcur->pFieldValue, sTemp, DFPcur->DataType);
    }
    else
    {
        switch(DFPcur->DataType) {
        case DATA_ASCII:
            strcpy(sTemp, DFPcur->pFieldValue);
            break;
        case DATA_FLOAT:
            if (DFPcur->FieldWidth == 4)
                sprintf(sTemp, "%f", *((float *) DFPcur->pFieldValue));
            else
                sprintf(sTemp, "%lf", *((double *) DFPcur->pFieldValue));
            break;
        case DATA_UNSIGNED_INT:
            if (DFPcur->FieldWidth == 2) {
                sprintf(sTemp, "%hu", *((unsigned short *) DFPcur->pFieldValue));
            }
            else
                sprintf(sTemp, "%lu", *((unsigned long *) DFPcur->pFieldValue));
            break;
        case DATA_SIGNED_INT:
            if (DFPcur->FieldWidth == 2)
                sprintf(sTemp, "%hd", *((signed short *) DFPcur->pFieldValue));
            else
                sprintf(sTemp, "%ld", *((signed long *) DFPcur->pFieldValue));
            break;
        default:
            printf("DFPPrintLine: Unknown DataType %d\n", DFPcur->DataType);
            break;
        }
    }

    fprintf(fp, "%s %s\n", sTemp, DFPcur->units);
}



/*******************************************************************************
 * Name:    HandleConversions
 *
 * Description: This function handles any necessary unit conversions, and puts
 *              the result in a string
 *
 * Parameters:
 *      function    String holding the function to perform
 *      sIn         Input string
 *      sOut        Output string
 *
 * Returns:
 ******************************************************************************/

void HandleConversions(DFParseType *DFPcur, char *sOut)
{
    double d;
    long     *pLSI;
    short    *pSSI;
    long unsigned  *pLUI;
    short unsigned *pSUI;
    float  *pF;
    double *pD;
    char *function = DFPcur->IoConversionFn;


    switch(DFPcur->DataType) {
    case DATA_ASCII:
        d = atof((char *) DFPcur->pFieldValue);
        break;
    case DATA_FLOAT:
        if (DFPcur->FieldWidth == 2) {
            pF = (float *) DFPcur->pFieldValue;
            d = *pF;
        }
        else
        {
            pD = (double *) DFPcur->pFieldValue;
            d = *pD;
        }
        break;
    case DATA_UNSIGNED_INT:
        if (DFPcur->FieldWidth == 2)
        {
            pSUI = (short unsigned int *) DFPcur->pFieldValue;
            d = *pSUI;
        }
        else
        {
            pLUI = (long unsigned int *) DFPcur->pFieldValue;
            d = *pLUI;
        }
        break;
    case DATA_SIGNED_INT:
        if (DFPcur->FieldWidth == 2) {
            pSSI = (short int *) DFPcur->pFieldValue;
            d = *pSSI;
        }
        else
        {
            pLSI = (long int *) DFPcur->pFieldValue;
            d = *pLSI;
        }
        break;
    default:
        printf("HandleConversions: datatype %d not handled\n", DFPcur->DataType);
        d = -1;
        break;
    }
    
    
    if (strcmp_caseless(function, "ft2m") == 0) {
        d *= 0.3048;
        sprintf(sOut, "%f", d);
    }
    else if (strcmp_caseless(function, "m2ft") == 0) {
        d *= 3.28083;
        sprintf(sOut, "%f", d);
    }
    else
    {
        printf("HandleConversions: Unknown conversion function \"%s\", ignoring.\n", function);
        sprintf(sOut, "%f", d);
    }
}

