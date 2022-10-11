/* V */
/* defines.h - defines for NITF file extraction pgm */

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
*       Defines for NITF file extraction pgm
*
* Revision History:
*       DATE - DESCRIPTION OF CHANGE - PROGRAMMER'S INITIALS
******************************************************************************/

#include "nitf_s20.h"
/*#include "nitf_s21.h" -- THIS IS INCLUDED BY 2.0 HEADER */

#define EXTRACT_VERSION "7.5"

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0
#endif

#define bool short unsigned int

#define V2_0 10
#define V2_1 11


#define JPEG_FILES       2
#define IMAGE_DATA_FILES 4


#define DO_1_TRE         1
#define DO_ALL           2
#define DO_ALL_PROMPT    4
#define PROMPT_FILENAMES 8
#define TRE_NAMES        16
#define WRITE_FILES      32
#define CHECK_MINIMUM_EXTRACT_VERSION 64

#define LOOP 1
#define FIELD 2
#define CONDITION 4

#define DATA_BINARY 1
#define DATA_ASCII 2
#define DATA_SIGNED_INT 3
#define DATA_UNSIGNED_INT 4
#define DATA_FLOAT 5
//#define ASCII DATA_ASCII
//#define BINARY DATA_BINARY

#define NITF_AND_IMAGES 10
#define DUMP_ALL 11
#define DUMP_TRES 12

#define QPM 1

extern int gTreVersion;

/* Values for gTreVersion */
#define TRE_STANDARD 9999
#define TRE_AIRBORNE_0_9A 2
#define TRE_ILLEGAL_NAME -1

typedef struct {
    int bWriteImageInfoFile;
    int bVeridianNitfFile;
    int extract_mode;
    int dump_tres;
    int mag_only;
    int dump_ssummary;
    int dump_geo;
    int dump_qpm;    /* quarter power magnitude */
    int dump_text_only;
    int dump_jpeg_only;
    int dump_raw_only;
    int dump_tre_only;

    int dump_text;     /* Set by dump_text_only, dump_* */
    int dump_jpeg;     /* Set by dump_jpeg_only, dump_* */
    int dump_raw;      /* set by dump_raw_only, dump_* */
    int dump_tre;      /* set by dump_tre_only, dump_* */

    int dump_phoenix_tre;
    int bands;
    int added_pad_pixels;
    int gui_mode;       /* True to save GUI information */
    int dump_standard_filenames;
    int debug;
} options_type;

typedef struct {
    int rgb_mode;
    int ffc0_remove;
    int ffda_remove;
    int ffdb_remove;
    int ffe6_remove;
    char ffc0_data[200];
    char ffda_data[200];
    char ffdb_data[200];
    char ffe0_data[200];
} jpeg_info_type;


typedef struct {
   long length_of_subheader;
   long nitf_file_offset;
   long length_of_data;
   bool bFile_written;
/*   char *pData; */
   } image_info_type;

typedef struct {
   long length_of_subheader;
   long length_of_data;
   bool bFile_written;
   char *pData;
   } segment_info_type;

/* This structure holds information to help the input/output translation */
/* process.  */
typedef struct {
    char name[40+1];       /* NITF field name */
//    char location [30+1];  /* Location */ 
    int length;            /* size of field in characters */
    bool required;         /* TRUE if required */
    char *ptr;             /* pointer to start of field */
    } nitf_field_info_type;

typedef struct {
    char location[21];
    nitf_field_info_type *ptr_first_field;
    } nitf_location_list_type;


/*
[LOOP][000000000000020][00003]     //(OK) Static Loop Instruction example
[LOOP][REQORGREP      ][00001]     //(OK) Conditional Loop Instruction example

[if contains “c3” “c4” “c5”] [fieldname] [00002]
[if not “c3” “c4” “c5”] [fieldname] [00002]

*/


/* Variables for Parsing Definition Files */
typedef struct ParseStruct{
    /* General attributes */
    struct ParseStruct *Next;
    struct ParseStruct *Prev;
    struct ParseStruct *Parent;
    int bNonPrinting;
    int NodeType;    /* {LOOP, FIELD, CONDITION (loop of repetition 1} */
    char *pFieldValue;  /* FIELD - holds value read for that field */
                        /* LOOP (Static) - holds NULL in first character */
                        /* CONDITION, LOOP (Dynamic) - holds value from field look-up */

    /* Field attributes */
    char FieldName[80]; /* FIELD - holds name for that field */
                        /* LOOP (Static) - holds NULL in first character */
                        /* CONDITION, LOOP (Dynamic) - holds name of field to look-up */

    int FieldWidth;
    int DataType;       /* DATA_BINARY, DATA_ASCII, DATA_SIGNED_INT, DATA_UNSIGNED_INT, DATA_FLOAT */
    char units[40];
    char IoConversionFn[80];

    /* Loop / Condition Attributes */
    int Repetitions;    /* LOOP, CONDITION (Static) - holds fixed value */
                        /* LOOP, CONDITION (Dynamic) - holds -1 */

    int NumItems;       /* LOOP, CONDITION - holds # fields/loops/conditions */
                        /* to execute in loop or condition */

    char *pCondition;   /* Hold condition text separately */

    struct ParseStruct *LoopCondFields;  /* Points to loop/condition fields */
} DFParseType;


//****************************************
// EXTERNs
//****************************************
extern long main_user_def_header_data_len;
extern char *main_user_def_data;

extern long main_extended_header_data_len;
extern char *main_extended_hdr_data;

/*extern nitf_20_main_type_a nitf_20_main_a; */
extern char sOutputPath[260];
extern char sDFFpath[260];
//extern char sAirborne9aDFFpath[260];

extern char sNITFfilename[255];
extern char ssummary_filename[256];
extern int bExtractAllTextFiles;
extern options_type gOptions;
extern char Gstr[1024];

extern image_info_type *image_info;
extern jpeg_info_type  jpeg_info;
/* extern float bandsa_version; */

extern int hNITF;   /* handle to NITF file */

extern nitf_20_main_type nitf_20_main;
extern nitf_21_main_type nitf_21_main;

extern nitf_20_imagesub_type *i20hdr;
extern nitf_21_imagesub_type *i21hdr;

extern nitf_20_symbolsub_type *s20hdr;

extern nitf_20_labelsub_type *l20hdr;

extern nitf_21_graphicsub_type *g21hdr;

extern nitf_20_textsub_type *t20hdr;
extern nitf_21_textsub_type *t21hdr;

extern nitf_20_dessub_type *d20hdr;
extern nitf_21_dessub_type *d21hdr;


/* V2_0, V2_1 */
extern int number_of_images;
extern image_info_type *image_info;

/* V2_0 */
extern int number_of_symbols;
extern segment_info_type *symbol_info;

/* V2_1 */
extern int number_of_graphics;
extern segment_info_type *graphics_info;

/* V2_0 */
extern int number_of_labels;
extern segment_info_type *label_info;

/* V2_0, V2_1 */
extern int number_of_text_files;
extern segment_info_type *text_info;

/* V2_0, V2_1 */
extern int number_of_DESs;
extern segment_info_type *des_info;

/* V2_0, V2_1 */
extern int number_of_res;
extern segment_info_type *res_info;


extern int iNITF_version;

extern long main_user_def_header_data_len;
extern int gTreIllegalNameCount;


/*************************/
/*                       */
/*  FUNCTION PROTOTYPES  */
/*                       */
/*************************/

void iQuit(int num);
void errmessage(const char *s);
void print_usage(void);

/* Definitions from misc.c */

int read_nonblank_line(FILE *pInputFile, char *pLine, int iMaxLen);
int uread_nonblank_line(int hInputFile, char *pLine, int iMaxLen);
int read_line(FILE *f);
int uread_line(int hf);
int get_line(FILE *f, char *s, int len);
int uget_line(int hf, char *s, int len);
void remove_trailing_spaces(char *s);
int index_caseless(char *string_to_search, char *string_to_find);
int strncmp_caseless (const char *str1, const char *str2, int n);
int strlinecount(char *s);
int strsetlower(char *s);
void wrap_lines(int max_columns, char *sIn, char *sOut, int starting_column);

int getContents(char *location, int offset, int length, char *result);
long read_verify(int fh, char *destination, long length, const char *sErrorMessage);
long write_verify(int fh, char *source, long length, const char *sErrorMessage);
long skip_read(int fh, char *destination, long length, const char *sErrorMessage);
void dump_header(int hNITF);
int read_image_data(int number_of_images, image_info_type *image_info);
int read_symbol_data(int number_of_symbols, segment_info_type *symbol_info);
int read_label_data(int number_of_labels, segment_info_type *label_info);
int read_text_data(int number_of_text_files, segment_info_type *text_info);
int read_DES_data(int number_of_DESs, segment_info_type *DES_info);
void dump_str(const char *name, char *source, int length);
void fDumpStr(FILE *fp, const char *name, char *source, int length);
void dump_bytes_decimal(char *name, char *source, int length);

int read_graphic_data(int number_of_graphics, segment_info_type *graphic_info);
int parse_phoenix_line(char *line, char *variable, char *value);

/* Defines for write_fs.c */

void write_output_files(char *fname, int doSeparate);
void write_special_files(void);
int find_text_tag(const char *sFindTag, char *sText, char *sTagFound);
void write_SAR_image(int text_num);
int get_stored_line(char *sText, char *sLine);
void NITF_l_to_s(char *s, long l);
long s_to_NITF_l(char *str);
long get_long(char *s, int len);
/* Defines for menu.c */

int menu(char* fname, int *numbers, char *new_fname);
int open_write_append_ufile(char *filename);
int open_write_clear_ufile(char *filename);
int open_read_ufile(char *filename);
void copy_fh_2_fh(int fh_out, int fh_in, long len /*, bool bMD5*/);
void write_text_data(int fh, int text_num);
void write_image_data(int fh, int image_num, char *filename);
void write_image_files(int image_types);
void print_tre_list(int image_num);
void print_inventory(void);
void trim(char *s);
int menu2(char* fname, char *new_fname);
int write_screen_lines(int bReset, int max_lines, const char *sLine);
void write_JPEG_image(int text_num);
unsigned char read_byte(FILE *fp);
void write_byte(FILE *fp, unsigned char c);
int strcmp_caseless (const char *str1, const char *str2);
void rebuild_jpeg(int image_num);
int verify_filename(char *filename);
DFParseType *CreateDFParse(void);
void DestroyDFParse(DFParseType *pDFP);
DFParseType *DFPNext(DFParseType *pDFP);
DFParseType *DFPPrev(DFParseType *pDFP);
DFParseType *DFPLoopFields(DFParseType *pDFP);
DFParseType *DFPParent(DFParseType *pDFP);
DFParseType *DFPCreateNext(DFParseType *pDFP);
DFParseType *DFPCreateNextLoopMarker(DFParseType *pDFP);
DFParseType *DFPLast(DFParseType *pDFP);
DFParseType *DFPFirst(DFParseType *pDFP);
DFParseType *DFPAddField(DFParseType *pDFP, char *FieldName, int FieldWidth, int DataType, int bNonPrinting, char *IoConversionFn);
void DFPLineHelp(void);
DFParseType *DFPCreate(void);
void DFPAttachNext(DFParseType *Node, DFParseType *NodeToAttach);
DFParseType *ReadDefinitionFile(const char *FileName);
DFParseType *DFPReadDefinitions(FILE *fp, DFParseType *parent, int RequiredItems, int *rc);
void DFPAttachTagToLoopField(DFParseType *Node, DFParseType *NodeToAttach);
void unblock_image(int fh_in, int fh_out, int nbpp,
        long pixels_h, long pixels_v, int blocks_h, int blocks_v,
        int ppb_h, int ppb_v, int bands, int flag);
char *DFPExportLoopCond(FILE *OutFp, char *Data, DFParseType *DFPStruct, const char *parent_level_index, const char *output_file);
int DFPParseLoop(DFParseType *p, char *sBuffer);
int DFPParseCondition(DFParseType *p, char *sBuffer);
int DFPParseTag(DFParseType *p, char *sBuffer);
int DFPTestCondition(DFParseType *DFPStruct);
void get_string(char *dest, char *s, int len);
DFParseType *DFPFindFieldName(DFParseType *DFPcur, char *FieldName);
char *DFPFillinField(DFParseType *p, char *data, const char *output_filename);
void write_text_files_separately(void);
void write_nitf_headers(void);
void write_tres(void);
DFParseType *DFPGetLoopCondInfo(DFParseType *p, int *NumItemFields, int *LoopCount);
void dump_tres(FILE *dest_fp);
int file_exists(char *filename);
void dump_image_information(int number_of_images);
int DFPExportData(char *out_filename, const char *dff_filename, char *data, int bAutoIncrement, int data_len);
int DFPExportDataFp(FILE *dest_fp, const char *dff_filename, char *data, int bAutoIncrement, int data_length);
void write_meta_data(int fh, int text_num);
void loop_through_TREs(FILE *dest_fp, int image_num, int options, const char *tre_to_dump);
FILE *open_write_append(char *filename);
void dump_ssummary(char *filename);
void fDumpChars(FILE *fp, char *source, int length);
void loop_through_TREs_to_file(FILE *fp, int image_num, int options, char *tre_to_dump);
void do_loop_through_TREs_to_file(FILE *fp, int image_num, char *sData, long total_length, int options, const char *tre_to_dump);
long get_file_length(char *filename);
void parse_igeolo20(char icords, char *s_in, char *s_out);
void parse_igeolo21(char icords, char *s_in, char *s_out);
void dump_geo(void);
FILE *open_write_file(char *filename);
void qpm_data(float *f, int read_count);
void copy_fh_2_fh2(int fh_out, int fh_in, long len, int flag);
void do_loop_through_TREs(FILE *dest_fp, int image_num, char *sData, long total_length,
                            int options, const char *tre_to_dump);
void DFPPrintLine(FILE *fp, DFParseType *DFPcur, const char *sLevelIndex);
void HandleConversions(DFParseType *DFPcur, char *sOut); //char *function, char *sIn, char *sOut);
void set_long(char *dest, long v, int len);
void write_nitf_inventory_file(void);
void local_filename(char *filename);
void format_nitf21datetime(char *value, char *out);
void nitf20dt_2_nitf21dt(char *value);
void write_phoenix_files(char *fname);

#ifdef USE_MD5
void MD5DigestCopy (unsigned char digest[16], char *dest);
void md5_subfile(int fh_in, long file_start_offset, long subfile_len, char *sResult);
#endif

