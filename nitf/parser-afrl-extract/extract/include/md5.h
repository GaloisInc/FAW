/* MD5.H - header file for MD5C.C */
/* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.
License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.
License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.
RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.
These notices must be retained in any copies of any part of this
documentation and/or software. */

/* MD5 context. */
typedef struct {
  UINT4 state[4];                                   /* state (ABCD) */
  UINT4 count[2];        /* number of bits, modulo 2^64 (lsb first) */
  unsigned char buffer[64];                         /* input buffer */
} MD5_CTX;

void MD5Transform(UINT4 [4], unsigned char [64]);
void Encode (unsigned char *, UINT4 *, unsigned int);
void Decode (UINT4 *, unsigned char *, unsigned int);
void MD5_memcpy(POINTER, POINTER, unsigned int);
void MD5_memset(POINTER, int, unsigned int);
void MD5Init (MD5_CTX *context);
void MD5Update (MD5_CTX *context,   /* context */
    unsigned char *input,           /* input block */
    unsigned int inputLen);          /* length of input block */
void MD5Final (unsigned char digest[16],       /* message digest */
        MD5_CTX *context);                      /* context */
void MD5Transform (UINT4 state[4], unsigned char block[64]);
void Encode (unsigned char *output, UINT4 *input, unsigned int len);
void Decode (UINT4 *output, unsigned char *input, unsigned int len);
void MD5_memcpy (POINTER output, POINTER input, unsigned int len);
void MD5_memset (POINTER output, int value, unsigned int len);


