import json
import os
from pprint import pprint
import pymongo
import subprocess

pdf_etl_parse_dir = os.path.normpath(os.path.join(os.path.abspath(__file__), '..', '..'))
main_py = os.path.join(pdf_etl_parse_dir, 'main.py')
pdf_config_path = os.path.join(pdf_etl_parse_dir, '..', '..', 'pdf', 'config.json5')

def call(args):
    p = subprocess.Popen(['python', main_py] + args, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    return p.wait(), stdout.decode(), stderr.decode()


def call_parser(doc):
    client = pymongo.MongoClient()
    addr = lambda c: f'localhost:27017/test/{c.name}'
    s = client['test']['test_src']
    q = client['test']['test_queue']
    d = client['test']['test_dst']

    s.drop()
    q.drop()
    d.drop()

    s.insert_one(doc)

    r = call(['-s', addr(s), '-q', addr(q),'-d', addr(d),
            '--config', pdf_config_path, '--run-one', doc['_id']])
    print(f'stdout\n{r[1]}\n\nstderr\n{r[2]}')
    assert r[0] == 0
    assert r[1] == ''

    result = d.find_one(doc['_id'])
    pprint(result)
    return result


def test_caradoc():
    doc = json.loads(r'''
    {
            "_id" : "5dce06d1e64d1e0001000006",
            "invoker" : {
                    "exec" : "caradoc",
                    "preArgs" : [
                            "stats"
                    ],
                    "postArgs" : [ ],
                    "timeoutScale" : 18,
                    "version" : "caradoc-0.3",
                    "invName" : "caradoc-stats",
                    "_cons" : "Invoker"
            },
            "file" : "/home/pdf-files/000009.pdf",
            "result" : {
                    "exitcode" : 2,
                    "timeElapsed" : 1,
                    "stdoutRes" : "Version : 1.4\nIncremental updates : 1\nObject count : 64\nFilter : FlateDecode -> 18 times\nFilter : Raw -> 1 times\nObjects of known type : 52\nKnown type rate : 0.812500\nSome types were not fully checked\nGraph has no known error\n/Producer : ()\n/Creator : (XPP)\n/CreationDate : (D:20020425130224Z)\n/ModDate : (D:20020425141417-03'00')\n/ID : <B39F67502B96D526A14E627A7C076E62>\n",
                    "stderrRes" : "Warning : Flate/Zlib stream with appended newline in object 3\nWarning : Flate/Zlib stream with appended newline in object 6\nWarning : Flate/Zlib stream with appended newline in object 9\nPDF error : Syntax error in content stream at offset 2447 [0x98f] in stream 45 !\n",
                    "_cons" : "GoodResult"
            },
            "_cons" : "Invocation"
    }
    ''')

    doc_strict = json.loads(r'''
    {
            "_id" : "5dce06d1e64d1e0001000007",
            "invoker" : {
                    "exec" : "caradoc",
                    "preArgs" : [
                            "stats",
                            "--strict"
                    ],
                    "postArgs" : [ ],
                    "timeoutScale" : 18,
                    "version" : "caradoc-0.3",
                    "invName" : "caradoc-strict",
                    "_cons" : "Invoker"
            },
            "file" : "/home/pdf-files/000009.pdf",
            "result" : {
                    "exitcode" : 2,
                    "timeElapsed" : 1,
                    "stdoutRes" : "Not a PDF file\n",
                    "stderrRes" : "PDF error : Syntax error at offset 183 [0xb7] in file !\n",
                    "_cons" : "GoodResult"
            },
            "_cons" : "Invocation"
    }
    ''')

    r = call_parser(doc)
    assert r['result']['Filter : FlateDecode'] == 1
    r = call_parser(doc_strict)
    assert r['result']['Not a PDF file'] == 1
    assert r['result']['PDF error : Syntax error at offset  [] in file !'] == 1


def test_pdfid():
    doc = json.loads(r'''{ "_id" : "5dc351a60923be6f86000005", "invoker" : { "exec" : "python", "invName": "pdfid", "preArgs" : [ "../lib/pdfid_v0_2_5/pdfid.py", "-e" ], "postArgs" : [ ], "timeoutScale" : 18, "version" : "pdfid_v0_2_5", "_cons" : "Invoker" }, "file" : "../test/data/000009.pdf", "result" : { "exitcode" : 0, "timeElapsed" : 1, "stdoutRes" : "PDFiD 0.2.5 ../test/data/000009.pdf\n PDF Header: %PDF-1.4\n obj                   64\n endobj                64\n stream                19\n endstream             19\n xref                   2\n trailer                2\n startxref              2\n /Page                  4\n /Encrypt               0\n /ObjStm                0\n /JS                    0\n /JavaScript            0\n /AA                    0\n /OpenAction            1\n /AcroForm              0\n /JBIG2Decode           0\n /RichMedia             0\n /Launch                1\n /EmbeddedFile          0\n /XFA                   0\n /URI                   0\n /Colors > 2^24         0\n %%EOF                  2\n After last %%EOF       0\n D:20020425141417-03'00  /ModDate\n Total entropy:           7.688588 (     39586 bytes)\n Entropy inside streams:  7.978351 (     30298 bytes)\n Entropy outside streams: 4.970143 (      9288 bytes)\n\n", "stderrRes" : "", "_cons" : "GoodResult" }, "_cons" : "Invocation" }''')

    dst = call_parser(doc)
    assert dst['result'].get('/Colors > 2^24', 0) == 0
    assert dst['result'].get('/OpenAction', 0) == 1
    assert dst['result'].get('Entropy outside streams', 0) == 1


def test_pdfinfo():
    doc_norm = json.loads(r'''{ "_id" : "5dc5e21ee64d1e000100006c", "invoker" : { "exec" : "pdfinfo", "invName": "pdfinfo", "preArgs" : [ ], "postArgs" : [ ], "timeoutScale" : 3, "version" : "poppler-0.84.0", "_cons" : "Invoker" }, "file" : "/home/pdf-files/099692.pdf", "result" : { "exitcode" : 0, "timeElapsed" : 1, "stdoutRes" : "Title:          Microsoft Word - AppendixA.doc\nAuthor:         Charles Turner\nCreator:        Microsoft Word: PSPrinter 8.3.1\nProducer:       Acrobat Distiller 5.0.5 for Macintosh\nCreationDate:   Mon May  6 19:56:50 2002 UTC\nModDate:        Tue Jan 28 18:35:23 2003 UTC\nTagged:         yes\nUserProperties: no\nSuspects:       no\nForm:           AcroForm\nJavaScript:     no\nPages:          24\nEncrypted:      no\nPage size:      612 x 792 pts (letter)\nPage rot:       0\nFile size:      380734 bytes\nOptimized:      no\nPDF version:    1.4\n", "stderrRes" : "", "_cons" : "GoodResult" }, "_cons" : "Invocation" }''')
    doc_struct = json.loads(r'''{
            "_id" : "5dc5e220e64d1e000100007a",
            "invoker" : {
                    "exec" : "pdfinfo",
                    "invName": "pdfinfo-struct",
                    "preArgs" : [
                            "-struct"
                    ],
                    "postArgs" : [ ],
                    "timeoutScale" : 18,
                    "version" : "poppler-0.84.0",
                    "_cons" : "Invoker"
            },
            "file" : "/home/pdf-files/974733.pdf",
            "result" : {
                    "exitcode" : 0,
                    "timeElapsed" : 1,
                    "stdoutRes" : "Part\n  H1 (block):\n     /SpaceAfter 15.875\n     /TextAlign /Center\n  P (block):\n     /SpaceBefore 15.875\n     /SpaceAfter 15\n     /TextAlign /Center\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n     /TextAlign /Center\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 30\n     /TextAlign /Center\nPart\n  H1 (block):\n     /SpaceBefore 30\n     /SpaceAfter 15.75\n  P (block):\n     /EndIndent 6\n     /SpaceBefore 15.75\n     /SpaceAfter 18.75\nPart\n  P (block):\n     /SpaceBefore 18.75\n     /SpaceAfter 15.75\n  P (block):\n     /SpaceBefore 15.75\n     /SpaceAfter 45\n  P (block):\n     /EndIndent 8.875\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 4.5\n     /SpaceBefore 15\n     /SpaceAfter 75\n  Figure:\n     /BBox [351 597 533 718]\n     /InlineAlign /End\n     /Placement /Block\n     /StartIndent 306.375\n     /EndIndent 31.25\n     /SpaceBefore 11.125\n     /SpaceAfter 5.5\nPart\n  H1 (block):\n     /SpaceBefore 31.5\n     /SpaceAfter 15.875\n  P (block):\n     /EndIndent 9.5\n     /SpaceBefore 15.875\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n    Span (inline):\n       /BaselineShift -1.37498\n  Figure:\n     /BBox [160 57 438 718]\n     /InlineAlign /Center\n     /Placement /Block\n  Table (block):\n     /BBox [45 357 700 548]\n     /InlineAlign /Center\n     /Placement /Block\n     /SpaceAfter 2.25\n    P \"��\" (block)\nPart\n  H1 (block):\n     /SpaceAfter 15.875\n  P (block):\n     /SpaceBefore 15.875\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 21.75\n  Figure \"��\"\n  P (block):\n     /EndIndent 9.75\n     /SpaceAfter 15\n    Link (inline)\n      Object 51 0\n      Span (inline):\n         /TextDecorationType /Underline\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 45\n  Figure:\n     /BBox [71 181 546 713]\n     /InlineAlign /Center\n     /Placement /Block\n     /SpaceAfter 2.875\n  P (block)\n  P (block):\n     /EndIndent 5.75\n     /SpaceAfter 16.5\nPart\n  P (block):\n     /SpaceBefore 16.5\n     /SpaceAfter 15.75\n  P (block):\n     /EndIndent 12.5\n     /SpaceBefore 15.75\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 6.75\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 17.125\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 14\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 62.875\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 18.625\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 4.375\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 5\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 3.875\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 7.5\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 23.25\n     /SpaceBefore 15\n     /SpaceAfter 15\n  P (block):\n     /EndIndent 10.5\n     /SpaceBefore 15\n     /SpaceAfter 14.25\n  P (block):\n     /SpaceBefore 14.25\n     /SpaceAfter 228\n",
                    "stderrRes" : "Syntax Error: Nums item at position 20 is invalid value (11): [0..10]\nSyntax Warning: K in StructTreeRoot has more than one children in a tagged PDF\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\nSyntax Warning: Wrong Attribute 'LineHeight' in element P\n",
                    "_cons" : "GoodResult"
            },
            "_cons" : "Invocation"
    }
    ''')
    doc_meta = json.loads(r'''{
            "_id" : "5dc5e220e64d1e0001000079",
            "invoker" : {
                    "exec" : "pdfinfo",
                    "invName": "pdfinfo-meta",
                    "preArgs" : [
                            "-meta"
                    ],
                    "postArgs" : [ ],
                    "timeoutScale" : 3,
                    "version" : "poppler-0.84.0",
                    "_cons" : "Invoker"
            },
            "file" : "/home/pdf-files/974733.pdf",
            "result" : {
                    "exitcode" : 0,
                    "timeElapsed" : 1,
                    "stdoutRes" : "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d' bytes='1389'?>\n\n<rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'\n xmlns:iX='http://ns.adobe.com/iX/1.0/'>\n\n <rdf:Description about=''\n  xmlns='http://ns.adobe.com/pdf/1.3/'\n  xmlns:pdf='http://ns.adobe.com/pdf/1.3/'>\n  <pdf:Creator>C:\\WINDOWS\\Desktop\\APPROVED FINALIZED PAPERS\\24MiltnerFINAL2ndB</pdf:Creator>\n  <pdf:CreationDate>2003-02-04T09:28:50Z</pdf:CreationDate>\n  <pdf:Title>PAPERS\\24MiltnerFINAL2ndB</pdf:Title>\n  <pdf:Author>pheimbro</pdf:Author>\n  <pdf:Producer>Acrobat PDFWriter 4.0 for Windows</pdf:Producer>\n  <pdf:ModDate>2003-02-04T09:37:36-05:00</pdf:ModDate>\n </rdf:Description>\n\n <rdf:Description about=''\n  xmlns='http://ns.adobe.com/xap/1.0/'\n  xmlns:xap='http://ns.adobe.com/xap/1.0/'>\n  <xap:CreateDate>2003-02-04T09:28:50Z</xap:CreateDate>\n  <xap:Title>\n   <rdf:Alt>\n    <rdf:li xml:lang='x-default'>PAPERS\\24MiltnerFINAL2ndB</rdf:li>\n   </rdf:Alt>\n  </xap:Title>\n  <xap:Author>pheimbro</xap:Author>\n  <xap:ModifyDate>2003-02-04T09:37:36-05:00</xap:ModifyDate>\n  <xap:MetadataDate>2003-02-04T09:37:36-05:00</xap:MetadataDate>\n </rdf:Description>\n\n <rdf:Description about=''\n  xmlns='http://purl.org/dc/elements/1.1/'\n  xmlns:dc='http://purl.org/dc/elements/1.1/'>\n  <dc:title>PAPERS\\24MiltnerFINAL2ndB</dc:title>\n  <dc:creator>pheimbro</dc:creator>\n </rdf:Description>\n\n</rdf:RDF>\n<?xpacket end='r'?>\n",
                    "stderrRes" : "",
                    "_cons" : "GoodResult"
            },
            "_cons" : "Invocation"
    }
    ''')
    dst = call_parser(doc_norm)
    assert dst['result'].get('Author', 0) > 0
    assert dst['result'].get('CreationDate', 0) > 0
    assert dst['result'].get('Encrypted', 0) == 0
    assert dst['result'].get('Title', 0) > 0
    dst = call_parser(doc_struct)
    assert dst['result'].get('struct P (block)', 0) > 0
    dst = call_parser(doc_meta)
    assert dst['result'].get('meta not implemented', 0) > 0


def test_pdftocairo():
    doc = json.loads(r'''{
            "_id" : "5dab2c4c729e7678e60152a6",
            "invoker" : {
                    "exec" : "pdftocairo",
                    "preArgs" : [
                            "-pdf"
                    ],
                    "postArgs" : [
                            ".temp_pdf-etl-tools.pdf"
                    ],
                    "timeoutScale" : 18,
                    "version" : "poppler-0.84.0",
                    "_cons" : "Invoker",
                    "invName" : "pdftocairo-pdf"
            },
            "file" : "/media/data/raw/govdocs/236033.pdf",
            "result" : {
                    "exitcode" : 0,
                    "timeElapsed" : 1,
                    "stdoutRes" : "",
                    "stderrRes" : "Syntax Error (2515): Unknown operator 'Qq'\nSyntax Error (39493): Unknown operator 'Qq'\n",
                    "_cons" : "GoodResult"
            },
            "_cons" : "Invocation"
    }
    ''')
    dst = call_parser(doc)
    assert dst['result'].get("Syntax Error (): Unknown operator 'Qq'", 0) == 2


def test_qpdf():
    doc = json.loads(r'''{ "_id" : "5dc5e220e64d1e000100007c", "invoker" : { "exec" : "qpdf", "invName": "qpdf-check", "preArgs" : [ "--check" ], "postArgs" : [ ], "timeoutScale" : 18, "version" : "8.0.2", "_cons" : "Invoker" }, "file" : "/home/pdf-files/974733.pdf", "result" : { "exitcode" : 2, "timeElapsed" : 1, "stdoutRes" : "checking /home/pdf-files/974733.pdf\nPDF Version: 1.4\nR = 3\nP = -1324\nUser password = \nextract for accessibility: allowed\nextract for any purpose: allowed\nprint low resolution: allowed\nprint high resolution: allowed\nmodify document assembly: not allowed\nmodify forms: not allowed\nmodify annotations: not allowed\nmodify other: not allowed\nmodify anything: not allowed\nFile is linearized\nWARNING: end of first page section (/E) mismatch: /E = 10457; computed = 10137..10138\nWARNING: page 0 has shared identifier entries\nWARNING: page 0: shared object 151: in hint table but not computed list\nWARNING: object count mismatch for page 2: hint table = 3; computed = 10\nWARNING: page 2: shared object 41: in hint table but not computed list\nWARNING: page 2: shared object 42: in hint table but not computed list\nWARNING: page 2: shared object 45: in hint table but not computed list\nWARNING: page 2: shared object 46: in hint table but not computed list\nWARNING: page 2: shared object 47: in hint table but not computed list\nWARNING: page 2: shared object 63: in hint table but not computed list\nWARNING: page 2: shared object 64: in hint table but not computed list\nWARNING: object count mismatch for page 4: hint table = 3; computed = 10\nWARNING: page 4: shared object 43: in hint table but not computed list\nWARNING: page 4: shared object 44: in hint table but not computed list\nWARNING: page 4: shared object 48: in hint table but not computed list\nWARNING: page 4: shared object 49: in hint table but not computed list\nWARNING: page 4: shared object 50: in hint table but not computed list\nWARNING: page 4: shared object 65: in hint table but not computed list\nWARNING: page 4: shared object 66: in hint table but not computed list\nWARNING: page 6: shared object 51: in hint table but not computed list\nWARNING: page 6: shared object 52: in hint table but not computed list\nWARNING: object count mismatch for page 7: hint table = 7; computed = 9\nWARNING: page 7: shared object 67: in hint table but not computed list\nWARNING: page 7: shared object 68: in hint table but not computed list\n", "stderrRes" : "", "_cons" : "GoodResult" }, "_cons" : "Invocation" }''')
    dst = call_parser(doc)
    assert dst['result'].get('WARNING: page : shared object : in hint table but not computed list', 0) > 0
    assert dst['result'].get('extract for accessibility', 0) > 0
    assert dst['result'].get('modify forms', 0) == 0

