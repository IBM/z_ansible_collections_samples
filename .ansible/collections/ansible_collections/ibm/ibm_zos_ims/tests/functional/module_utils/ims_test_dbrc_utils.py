from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


class DBRCInputParameters():
    HLQ1 = "IMSTESTL."
    HLQ2 = "IMS1."
    STEPLIB = [
        "IMSTESTU.IMS1501.MARKER",
        HLQ1 + HLQ2 + "EXITLIB",
        "IMSTESTG.IMS15R.TSTRES",
        "IMSBLD.IMS15R.USERLIB",
        "IMSBLD.I15RTSMM.CRESLIB"
    ]
    DBD_LIB = HLQ1 + HLQ2 + "DBDLIB"
    DYNALLOC = HLQ1 + HLQ2 + "SDFSRESL"
    GENJCL_INPUT_DS = HLQ1 + HLQ2 + "PROCLIB"
    GENJCL_OUTPUT_DS = HLQ1 + HLQ2 + "JCLOUT"
    RECON1 = HLQ1 + HLQ2 + "RECON1"
    RECON2 = HLQ1 + HLQ2 + "RECON2"
    RECON3 = HLQ1 + HLQ2 + "RECON3"
