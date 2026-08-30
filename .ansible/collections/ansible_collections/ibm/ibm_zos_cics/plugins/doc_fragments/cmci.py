# -*- coding: utf-8 -*-

# (c) Copyright IBM Corp. 2020,2023
# Apache License, Version 2.0 (see https://opensource.org/licenses/Apache-2.0)

from __future__ import (absolute_import, division, print_function)
__metaclass__ = type


class ModuleDocFragment(object):

    COMMON = r'''
options:
  cmci_host:
    description:
      - The TCP/IP host name of CMCI connection.
    type: str
    required: true
  cmci_port:
    description:
      - The port number of the CMCI connection.
    type: int
    required: true
  cmci_user:
    description:
      - The user ID under which the CMCI request will run.
      - Can also be specified using the environment variable CMCI_USER.
      - Required if I(cmci_password) is specified.
      - Authentication prioritises certificate authentication if I(cmci_cert)
        and I(cmci_key) are provided, then basic authentication if I(cmci_user)
        and I(cmci_password) are provided, and then unauthenticated if none is
        provided.
    type: str
  cmci_password:
    description:
      - The password of I(cmci_user) to pass HTTP basic authentication.
      - Can also be specified using the environment variable CMCI_PASSWORD.
      - Required if I(cmci_user) is specified.
      - Authentication prioritises certificate authentication if I(cmci_cert)
        and I(cmci_key) are provided, then
        basic authentication if I(cmci_user) and I(cmci_password) are provided,
        and then unauthenticated if none is provided.
    type: str
  cmci_cert:
    description:
      - Location of the PEM-formatted certificate chain file to be used for
        HTTPS client authentication.
      - Can also be specified using the environment variable CMCI_CERT.
      - Required if I(cmci_key) is specified.
      - Authentication prioritises certificate authentication if I(cmci_cert)
        and I(cmci_key) are provided, then basic authentication if I(cmci_user)
        and I(cmci_password) are provided, and then unauthenticated if none is
        provided.
    required: false
    type: str
  cmci_key:
    description:
      - Location of the PEM-formatted file storing your private key to be used
        for HTTPS client authentication.
      - Can also be specified using the environment variable CMCI_KEY.
      - Required if I(cmci_cert) is specified.
      - Authentication prioritises certificate authentication if I(cmci_cert)
        and I(cmci_key) are provided, then basic authentication if I(cmci_user)
        and I(cmci_password) are provided, and then unauthenticated if none is
        provided.
    required: false
    type: str
  context:
    description:
      - If CMCI is installed in a CICSPlex® SM environment, I(context) is the
        name of the CICSplex or CMAS associated with the request, for example,
        C(PLEX1). To determine whether a CMAS can be specified as I(context),
        see the B(CMAS context) entry in the CICSPlex SM resource table
        reference of a resource. For example, according to the
        L(PROGRAM resource table,https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-program-resource-table),
        CMAS context is not supported for PROGRAM.
      - If CMCI is installed in a single region (SMSS), I(context) is the
        APPLID of the CICS region associate with the request.
      - The value of I(context) must contain no spaces. I(context) is not
        case-sensitive.
    type: str
    required: true
  scope:
    description:
      - Specifies the name of a CICSplex, CICS region group, CICS region, or
        logical scope that is associated with the query.
      - I(scope) is a subset of I(context) and limits the request to particular
        CICS systems or resources.
      - I(scope) is optional. If it's not specified, the request is limited by
        the value of I(context) alone.
      - The value of I(scope) must contain no spaces. I(scope) is not
        case-sensitive.
    type: str
  type:
    description:
      - The CMCI external resource name that maps to the target CICS or CICSPlex
        SM resource type.
        For a list of CMCI external resource names, see
        L(CMCI resource names,https://www.ibm.com/docs/en/cics-ts/latest?topic=reference-cmci-resource-names).
    type: str
    required: true
  scheme:
    description: The HTTP scheme to use when establishing a connection to the
      CMCI REST API.
    type: str
    choices:
      - http
      - https
    default: https
  insecure:
    description: When set to C(true), disables SSL certificate trust chain
      verification when using HTTPS.
    type: bool
    required: false
    default: false
  timeout:
    description: HTTP request timeout in seconds
    type: int
    required: false
    default: 30
'''

    RESOURCES = r'''
options:
  resources:
    description:
      - Options that specify a target resource.
    type: dict
    required: false
    suboptions:
      filter:
        description:
          - A dictionary with attribute names as keys, and target values, to be
            used as criteria to filter the set of resources returned from
            CICSPlex SM.
          - Filters implicitly use the C(=) operator.
          - Filters for C(string) type attributes can use the C(*) and C(+)
            wildcard operators.
          - C(*) is a wildcard representing an unknown number of characters,
            and must appear at the end of the value.
          - C(+) is a wildcard representing a single character, and can appear
            in any place in the value, potentially multiple times.
          - To use more complicated filter expressions, including a range of
            different filter operators, and the ability to compose filters with
            C(and) and C(or) operators, see the C(complex_filter) parameter.
          - For more details, see
            L(How to build a filter expression,https://www.ibm.com/docs/en/cics-ts/latest?topic=expressions-how-build-filter-expression).
          - For examples, see M(ibm.ibm_zos_cics.cmci_get).
          - For supported attributes of different resource types, see their
            resource table reference, for example,
            L(PROGDEF resource table reference,https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-progdef-resource-table).
        type: dict
        required: false
      complex_filter:
        description:
          - A dictionary representing a complex filter expression. Complex
            filters are composed of filter expressions, represented as
            dictionaries. Each dictionary can specify either an attribute
            expression, a list of filter expressions to be composed with the
            C(and) operator, or a list of filter expressions to be composed
            with the C(or) operator.
          - The C(attribute), C(and) and C(or) options are mutually exclusive
            with each other.
          - Can contain one or more filters. Multiple filters must be combined
            using C(and) or C(or) logical operators.
          - Filters can be nested.
          - When supplying the C(attribute) option, you must also supply a
            C(value) for the filter. You can also override the default
            operator of C(=) with the C(operator) option.
          - For examples, see "Examples" in M(ibm.ibm_zos_cics.cmci_get).
        type: dict
        required: false
        suboptions:
          and:
            description:
              - A list of filter expressions to be combined with an C(and)
                operation.
              - Filter expressions are nested C(complex_filter) elements. Each
                nested filter expression can be either an C(attribute), C(and)
                or C(or) complex filter expression.
            type: list
            elements: dict
            required: false
          or:
            description:
              - A list of filter expressions to be combined with an C(or)
                operation.
              - Filter expressions are nested C(complex_filter) elements. Each
                nested filter expression can be either an C(attribute), C(and)
                or C(or) complex filter expression.
            type: list
            elements: dict
            required: false
          attribute:
            description:
              - The name of a resource table attribute on which to filter.
              - For supported attributes of different resource types, see their
                resource table reference, for example,
                L(PROGDEF resource table reference, https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-progdef-resource-table).
            type: str
            required: false
          operator:
            description: >
              These operators are accepted: C(<) or C(LT) (less than), C(<=) or
              C(LE) (less than or equal to), C(=) or C(EQ) (equal to), C(>) or
              C(GT) (greater than), C(>=) or C(GE) (greater than or equal to),
              C(==) or C(IS) (is), C(¬=), C(!=), or C(NE) (not equal to). If
              not supplied when C(attribute) is used, C(EQ) is assumed.
            type: str
            required: false
            choices:
              - "<"
              - ">"
              - "<="
              - ">="
              - "="
              - "=="
              - "!="
              - "¬="
              - EQ
              - GT
              - GE
              - LT
              - LE
              - NE
              - IS
          value:
            description:
              - The value by which you are to filter the resource attributes.
              - The value must be a valid one for the resource table attribute
                as documented in the resource table reference, for example,
                L(PROGDEF resource table reference,https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-progdef-resource-table).
            type: str
            required: false
      get_parameters:
        description: >
          A list of one or more parameters with optional values used to identify
          the resources for this request. Eligible parameters for identifying
          the target resources can be found in the resource table reference for
          the target resource type, as valid parameters for the GET operation in
          the "Valid CPSM operations" table. For example, the valid parameters
          for identifying a PROGDEF resource are CICSSYS, CSDGROUP and RESGROUP,
          as found in the
          L(PROGDEF resource table reference,https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-progdef-resource-table).
        type: list
        elements: dict
        suboptions:
          name:
            description: Parameter name available for the GET operation.
            required: true
            type: str
          value:
            description: Parameter value if any.
            required: false
            type: str
        required: false
'''

    ATTRIBUTES = r'''
options:
  attributes:
    description:
      - The resource attributes to be created or updated. Available attributes
        can be found in the CICSPlex® SM resource table reference for the
        target resource type, for example,
        L(PROGDEF resource table reference,https://www.ibm.com/docs/en/cics-ts/latest?topic=tables-progdef-resource-table).
    type: dict
    required: false
'''
