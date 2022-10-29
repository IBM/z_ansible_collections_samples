# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function

ANSIBLE_METADATA = {
    'metadata_version': '1.3',
    'status': ['preview'],
    'supported_by': 'Blake Becker, IMS Cloud Native'
}

DOCUMENTATION = '''
---
module: zport.py

short_description: Allocate TCP/UDP port on z/OS through Unix System Services

version_added: "2.4"

description:
    - Using Unix System Services on z/OS, dynamically allocate a port on TCP and/or UDP to be used by a given job or other name.
options:
    name:
        description:
            -  The job or other name for which to allocate the port. Defaults to wildcard *
        required: True
    state:
        description:
            - The expected final state. Options are open or closed. Defaults to open.
    port:
        description:
            - A specific port to allocate or deallocate, only works for allocation if either TCP or UDP is specified, not both.
    description:
        description:
            - A description placed with the allocated port in the TCPIP profile.
        required: False
    min_port:
        description:
            - The lowest-numbered port to consider for allocation.
        required: False
    max_port:
        description:
            - The highest-numbered port to consider for allocation.
        required: False
    tcp:
        description:
            - Request TCP port. Default is no. At least one of the arguments tcp or udp must be true.
        required: False
    udp:
        description:
            - Request UDP port. Default is no. At least one of the arguments tcp or udp must be true.
        required: False
    tmp_dsname:
        description:
            - The MVS dataset to store temporary files used in port allocation.
        required: False
    dsname:
        description:
            - The MVS dataset in which the active TCPIP profile resides.
        required: False                                                                                                                           
author:
    - Blake Becker(blake.becker@ibm.com)
'''

EXAMPLES = '''
# basic example of how to use module
- name: Allocate a TCP and UDP port between 500 and 1000
  zport:
    name: ANSIBLE
    description: used by job ansible
    min_port: 500
    max_port: 1000
    tcp: yes
    udp: yes

# detailed example specifying all parameters, overriding defaults
- name: Allocate a UDP port between 600 and 1200
  zport:
    name: ANSIBLE
    description: for job ansible
    min_port: 600
    max_port: 1200
    tcp: no
    udp: yes
    tmp_path: /tmp
    tmp_dsname: ansib.sysut1(profile)
    dsname: TCPIP.PROFILE

# example of using zport to release an in-use port
- name: Release a previously allocated port
  zport:
    state: closed
    port: 1105

'''

RETURN = '''
original_message:
    description: The original list of parameters passed in, plus any default parameters used
    type: str
message:
    description: The output message that the sample module generates
    type: dict
    stdout:
        description: The output from the module
        type: str
    stderr:
        description: Any error text from the module
        type: str
    port:
        description: The newly allocated port number
        type: int
'''

__metaclass__ = type

from ansible.module_utils.basic import AnsibleModule
import re
import subprocess
import os
import tempfile
import shutil


class UnixFileSystem(object):
    # Translation table from codepage 1047 swap NL,LF to 819
    tt1047_819 = \
        b"\x00\x01\x02\x03\x9C\x09\x86\x7F\x97\x8D\x8E\x0B\x0C\x0D\x0E\x0F" \
        b"\x10\x11\x12\x13\x9D\x0A\x08\x87\x18\x19\x92\x8F\x1C\x1D\x1E\x1F" \
        b"\x80\x81\x82\x83\x84\x85\x17\x1B\x88\x89\x8A\x8B\x8C\x05\x06\x07" \
        b"\x90\x91\x16\x93\x94\x95\x96\x04\x98\x99\x9A\x9B\x14\x15\x9E\x1A" \
        b"\x20\xA0\xE2\xE4\xE0\xE1\xE3\xE5\xE7\xF1\xA2\x2E\x3C\x28\x2B\x7C" \
        b"\x26\xE9\xEA\xEB\xE8\xED\xEE\xEF\xEC\xDF\x21\x24\x2A\x29\x3B\x5E" \
        b"\x2D\x2F\xC2\xC4\xC0\xC1\xC3\xC5\xC7\xD1\xA6\x2C\x25\x5F\x3E\x3F" \
        b"\xF8\xC9\xCA\xCB\xC8\xCD\xCE\xCF\xCC\x60\x3A\x23\x40\x27\x3D\x22" \
        b"\xD8\x61\x62\x63\x64\x65\x66\x67\x68\x69\xAB\xBB\xF0\xFD\xFE\xB1" \
        b"\xB0\x6A\x6B\x6C\x6D\x6E\x6F\x70\x71\x72\xAA\xBA\xE6\xB8\xC6\xA4" \
        b"\xB5\x7E\x73\x74\x75\x76\x77\x78\x79\x7A\xA1\xBF\xD0\x5B\xDE\xAE" \
        b"\xAC\xA3\xA5\xB7\xA9\xA7\xB6\xBC\xBD\xBE\xDD\xA8\xAF\x5D\xB4\xD7" \
        b"\x7B\x41\x42\x43\x44\x45\x46\x47\x48\x49\xAD\xF4\xF6\xF2\xF3\xF5" \
        b"\x7D\x4A\x4B\x4C\x4D\x4E\x4F\x50\x51\x52\xB9\xFB\xFC\xF9\xFA\xFF" \
        b"\x5C\xF7\x53\x54\x55\x56\x57\x58\x59\x5A\xB2\xD4\xD6\xD2\xD3\xD5" \
        b"\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\xB3\xDB\xDC\xD9\xDA\x9F" \

    # Translation table from codepage 819 to 1047 swap NL,LF
    tt819_1047 = \
        b"\x00\x01\x02\x03\x37\x2D\x2E\x2F\x16\x05\x15\x0B\x0C\x0D\x0E\x0F" \
        b"\x10\x11\x12\x13\x3C\x3D\x32\x26\x18\x19\x3F\x27\x1C\x1D\x1E\x1F" \
        b"\x40\x5A\x7F\x7B\x5B\x6C\x50\x7D\x4D\x5D\x5C\x4E\x6B\x60\x4B\x61" \
        b"\xF0\xF1\xF2\xF3\xF4\xF5\xF6\xF7\xF8\xF9\x7A\x5E\x4C\x7E\x6E\x6F" \
        b"\x7C\xC1\xC2\xC3\xC4\xC5\xC6\xC7\xC8\xC9\xD1\xD2\xD3\xD4\xD5\xD6" \
        b"\xD7\xD8\xD9\xE2\xE3\xE4\xE5\xE6\xE7\xE8\xE9\xAD\xE0\xBD\x5F\x6D" \
        b"\x79\x81\x82\x83\x84\x85\x86\x87\x88\x89\x91\x92\x93\x94\x95\x96" \
        b"\x97\x98\x99\xA2\xA3\xA4\xA5\xA6\xA7\xA8\xA9\xC0\x4F\xD0\xA1\x07" \
        b"\x20\x21\x22\x23\x24\x25\x06\x17\x28\x29\x2A\x2B\x2C\x09\x0A\x1B" \
        b"\x30\x31\x1A\x33\x34\x35\x36\x08\x38\x39\x3A\x3B\x04\x14\x3E\xFF" \
        b"\x41\xAA\x4A\xB1\x9F\xB2\x6A\xB5\xBB\xB4\x9A\x8A\xB0\xCA\xAF\xBC" \
        b"\x90\x8F\xEA\xFA\xBE\xA0\xB6\xB3\x9D\xDA\x9B\x8B\xB7\xB8\xB9\xAB" \
        b"\x64\x65\x62\x66\x63\x67\x9E\x68\x74\x71\x72\x73\x78\x75\x76\x77" \
        b"\xAC\x69\xED\xEE\xEB\xEF\xEC\xBF\x80\xFD\xFE\xFB\xFC\xBA\xAE\x59" \
        b"\x44\x45\x42\x46\x43\x47\x9C\x48\x54\x51\x52\x53\x58\x55\x56\x57" \
        b"\x8C\x49\xCD\xCE\xCB\xCF\xCC\xE1\x70\xDD\xDE\xDB\xDC\x8D\x8E\xDF" \

    @staticmethod
    def write_file(file_path, file_content):
        with open(file_path, 'w+') as f:
            f.write(file_content)
        return file_path

    @staticmethod
    def write_file_as_ebcdic(file_path, file_content):
        with open(file_path, 'w+') as f:
            f.write(file_content.translate(UnixFileSystem.tt819_1047))
        return file_path

    @staticmethod
    def write_temp_file(file_content):
        tmp_file = tempfile.NamedTemporaryFile(delete=False)
        with open(tmp_file.name, 'w') as f:
            f.write(file_content)
        return tmp_file

    @staticmethod
    def write_temp_file_as_ebcdic(file_content):
        tmp_file = tempfile.NamedTemporaryFile(delete=False)
        with open(tmp_file.name, 'w') as f:
            f.write(file_content.translate(UnixFileSystem.tt819_1047))
        return tmp_file

    @staticmethod
    def close_temp_file(tmp_file):
        tmp_file.close()

    @staticmethod
    def read_file(file_path):
        with open(file_path, 'r') as f:
            content = f.read()
        return content

    @staticmethod
    def read_dataset(dsn):
        content = ''
        with open(dsn, 'r') as f:
            content = f.read().translate(UnixFileSystem.tt1047_819)
        return content

    @staticmethod
    def ascii_to_ebcdic(file_path):
        filename, path = UnixFileSystem.split_filename_and_path(file_path)
        tmp_filename = filename + '_new'
        with open(path + tmp_filename, 'w+b') as f:
            request = subprocess.Popen(['iconv', '-f', 'ISO8859-1', '-t', 'IBM-1047', filename], cwd=path, stdout=f, universal_newlines=True)
        stdout = request.communicate()
        request = subprocess.Popen(['mv', tmp_filename, filename], cwd=path, stdout=subprocess.PIPE, universal_newlines=True)
        stdout += request.communicate()
        return stdout

    @staticmethod
    def copy_from_mvs(mvs_dsn, uss_file_path):
        filename, path = UnixFileSystem.split_filename_and_path(uss_file_path)
        request = subprocess.Popen(['cp', "//\'" + mvs_dsn + "\'", filename], cwd=path, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = request.communicate()
        dsn_not_found_text = 'No such file or directory.'
        if dsn_not_found_text in stdout or dsn_not_found_text in stderr:
            raise ValueError('The dataset name provided could not be found. ' + stdout)
        return {'stdout': stdout, 'stderr': stderr, 'rc': request.returncode}

    @staticmethod
    def copy_to_mvs(uss_file_path, mvs_dsn):
        filename, path = UnixFileSystem.split_filename_and_path(uss_file_path)
        request = subprocess.Popen(['cp', filename, "//\'" + mvs_dsn + "\'"], cwd=path,
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = request.communicate()
        return {'stdout': stdout, 'stderr': stderr, 'rc': request.returncode}

    # change file permissions for file in z/OS USS file system, accepts an octal value in the form 0o777
    @staticmethod
    def change_file_permissions(file_path, new_permissions):
        os.chmod(file_path, new_permissions)
        return file_path

    @staticmethod
    def split_filename_and_path(file_path):
        return (os.path.basename(file_path), os.path.dirname(file_path))

    @staticmethod
    def fix_path(path):
        fixed_path = path
        if path and len(path) > 0:
            if fixed_path[0] != '/':
                fixed_path = '/' + fixed_path
            if fixed_path[-1] != '/':
                fixed_path += '/'
        else:
            fixed_path = '/tmp/'
        return fixed_path


class MVSConsole(object):
    @staticmethod
    def command(cmd_text):
        command_wrapper = (
            "/* REXX - SDSF/REXX CONSOLE COMMAND */\n"
            "ARG  COMMAND\n"
            "COMMAND = '{0}"
            "'\n"
            "IF ISFCALLS('ON') <> 0 THEN EXIT 99\n"
            "ADDRESS SDSF \"ISFEXEC '/\"COMMAND\"' (WAIT\"\n"
            "DO I=1 TO  ISFULOG.0\n"
            "SAY  ISFULOG.I\n"
            "END\n"
            "CALL ISFCALLS 'OFF'\n"
        )
        wrapped_command = command_wrapper.format(cmd_text)
        wrapped_command_file = UnixFileSystem.write_temp_file(wrapped_command)
        UnixFileSystem.change_file_permissions(wrapped_command_file.name, 0o755)
        request = subprocess.Popen([wrapped_command_file.name], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = request.communicate()
        UnixFileSystem.close_temp_file(wrapped_command_file)
        return {'stdout': stdout, 'stderr': stderr, 'rc': request.returncode}


class IPConfigEditor(object):
    def __init__(self, ip_config, tmp_dsn):
        self.tmp_dsn = tmp_dsn
        self.tmp_path = UnixFileSystem.fix_path(ip_config.get_tmp_path())
        self.tmp_file_name = 'tcp_ip_profile_new'

    def verify_config(self):
        cmd_text = 'v tcpip,,syntaxcheck,dsn=' + self.tmp_dsn
        return MVSConsole.command(cmd_text)  # {'stdout': stdout, 'stderr': stderr, 'rc': request.returncode}

    def submit_config(self):
        cmd_text = 'v tcpip,,obeyfile,dsn=' + self.tmp_dsn
        return MVSConsole.command(cmd_text)  # {'stdout': stdout, 'stderr': stderr, 'rc': request.returncode}

    def update_ip_config(self, new_config_content):
        UnixFileSystem.write_file_as_ebcdic(self.tmp_path + self.tmp_file_name, new_config_content)
        UnixFileSystem.copy_to_mvs(self.tmp_path + self.tmp_file_name, self.tmp_dsn)
        verify_config_response = self.verify_config()
        if 'EZZ0064I' in verify_config_response.get('stdout'):
            raise ValueError('An error occurred during the verification of new configuration. RC=' + str(verify_config_response.get('rc')) + ' ' +
                             verify_config_response.get('stdout') + verify_config_response.get('stderr'))
        submit_config_response = self.submit_config()
        if 'EZZ0053I' not in submit_config_response.get('stdout'):
            raise ValueError('An error occurred during submission of new configuration. ' +
                             submit_config_response.get('stdout') + submit_config_response.get('stderr'))
        return


class IPConfiguration(object):
    def __init__(self, dsn):
        self.tmp_path = UnixFileSystem.fix_path(tempfile.mkdtemp())
        self.file_name = 'tcp_ip_profile'
        self.dsn = dsn
        self.profile_content = self.retrieve()
        self.in_use_ports = self.get_all_in_use_ports()

    def __del__(self):
        shutil.rmtree(self.tmp_path)

    def retrieve(self):
        request = subprocess.Popen(['cat', "//\'" + self.dsn + "\'"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = request.communicate()
        return stdout
        # UnixFileSystem.copy_from_mvs(self.dsn, self.tmp_path + self.file_name)
        # return UnixFileSystem.read_file(self.tmp_path + self.file_name)

    def find_netstat_ports(self):
        request = subprocess.Popen(['netstat', '-o'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = request.communicate()
        reserved_ports = re.findall(r'^(\d{1,4})\s+(?:TCP|UDP)\s+', stdout, re.MULTILINE)
        ports_in_use = [int(port) for port in set(reserved_ports)]
        return ports_in_use

    def find_tcpip_profile_reserved_ports(self):
        port_file_section = re.search(r'(^PORT\s*\n(?:^;?\s*\d{1,4}\s+(?:TCP|UDP)\s.*\n)*)', self.profile_content, re.MULTILINE)
        reserved_ports = re.findall(r'^;?\s*(\d{1,4})\s+(?:TCP|UDP)\s.*', port_file_section.group(0), re.MULTILINE)
        ports_in_use = []
        if reserved_ports:
            ports_in_use = [int(port) for port in set(reserved_ports)]
        return ports_in_use

    def find_tcpip_profile_reserved_port_ranges(self):
        reserved_ports = []
        reserved_port_ranges = re.findall(r'^;?\s*(\d{1,4})\s+(\d{1,4})\s+(?:TCP|UDP)\s.*', self.profile_content, re.MULTILINE)
        reserved_ranges = []
        if reserved_port_ranges:
            for port_range in reserved_port_ranges:
                for i in range(int(port_range[1])):
                    reserved_ports.append(str(int(port_range) + i))
            reserved_ranges = [int(port) for port in set(reserved_ports)]
        return reserved_ranges

    def get_dsn(self):
        return self.dsn

    def get_all_in_use_ports(self):
        tcpip_reserved_ports = self.find_tcpip_profile_reserved_ports()
        tcpip_reserved_port_ranges = self.find_tcpip_profile_reserved_port_ranges()
        netstat_in_use_ports = self.find_netstat_ports()
        return list(set(tcpip_reserved_ports + tcpip_reserved_port_ranges + netstat_in_use_ports))

    def port_in_use(self, port):
        return port in self.in_use_ports

    def get_file_name(self):
        return self.file_name

    def get_tmp_path(self):
        return self.tmp_path


class Port(object):
    @staticmethod
    def acquire_specific(ip_configuration, name, port, tmp_dsn, allocate_tcp, allocate_udp, description=''):
        port_requester = SpecificPortRequest(ip_configuration, name, port, allocate_tcp, allocate_udp, description)
        response = port_requester.request()
        port_request_content = response.get('request_content')
        ip_config_editor = IPConfigEditor(ip_configuration, tmp_dsn)
        ip_config_editor.update_ip_config(port_request_content)
        return port

    @staticmethod
    def acquire_dynamic(ip_configuration, name, tmp_dsn, allocate_tcp=True, allocate_udp=True, min_port=1300, max_port=6000, description=''):
        port_requester = DynamicPortRequest(ip_configuration, name, allocate_tcp, allocate_udp, min_port, max_port, description)
        response = port_requester.request()
        port = response.get('port')
        port_request_content = response.get('request_content')
        ip_config_editor = IPConfigEditor(ip_configuration, tmp_dsn)
        ip_config_editor.update_ip_config(port_request_content)
        return port

    @staticmethod
    def release_specific(ip_configuration, port):
        connection_id = Port.get_connection_id(port)
        if connection_id:
            MVSConsole.command('v tcpip,,cmd=drop,connection=' + str(port))
        return port

    @staticmethod
    def get_connection_id(port):
        port = str(port)
        request = subprocess.Popen(['netstat'], stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
        stdout, stderr = request.communicate()
        regex_str = r'^[A-Z0-9]{1,8}\s+([A-Z0-9]{8})\s+[a-zA-Z]+\n\s+Local\sSocket\:\s+(?:(?:[0-9]{1,3}\.){4}|\:\:)\.\.(?:' + port + ')'
        reserved_ports = re.findall(regex_str, stdout, re.MULTILINE)
        connection_id = None
        if len(reserved_ports) > 0:
            connection_id = reserved_ports[0]
        return connection_id


class PortRequest(object):
    def __init__(self, ip_configuration, name, allocate_tcp, allocate_udp, description=''):
        self.ip_configuration = ip_configuration
        self.name = name
        self.allocate_tcp = allocate_tcp
        self.allocate_udp = allocate_udp
        self.description = description

    def request(self):
        pass

    def get_single_port_allocation_statement(self, port_to_allocate, protocol):
        alloc_str = str(port_to_allocate).rjust(6) + ' ' + protocol + ' ' + self.name
        if self.description != '':
            needed_spaces_before_description = 31 - len(alloc_str)
            alloc_str += (' ' * needed_spaces_before_description) + '; ' + self.description
        alloc_str += '\n'
        return alloc_str


class SpecificPortRequest(PortRequest):
    def __init__(self, ip_configuration, name, port, allocate_tcp, allocate_udp, description=''):
        super(SpecificPortRequest, self).__init__(ip_configuration, name, allocate_tcp, allocate_udp, description)
        self.port_to_allocate = port

    def request(self):
        protocol = 'TCP' if self.allocate_tcp else 'UDP'
        lines_to_insert = 'PORT\n'
        lines_to_insert += self.get_single_port_allocation_statement(self.port_to_allocate, protocol)
        return {'port': self.port_to_allocate, 'request_content': lines_to_insert}


class DynamicPortRequest(PortRequest):
    def __init__(
        self,
        ip_configuration,
        name,
        allocate_tcp=True,
        allocate_udp=True,
        min_port=1300,
        max_port=6000,
        description='',
    ):
        super(DynamicPortRequest, self).__init__(ip_configuration, name, allocate_tcp, allocate_udp, description)
        self.min_port = int(min_port)
        self.max_port = int(max_port)

    def request(self):
        port_to_allocate = self.choose_port()
        lines_to_insert = self.get_port_allocation_statements(port_to_allocate)
        return {'port': port_to_allocate, 'request_content': lines_to_insert}

    def get_port_allocation_statements(self, port_to_allocate):
        lines_to_insert = 'PORT\n'
        if self.allocate_tcp:
            tcp_str = self.get_single_port_allocation_statement(port_to_allocate, 'TCP')
            lines_to_insert += tcp_str
        if self.allocate_udp:
            udp_str = self.get_single_port_allocation_statement(port_to_allocate, 'UDP')
            lines_to_insert += udp_str
        return lines_to_insert

    # choose the port we want to reserve based on what is still available
    def choose_port(self):
        ports_in_use = self.ip_configuration.get_all_in_use_ports()
        ports_to_avoid = [port for port in ports_in_use if (port >= self.min_port and port <= self.max_port)]
        if ports_to_avoid and len(ports_to_avoid) == (self.max_port - self.min_port):
            raise ValueError('No ports are available in the specified range.')
        open_port_found = False
        new_port = self.min_port
        while not open_port_found:
            if new_port not in ports_to_avoid:
                open_port_found = True
            else:
                new_port += 1
        return new_port


def run_module():
    #  TODO: if reserved port is already reserved for the same instance don't fail

    #  TODO: Add defaults and aliases here, remove parameter_defaults dict for applicable values
    module_args = dict(
        name=dict(type='str', required=False),
        state=dict(type='str', required=False),
        port=dict(type='int', required=False),
        description=dict(type='str', required=False),
        min_port=dict(type='int', required=False),
        max_port=dict(type='int', required=False),
        tmp_dsname=dict(type='str', required=False),
        dsname=dict(type='str', required=False),
        tcp=dict(type='bool', required=False),
        udp=dict(type='bool', required=False)
    )

    #  TODO: avoid requiring temporary dataset to store new TCPIP profile
    parameter_defaults = {
        'description': '',  # TODO: add max description length checking
        'min_port': 1300,
        'max_port': 6000,
        'tmp_dsname': 'blake.sysut1(profile)',
        'dsname': 'TCPIP.PROFILE.TCPIP',
        'tcp': False,
        'udp': False,
        'state': 'open'
    }

    result = dict(
        changed=False,
        original_message='',
        message=3000  # return dummy port in check mode, won't actually be used
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    if module.check_mode:
        return result

    port = None
    port_id = None
    try:
        for key, value in parameter_defaults.items():
            if not module.params.get(key):
                module.params[key] = value

        if (module.params.get('state') == 'open'):
            if not (module.params.get('tcp') or module.params.get('udp')):
                raise SyntaxError('No protocol specified, must specify at least one of the following arguments: tcp, udp')
            if (module.params.get('tcp') and module.params.get('udp') and module.params.get('port')):
                raise SyntaxError('Invalid protocol arguments: when specifying specific port must choose one of tcp or udp, but not both.')

            module.params['name'] = module.params.get('name').upper() if module.params.get('name') else '*'

            ip_configuration = IPConfiguration(module.params.get('dsname'))

            if module.params.get('port'):
                if (ip_configuration.port_in_use(module.params.get('port'))):
                    raise SyntaxError('Requested port is already in use.')
                port = Port.acquire_specific(
                    ip_configuration=ip_configuration,
                    name=module.params.get('name'),
                    port=module.params.get('port'),
                    tmp_dsn=module.params.get('tmp_dsname'),
                    allocate_tcp=module.params.get('tcp'),
                    allocate_udp=module.params.get('udp'),
                    description=module.params.get('description'),
                )
            else:
                port = Port.acquire_dynamic(
                    ip_configuration=ip_configuration,
                    name=module.params.get('name'),
                    tmp_dsn=module.params.get('tmp_dsname'),
                    allocate_tcp=module.params.get('tcp'),
                    allocate_udp=module.params.get('udp'),
                    min_port=module.params.get('min_port'),
                    max_port=module.params.get('max_port'),
                    description=module.params.get('description'),
                )
            # port_id = Port.get_connection_id(port)
        elif module.params.get('state') == 'closed':
            if not (module.params.get('port')):  # or module.params.get('name')):
                raise SyntaxError('No identifier specified. Need to specify at least one of the following arguments: port')  # ,name
            ip_configuration = IPConfiguration(module.params.get('dsname'))
            port = Port.release_specific(ip_configuration, module.params.get('port'))
            # port_id = Port.get_connection_id(port)
        else:
            raise SyntaxError('Invalid state value provided. Valid values are: open, closed')  # ,name

    except Exception as e:
        module.fail_json(msg=e, **result)

    result['original_message'] = module.params
    result['message'] = {'stdout': 'Port allocated', 'stderr': ''}
    result['port'] = port
    # result['port_id'] = port_id
    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
