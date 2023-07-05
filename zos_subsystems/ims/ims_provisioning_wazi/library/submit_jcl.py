# -*- coding: utf-8 -*-
from __future__ import absolute_import, division, print_function

ANSIBLE_METADATA = {
    'metadata_version': '1.3',
    'status': ['preview'],
    'supported_by': 'Blake Becker, IMS Cloud Native'
}

__metaclass__ = type

from ansible.module_utils.basic import AnsibleModule
import re
from subprocess import Popen, PIPE
from os import chmod, path
from tempfile import NamedTemporaryFile
from time import sleep


class UnixFileSystem(object):
    @staticmethod
    def write_temp_file(file_content, delete_on_close=True):
        tmp_file = NamedTemporaryFile(delete=delete_on_close)
        with open(tmp_file.name, 'w') as f:
            f.write(file_content)
        return tmp_file

    # change file permissions for file in z/OS USS file system, accepts an octal value in the form 0o777
    @staticmethod
    def change_file_permissions(file_path, new_permissions):
        chmod(file_path, new_permissions)
        return file_path

    @staticmethod
    def split_filename_and_path(file_path):
        return (path.dirname(file_path), path.basename(file_path))

    @staticmethod
    def fix_path(path):
        fixed_path = path
        if path and len(path) > 0:
            if fixed_path[0] != '/':
                fixed_path = '/' + fixed_path
            if fixed_path[-1] == '/':
                fixed_path = fixed_path[0:-1]
        else:
            fixed_path = '/tmp'
        return fixed_path


class Job(object):
    log_script = """/* REXX */
arg options
trace 'o'
parse var options command
parse var command jobid
rc=isfcalls('ON')
ddname = \"JESMSGLG\"
Address SDSF \"ISFEXEC ST \"
if rc<>0 then do
  Call SayErr 'Bad return from log.  rc is ' rc
  Exit rc
end
numrows=isfrows
tok=''
do ix=1 to numrows
  cur_jobid = jobid.ix
  if (cur_jobid == jobid) then do
     tok = token.ix
     leave
  end
end
if (tok = '') then do
  Call SayErr 'Unable to find job id:' jobid
  exit 4
end
Address SDSF \"ISFACT ST TOKEN('\"tok\"') PARM(NP ?)\"
tok = ''
do ix=1 to ddname.0
  cur_dd = ddname.ix
  if (cur_dd == ddname) then do
    tok = token.ix
    leave
  end
end
if (tok = '') then do
  Call SayErr 'ddname not found'
  exit 4
end
Address SDSF \"ISFBROWSE ST TOKEN('\"tok\"')\"
do ix=1 to isfline.0
  Say isfline.ix
end
rc=isfcalls('OFF')
exit rc
call syscalls 'ON'
buf=text || esc_n
address syscall \"write\" 2 \"buf\"
Return 0

"""

    def __init__(self, jcl_path, reuse_script=False, script_path=''):
        self.reused_file = script_path and script_path != ''
        self.script_path = script_path
        self.jcl_path = jcl_path
        self.job_name = None
        self.tmp_file = None   # tempfile object generated on initial run
        if not script_path:
            self.tmp_file = self.write_job_log_script(reuse_script)
            self.script_path = str(self.tmp_file.name)
            self.tmp_file.file.close()

    def cleanup(self):
        if not self.reused_file:
            self.tmp_file.close()

    def submit(self):
        try:
            request = Popen(['submit', '-j', self.jcl_path], stdout=PIPE, stderr=PIPE, universal_newlines=True)
            stdout, stderr = request.communicate()
            result = re.search(r'((?:job|JOB)[0-9]+)', stdout)
            if not result:
                raise RuntimeError('An error occurred during submission of batch job.' + stdout)
            self.job_name = result.group(1)
            rc = self.get_rc()
            return rc
        except Exception:
            raise
        finally:
            self.cleanup()

    def get_rc(self, wait_seconds=2, retries=60):
        rc = None
        for attempt in range(retries):
            path, script = UnixFileSystem.split_filename_and_path(self.script_path)
            request = Popen(['./' + script, self.job_name], cwd=path, stdout=PIPE, stderr=PIPE, universal_newlines=True)
            stdout, stderr = request.communicate()
            if 'ABEND' in stdout or 'JCL ERROR' in stdout:
                raise RuntimeError('JOB FAILED: ' + stdout)
            if 'ENDED' in stdout:
                result = re.search(r'ENDED\s-\sRC=([0-9]{4})', stdout)
                if result:
                    rc = int(result.group(1))
                    break
                raise RuntimeError('JOB FAILED: ' + stdout)
            sleep(wait_seconds)
        if rc is None:
            raise RuntimeError('JOB FAILED: job logs could not be located')
        return rc

    def get_job_name(self):
        return self.job_name

    def write_job_log_script(self, reuse_script):
        script_file = UnixFileSystem.write_temp_file(Job.log_script, not reuse_script)
        UnixFileSystem.change_file_permissions(script_file.name, 0o755)
        return script_file

    def get_script_path(self):
        return self.script_path


def run_module():

    module_args = dict(
        name=dict(type='str', required=True),
        path=dict(type='str', required=True),
        max_rc=dict(type='int', required=False),
        script_path=dict(type='str', required=False),
        reuse_script=dict(type='bool', required=False)
    )

    parameter_defaults = {
        'max_rc': 0,
        'script_path': '',
        'reuse_script': False,
    }

    result = dict(
        changed=False,
        original_message='',
        message=''  # return dummy port in check mode, won't actually be used
    )

    module = AnsibleModule(
        argument_spec=module_args,
        supports_check_mode=True
    )

    if module.check_mode:
        return result

    rc = None
    job_name = None
    script_path = None
    try:
        for key, value in parameter_defaults.items():
            if not module.params.get(key):
                module.params[key] = value
        jcl_path = UnixFileSystem.fix_path(module.params.get('path')) + '/' + module.params.get('name')
        batch_job = Job(jcl_path, module.params.get('reuse_script'), module.params.get('script_path'))
        if module.params.get('reuse_script'):
            script_path = batch_job.get_script_path()
        rc = batch_job.submit()
        job_name = batch_job.get_job_name()
        if rc > module.params.get('max_rc'):
            raise RuntimeError('JOB FAILED! max return code allowed: ' + str(module.params.get('max_rc')) + ' job returned ' + str(rc))
    except Exception as e:
        module.fail_json(msg=e, **result)

    result['changed'] = True
    result['original_message'] = module.params
    result['message'] = {'stdout': 'Job ran successfully', 'stderr': ''}
    result['job_rc'] = rc
    result['job_name'] = job_name
    result['script_path'] = script_path
    module.exit_json(**result)


def main():
    run_module()


if __name__ == '__main__':
    main()
