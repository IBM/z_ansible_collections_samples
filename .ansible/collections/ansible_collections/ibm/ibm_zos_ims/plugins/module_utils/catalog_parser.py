from __future__ import (absolute_import, division, print_function)
from ansible.module_utils.basic import AnsibleModule
from ansible_collections.ibm.ibm_zos_core.plugins.module_utils.better_arg_parser import BetterArgParser   # pylint: disable=import-error

__metaclass__ = type


class catalog_parser(object):
    def __init__(self, module, params, result):
        self.params = params
        self.result = result
        self.module = module

    def _validate_common_input(self):
        try:
            module_defs = dict(
                online_batch=dict(arg_type="bool", required=False),
                ims_id=dict(arg_type="str", required=False),
                dbrc=dict(arg_type="bool", required=False),
                irlm_id=dict(arg_type="str", required=False),
                reslib=dict(arg_type="list", elements="data_set", required=False),
                buffer_pool_param_dataset=dict(arg_type="data_set", required=False),
                primary_log_dataset=dict(arg_type="dict", required=False,
                                         options=dict(
                                             dataset_name=dict(arg_type="data_set", required=True),
                                             disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                             primary=dict(arg_type="int", required=False),
                                             primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G',
                                                                                                        'GB', 'C', 'CYL', 'T', 'TRK']),
                                             secondary=dict(arg_type="int", required=False),
                                             secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB',
                                                                                                          'C', 'CYL', 'T', 'TRK']),
                                             normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'UNCATLG']),
                                             conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'UNCATLG']),
                                             record_format=dict(arg_type="str", required=False, choices=['FB', 'VB', 'FBA', 'VBA', 'U']),
                                             record_length=dict(arg_type="int", required=False),
                                             block_size=dict(arg_type="int", required=False),
                                             type=dict(arg_type="str", required=False, choices=['SEQ', 'BASIC', 'LARGE', 'PDS', 'PDSE', 'LIBRARY',
                                                                                                'LDS', 'RRDS', 'ESDS', 'KSDS']),
                                             volumes=dict(type="list", required=False, elements="str"),
                                             storage_class=dict(type="str", required=False),
                                             management_class=dict(type="str", required=False),
                                             data_class=dict(type="str", required=False)
                                         )
                                         ),
                psb_lib=dict(arg_type="list", elements="data_set", required=True),
                dbd_lib=dict(arg_type="list", elements="data_set", required=True),
                proclib=dict(arg_type="list", elements="data_set", required=True),
                steplib=dict(arg_type="list", elements="data_set", required=False),
                sysprint=dict(arg_type="data_set", required=False)
            )

            parser = BetterArgParser(module_defs)
            self.parsed_args = parser.parse_args(self.params)

        except ValueError as error:
            self.result['msg'] = error.args
            self.result['rc'] = 1
            self.module.fail_json(**self.result)

    def validate_populate_input(self):

        self._validate_common_input()

        try:
            module_defs = dict(
                modstat=dict(arg_type="data_set", required=False),
                mode=dict(arg_type="str", required=True, choices=['LOAD', 'UPDATE', 'READ']),
                check_timestamp=dict(arg_type="bool", required=False),
                secondary_log_dataset=dict(arg_type="dict", required=False,
                                           options=dict(
                                               dataset_name=dict(arg_type="data_set", required=True),
                                               disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                               primary=dict(arg_type="int", required=False),
                                               primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G',
                                                                                                          'GB', 'C', 'CYL', 'T', 'TRK']),
                                               secondary=dict(arg_type="int", required=False),
                                               secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB',
                                                                                                            'C', 'CYL', 'T', 'TRK']),
                                               normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                'UNCATLG']),
                                               conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                     'UNCATLG']),
                                               record_format=dict(arg_type="str", required=False, choices=['FB', 'VB', 'FBA', 'VBA', 'U']),
                                               record_length=dict(arg_type="int", required=False),
                                               block_size=dict(arg_type="int", required=False),
                                               type=dict(arg_type="str", required=False, choices=['SEQ', 'BASIC', 'LARGE', 'PDS', 'PDSE', 'LIBRARY', 'LDS',
                                                                                                  'RRDS', 'ESDS', 'KSDS']),
                                               volumes=dict(type="list", required=False, elements="str"),
                                               storage_class=dict(type="str", required=False),
                                               management_class=dict(type="str", required=False),
                                               data_class=dict(type="str", required=False)
                                           )
                                           ),
                acb_lib=dict(arg_type="list", elements="data_set", required=True),
                bootstrap_dataset=dict(arg_type="dict", required=False,
                                       options=dict(
                                           dataset_name=dict(arg_type="data_set", required=True),
                                           disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                           block_size=dict(arg_type="int", required=False),
                                           primary=dict(arg_type="int", required=False),
                                           primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C',
                                                                                                      'CYL', 'T', 'TRK']),
                                           secondary=dict(arg_type="int", required=False),
                                           secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C',
                                                                                                        'CYL', 'T', 'TRK']),
                                           normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                            'UNCATLG']),
                                           conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                 'UNCATLG']),
                                           storage_class=dict(type="str", required=False),
                                           management_class=dict(type="str", required=False),
                                           data_class=dict(type="str", required=False),
                                           volumes=dict(type="list", required=False, elements="str")
                                       )
                                       ),
                directory_datasets=dict(arg_type="list", elements="dict", required=False,
                                        options=dict(
                                            dataset_name=dict(arg_type="data_set", required=True),
                                            disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                            primary=dict(arg_type="int", required=False),
                                            primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C',
                                                                                                       'CYL', 'T', 'TRK']),
                                            secondary=dict(arg_type="int", required=False),
                                            secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C',
                                                                                                         'CYL', 'T', 'TRK']),
                                            normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG',
                                                                                                             'CATALOG', 'UNCATLG']),
                                            conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                  'UNCATLG']),
                                            storage_class=dict(type="str", required=False),
                                            management_class=dict(type="str", required=False),
                                            data_class=dict(type="str", required=False),
                                            volumes=dict(type="list", required=False, elements="str")
                                        )
                                        ),
                temp_acb_dataset=dict(arg_type="dict", required=False,
                                      options=dict(
                                          dataset_name=dict(arg_type="data_set", required=True),
                                          disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                          primary=dict(arg_type="int", required=False),
                                          primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C', 'CYL', 'T', 'TRK']),
                                          secondary=dict(arg_type="int", required=False),
                                          secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C', 'CYL',
                                                                                                       'T', 'TRK']),
                                          normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG', 'UNCATLG']),
                                          conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                'UNCATLG']),
                                          volumes=dict(type="list", required=False, elements="str"),
                                          storage_class=dict(type="str", required=False),
                                          management_class=dict(type="str", required=False),
                                          data_class=dict(type="str", required=False)
                                      )
                                      ),
                directory_staging_dataset=dict(arg_type="dict", required=False,
                                               options=dict(
                                                   dataset_name=dict(arg_type="data_set", required=True),
                                                   disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                                   primary=dict(arg_type="int", required=False),
                                                   primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C',
                                                                                                              'CYL', 'T', 'TRK']),
                                                   secondary=dict(arg_type="int", required=False),
                                                   secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C', 'CYL',
                                                                                                                'T', 'TRK']),
                                                   normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                    'UNCATLG']),
                                                   conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG',
                                                                                                                         'UNCATLG']),
                                                   storage_class=dict(type="str", required=False),
                                                   management_class=dict(type="str", required=False),
                                                   data_class=dict(type="str", required=False),
                                                   volumes=dict(type="list", required=False, elements="str")
                                               )
                                               ),
                sysabend=dict(arg_type="data_set", required=False),
                control_statements=dict(arg_type="dict", required=False,
                                        options=dict(
                                            print_duplicate_resources=dict(arg_type="bool", required=False),  # , default=False),
                                            max_error_msgs=dict(arg_type="int", required=False),
                                            resource_chkp_freq=dict(arg_type="int", required=False),
                                            segment_chkp_freq=dict(arg_type="int", required=False),
                                            print_inserted_resources=dict(arg_type="bool", required=False),  # , default=True),
                                            managed_acbs=dict(arg_type="dict",
                                                              required=False,
                                                              options=dict(
                                                                  setup=dict(arg_type="bool", required=False),
                                                                  stage=dict(arg_type="dict", required=False,
                                                                             options=dict(
                                                                                 save_acb=dict(arg_type="str", required=False, choices=['LATEST', 'UNCOND']),
                                                                                 clean_staging_dataset=dict(arg_type="bool", required=False, default=False),
                                                                                 gsampcb=dict(arg_type="bool", required=False, default=False),
                                                                                 gsamdbd=dict(arg_type="str", required=False)
                                                                             )
                                                                             ),
                                                                  update=dict(arg_type="dict", required=False,
                                                                              options=dict(
                                                                                  replace_acb=dict(arg_type="str", required=False, choices=['LATEST',
                                                                                                                                            'UNCOND']),
                                                                                  share_mode=dict(arg_type="bool", required=False, default=False),
                                                                                  gsampcb=dict(arg_type="bool", required=False, default=False),
                                                                                  gsamdbd=dict(arg_type="str", required=False)
                                                                              )
                                                                              )
                                                              )
                                                              )
                                        )
                                        )
            )

            parser = BetterArgParser(module_defs)
            self.parsed_args.update(parser.parse_args(self.params))

            if self.parsed_args.get('directory_datasets') is not None:
                self.directory_datasets = self.parsed_args.get('directory_datasets')
                self._validate_directory_staging_dataset()

            self._validate_optional_datasets()
            self._validate_acb_mode()

        except ValueError as error:
            self.result['msg'] = error.args
            self.result['rc'] = 1
            self.module.fail_json(**self.result)

        return self.parsed_args

    def validate_purge_input(self):

        self._validate_common_input()

        try:
            module_defs = dict(
                mode=dict(arg_type="str", required=True, choices=['ANALYSIS', 'PURGE', 'BOTH']),
                delete_dbd_by_version=dict(arg_type="list", elements="dict", required=False,
                                           options=dict(
                                               member_name=dict(arg_type="str", required=True),
                                               version_number=dict(arg_type="int", required=True)
                                           )
                                           ),
                update_retention_criteria=dict(arg_type="list", elements="dict", required=False,
                                               options=dict(
                                                   resource=dict(arg_type="str", required=True, choices=['DBD', 'PSB']),
                                                   member_name=dict(arg_type="str", required=True),
                                                   instances=dict(arg_type="int", required=True),
                                                   days=dict(arg_type="int", required=False)
                                               ),
                                               ),
                delete=dict(arg_type="list", elements="dict", required=False,
                            options=dict(
                                resource=dict(arg_type="str", required=True, choices=['DBD', 'PSB']),
                                member_name=dict(arg_type="str", required=True),
                                time_stamp=dict(arg_type="str", required=True)
                            )
                            ),
                managed_acbs=dict(arg_type="bool", required=False),
                resource_chkp_freq=dict(arg_type="int", required=False),
                sysut1=dict(arg_type="dict", required=False,
                            options=dict(
                                dataset_name=dict(arg_type="data_set", required=True),
                                disposition=dict(arg_type="str", required=False, choices=['EXCL', 'OLD', 'SHR', 'NEW']),
                                primary=dict(arg_type="int", required=False),
                                primary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C', 'CYL', 'T', 'TRK']),
                                secondary=dict(arg_type="int", required=False),
                                secondary_unit=dict(arg_type="str", required=False, choices=['K', 'KB', 'M', 'MB', 'G', 'GB', 'C', 'CYL', 'T', 'TRK']),
                                normal_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG', 'UNCATLG']),
                                conditional_disposition=dict(arg_type="str", required=False, choices=['KEEP', 'DELETE', 'CATLG', 'CATALOG', 'UNCATLG']),
                                block_size=dict(arg_type="int", required=False),
                                type=dict(arg_type="str", required=False, choices=['SEQ', 'BASIC', 'LARGE', 'PDS', 'PDSE', 'LIBRARY', 'LDS', 'RRDS',
                                                                                   'ESDS', 'KSDS']),
                                volumes=dict(type="list", required=False, elements="str"),
                                storage_class=dict(type="str", required=False),
                                management_class=dict(type="str", required=False),
                                data_class=dict(type="str", required=False)
                            )
                            )
            )

            parser = BetterArgParser(module_defs)
            self.parsed_args.update(parser.parse_args(self.params))

            if self.parsed_args.get("mode") == "ANALYSIS" or self.parsed_args.get("mode") == "BOTH":
                if self.parsed_args.get("delete") is not None:
                    self.result['msg'] = "Cannot specify delete parameters with 'ANALYSIS' or 'BOTH' mode"
                    self.result['rc'] = 1
                    self.module.fail_json(**self.result)

            if self.parsed_args.get("mode") == "PURGE":
                if self.parsed_args.get("update_retention_criteria") is not None:
                    self.result['msg'] = "Cannot specify update_retention_criteria parameter with 'PURGE' mode"
                    self.result['rc'] = 1
                    self.module.fail_json(**self.result)

        except ValueError as error:
            self.result['msg'] = error.args
            self.result['rc'] = 1
            self.module.fail_json(**self.result)

        return self.parsed_args

    def _validate_directory_staging_dataset(self):
        if len(self.parsed_args.get("directory_datasets")) > 20:
            self.result['msg'] = "You cannot specify more than 20 IMS directory datasets"
            self.result['rc'] = 1
            self.module.fail_json(**self.result)

    def _validate_optional_datasets(self):
        if self.parsed_args.get("directory_datasets") is not None or \
           self.parsed_args.get("directory_staging_dataset") is not None or \
           self.parsed_args.get("bootstrap_dataset") is not None:
            if self.parsed_args.get("control_statements") is not None:
                if self.parsed_args.get("control_statements").get("managed_acbs") is not None:
                    if self.parsed_args.get("control_statements").get("managed_acbs").get("stage") is not None or \
                       self.parsed_args.get("control_statements").get("managed_acbs").get("update") is not None:
                        self.result['msg'] = ("You cannot define directory datasets, the bootstrap dataset, "
                                              "or directory staging datasets with MANAGEDACBS=STAGE or MANAGEDACBS=UPDATE")
                        self.result['rc'] = 1
                        self.module.fail_json(**self.result)

    def _validate_acb_mode(self):
        if self.parsed_args.get("control_statements") is not None:
            if self.parsed_args.get("control_statements").get("managed_acbs") is not None:
                if self.parsed_args.get("control_statements").get("managed_acbs").get("stage") is not None or \
                   self.parsed_args.get("control_statements").get("managed_acbs").get("update") is not None:
                    if self.parsed_args.get("mode") == "LOAD":
                        self.result['msg'] = "You cannot update or stage ACBs in catalog LOAD mode."
                        self.result['rc'] = 1
                        self.module.fail_json(**self.result)
