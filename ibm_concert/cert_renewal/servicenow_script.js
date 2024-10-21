// Add extra_vars for Ansible template
r.setRequestHeader('Content-Type', 'application/json');
var obj = {};
var new_desc = current.number + "@@@" + current.short_description;
obj['sn_short_desc'] = new_desc;
var final_obj = {};
final_obj['extra_vars']= obj;
r.setRequestBody(JSON.stringify(final_obj));