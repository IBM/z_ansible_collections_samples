try { 
    var r = new sn_ws.RESTMessageV2('Renew z/OS Cert REST message', 'Invoke Job Template');
   
   //override authentication profile 
   //authentication type ='basic'/ 'oauth2'
   //r.setAuthenticationProfile(authentication type, profile name);
   r.setRequestHeader('Content-Type', 'application/json');
   
   //set a MID server name if one wants to run the message on MID
   //r.setMIDServer('MY_MID_SERVER');
   
   //if the message is configured to communicate through ECC queue, either
   //by setting a MID server or calling executeAsync, one needs to set skip_sensor
   //to true. Otherwise, one may get an intermittent error that the response body is null
   //r.setEccParameter('skip_sensor', true);
   
    var obj = {};
    var new_desc = current.number + "@@@" + current.short_description;
    obj['sn_short_desc'] = new_desc;
    var final_obj = {};
    final_obj['extra_vars']= obj;
   
    r.setRequestBody(JSON.stringify(final_obj));
    var response = r.execute();
    var responseBody = response.getBody();
    var httpStatus = response.getStatusCode();
   }
   catch(ex) {
    var message = ex.message;
   }