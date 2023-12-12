# IBM Db2 Automation Expert Role: ums_login

Authenticate with the UMS server and return the generated JWT.
The roles returns
* the access token (ums_access_token), which can be used with subsequent calls to UMS APIs
* the refresh token (ums_refresh_token), which can be used to refresh an expired access token 
(see the role [ums_refresh_token](../ums_refresh_token)


# Copyright

© Copyright Rocket Software 2023

# License

Licensed under [Apache License,
Version 2.0](https://opensource.org/licenses/Apache-2.0)
