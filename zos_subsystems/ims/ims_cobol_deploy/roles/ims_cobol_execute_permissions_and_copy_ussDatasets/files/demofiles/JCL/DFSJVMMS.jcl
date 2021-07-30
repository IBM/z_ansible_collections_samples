********************************************************************
* Specify the profile that has environment settings and JVM options.
********************************************************************
-Xoptionsfile=/tmp/andyn/dfsjvmpr.props
-Xsignal:userConditionHandler=percolate
-Xmx128M
-Xmn64M
*Xdebug
*Xnoagent
*Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=7777
*