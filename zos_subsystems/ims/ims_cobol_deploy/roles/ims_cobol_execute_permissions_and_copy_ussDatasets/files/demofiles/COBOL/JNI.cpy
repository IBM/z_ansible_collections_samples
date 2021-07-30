      *****************************************************************
      * COBOL declarations for Java native method interoperation      *
      *                                                               *
      * To use the Java Native Interface callable services from a     *
      * COBOL program:                                                *
      * 1) use a COPY statement to include this file into the         *
      *    the Linkage Section of the program, e.g.                   *
      *      Linkage Section.                                         *
      *      Copy JNI                                                 *
      * 2) code the following statements at the beginning of the      *
      *    Procedure Division:                                        *
      *      Set address of JNIEnv to JNIEnvPtr                       *
      *      Set address of JNINativeInterface to JNIEnv              *
      *****************************************************************
      *
      * Sample JNI type definitions in COBOL
      *
      *01 jboolean1 pic X.
      *  88 jboolean1-true  value X'01' through X'FF'.
      *  88 jboolean1-false value X'00'.
      *
      *01 jbyte1 pic X.
      *
      *01 jchar1 pic N usage national.
      *
      *01 jshort1 pic s9(4)  comp-5.
      *01 jint1   pic s9(9)  comp-5.
      *01 jlong1  pic s9(18) comp-5.
      *
      *01 jfloat1  comp-1.
      *01 jdouble1 comp-2.
      *
      *01 jobject1 object reference.
      *01 jclass1  object reference.
      *01 jstring1 object reference jstring.
      *01 jarray1  object reference jarray.
      *
      *01 jbooleanArray1 object reference jbooleanArray.
      *01 jbyteArray1    object reference jbyteArray.
      *01 jcharArray1    object reference jcharArray.
      *01 jshortArray1   object reference jshortArray.
      *01 jintArray1     object reference jintArray.
      *01 jlongArray1    object reference jlongArray.
      *01 floatArray1    object reference floatArray.
      *01 jdoubleArray1  object reference jdoubleArray.
      *01 jobjectArray1  object reference jobjectArray.


      * Possible return values for JNI functions.
       01 JNI-RC pic S9(9) comp-5.
      * success
         88 JNI-OK        value  0.
      * unknown error
         88 JNI-ERR       value -1.
      * thread detached from the VM
         88 JNI-EDETACHED value -2.
      * JNI version error
         88 JNI-EVERSION  value -3.
      * not enough memory
         88 JNI-ENOMEM    value -4.
      * VM already created
         88 JNI-EEXIST    value -5.
      * invalid arguments
         88 JNI-EINVAL    value -6.

      * Used in ReleaseScalarArrayElements
       01 releaseMode pic s9(9) comp-5.
         88 JNI-COMMIT value 1.
         88 JNI-ABORT  value 2.

       01 JNIenv pointer.

      * JNI Native Method Interface - environment structure.
       01 JNINativeInterface.
         02 pointer.
         02 pointer.
         02 pointer.
         02 pointer.
         02 GetVersion                       function-pointer.
         02 DefineClass                      function-pointer.
         02 FindClass                        function-pointer.
         02 FromReflectedMethod              function-pointer.
         02 FromReflectedField               function-pointer.
         02 ToReflectedMethod                function-pointer.
         02 GetSuperclass                    function-pointer.
         02 IsAssignableFrom                 function-pointer.
         02 ToReflectedField                 function-pointer.
         02 Throw                            function-pointer.
         02 ThrowNew                         function-pointer.
         02 ExceptionOccurred                function-pointer.
         02 ExceptionDescribe                function-pointer.
         02 ExceptionClear                   function-pointer.
         02 FatalError                       function-pointer.
         02 PushLocalFrame                   function-pointer.
         02 PopLocalFrame                    function-pointer.
         02 NewGlobalRef                     function-pointer.
         02 DeleteGlobalRef                  function-pointer.
         02 DeleteLocalRef                   function-pointer.
         02 IsSameObject                     function-pointer.
         02 NewLocalRef                      function-pointer.
         02 EnsureLocalCapacity              function-pointer.
         02 AllocObject                      function-pointer.
         02 NewObject                        function-pointer.
         02 NewObjectV                       function-pointer.
         02 NewObjectA                       function-pointer.
         02 GetObjectClass                   function-pointer.
         02 IsInstanceOf                     function-pointer.
         02 GetMethodID                      function-pointer.
         02 CallObjectMethod                 function-pointer.
         02 CallObjectMethodV                function-pointer.
         02 CallObjectMethodA                function-pointer.
         02 CallBooleanMethod                function-pointer.
         02 CallBooleanMethodV               function-pointer.
         02 CallBooleanMethodA               function-pointer.
         02 CallByteMethod                   function-pointer.
         02 CallByteMethodV                  function-pointer.
         02 CallByteMethodA                  function-pointer.
         02 CallCharMethod                   function-pointer.
         02 CallCharMethodV                  function-pointer.
         02 CallCharMethodA                  function-pointer.
         02 CallShortMethod                  function-pointer.
         02 CallShortMethodV                 function-pointer.
         02 CallShortMethodA                 function-pointer.
         02 CallIntMethod                    function-pointer.
         02 CallIntMethodV                   function-pointer.
         02 CallIntMethodA                   function-pointer.
         02 CallLongMethod                   function-pointer.
         02 CallLongMethodV                  function-pointer.
         02 CallLongMethodA                  function-pointer.
         02 CallFloatMethod                  function-pointer.
         02 CallFloatMethodV                 function-pointer.
         02 CallFloatMethodA                 function-pointer.
         02 CallDoubleMethod                 function-pointer.
         02 CallDoubleMethodV                function-pointer.
         02 CallDoubleMethodA                function-pointer.
         02 CallVoidMethod                   function-pointer.
         02 CallVoidMethodV                  function-pointer.
         02 CallVoidMethodA                  function-pointer.
         02 CallNonvirtualObjectMethod       function-pointer.
         02 CallNonvirtualObjectMethodV      function-pointer.
         02 CallNonvirtualObjectMethodA      function-pointer.
         02 CallNonvirtualBooleanMethod      function-pointer.
         02 CallNonvirtualBooleanMethodV     function-pointer.
         02 CallNonvirtualBooleanMethodA     function-pointer.
         02 CallNonvirtualByteMethod         function-pointer.
         02 CallNonvirtualByteMethodV        function-pointer.
         02 CallNonvirtualByteMethodA        function-pointer.
         02 CallNonvirtualCharMethod         function-pointer.
         02 CallNonvirtualCharMethodV        function-pointer.
         02 CallNonvirtualCharMethodA        function-pointer.
         02 CallNonvirtualShortMethod        function-pointer.
         02 CallNonvirtualShortMethodV       function-pointer.
         02 CallNonvirtualShortMethodA       function-pointer.
         02 CallNonvirtualIntMethod          function-pointer.
         02 CallNonvirtualIntMethodV         function-pointer.
         02 CallNonvirtualIntMethodA         function-pointer.
         02 CallNonvirtualLongMethod         function-pointer.
         02 CallNonvirtualLongMethodV        function-pointer.
         02 CallNonvirtualLongMethodA        function-pointer.
         02 CallNonvirtualFloatMethod        function-pointer.
         02 CallNonvirtualFloatMethodV       function-pointer.
         02 CallNonvirtualFloatMethodA       function-pointer.
         02 CallNonvirtualDoubleMethod       function-pointer.
         02 CallNonvirtualDoubleMethodV      function-pointer.
         02 CallNonvirtualDoubleMethodA      function-pointer.
         02 CallNonvirtualVoidMethod         function-pointer.
         02 CallNonvirtualVoidMethodV        function-pointer.
         02 CallNonvirtualVoidMethodA        function-pointer.
         02 GetFieldID                       function-pointer.
         02 GetObjectField                   function-pointer.
         02 GetBooleanField                  function-pointer.
         02 GetByteField                     function-pointer.
         02 GetCharField                     function-pointer.
         02 GetShortField                    function-pointer.
         02 GetIntField                      function-pointer.
         02 GetLongField                     function-pointer.
         02 GetFloatField                    function-pointer.
         02 GetDoubleField                   function-pointer.
         02 SetObjectField                   function-pointer.
         02 SetBooleanField                  function-pointer.
         02 SetByteField                     function-pointer.
         02 SetCharField                     function-pointer.
         02 SetShortField                    function-pointer.
         02 SetIntField                      function-pointer.
         02 SetLongField                     function-pointer.
         02 SetFloatField                    function-pointer.
         02 SetDoubleField                   function-pointer.
         02 GetStaticMethodID                function-pointer.
         02 CallStaticObjectMethod           function-pointer.
         02 CallStaticObjectMethodV          function-pointer.
         02 CallStaticObjectMethodA          function-pointer.
         02 CallStaticBooleanMethod          function-pointer.
         02 CallStaticBooleanMethodV         function-pointer.
         02 CallStaticBooleanMethodA         function-pointer.
         02 CallStaticByteMethod             function-pointer.
         02 CallStaticByteMethodV            function-pointer.
         02 CallStaticByteMethodA            function-pointer.
         02 CallStaticCharMethod             function-pointer.
         02 CallStaticCharMethodV            function-pointer.
         02 CallStaticCharMethodA            function-pointer.
         02 CallStaticShortMethod            function-pointer.
         02 CallStaticShortMethodV           function-pointer.
         02 CallStaticShortMethodA           function-pointer.
         02 CallStaticIntMethod              function-pointer.
         02 CallStaticIntMethodV             function-pointer.
         02 CallStaticIntMethodA             function-pointer.
         02 CallStaticLongMethod             function-pointer.
         02 CallStaticLongMethodV            function-pointer.
         02 CallStaticLongMethodA            function-pointer.
         02 CallStaticFloatMethod            function-pointer.
         02 CallStaticFloatMethodV           function-pointer.
         02 CallStaticFloatMethodA           function-pointer.
         02 CallStaticDoubleMethod           function-pointer.
         02 CallStaticDoubleMethodV          function-pointer.
         02 CallStaticDoubleMethodA          function-pointer.
         02 CallStaticVoidMethod             function-pointer.
         02 CallStaticVoidMethodV            function-pointer.
         02 CallStaticVoidMethodA            function-pointer.
         02 GetStaticFieldID                 function-pointer.
         02 GetStaticObjectField             function-pointer.
         02 GetStaticBooleanField            function-pointer.
         02 GetStaticByteField               function-pointer.
         02 GetStaticCharField               function-pointer.
         02 GetStaticShortField              function-pointer.
         02 GetStaticIntField                function-pointer.
         02 GetStaticLongField               function-pointer.
         02 GetStaticFloatField              function-pointer.
         02 GetStaticDoubleField             function-pointer.
         02 SetStaticObjectField             function-pointer.
         02 SetStaticBooleanField            function-pointer.
         02 SetStaticByteField               function-pointer.
         02 SetStaticCharField               function-pointer.
         02 SetStaticShortField              function-pointer.
         02 SetStaticIntField                function-pointer.
         02 SetStaticLongField               function-pointer.
         02 SetStaticFloatField              function-pointer.
         02 SetStaticDoubleField             function-pointer.
         02 NewString                        function-pointer.
         02 GetStringLength                  function-pointer.
         02 GetStringChars                   function-pointer.
         02 ReleaseStringChars               function-pointer.
         02 NewStringUTF                     function-pointer.
         02 GetStringUTFLength               function-pointer.
         02 GetStringUTFChars                function-pointer.
         02 ReleaseStringUTFChars            function-pointer.
         02 GetArrayLength                   function-pointer.
         02 NewObjectArray                   function-pointer.
         02 GetObjectArrayElement            function-pointer.
         02 SetObjectArrayElement            function-pointer.
         02 NewBooleanArray                  function-pointer.
         02 NewByteArray                     function-pointer.
         02 NewCharArray                     function-pointer.
         02 NewShortArray                    function-pointer.
         02 NewIntArray                      function-pointer.
         02 NewLongArray                     function-pointer.
         02 NewFloatArray                    function-pointer.
         02 NewDoubleArray                   function-pointer.
         02 GetBooleanArrayElements          function-pointer.
         02 GetByteArrayElements             function-pointer.
         02 GetCharArrayElements             function-pointer.
         02 GetShortArrayElements            function-pointer.
         02 GetIntArrayElements              function-pointer.
         02 GetLongArrayElements             function-pointer.
         02 GetFloatArrayElements            function-pointer.
         02 GetDoubleArrayElements           function-pointer.
         02 ReleaseBooleanArrayElements      function-pointer.
         02 ReleaseByteArrayElements         function-pointer.
         02 ReleaseCharArrayElements         function-pointer.
         02 ReleaseShortArrayElements        function-pointer.
         02 ReleaseIntArrayElements          function-pointer.
         02 ReleaseLongArrayElements         function-pointer.
         02 ReleaseFloatArrayElements        function-pointer.
         02 ReleaseDoubleArrayElements       function-pointer.
         02 GetBooleanArrayRegion            function-pointer.
         02 GetByteArrayRegion               function-pointer.
         02 GetCharArrayRegion               function-pointer.
         02 GetShortArrayRegion              function-pointer.
         02 GetIntArrayRegion                function-pointer.
         02 GetLongArrayRegion               function-pointer.
         02 GetFloatArrayRegion              function-pointer.
         02 GetDoubleArrayRegion             function-pointer.
         02 SetBooleanArrayRegion            function-pointer.
         02 SetByteArrayRegion               function-pointer.
         02 SetCharArrayRegion               function-pointer.
         02 SetShortArrayRegion              function-pointer.
         02 SetIntArrayRegion                function-pointer.
         02 SetLongArrayRegion               function-pointer.
         02 SetFloatArrayRegion              function-pointer.
         02 SetDoubleArrayRegion             function-pointer.
         02 RegisterNatives                  function-pointer.
         02 UnregisterNatives                function-pointer.
         02 MonitorEnter                     function-pointer.
         02 MonitorExit                      function-pointer.
         02 GetJavaVM                        function-pointer.
         02 GetStringRegion                  function-pointer.
         02 GetStringUTFRegion               function-pointer.
         02 GetPrimitiveArrayCritical        function-pointer.
         02 ReleasePrimitiveArrayCritical    function-pointer.
         02 GetStringCritical                function-pointer.
         02 ReleaseStringCritical            function-pointer.
         02 NewWeakGlobalRef                 function-pointer.
         02 DeleteWeakGlobalRef              function-pointer.
         02 ExceptionCheck                   function-pointer.
         02 NewDirectByteBuffer              function-pointer.
         02 GetDirectBufferAddress           function-pointer.
         02 GetDirectBufferCapacity          function-pointer.
         02 GetObjectRefType                 function-pointer.


