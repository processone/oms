Compilation instruction:

Two compilation parameters are mandatory for compilation to succeed.

1) Choosing whenether to use speex or nellymoser codec:

The choice of the codec used is made with compilation parameter: FJCONF::USESPEEX

To use speed, set it to false like this: -define=FJCONF::USESPEEX,"true"
To use nellymoser, set this parameter to false: -define=FJCONF::USESPEEX,"false"

INPORTANT: This parameter is mandatory, if you don't want to use speex, it is mandatory to specify: -define=FJCONF::USESPEEX,"false"

2) Specifying the list of rtmp server at compilation time

You can add available rtmp servers at compilation time with the use of FJCONF::TS parameter.

As an exemple to add dev2.process-one.net and localhost as available transport servers add this to compilation command line:

-define=FJCONF::TS,"'dev2.process-one.net/jinlgeserv;localhost/jingleserv'"

IMPORTANT: This parameter is also mandatory, event if no server have to be added. Exemple:

-define=FJCONF::TS,"''"


Conclusion:

Supposing you want to compile FlashyJingle without speex and without additionnal servers, these parameters would be mandatory:

-define=FJCONF::USESPEEX,"false" -define=FJCONF::TS,"''"
