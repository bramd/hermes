#!/bin/sh

expect -c '
set timeout -1;
spawn /opt/android/sdk/tools/android update sdk --no-ui -t tool,platform-tools,build-tools-19.0.1,android-19;
expect {
"Do you accept the license" { exp_send "y\r"; exp_continue }
eof
}'
