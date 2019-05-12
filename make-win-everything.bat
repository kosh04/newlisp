make clean
make winall
tar czvf newlispWin32.tgz newlisp.exe newlisp.dll
cp newlispWin32.tgz /c/Windev/

make clean
make winall_utf8
tar czvf newlispWin32-utf8.tgz newlisp.exe newlisp.dll
cp newlispWin32-utf8.tgz /c/Windev/

make clean
make winall64
tar czvf newlispWin64.tgz newlisp.exe newlisp.dll
cp newlispWin64.tgz /c/Windev/

make clean
make winall64_utf8
tar czvf newlispWin64-utf8.tgz newlisp.exe newlisp.dll
cp newlispWin64-utf8.tgz /c/Windev/

