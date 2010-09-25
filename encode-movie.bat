@echo off
set SRC=out\scaled\test_%%05d.tga
set OUT=out\output.avi

c:\programme\mplayer\ffmpeg -r 25 -b 1800k -i %SRC% %OUT%

