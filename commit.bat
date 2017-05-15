@ echo off
rem 本文件撰写者：雪山凌狐
rem 版本号：V1.2
rem 时间：20170324
rem 功能：自动把项目文件夹文件加入暂存并提交
rem 文件编码：Utf-8无BOM格式编码

echo ----------------------git status before----------------------
git status
echo.
echo ----------------------git add file---------------------------
git add -A
echo.
echo ----------------------git status now-------------------------
git status
echo.
echo --set commit message(you can type quit to stop the bat)------
set /p commit_message=Please leave the message to commit:
if "%commit_message%" == "quit" goto end
set msg1=your commit message is:
set msg2=-------
echo.
echo %msg2%%msg1%%commit_message%%msg2%
git commit -m "%commit_message%"
echo.
echo ----------------------commit done----------------------------


:end
echo.
echo --------------------We stop the bat now----------------------
