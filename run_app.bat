@echo off
title IUCN Bahari Yetu Portal Launcher
echo -------------------------------------------------------------
echo  Starting IUCN Bahari Yetu Scholarly Portal Offline...
echo -------------------------------------------------------------
echo.
echo Launching your web browser...
start "" "http://127.0.0.1:8888"
echo Starting Shiny App Server in R...
"C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp(port = 8888, launch.browser = FALSE)"
if %errorlevel% neq 0 (
    echo.
    echo [ERROR] Failed to start R or the Shiny package is missing.
    echo Please make sure R-4.6.1 is installed in C:\Program Files\R\.
    pause
)
