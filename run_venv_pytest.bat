

@echo off
setlocal
set SAVED_DIR=%CD%

if "%1"=="" (
    echo Usage: run_venv_pytest.bat 10
    echo         run_venv_pytest.bat 12
    exit /b 1
)

set PYVER=%1
set VENV=venvs\py3%PYVER%

echo === Using Python 3.%PYVER% ===

rem Create venv if missing
if not exist "%VENV%" (
    echo Creating venv for Python 3.%PYVER%...
    py -3.%PYVER% -m venv "%VENV%"
)

rem Activate venv
call "%VENV%\Scripts\activate"

rem Install package and pytest
rem pip install --upgrade pip
pip install .
cd %VENV%\Lib\site-packages\rocketcea
pip install pytest

rem Run pytest from Lib\site-packages\rocketcea
pytest

rem Deactivate
call deactivate

cd "%SAVED_DIR%"
endlocal

