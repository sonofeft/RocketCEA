
SET "MYPYTHONPATH=C:\Python310" 
set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts;C:\Windows\System32

cd .\docs
C:\Python310\Scripts\sphinx-build.exe -b html -d _build/doctrees  . _build/html
cd ..

xcopy /S .\docs\_build\html .\rocketcea\sphinx_html /y

python setup.py sdist

rem only upload source to PyPI
rem C:\Python310\Scripts\twine.exe upload --skip-existing dist/*.gz
C:\Python310\Scripts\twine.exe upload --config-file "%HOMEPATh%\.pypirc" --skip-existing dist/*.*

rem use:  twine check dist/*  OR twine check dist/*24.tar.gz (where: 24 is current version)
