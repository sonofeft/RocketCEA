
SET "MYPYTHONPATH=D:\Python37_64" 
set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts;C:\Windows\System32

cd .\docs
D:\python37_64\Scripts\sphinx-build.exe -b html -d _build/doctrees  . _build/html
cd ..

xcopy /S .\docs\_build\html .\rocketcea\sphinx_html /y

python setup.py sdist

rem only upload source to PyPI
rem C:\Users\Charlie\AppData\Roaming\Python\Python37\Scripts\twine.exe upload --skip-existing dist/*.gz
C:\Users\Charlie\AppData\Roaming\Python\Scripts\twine.exe upload --skip-existing dist/*.gz

rem use:  twine check dist/*  OR twine check dist/*24.tar.gz (where: 24 is current version)
