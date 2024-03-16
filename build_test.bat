
SET "MYPYTHONPATH=C:\Python310" 

rem Make sure that PATH is as simple as possible
set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

pip uninstall -y rocketcea

python setup.py build --compiler=mingw32

python setup.py develop
rem python setup.py sdist bdist_wheel

rem Test the compiled module
python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"

python -c "from rocketcea.cea_obj import __version__;  print( 'RocketCEA Version:', __version__)"
