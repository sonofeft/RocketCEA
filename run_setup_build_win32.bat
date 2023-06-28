
rem set python path variable (Default is Python 3.7 32 bit)
IF "%1"=="" ( SET "MYPYTHONPATH=D:\py_versions\Python37_32" ) ELSE ( SET "MYPYTHONPATH=D:\py_versions\Python%1_32" )

rem Make sure that PATH is as simple as possible
set PATH=C:\MinGW\mingw32\bin;C:\MinGW\mingw32\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

pip uninstall -y rocketcea

python setup.py build --compiler=mingw32

python setup.py install
python setup.py sdist bdist_wheel

rem Test the compiled module
rem python .\rocketcea\examples\quick_chk.py
python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
