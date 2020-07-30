
rem set python path variable (Default is Python 3.5 64 bit)
IF "%1"=="" ( SET "MYPYTHONPATH=D:\Python37_64" ) ELSE ( SET "MYPYTHONPATH=D:\Python%1_64" )

rem Make sure that PATH is as simple as possible
set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

pip uninstall -y rocketcea

python setup.py build --compiler=mingw32

python setup.py install
python setup.py sdist bdist_wheel

rem Test the compiled module
rem python .\rocketcea\examples\quick_chk.py
python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
