
.. quickstart

QuickStart
==========
.. note::
    
    On Windows, 32 bit python 3.8 and above are not supported.
    
For a Windows install, jump straight to :ref:`link_windows_bat_file`

Install Numpy & Matplotlib
--------------------------

Because RocketCEA depends on `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_ to
compile the FORTRAN `NASA CEA code <https://www1.grc.nasa.gov/research-and-engineering/ceaweb/>`_ and
`f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_
is part of `numpy <https://numpy.org/>`_ ,
`numpy <https://numpy.org/>`_ will need to be installed.

RocketCEA also makes use of the `matplotlib <https://matplotlib.org/>`_ package for 
creating plots.

To install `numpy <https://numpy.org/>`_ and `matplotlib <https://matplotlib.org/>`_, give the commands::
    
    pip install numpy
    pip install matplotlib
    
    OR perhaps...
    
    pip install --upgrade numpy
    pip install --upgrade matplotlib

Some Linux systems may require::

    sudo pip install numpy
    sudo pip install matplotlib


Install Compiler
----------------

Using `f2py <https://numpy.org/devdocs/f2py/python-usage.html>`_ to compile FORTRAN requires
a FORTRAN compiler. I recommend using `gfortran <https://www.gnu.org/software/gcc/fortran/>`_ 
on all platforms so that there are no FORTRAN incompatibilities between platforms.

Each operating system has its own approach to install `gfortran <https://www.gnu.org/software/gcc/fortran/>`_ 


Click: :ref:`link_installgfortran` to see install instructions for a few platforms that I have tested.

.. important::

    Windows users MUST put MinGW into environment PATH variable.
    (see: :ref:`link_windowspath`)
    
    for 32 bit:
    C:\\MinGW\\mingw32\\bin  AND
    C:\\MinGW\\mingw32\\bin
    
    For 64 bit:
    C:\\MinGW\\mingw64\\lib  AND 
    C:\\MinGW\\mingw64\\lib


.. image:: ./_static/full_mingw_folder.jpg
    



Install RocketCEA
-----------------

**After** the above installs have been accomplished, the easiest way to install RocketCEA is::

    pip install rocketcea
    
        OR on Linux
    sudo pip install rocketcea
        OR perhaps
    pip install --user rocketcea

Try a quick test of the install by pasting the following into a command terminal::

    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
    
    should result in:
    374.30361765576265

.. _link_windows_bat_file:

Windows Batch File
------------------

.. note::

    for `Anaconda <https://www.anaconda.com/products/individual>`_ on Windows, see "Anaconda Windows Batch File" section below.

RocketCEA on Windows can be problematic. Often the problem is in the PATH environment variable
where the wrong files are found for the intended install, and even more often it is because 
the proper compiler build tools are not available.

The Windows batch file below addresses both of those problems.

Remember that the FORTRAN compiler must also be installed
(see: :ref:`Install MinGW on Windows 10 <link_installmingw>`)

Notice that the batch file makes the PATH as simple as possible so that only the 64 bit MinGW files 
and desired python files are found.

Note that the batch file assumes that python 3.9 64 bit is the python version installed at **C:\\Python39_64**
and that MinGW 64 bit is installed at **C:\\MinGW\\mingw64\\bin** and **C:\\MinGW\\mingw64\\lib**.
*Edit those path names for your situation.*

Note that it starts by uninstalling rocketcea in case bad files are left from previous attempts.
(Plan for the worst, hope for the best.)

**You may need to edit the hard-coded paths to your own location of python and MinGW.**

- Copy and paste the batch file code below into an editor 
- Edit the hard-coded paths to your own location of python and MinGW
  (i.e. perhaps change C:\\Python39_64 and C:\\MinGW\\mingw64)
- Save the edited BAT file (e.g. as RUN_SETUP_BUILD_WIN64.BAT)
- Open a command prompt terminal and navigate to the BAT file directory.
- Give the command RUN_SETUP_BUILD_WIN64.BAT

.. note::

    9/16/2021: BAT File successful on Windows 10 with 64 bit python 3.8.10 and 3.9.7

.. code-block:: batch

    rem =============== RUN_SETUP_BUILD_WIN64.BAT ================
    
    SET "MYPYTHONPATH=C:\Python39_64"

    rem Make sure that PATH is as simple as possible
    set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

    pip uninstall -y rocketcea
    
    pip install future
    pip install numpy
    pip install scipy
    
    pip install pillow
    pip install matplotlib
    pip install --global-option build_ext --global-option --compiler=mingw32 rocketcea

    rem Test the compiled module
    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
    

.. note::

  The "trick" for installing RocketCEA on Windows seems to be pre-installing the dependency packages 
  (e.g. numpy, scipy and matplotlib) so that the pip options for rocketcea only apply to rocketcea.


Anaconda Windows Batch File
---------------------------

As stated above, the "trick" for installing RocketCEA on Windows seems 
to be pre-installing the dependency packages  (e.g. numpy, scipy and matplotlib) 
so that the pip options for rocketcea only apply to rocketcea.

On Anaconda, many of the required dependencies are preinstalled.

To test the required installs, open an 
Anaconda prompt, launch python and attempt to import each package.
(An example is shown below for Anaconda with python 3.9)

.. image:: ./_static/Anaconda_check_installs.jpg
    
Notice that a fresh install of Anaconda does not include the 
`pillow <https://pypi.org/project/Pillow/>`_ package.

use the command::

    conda install -c anaconda pillow

to install `pillow <https://pypi.org/project/Pillow/>`_ . The results will look
something like the image below.

.. image:: ./_static/Anaconda_install_pillow.jpg

With all the dependencies handled, the batch file can be much more simple 
than the one shown above. Before you can run it, you need to verify the location
of the python folders under Anaconda.

To find your python folder, open an Anaconda prompt, launch python, import sys 
and give the command "sys.executable" as in the image below.

.. image:: ./_static/Anaconda_find_python_exe.jpg

In my case, the path was::

    C:\Users\Charlie\anaconda3

Yours will likely be similar.

For the final step, edit the batch file below to replace **<YourUserName>** with 
the user name shown in the python path above.

.. code-block:: batch

    rem NOTICE: MUST Verify dependency installs prior to running this batch file.

    SET "MYPYTHONPATH=C:\Users\<YourUserName>\anaconda3"

    rem Make sure that PATH is as simple as possible... For Anaconda notice "\Libary\bin"
    set PATH=C:\MinGW\mingw64\bin;C:\MinGW\mingw64\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts;%MYPYTHONPATH%\Library\bin

    pip install --global-option build_ext --global-option --compiler=mingw32 rocketcea

    rem Test the compiled module
    python -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"

Run the batch file and the results will hopefully be similar to the results 
shown below.


.. image:: ./_static/Anaconda_successful_install.jpg

Notice that the file ran the "quick test" at the bottom of the batch file and 
properly output the Isp of 374.30...

Getting Help
------------

After installing with ``pip``, there should be a launch command line program called **rocketcea** or, on Windows, **rocketcea.exe**. 

From a terminal or command prompt window simply type::

    rocketcea

Your browser will launch with these RocketCEA help pages.

Running RocketCEA
-----------------

Any use of RocketCEA begins with an import statement and an instance of CEA_obj::

    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')


In the above example, LOX and LH2 are called out, but any propellants on the :ref:`Propellants <propellants_link>` page can be used.

There are a large number of examples included in this document

For instance, look at :ref:`LOX/LH2 Performance <example_1_link>` on the :ref:`Standard Examples <std_examples_link>` page. 

To run an example, highlight the source code with your mouse, right click the highlighted code and select **Copy**. 
Paste that code into your text editor and save it to a python file.(for example D:\\rocketcea\\example_1.py).

    
Example files can be run with the command::

    python example1.py

Or, in many text editors hitting the **F5** key will execute the code.

.. note::
    
    RocketCEA is compiled with the mingw and mingw-w64 gfortran compilers using default f2py options
    giving a "shared" `*.pyd` file that requires mingw libraries at run time.
    
    If you see the error: ``Import Error: DLL load failed: The specified module could not be found``
    You may need to install the MinGW Compiler Suite and perhaps even recompile RocketCEA in order 
    for RocketCEA to work (see below)
    
    On Windows, make sure the environment PATH variable is set properly (see: :ref:`link_windowspath`)

Test The Install
----------------

Paste the following code into your text editor and save it to your test folder as basic_cea.py 
(for example, D:\\rocketcea\\basic_cea.py)::

    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')
    for mr in range(2,9):
        print(mr, C.get_Isp(Pc=100.0, MR=mr, eps=40.0) )

At the command prompt, give the command::

    python basic_cea.py
    
If you see the following output::

    (2, 424.3597085736007)
    (3, 445.44434236555196)
    (4, 453.13271951921837)
    (5, 453.240429182719)
    (6, 448.190232998362)
    (7, 438.74340042907266)
    (8, 424.6998266323161)

Great... you are good to go.

If not, see the information below.

Google Colaboratory
-------------------

If you are having trouble installing RocketCEA on your system,
RocketCEA can be run on `Google Colaboratory <https://colab.research.google.com/notebooks/welcome.ipynb>`_
(either python3 or python2).

`Colaboratory <https://colab.research.google.com/notebooks/welcome.ipynb>`_ 
is a free Jupyter notebook environment that requires no setup and runs entirely in the cloud.

After creating a Colaboratory notebook, install RocketCEA.::

    !pip install RocketCEA


.. image:: ./_static/colab_pip_rocketcea.jpg
    :width: 60%


If Needed, install libgfortran3::

    !apt-get install libgfortran3
    
.. image:: ./_static/colab_apt_libgfortran3.jpg
    :width: 70%

Create a python script to run RocketCEA::

    %%file chk_cea.py
    from rocketcea.cea_obj import CEA_Obj
    C = CEA_Obj( oxName='LOX', fuelName='LH2')
    for mr in range(2,9):
        print(mr, C.get_Isp(Pc=100.0, MR=mr, eps=40.0) )
    
.. image:: ./_static/colab_save_pyfile.jpg
    :width: 60%

And then run the file::

    !python chk_cea.py

.. image:: ./_static/colab_run_chk_cea.jpg
    :width: 50%

Colab plots work with RocketCEA as well.

.. image:: ./_static/colab_cstar_plot_example.jpg
    :width: 80%

Windows 10 with WSL
-------------------

RocketCEA can also be installed on `Windows Subsystem for Linux (WSL) <https://docs.microsoft.com/en-us/windows/wsl/install-win10>`_

After setting up your Linux distribution on WSL, installing RocketCEA is quick and easy.

For example on a Ubuntu distribution.
   
*  Update Ubuntu
    * sudo apt-get update
   
*  configure Ubuntu for python3 and FORTRAN
    * sudo apt install python3-pip
    * sudo apt-get install gfortran

*  Install RocketCEA required libraries
    * pip install numpy
    * pip install matplotlib
    * pip install rocketcea
   
*  Test installation with quick example
    * python3 -c "from rocketcea.cea_obj import CEA_Obj; C=CEA_Obj(oxName='LOX', fuelName='LH2'); print(C.get_Isp())"
    * SHOULD RETURN:  374.30361765576896 (or perhaps just very close to it.)

WSL GUI
~~~~~~~

Note that, if desired, `VsXsrv <https://sourceforge.net/projects/vcxsrv/files/latest/download>`_ 
can be used to enable Linux-driven graphic windows on WSL.
 
This will enable matplotlib show() command, tkinter programs or any gui-based Linux application
installed on the WSL Linux distribution.

WSLg
~~~~

In the presumably near future, `WSLg <https://github.com/microsoft/wslg>`_ will be available that has GUI support built in.
Windows 11 is expected to come with `WSLg <https://github.com/microsoft/wslg>`_ 
and Windows 10 will be able to install `WSLg <https://github.com/microsoft/wslg>`_.
