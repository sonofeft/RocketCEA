
.. quickstart

QuickStart
==========

Install RocketCEA
-----------------

The easiest way to install RocketCEA is::

    pip install rocketcea
    
        OR on Linux
    sudo pip install rocketcea
        OR perhaps
    pip install --user rocketcea


Getting Help
------------

After installing with ``pip``, there will be a launch command line program called **rocketcea** or, on Windows, **rocketcea.exe**. 

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


Install libgfortran3::

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
    :width: 70%


Install Problems
----------------

RocketCEA makes heavy use of the `NASA CEA FORTRAN code <https://www.grc.nasa.gov/WWW/CEAWeb/ceaHome.htm>`_.
For RocketCEA, the NASA FORTRAN code has been modified and turned into a python module using `f2py <https://docs.scipy.org/doc/numpy/f2py/python-usage.html>`_.

When compiling FORTRAN into a python library, each version of python needs its own compiled library.
The libraries I've provided are::

    Windows python 2.7 32 bit and 64 bit
    Windows python 3.5 32 bit and 64 bit
    Windows python 3.6 32 bit and 64 bit
    Windows python 3.7 32 bit and 64 bit
    
    Ubuntu Linux python 2.7 32 bit and 64 bit
    Ubuntu Linux python 3.5 32 bit and 64 bit
    Ubuntu Linux python 3.6 32 bit and 64 bit
    
    MacOS python 2.7 64 bit
    MacOS python 3.7 32 bit and 64 bit

If your version of python is not shown above, or if RocketCEA fails to load the ``py_cea`` module,
you may need to run f2py on the FORTRAN code on your system in order to create ``py_cea.pyd`` or
``py_cea.so`` or some variation thereof.

The source code ``py_cea.f`` is in the install directory of ``rocketcea`` along with a FORTRAN
include file called ``py_cea.inc``. 

Linux ImportError:
------------------

On Linux, the error message ``ImportError: Libgfortran.so.3: cannot open shared object file:`` indicates
that the gfortran libraries were not found.

Begin by installing gfortran::

    sudo apt-get install gfortran
    

Rerun the above test with the command::

    python basic_cea.py

If you get the ImportError again, try installing the same version of Libgfortran 
(Libgfortran.so.3 in the above error message.)::

    sudo apt-get install libgfortran3

The basic_cea.py file should be working now.

Windows DLL load failed:
------------------------

On Windows, the error message ``Import Error: DLL load failed: The specified module could not be found`` indicates
that the MinGW gfortran libraries were not found.

On Windows the procedure is much more involved than on Linux. You will need to download MinGW gfortran,
recompile the NASA CEA code, and place the resulting executable library into the RocketCEA site-packages.

Install MinGW
~~~~~~~~~~~~~

Go to the site `SourceForge MinGW-w64 for 32 and 64 bit Windows <https://sourceforge.net/projects/mingw-w64/>`_.
and download MinGW. At the time of this writing, the downloaded file is ``mingw-w64-install.exe``.
Run mingw-w64-install.exe.


.. image:: ./_static/mingw_welcome.jpg
    :width: 60%

After clicking ``Next``, the settings screen will appear.  

For 64 bit compilation, the settings that worked for me are shown below.

.. image:: ./_static/mingw64_install.jpg
    :width: 60%


For 32 bit compilation, these were my settings.

.. image:: ./_static/mingw32_install.jpg
    :width: 60%
    
A location for the MinGW install then needs to be selected. The default location is in 
C:\\Program Files (x86)\\mingw-w64\\... etc.  However I recommend a simpler path like C:\\MinGW
so that some of the later steps will be easier.


.. image:: ./_static/mingw_folder_select.jpg
    :width: 60%

After several minutes of an ``Installing Files`` you should arrive at a successful finish screen.


.. image:: ./_static/mingw_installing_files.jpg
    :width: 45%

.. image:: ./_static/mingw_finished.jpg
    :width: 45%

When both 32 and 64 bit compilers are installed, and if you selected `C:\\MinGW` as your install directory.
You should have a `C:\\MinGW` directory that looks like the one below.

.. image:: ./_static/MinGW_folder.jpg
    :width: 40%

Recompile RocketCEA
~~~~~~~~~~~~~~~~~~~

We are now ready to recompile RocketCEA.

I recommend setting up a temporary directory for this, something like C:\\temp.

You will need to locate RocketCEA in the python site-packages (assuming your pip install succeeded).
One way to do that is at the command prompt. Call up the python interpreter, import rocketcea and then 
print the value of rocketcea.__file__.


.. image:: ./_static/find_rocketcea.jpg

Copy FORTRAN Source
...................

Using the Windows file explorer, go to the RocketCEA site-packages subdirectory (discovered above) 
and copy the two files shown below to C:\\temp:: 

    py_cea.f
    py_cea.inc

Make Batch File
...............

In addition to those two source files, we need to create a Windows BAT file.

Copy the following lines and paste them into a text editor.

Save them to a file named ``compile_rocketcea.bat``.

``compile_rocketcea.bat``::

    rem set python path variable
    set MYPYTHONPATH=D:\Python27

    rem set name of FORTRAN program (also import name of compiled module)
    set MYPROGRAMNAME=py_cea

    rem Make PATH as simple as possible
    set PATH=C:\MinGW\mingw32\bin;C:\MinGW\mingw32\lib;C:\MinGW\mingw32\lib;%MYPYTHONPATH%;%MYPYTHONPATH%\Scripts

    python -m numpy.f2py -m %MYPROGRAMNAME% -c %MYPROGRAMNAME%.f  --opt="-shared -static" --compiler=mingw32 
    
    rem should now have pyd file 


.. note::

    You will need to change the MYPYTHONPATH value to match your python install.
    
    The batch file shown is for 32 bit compilation.
    
    For 64 bit compilation, change mingw32 to mingw64 everywhere it occurs.


When you have done all of the above, you are ready to compile RocketCEA.


Cross Your Fingers
~~~~~~~~~~~~~~~~~~

Using a command prompt, navigate to C:\\temp and enter the command::

    compile_rocketcea.bat

.. note::

    The above BAT file will change your system PATH in this command prompt **ONLY**
    
    Open a new command prompt if you need to execute more system-wide commands.
    
With any luck, the long series of output will end as shown below

.. image:: ./_static/compile_success.jpg

The resulting ``pyd`` file should now be in C:\\temp as shown below.
In this case it is ``py_cea.pyd`` (The name resulting for **BOTH** 32 and 64 bit Python 2.7

64 bit Python 3.7 would create ``py_cea.cp37-win_amd64.pyd``


.. image:: ./_static/post_compile_dir.jpg


A possible compile error is 
``TypeError: '>=' not supported between instances of 'NoneType' and 'str'``

.. image:: ./_static/bad_mingw_path.jpg

If you have this error, go to `MinGW PATH Error`_.


Move pyd Into RocketCEA
~~~~~~~~~~~~~~~~~~~~~~~

Now that all the hard work is done, the final step is to move the resulting ``pyd`` file 
into the RocketCEA site-packages.

Use Windows file explorer to right click on the ``pyd`` file 
(For Example: ``py_cea.pyd`` or ``py_cea.cp37-win_amd64.pyd``)
and select ``Copy``.

Navigate to the path that you located by printing the rocketcea.__file__ parameter
and paste the file into the rocketcea subdirectory. **(EXCEPT for Python 2.7 64 bit,  SEE BELOW)**

You will likely be prompted to replace or skip the operation.  Choose ``Replace``.

.. image:: ./_static/replace_old_pyd.jpg

That subdirectory will now look something like the following.

.. image:: ./_static/destination_of_pyd.jpg

.. note::

    Python 2.7 64 bit should be pasted below rocketcea at rocketcea\\py27_64

.. image:: ./_static/py27_64bit_location.jpg


Re-Test RocketCEA
~~~~~~~~~~~~~~~~~

Go back to `Test The Install`_ and run the test.

MinGW PATH Error
~~~~~~~~~~~~~~~~

A possible compile error is 
``TypeError: '>=' not supported between instances of 'NoneType' and 'str'``

.. image:: ./_static/bad_mingw_path.jpg



This occurs when the various path entries to the MinGW libraries in the batch file is incorrect, OR,
when the PATH to MinGW in the environment variables is wrong or not entered.

Go back and verify the PATH environment variable in `Make Batch File`_.

You should now be ready for another compile attempt at `Cross Your Fingers`_.

Windows Issues
--------------

Commands like::

    pip install rocketcea
    pip3.6 install rocketcea
    
should just work on Windows.

So far, the main issue I've had on a Windows platform is when python is installed in a directory
with a space in the name.  Any directories like ``C:/Python27`` or ``C:/Python37`` should work fine.

Another issue is with python 3.7 64 bit.
At the time of this writing, matplotlib was not properly installing.
I made matplotlib a dependency of RocketCEA and I assume that python 3.7 will have working support for 
matplotlib soon.

Ubuntu Linux Issues
-------------------


Commands like::

    pip install rocketcea
    pip3 install rocketcea
    
may well fail with any number of messages.

The most common problems can be solved by first installing dependencies like the following.::

    sudo apt-get install python-pip
    sudo apt-get install python-matplotlib
    sudo apt-get install python-tkinter

    OR

    sudo apt-get install python3-pip
    sudo apt-get install python3-matplotlib
    sudo apt-get install python3-tkinter

    AND PERHAPS

    sudo apt-get install libfreetype6-dev
    sudo apt-get install pkg-config
    sudo apt-get install libgfortran3:i386
    sudo pip install cairocffi
    sudo apt-get install python-gi-cairo

