
rem 1st do 32 bit 
call run_setup_build_win32.bat 35
python .\rocketcea\examples\quick_chk.py > build_results.txt

call run_setup_build_win32.bat 36
python .\rocketcea\examples\quick_chk.py >> build_results.txt

call run_setup_build_win32.bat 37
python .\rocketcea\examples\quick_chk.py >> build_results.txt

call run_setup_build_win32.bat 27
python .\rocketcea\examples\quick_chk.py >> build_results.txt

rem then do 64 bit 
call run_setup_build_win64.bat 35
python .\rocketcea\examples\quick_chk.py >> build_results.txt

call run_setup_build_win64.bat 36
python .\rocketcea\examples\quick_chk.py >> build_results.txt

call run_setup_build_win64.bat 37
python .\rocketcea\examples\quick_chk.py >> build_results.txt

call run_setup_build_win64.bat 27
python .\rocketcea\examples\quick_chk.py >> build_results.txt

rem display final results
type build_results.txt

