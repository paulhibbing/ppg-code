## Run file by selecting all the text (ctrl + a) and then
## clicking 'Run -> Run Selection'.
## (For the last step, you can also just press F9 on Windows.)

## YOU NEED TO CLOSE OCTAVE AFTER RUNNING THIS CODE

[old_path]=which('rdsamp'); if(~isempty(old_path)) rmpath(old_path(1:end-8)); end
wfdb_url='https://physionet.org/physiotools/matlab/wfdb-app-matlab/wfdb-app-toolbox-0-10-0.zip';
[filestr,status] = urlwrite(wfdb_url,'wfdb-app-toolbox-0-10-0.zip');
unzip('wfdb-app-toolbox-0-10-0.zip');
cd mcode
addpath(pwd)
savepath

## CLOSE OCTAVE AFTER THIS CODE IS FINISHED RUNNING

