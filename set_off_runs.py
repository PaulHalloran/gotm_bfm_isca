import glob
import subprocess
import tempfile
import shutil
import os
import subprocess


def replace(file_path, pattern, subst):
    #Create temp file
    fh, abs_path = tempfile.mkstemp()
    with os.fdopen(fh,'w') as new_file:
        with open(file_path) as old_file:
            for line in old_file:
                new_file.write(line.replace(pattern, subst))
    shutil.move(abs_path, file_path)


base_directory = '/gpfs/ts0/home/ph290/gotm_090518/'
directory = base_directory+'GOTM_CASES/'
template_directory = 'OG_template'
generic_run_name= 'OG_ensemble_not_set_ulim_'
my_start_date = "1958-01-01 12:00:00"
my_end_date = "2007-12-31 12:00:00"
#my_start_date = "1900-01-01 12:00:00"
#my_end_date = "2008-12-31 12:00:00"
estimated_time_of_run = "00:04:00:00" #days:hours:minutes:seconds
param_old = ['p_sum = 0.13']
param_new = ['p_sum = 0.13']


for i,dummy in enumerate(param_old):
    try:
        subprocess.call(['rm -rf '+directory+generic_run_name+str(i)], shell=True)
    except:
        print 'no directory to delete'
    try:
        subprocess.call(['rm -rf '+base_directory+'gotm_out/'+generic_run_name+str(i)], shell=True)
    except:
        print 'no directory to delete'

    subprocess.call(['cp -r '+directory+template_directory+' '+directory+generic_run_name+str(i)], shell=True)

    replace(directory+generic_run_name+str(i)+'/'+'run_gotm','my_new_start_date', my_start_date)
    replace(directory+generic_run_name+str(i)+'/'+'run_gotm','my_new_end_date', my_end_date)
    replace(directory+generic_run_name+str(i)+'/'+'run_gotm','my_output_name', generic_run_name+str(i))
    replace(directory+generic_run_name+str(i)+'/'+'run_gotm','my_output_directory', base_directory+'gotm_out')
    replace(directory+generic_run_name+str(i)+'/'+'run_gotm','my_compilation_location', base_directory.split('/')[-2])

    replace(directory+generic_run_name+str(i)+'/'+'gotmrun.inp','my_new_start_date', my_start_date)
    replace(directory+generic_run_name+str(i)+'/'+'gotmrun.inp','my_new_end_date', my_end_date)
    replace(directory+generic_run_name+str(i)+'/'+'gotmrun.inp','my_output_name', generic_run_name+str(i))
    replace(directory+generic_run_name+str(i)+'/'+'gotmrun.inp','my_output_directory', base_directory+'gotm_out')

    replace(directory+generic_run_name+str(i)+'/'+'runscript','replace_time_estimate', estimated_time_of_run)
 
    replace(directory+generic_run_name+str(i)+'/'+'FilterFeeder.nml',param_old[i], param_new[i])
    subprocess.call(['mkdir '+base_directory+'gotm_out/'+generic_run_name+str(i)], shell=True)

    os.chdir(directory+generic_run_name+str(i))
    subprocess.call(['chmod 777 '+directory+generic_run_name+str(i)+'/run_gotm'], shell=True)
    subprocess.call(['msub runscript'], shell=True)

