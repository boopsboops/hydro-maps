# hydro-maps

### Hydrological and topological maps.

Install using renv

```bash
# clone the respitory
git clone https://github.com/boopsboops/hydro-maps.git
cd hydro-maps

# install repos
sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt update
sudo apt install libudunits2-dev libgdal-dev libgeos-dev libproj-dev libsqlite3-dev libtbb-dev libnetcdf-dev

# install R with renv-installer
renv install 4.6.1

# restore packages with other renv
Rscript -e "renv::restore()"
```


This script should install the software on a conda environment on a remote server or machine that you don't have sudo for.

```bash
# clone the repository 
git clone https://github.com/boopsboops/hydro-maps.git
cd hydro-maps

# recreate the conda environment from the yaml
conda env create -f conda.yaml

# activate conda env
conda activate hydro-maps

# once you have finished analysis leave conda env
conda deactivate

# useful to nuke the whole env if you need to start fresh
conda remove --name hydro-maps --all
```
