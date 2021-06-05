Teaming tests are done in vagrant instead of docker to avoid docker-in-docker issues. Those can be avoided by sharing the host system's docker, BUT then all containers are siblings. For this use case, we give containers specific names on each host -- therefore, for docker-in-docker, that would need to change. To avoid all of that complexity, we eat the disk-space complexity of using vagrant instead.

To run the teaming tests:

0. Run `./workbench.py pdf build/test_teaming` to specify the FAW distribution to be tested
1. Install Vagrant, and `cd` to this folder
1a. Optionally reset past tests with `vagrant destroy -f`
2. Run `vagrant up`
3. Point your browser at localhost:8123
4. Optionally ssh into machines with e.g., `vagrant ssh work1` or `vagrant ssh work2`
5. Check all error messages, etc.
5a. Error messages via e.g. `vagrant ssh work1`, `docker ps`, `docker exec -it <container> /bin/bash`, `s6-logwatch /var/log/(observatory|dask-worker)/current`
6. Optionally bring down setup with `vagrant halt`, then soft restart with `vagrant up --provision` on changes.

