OpenWhisk SPL implementation for the paper [*Formal Foundations of Serverless Computing*](https://arxiv.org/abs/1902.05870).

### Deploying OpenWhisk using Docker container on Ubuntu 14.04
Deployment has been tested on Ubuntu 14.04

* Install pre-requisites 
  * ```cd tools/ubuntu-setup && ./all.sh ```
  * sudo apt-get install python-pip
  * sudo pip install ansible==2.3.0.0
  * sudo pip install jinja2==2.9.6
* Make sure you are able to get all running docker container using ```docker ps```. 
* If not then add yourself to docker group, and relogin
  * ```sudo usermod -aG docker $USER```
  * ```newgrp docker```
  * ```bash -l```
* We will use an ephemeral CouchDB with OpenWhisk
* Compiling OpenWhisk
  * Go to OpenWhisk direcory ```cd <home_openwhisk>```
  * ```./gradlew distDocker```
  * ```printf "systemProp.http.proxyHost=localhost \n systemProp.http.proxyPort=3128" | cat > ~/.gradle/gradle.properties```
  * ```cd ansible```
* We will use the local environment, hence, no need to provide a new environment explicitly.
  * ```ansible-playbook setup.yml```
  * ```ansible-playbook prereq.yml```
  * ```ansible-playbook couchdb.yml```
  * ```ansible-playbook initdb.yml```
  * ```ansible-playbook wipe.yml```
  * ```ansible-playbook apigateway.yml```
  * ```ansible-playbook openwhisk.yml```
  * ```ansible-playbook postdeploy.yml```
* Check that different docker containers are running:
  * ```docker ps```
* Clone and compile OpenWhisk CLI from https://github.com/plasma-umass/openwhisk-cli/ 
* Go to directory where CLI Binaries are
  * ```cd <openwhisk_home>/bin```
* Set 172.17.0.1 as api host address and auth key
  * ```./wsk property set --apihost 172.17.0.1```
* Auth key can be manually setup to    
  * ```./wsk property set --auth 23bc46b1-71f6-4ed5-8c54-816aa4f8c502:123zO3xZCLrMN6v2BKK1dXYFpXlPkccOFqm12CdAsMgRU4VrNZ9lyGVCGuMDGIwP```
* Check if list namespaces contains a guest namespace, otherwise there is something wrong.
```./wsk -i namespace list```

### To Redeploy Openwhisk after changing source code or a restart
* Change directory to OpenWhisk
  * ```cd <openwhisk_home>```
* Compile OpenWhisk
  * ```./gradlew distDocker```
  * ```cd ansible/ && ansible-playbook -i environments/local openwhisk.yml -e mode=clean```
  * ```ansible-playbook -i environments/local openwhisk.yml```

### Invoke actions through HTTP interface
```curl -v -s -k https://172.17.0.1/api/v1/namespaces/whisk.system/actions/utils/echo?blocking=true\&result=true -X POST -H "Authorization: BaszFmNi00ZWQ1LThjNTQtODE2YWE0ZjhjNTAyOjEyM3pPM3haQ0xyTU42djJCS0sxZFhZRnBYbFBrY2NPRnFtMTJDZEFzTWdSVTRWck5aOWx5R1ZDR3VNREdJd1A=" -H "Content-Type: application/json" -d '{"text":"hello"}'```

## Known Issues

* Incase of nginx error, ```sudo killall nginx```
* If there are permission errors, then ``` sudo chown -R <username> /tmp/wsk*```
